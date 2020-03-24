#remotes::install_github("tlverse/tmle3@devel")
#remotes::install_github("tlverse/sl3@devel")
library(here)
library(tmle3)
library(data.table)
library(sl3)

################################################################################
# generate_triplets ---
# generates all triplets or stops based on a penatly
################################################################################

generate_triplets <- function(tmle_spec, tmle_task, Q_fit, g_fits, 
                              convergence_type, cv_tmle){
  
  tmle_fits <- list()
  risks <- list()
  n_clever_covs <- list() 
  
  learner_list[[1]] <- list(A = g_fits[[1]], Y = Q_fit)
  initial_likelihood <- tmle_spec$make_initial_likelihood(tmle_task,
                                                          learner_lists[[1]])
  tmle_fits[[1]] <- do_tmle_fit(tmle_spec, tmle_task, initial_likelihood, 
                                cv_tmle, convergence_type)
  risks[[1]] <- mean(calc_empirical_loss(tmle_fits[[1]], tmle_task))
  
  n_clever_covs[[1]] <- 1
  
  # build following triplets based on the previous triplet
  if(length(g_fits) > 1){
    for(i in 2:length(g_fits)){
      # set up i-specific g_task, g_fit, learner_list, lf_A
      learner_list <- list(A = g_fits[[i]], Y = Q_fit)
      lf_A <- LF_fit$new("A", learner_list[["A"]])
      lf_W <- LF_emp$new("W")
      
      # use previous *initial* likelihood as current initial likelihood 
      lf_Y <- tmle3:::LF_targeted$new("Y", tmle_fits[[i-1]]$likelihood$initial_likelihood)
      factor_list <- list(lf_W, lf_A, lf_Y)
      likelihood_def <- Likelihood$new(factor_list)
      initial_likelihood <- likelihood_def$train(tmle_task)
      tmle_fit <- do_tmle_fit(tmle_spec, tmle_task, initial_likelihood, cv_tmle,
                              convergence_type)
      risk <- mean(calc_empirical_loss(tmle_fit, tmle_task))
      
      if(risks[[i-1]] > risk){
        risks[[i]] <- risk
        tmle_fits[[i]] <- tmle_fit
        n_clever_covs[[i]] <- n_clever_covs[[i-1]]
      }
      if(risks[[i-1]] <= risk){
        # use previous *targeted* likelihood as current initial likelihood
        lf_targ <- tmle3:::LF_targeted$new("Y", tmle_fits[[i-1]]$likelihood)
        factor_list <- list(lf_W, lf_A, lf_targ)
        likelihood_def <- Likelihood$new(factor_list)
        initial_likelihood <- likelihood_def$train(tmle_task)
        tmle_fits[[i]] <- do_tmle_fit(tmle_spec, tmle_task, initial_likelihood, 
                                      cv_tmle, convergence_type)
        risks[[i]] <- mean(calc_empirical_loss(tmle_fits[[i]], tmle_task))
        n_clever_covs[[i]] <- n_clever_covs[[i-1]] + 1
      }
    }
  }
  return(list(tmle_fits = tmle_fits,
              risks = risks, 
              n_clever_covs = n_clever_covs))
}

################## generate_triplets --- internal funtions #####################

# calculates the empirical loss of a targeted Q
calc_empirical_loss <- function(tmle_fit, task){
  updater <- tmle_fit$updater
  submodel_data <- updater$generate_submodel_data(tmle_fit$likelihood, 
                                                  task, updater$update_fold)
  loss <- updater$loss_function(submodel_data$Y$initial, 
                                submodel_data$Y$observed)
  return(loss)
}

# fit tmle3 to store relevant objects
do_tmle_fit <- function(tmle_spec, tmle_task, initial_likelihood, cv_tmle,
                        convergence_type){
  updater <- tmle3_Update$new(cvtmle = cv_tmle, 
                              convergence_type = convergence_type)
  targeted_likelihood <- Targeted_Likelihood$new(initial_likelihood, 
                                                 updater = updater)
  tmle_params <- tmle_spec$make_params(tmle_task, targeted_likelihood)
  updater$register_param(tmle_params)
  tmle_fit <- fit_tmle3(tmle_task, targeted_likelihood, tmle_params, updater)
  return(tmle_fit)
}

################################################################################
# stopping_criteria_cv_revere  ---
# generates all triplets and selects optimal as minimizer CV empirical loss
################################################################################

stopping_criteria_cv_revere <- function(data, tmle_spec, tmle_task, Q_fit, 
                                        g_fits, convergence_type, cv_tmle){
  triplets <- generate_triplets(tmle_spec, tmle_task, Q_fit, g_fits, 
                                convergence_type, cv_tmle = TRUE)
  n_clever_covs <- unlist(triplets$n_clever_covs)
  risks <- unlist(triplets$risks)
  k <- which.min(risks)
  return(k)
}

################################################################################
# stopping_criteria_cv (original) ---
# generates all triplets and selects optimal as minimizer CV empirical loss
################################################################################

stopping_criteria_cv <- function(data, tmle_spec, tmle_task, node_list,
                                 Q_fit, g_fits, convergence_type, cv_tmle){
  # stack corresponding to first fold 
  # Q_fit$fit_object$cv_fit$fit_object$fold_fits[[1]] 
  # tmle_task$folds[[1]]
  folds <- tmle_task$folds
  all_losses <- cross_validate(generate_cv_losses, folds, data, tmle_spec, 
                               tmle_task, node_list, Q_fit, g_fits,
                               convergence_type, cv_tmle)
  k <- which.min(colMeans(all_losses$loss_validation))
  return(k)
}

################ stopping_criteria_cv --- internal funtions ####################

# generate triplets with training and calculate the loss with the validation
generate_cv_losses <- function(fold, data, tmle_spec, tmle_task, node_list,
                               Q_fit, g_fits, convergence_type, cv_tmle){
  
  # tmle_task_training <- training(tmle_task)
  # training_data <- training(data)
  tmle_task_training <- tmle_spec$make_tmle_task(training_data, node_list)
  Q_fit <- Q_fit$fold_fit(fold_index())
  triplets <- generate_triplets(tmle_spec, tmle_task_training, Q_fit, 
                                g_fits, convergence_type, cv_tmle)
  
  validation_data <- validation(data)
  tmle_task_validation <- tmle_spec$make_tmle_task(validation_data, node_list)
  loss_validation <- sapply(triplets[["tmle_fits"]], calc_empirical_loss, 
                            tmle_task_validation)
  return(list(loss_validation = loss_validation))
}

################################################################################
# ctmle ---
# put it all together
################################################################################

ctmle <- function(data, tmle_spec, tmle_task, node_list, Q_fit, g_fits, 
                  stopping_criteria = c("cv_full", "cv_revere"),
                  convergence_type = c("scaled_var", "sample_size"), 
                  cv_tmle){
  
  if(stopping_criteria == "cv_full"){
    k <- stopping_criteria_cv(data, tmle_spec, tmle_task, node_list, Q_fit, 
                              g_fits, convergence_type, cv_tmle)
  }
  if(stopping_criteria == "cv_revere"){
    k <- stopping_criteria_cv_revere(data, tmle_spec, tmle_task, node_list, 
                                     Q_fit, g_fits, convergence_type, 
                                     cv_tmle = TRUE)
  }
  ctmle <- generate_triplets(tmle_spec, tmle_task, Q_fit, g_fits[1:k],
                             convergence_type, cv_tmle)
  tmle_fit <- ctmle[["tmle_fits"]][[k]]
  return(tmle_fit)
}

ctmle3_truncation <- function(tmle_spec, tmle_task, initial_likelihood, 
                              bounds = "default"){
  
  # initialize list of upper and lower bounds
  if(bounds == "default"){
    lower_bounds <- c(1e-8, 1e-7, 1e-6, 1e-5, 1e-4, seq(1e-3, 9e-3, 5e-4), 
                      seq(1e-2, 9e-2, 5e-3), seq(.1, .3, 5e-3))
    upper_bounds <- 1-lower_bounds
    bounds <- list()
    for(i in 1:length(lower_bounds)){
      bounds[[i]] <- c(lower_bounds[i], upper_bounds[i])
    }
  }
  
  # subset to bounds in which truncation will occur (i.e. in range of preds)
  task_g <- tmle_task$get_regression_task("A")
  g_fit <- initial_likelihood$factor_list$A$learner
  g_fit_range <- range(g_fit$predict())
  
  bounds_in_range <- list()
  for(i in 1:length(bounds)){
    if(g_fit_range[1] < bounds[[i]][1] | g_fit_range[2] > bounds[[i]][2]){
      bounds_in_range[[i]] <- bounds[[i]]
    } 
  }
  # incorporate bound of no truncation and in-range bounds
  bounds_final <- c(list(bounds[[1]]), 
                    bounds_in_range[!sapply(bounds_in_range, is.null)])
    
  # list of g_fits corresponding to different bounds
  g_fits <- lapply(bounds_final, function(x){
    lrnr_bound <- Lrnr_bound$new(bound = x)
    bound_pipeline <- make_learner(Pipeline, g_fit, lrnr_bound)
    g_fit_bounded <- bound_pipeline$train(task_g)
    return(g_fit_bounded)
  })
  
  Q_fit <- initial_likelihood$factor_list$Y$learner
  
  fit <- ctmle(data, tmle_spec, tmle_task, node_list, Q_fit, g_fits, 
               stopping_criteria = "cv_full", convergence_type = "sample_size", 
               cvtmle = TRUE)
}

