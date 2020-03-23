# devtools::load_all("~/tmle3")
remotes::install_github("tlverse/tmle3@LF_targeted")
library(here)
library(tmle3)
library(data.table)
library(sl3)
library(SuperLearner)

# Load data
dat <- read.csv(here::here("get_data", "creditCardMod3.csv"))
dat <- data.table(dat)
covs <- colnames(dat[,-c("Y", "A")])

# Establish sequence (covs 18:22 are IVs)
g_models <- list()
for(i in 1:length(covs)){
  g_models[[i]] <- paste0("V", seq(1:i))
}

g_models <- g_models[1:10]

# Define tmle3 task and spec
node_list <- list(
  W = covs,
  A = "A",
  Y = "Y"
)
tmle_spec <- tmle_ATE(1,0)
tmle_task <- tmle_spec$make_tmle_task(dat, node_list)

# Set up SL library
grid_params = list(max_depth = c(2,4,6),
                   eta = c(0.01, 0.1, 0.2, 0.3))
grid = expand.grid(grid_params, KEEP.OUT.ATTRS = FALSE)
params_default = list(nthread = getOption("sl.cores.learners", 1))
xgb_learners = apply(grid, MARGIN = 1, function(params_tune) {
  do.call(Lrnr_xgboost$new, c(params_default, as.list(params_tune)))
})
lrnr_mean <- make_learner(Lrnr_mean)
lrnr_glm <- make_learner(Lrnr_glm)

mn_metalearner <- make_learner(Lrnr_solnp, 
                               loss_function = metalearner_linear_multinomial,
                               learner_function = loss_loglik_multinomial)
qb_metalearner <- make_learner(Lrnr_solnp, 
                               loss_function = loss_loglik_binomial, 
                               learner_function = metalearner_logistic_binomial) 
metalearner <- make_learner(Lrnr_nnls)

sl_Y <- Lrnr_sl$new(learners = unlist(list(lrnr_mean, lrnr_glm, xgb_learners), 
                                      recursive = TRUE),
                    metalearner = qb_metalearner)
sl_A <- Lrnr_sl$new(learners = unlist(list(lrnr_mean, lrnr_glm, xgb_learners), 
                                      recursive = TRUE),
                    metalearner = qb_metalearner)

################################################################################

################################################################################

################################################################################
# generate_triplets ---
# generates all triplets or stops based on a penatly
################################################################################

generate_triplets <- function(tmle_spec, tmle_task, sl_Y, sl_A, g_models,
                              convergence_type, cv_tmle = TRUE, all = TRUE, 
                              penalty_type = NULL){
  
  # retreive relevant tasks
  task_Q <- tmle_task$get_regression_task("Y")
  task_g <- tmle_task$get_regression_task("A")
  
  # Train SL for Q 
  Q_fit <- sl_Y$train(task_Q)
  
  tmle_fits <- list()
  risks <- list()
  n_clever_covs <- list() 
  
  # build initial triplet
  g_tasks[[1]] <- task_g$next_in_chain(covariates = g_models[[1]])
  g_fits[[1]] <- sl_A$train(g_tasks[[1]])
  learner_lists[[1]] <- list(A = g_fits[[1]], Y = Q_fit)
  initial_likelihood <- tmle_spec$make_initial_likelihood(tmle_task,
                                                          learner_lists[[1]])
  tmle_fits[[1]] <- do_tmle_fit(tmle_spec, tmle_task, initial_likelihood, 
                          cv_tmle, convergence_type)
  risks[[1]] <- mean(calc_empirical_loss(tmle_fits[[1]], tmle_task))
  
  n_clever_covs[[1]] <- 1
  
  # build following triplets based on the previous triplet
  for(i in 2:length(g_models)){
    # set up i-specific g_task, g_fit, learner_list, lf_A
    g_task <- task_g$next_in_chain(covariates = g_models[[i]])
    g_fit <- sl_A$train(g_task)
    learner_list <- list(A = g_fit, Y = Q_fit)
    lf_A <- LF_fit$new("A", learner_list[["A"]])
    lf_W <- LF_emp$new("W")
    
    # use previous *initial* likelihood as current initial likelihood 
    lf_Y <- LF_targeted$new("Y", tmle_fits[[i-1]]$likelihood$initial_likelihood)
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
    
    if(all == FALSE){
      if(penalty_type == "AIC"){
        AIC_current <- 2*n_clever_covs[[i]] + 2*n*risks[[i]]
        AIC_prior <- 2*n_clever_covs[[i-1]] + 2*n*risks[[i-1]]
        if(AIC_current > AIC_prior){
          break
        }
      }
      if(penalty_type == "BIC"){
        BIC_current <- log(n)*n_clever_covs[[i]] + 2*n*risks[[i]]
        BIC_prior <- log(n)*n_clever_covs[[i-1]] + 2*n*risks[[i-1]]
        if(BIC_current > BIC_prior){
          break
        }
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
# stopping_criteria_penalty_full ---
# generates all triplets and selects optimal as minimizer of BIC or AIC
################################################################################
stopping_criteria_penalty_full <- function(data, tmle_spec, tmle_task, sl_Y, 
                                           sl_A, g_models, convergence_type,
                                           penalty_type){
  
  triplets <- generate_triplets(tmle_spec, tmle_task, sl_Y, sl_A, g_models, 
                                convergence_type) 
  n <- nrow(tmle_task$data)
  n_clever_covs <- unlist(triplets$n_clever_covs)
  risks <- unlist(triplets$risks)

  if(penalty_type == "AIC"){
    AICs <- 2*n_clever_covs + 2*n*risks
    AIC_k <- which.min(AICs)
    return(AIC_k)
  }
  if(penalty_type == "BIC"){
    BICs <- log(n)*n_clever_covs + 2*n*risks
    BIC_k <- which.min(BICs)
    return(BIC_k)
  }
}

################################################################################
# stopping_criteria_penalty_fast ---
# generates triplets until increase in log-likelihood does not beat BIC/AIC 
################################################################################
# increase LL by 2 vs increase LL by log(n)
stopping_criteria_penalty_fast <- function(data, tmle_spec, tmle_task, sl_Y, 
                                           sl_A, g_models, convergence_type,
                                           type, penalty_type){
  triplets <- generate_triplets(tmle_spec, tmle_task, sl_Y, sl_A, g_models, 
                                convergence_type, all = FALSE, 
                                penalty_type = penalty_type)
  k <- length(triplets[["risks"]]) - 1
  return(k)
}

################################################################################
# stopping_criteria_cv (original) ---
# generates all triplets and selects optimal as minimizer CV empirical loss
################################################################################

stopping_criteria_cv <- function(data, tmle_spec, tmle_task, node_list,
                                 sl_Y, sl_A, g_models, convergence_type,
                                 cv_tmle, V = 5, strata_ids){
  folds <- origami::make_folds(data, V, strata_ids)
  all_losses <- cross_validate(generate_cv_losses, folds, data, tmle_spec, 
                               tmle_task, node_list, sl_Y, sl_A, g_models,
                               convergence_type, cv_tmle)
  k <- which.min(colMeans(all_losses$loss_validation))
  return(k)
}

################ stopping_criteria_cv --- internal funtions ####################

# generate triplets with training and calculate the loss with the validation
generate_cv_losses <- function(fold, data, tmle_spec, tmle_task, node_list,
                               sl_Y, sl_A, g_models, convergence_type, cv_tmle){
  training_data <- training(data)
  validation_data <- validation(data)
  tmle_task_training <- tmle_spec$make_tmle_task(training_data, node_list)
  tmle_task_validation <- tmle_spec$make_tmle_task(validation_data, node_list)
  triplets <- generate_triplets(tmle_spec, tmle_task_training, sl_Y, sl_A, 
                                g_models, convergence_type, cv_tmle)
  loss_validation <- sapply(triplets[["tmle_fits"]], calc_empirical_loss, 
                            tmle_task_validation)
  return(list(loss_validation = loss_validation))
}



ctmle <- function(data, tmle_spec, tmle_task, node_list, sl_Y, sl_A, g_models, 
                  stopping_criteria = c("cv", "penalty_full", "penalty_fast"),
                  penalty_type = c("AIC", "BIC"), V = 5, strata_ids,
                  convergence_type = "sample_size", cv_tmle = TRUE){
  
  if(stopping_criteria == "cv"){
    k <- stopping_criteria_cv(data, tmle_spec, tmle_task, node_list, sl_Y, sl_A, 
                              g_models, convergence_type, cv_tmle, V, 
                              strata_ids)
  }
  if(stopping_criteria == "penalty_full"){
    k <- stopping_criteria_penalty_full(data, tmle_spec, tmle_task, sl_Y, sl_A,
                                        g_models, convergence_type,
                                        penalty_type)
  }
  if(stopping_criteria == "penalty_fast"){
    k <- stopping_criteria_penalty_fast(data, tmle_spec, tmle_task, sl_Y, sl_A,
                                        g_models, convergence_type, 
                                        penalty_type)
  }
  ctmle <- generate_triplets(tmle_spec, tmle_task, sl_Y, sl_A, g_models[1:k],
                             cv_tmle)
  tmle_fit <- ctmle[["tmle_fits"]][[k]]
  return(tmle_fit)
}


