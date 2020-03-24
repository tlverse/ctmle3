#' Construction of a sequence of C-TMLE candidates
#'
#' Uses initial estimate for Q and initial g_fits to generate C-TMLEs
#'
#' @param tmle_spec a \code{\link[tmle3]{tmle3_Spec}} object
#' @param tmle_task a \code{\link[tmle3]{tmle3_Task}} object
#' @param Q_fit a trained \code{sl3} learner for Y likelihood factor 
#' @param g_fits a \code{list} of trained \code{sl3} learners for A likelihood 
#'         factor
#' @param ... additional arguments to include in \code{\link[tmle3]{tmle3_Update}} 
#'         procedure
#' @return A \code{g_fit}-specific \code{list} of \code{\link[tmle3]{tmle3_Fit}} 
#'          objects, estimates empirical risk, and number of clever covariates 
#'          included in the update procedure of the tmle.
#' @export

################################################################################

generate_candidates <- function(tmle_spec, tmle_task, Q_fit, g_fits, ...){
  
  tmle_fits <- list()
  risks <- list()
  n_clever_covs <- list() 
  
  learner_list[[1]] <- list(A = g_fits[[1]], Y = Q_fit)
  initial_likelihood <- tmle_spec$make_initial_likelihood(tmle_task,
                                                          learner_lists[[1]])
  tmle_fits[[1]] <- do_tmle_fit(tmle_spec, tmle_task, initial_likelihood, ...)
  risks[[1]] <- mean(calc_empirical_loss(tmle_fits[[1]], tmle_task))
  
  n_clever_covs[[1]] <- 1
  
  # build following candidates based on previous triplet (Q_fit, g_fit, tmle_fit)
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
      tmle_fit <- do_tmle_fit(tmle_spec, tmle_task, initial_likelihood, ...)
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
                                      ...)
        risks[[i]] <- mean(calc_empirical_loss(tmle_fits[[i]], tmle_task))
        n_clever_covs[[i]] <- n_clever_covs[[i-1]] + 1
      }
    }
  }
  return(list(tmle_fits = tmle_fits,
              risks = risks, 
              n_clever_covs = n_clever_covs))
}

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
do_tmle_fit <- function(tmle_spec, tmle_task, initial_likelihood, ...){
  updater <- tmle3_Update$new(...)
  targeted_likelihood <- Targeted_Likelihood$new(initial_likelihood, 
                                                 updater = updater)
  tmle_params <- tmle_spec$make_params(tmle_task, targeted_likelihood)
  updater$register_param(tmle_params)
  tmle_fit <- fit_tmle3(tmle_task, targeted_likelihood, tmle_params, updater)
  return(tmle_fit)
}