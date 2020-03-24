#' Criteria to select among sequence of C-TMLE candidates
#'
#' These functions represent different criteria that can be used to select an 
#' optimal candidate among the sequence of different C-TMLE candidates.
#' 
#' @param tmle_spec a \code{\link[tmle3]{tmle3_Spec}} object
#' @param tmle_task a \code{\link[tmle3]{tmle3_Task}} object
#' @param Q_fit a trained \code{sl3} learner for Y likelihood factor 
#' @param g_fits a \code{list} of trained \code{sl3} learners for A likelihood 
#'         factor 
#' @param ... additional arguments to include in \code{\link[tmle3]{tmle3_Update}} 
#'         procedure
#' @return The index of the optimal candidate among the sequence of C-TMLEs.
#' @export

################################################################################

# revere uses CV-TMLE and optimal k is minimizer of CV (revere) empirical loss
criteria_revere <- function(tmle_spec, tmle_task, Q_fit, g_fits, ...){
  candidates <- generate_candidates(tmle_spec, tmle_task, Q_fit, g_fits, 
                                    cvtmle = TRUE)
  risks <- unlist(candidates$risks)
  k <- which.min(risks)
}

# cv (original) where optimal k is minimizer of CV (V-fold) empirical loss
criteria_cv <- function(data, tmle_spec, tmle_task, node_list, Q_fit, g_fits, 
                        ...){
  # stack corresponding to first fold 
  # Q_fit$fit_object$cv_fit$fit_object$fold_fits[[1]] 
  # tmle_task$folds[[1]]
  folds <- tmle_task$folds
  all_losses <- cross_validate(generate_cv_losses, folds, data, tmle_spec, 
                               tmle_task, node_list, Q_fit, g_fits, ...)
  k <- which.min(colMeans(all_losses$loss_validation))
  return(k)
}

# generate candidates with training and calculate the loss with the validation
generate_cv_losses <- function(fold, data, tmle_spec, tmle_task, node_list,
                               Q_fit, g_fits, ...){
  
  # tmle_task_training <- training(tmle_task)
  # training_data <- training(data)
  tmle_task_training <- tmle_spec$make_tmle_task(training_data, node_list)
  Q_fit <- Q_fit$fold_fit(fold_index())
  candidates <- generate_candidates(tmle_spec, tmle_task_training, Q_fit, 
                                    g_fits, ...)
  
  validation_data <- validation(data)
  tmle_task_validation <- tmle_spec$make_tmle_task(validation_data, node_list)
  loss_validation <- sapply(candidates[["tmle_fits"]], calc_empirical_loss, 
                            tmle_task_validation)
  return(list(loss_validation = loss_validation))
}