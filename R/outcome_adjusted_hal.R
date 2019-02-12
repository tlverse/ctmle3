#' Run straitified Q fit and use the non-zero basis to restrict the basis for g fit.
#'
#' @param tmle_spec (tmle3_Spec) specifying the spec
#' @param tmle_task (tmle3_Task) specifying the data
#'
#' @return (Likelihood) initial likelihood object ready to be updated
#' @export
#' @importFrom sl3 Lrnr_hal9001 Lrnr_stratified
#'
#' @examples
#' # TO_DO
make_outcome_adjusted_likelihood <- function(tmle_spec, tmle_task, ...) {
  # stratified Q fit
  task_Q <- tmle_task$get_regression_task("Y")
  hal_Q <- sl3::Lrnr_hal9001$new(
    fit_type = "glmnet",
    n_folds = 3,
    use_min = TRUE,
    ...
  )
  stratified_Q <- sl3::Lrnr_stratified$new(
    lrnr = hal_Q,
    variable_stratify = task_Q$nodes$covariates[["A"]]
  )
  stratified_Q_fit <- stratified_Q$train(task_Q)

  union_basis_list <- function(lrnr) {
    # union two basis list
    basis_lists <- lapply(lrnr$fit_object, function(x) x$fit_object$basis_list)
    basis_list_union <- unique(do.call(c, basis_lists))
    return(basis_list_union)
  }
  basis_list_union <- union_basis_list(stratified_Q_fit)

  # start g fit
  task_g <- tmle_task$get_regression_task("A")
  hal_g <- sl3::Lrnr_hal9001$new(
    fit_type = "glmnet",
    n_folds = 3,
    use_min = TRUE,
    basis_list = basis_list_union
  )
  hal_g_hat <- hal_g$train(task = task_g)
  learner_list <- list(Y = stratified_Q_fit, A = hal_g_hat)

  # perform tmle.
  initial_likelihood <- tmle_spec$make_initial_likelihood(
    tmle_task,
    learner_list
  )
  return(initial_likelihood)
}
