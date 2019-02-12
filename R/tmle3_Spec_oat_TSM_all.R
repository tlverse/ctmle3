#' Defines a OAT TML Estimator (except for the data)
#'
#' OAT + TSM
#'
#' @importFrom R6 R6Class
#'
#' @export
#
tmle3_Spec_oat_TSM_all <- R6Class(
  classname = "tmle3_Spec_oat_TSM_all",
  portable = TRUE,
  class = TRUE,
  inherit = tmle3_Spec_oat,
  public = list(
    initialize = function(...) {
      super$initialize(...)
    },
    make_params = function(tmle_task, likelihood) {
      A_vals <- tmle_task$get_tmle_node("A")
      if (is.factor(A_vals)) {
        A_levels <- levels(A_vals)
        A_levels <- factor(A_levels, A_levels)
      } else {
        A_levels <- sort(unique(A_vals))
      }
      tmle_params <- lapply(A_levels, function(A_level) {
        intervention <- define_lf(LF_static, "A", value = A_level)
        tsm <- Param_TSM$new(likelihood, intervention)
        return(tsm)
      })
      return(tmle_params)
    }
  ),
  active = list(),
  private = list()
)

#' [OAT] All Treatment Specific Means
#'
#' O=(W,A,Y)
#' W=Covariates
#' A=Treatment (binary or categorical)
#' Y=Outcome (binary or bounded continuous)
#' @importFrom sl3 make_learner Lrnr_mean
#' @export
tmle_oat_TSM_all <- function() {
  # TODO: unclear why this has to be in a factory function
  tmle3_Spec_oat_TSM_all$new()
}