#' Defines a OAT TML Estimator (except for the data)
#'
#' will be subclassed into TSM
#'
#' @importFrom R6 R6Class
#'
#' @export
#
tmle3_Spec_oat <- R6Class(
  classname = "tmle3_Spec_oat",
  portable = TRUE,
  class = TRUE,
  inherit = tmle3::tmle3_Spec,
  public = list(
    make_initial_likelihood = function(tmle_task, learner_list = NULL) {
      # tmle_task is training task
      # Q fit learner (learner_list[["Y"]]) has to be hal9001 (test?)
      # produce trained likelihood when likelihood_def provided
      likelihood_def <- self$options$likelihood_override
      if (!is.null(likelihood_def)) {
        likelihood <- likelihood_def$train(tmle_task)
      } else {
        #generate LF_Q
        LF_Q <- define_lf(LF_fit, "Y", learner = learner_list[["Y"]], type = "mean")
        LF_Q_delayed <- LF_Q$delayed_train(tmle_task)
        Q_learner_fit <- LF_Q_delayed$compute()
        LF_Q$train(tmle_task, Q_learner_fit)

        # browser()
        factor_list <- list(
          define_lf(LF_emp, "W"),
          define_lf(LF_oat, "A", learner = learner_list[["A"]], LF_Q = LF_Q),
          LF_Q
        )

        likelihood_def <- Likelihood$new(factor_list)

        # fit_likelihood
        likelihood <- likelihood_def$train(tmle_task)
      }
      return(likelihood)
    }
  ),
  active = list(
    options = function() {
      return(private$.options)
    }
  ),
  private = list(
    .options = NULL
  )
)