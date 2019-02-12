#' Likelihood Factor perform outcome adjusted tmle using sl3.
#'
#' Uses an \code{sl3} learner to estimate OAT
#' Inherits from \code{\link{LF_fit}}; see that page for documentation on likelihood factors in general.
#'
#' @importFrom R6 R6Class
#' @importFrom uuid UUIDgenerate
#' @importFrom methods is
#' @family Likelihood objects
#' @keywords data
#'
#' @return \code{LF_fit} object
#'
#' @format \code{\link{R6Class}} object.
#'
#' @section Constructor:
#'   \code{define_lf(LF_oat, name, learner, ..., type = "density")}
#'
#'   \describe{
#'     \item{\code{name}}{character, the name of the factor. Should match a node name in the nodes specified by \code{\link{tmle3_Task}$npsem}
#'     }
#'     \item{\code{learner}}{An sl3 learner to be used to estimate the factor
#'     }
#'     \item{\code{...}}{Not currently used.
#'     }
#'     \item{\code{type}}{character, either "density", for conditional density or, "mean" for conditional mean
#'     }
#'     }
#'
#' @section Fields:
#' \describe{
#'     \item{\code{learner}}{The learner or learner fit object}
#'     }
#'
#' @export
LF_oat <- R6::R6Class(
  classname = "LF_oat",
  portable = TRUE,
  class = TRUE,
  inherit = LF_fit,
  public = list(
    LF_Q = NULL,
    tmle_task_training = NULL,
    initialize = function(
      name,
      learner,
      LF_Q,
      tmle_task_training = NULL,
      ...,
      type = "density"
      ) {
        super$initialize(name, learner, ..., type = type)
        self$LF_Q <- LF_Q
        self$tmle_task_training <- tmle_task_training
        private$.learner <- learner
    },
    delayed_train = function(tmle_task) {
      if (self$learner$is_trained) {
        return(self$learner)
      }

      self$tmle_task_training <- tmle_task
      outcome_node <- self$name
      learner_task <- self$create_regression_task(
        self$LF_Q,
        self$tmle_task_training,
        self$tmle_task_training
        )
      learner_fit <- delayed_learner_train(self$learner, learner_task)
      return(learner_fit)
    },
    get_mean = function(tmle_task, cv_fold) {
      # WILSON: I am not sure if user should only call this method directly,
      # since this LF only work for binomial or categorical outcome
      learner_task <- self$create_regression_task(
        self$LF_Q,
        tmle_task,
        self$tmle_task_training
      )
      learner <- self$learner
      if (cv_fold == -1) {
        preds <- learner$predict(learner_task)
      } else {
        preds <- learner$predict_fold(learner_task, cv_fold)
      }
      return(preds)
    },
    get_density = function(tmle_task, cv_fold) {
      learner_task <- self$create_regression_task(
        self$LF_Q,
        tmle_task,
        self$tmle_task_training
      )
      preds <- self$get_mean(tmle_task, cv_fold)
      outcome_type <- self$learner$training_task$outcome_type
      observed <- outcome_type$format(learner_task$Y)
      if (outcome_type$type == "binomial") {
        likelihood <- ifelse(observed == 1, preds, 1 - preds)
      } else if (outcome_type$type == "categorical") {
        unpacked <- sl3::unpack_predictions(preds)
        index_mat <- cbind(seq_along(observed), observed)
        likelihood <- unpacked[index_mat]
      } else if (outcome_type$type == "continuous") {
        likelihood <- unlist(preds)
      } else {
        stop(sprintf("unsupported outcome_type: %s", outcome_type$type))
      }
      return(likelihood)
    },
    create_regression_task = function(LF_Q, tmle_task, tmle_task_training){
      # helper function to make new design matrix given Q fit and tmle_task
      cv_fold <- -1
      #get Q values
      A_levels <- tmle_task_training$npsem[["A"]]$variable_type$levels
      A_values <- tmle_task$data[[tmle_task$npsem[["A"]]$variables]]
      Q_values <- lapply(A_levels, function(A_level){
        cf_task <- tmle_task$generate_counterfactual_task(
          uuid::UUIDgenerate(),
          data.table(A = A_level)
          )
        Q_value <- LF_Q$get_mean(cf_task, cv_fold)
        return(Q_value)
      })
      names(Q_values) <- sprintf("Q%sW", A_levels)
      covariates <- names(Q_values)
      Q_values <- data.table::as.data.table(Q_values)
      set(Q_values, , "A",A_values)
      g_task <- sl3::make_sl3_Task(
        Q_values,
        outcome = "A",
        covariates = covariates
        )
      return(g_task)
    }
  ),
  # WILSON: i am not sure to keep the followings or not?
  active = list(
    learner = function() {
      return(private$.learner)
    }
  ),
  private = list(
    .name = NULL,
    .learner = NULL
  )
)