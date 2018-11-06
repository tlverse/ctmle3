context("test-stratified_Q_fit")
set.seed(49753)
suppressMessages(library(data.table))
library(dplyr)
library(origami)
library(sl3)
library(hal9001)
library(tmle3)
library(ctmle3)

# load example data set
data(cpp)
cpp <- cpp %>%
  dplyr::filter(!is.na(haz)) %>%
  mutate_all(funs(replace(., is.na(.), 0)))
cpp$parity01 <- as.numeric(cpp$parity > 0)
cpp$haz01 <- as.numeric(cpp$haz > 0)
node_list <- list(
  W = c("apgar1", "apgar5", "sexn"),
  A = "parity01",
  Y = "haz"
)

tmle_spec <- tmle_TSM_all()
tmle_task <- tmle_spec$make_tmle_task(cpp, node_list)

hal_Q <- sl3::Lrnr_hal9001$new(
  fit_type = "glmnet",
  n_folds = 3,
  use_min = TRUE
)

#generate LF_Q
LF_Q <- define_lf(LF_fit, "Y", learner = hal_Q, type = "mean")
LF_Q_delayed <- LF_Q$delayed_train(tmle_task)
Q_learner_fit <- LF_Q_delayed$compute()
LF_Q$train(tmle_task, Q_learner_fit)











factor_list <- list(
  define_lf(LF_emp, "W"),
  define_lf(LF_oat, "A", learner = learner_list[["A"]]),
)

likelihood_def <- Likelihood$new(factor_list)

# fit_likelihood
likelihood <- likelihood_def$train(tmle_task)
return(likelihood)
initial_likelihood <- ctmle3::make_outcome_adjusted_likelihood(
    tmle_spec,
    tmle_task
    )
updater <- tmle3_Update$new()
targeted_likelihood <- Targeted_Likelihood$new(initial_likelihood, updater)
intervention <- define_lf(LF_static, "A", value = 1)
tsm <- define_param(Param_TSM, targeted_likelihood, intervention)
updater$tmle_params <- tsm
tmle_fit <- fit_tmle3(tmle_task, targeted_likelihood, list(tsm), updater)

# extract results
tmle3_psi <- tmle_fit$summary$tmle_est
tmle3_se <- tmle_fit$summary$se
tmle3_epsilon <- updater$epsilons[[1]]$Y
tmle3_psi
tmle3_se
tmle3_epsilon
