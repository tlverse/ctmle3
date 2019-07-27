library(here)
library(tmle3)
library(data.table)
library(sl3)

lrnr_mean <- make_learner(Lrnr_mean)
lrnr_xgboost <- make_learner(Lrnr_xgboost)

ls_metalearner <- make_learner(Lrnr_nnls)
mn_metalearner <- make_learner(
  Lrnr_solnp, metalearner_linear_multinomial,
  loss_loglik_multinomial
)

qb_metalearner <- make_learner(Lrnr_solnp, 
                               loss_function = loss_loglik_binomial, 
                               learner_function = metalearner_logistic_binomial) 

sl_Y <- Lrnr_sl$new(
  learners = list(lrnr_mean, lrnr_xgboost),
  metalearner = ls_metalearner
)
sl_A <- Lrnr_sl$new(
  learners = list(lrnr_mean, lrnr_xgboost),
  metalearner = qb_metalearner
)
learner_list <- list(A = sl_A, Y = sl_Y)
dat <- read.csv(here::here("get_data", "sample_data", "high1.CSV"))
dat <- data.table(dat)
covs <- colnames(dat[,-c("Y", "A")])
node_list <- list(
  W = covs,
  A = "A",
  Y = "Y"
)

ate_spec <- tmle_ATE(1,0)
tmle_task <- ate_spec$make_tmle_task(dat, node_list)
initial_likelihood <- ate_spec$make_initial_likelihood(task, learner_list)
updater <- tmle3_Update$new()
targeted_likelihood <- Targeted_Likelihood$new(initial_likelihood, 
                                               updater = updater)
tmle_params <- spec$make_params(task, targeted_likelihood)
updater$register_param(tmle_params)
submodel_data <- updater$generate_submodel_data(initial_likelihood, tmle_task)
mean(updater$loss_function(submodel_data$Y$initial, submodel_data$Y$observed))
