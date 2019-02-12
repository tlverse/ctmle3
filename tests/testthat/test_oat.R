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

tmle_spec <- tmle_oat_TSM_all()
tmle_task <- tmle_spec$make_tmle_task(cpp, node_list)

# WILSON: learner_list[["Y"]] = hal_Q
hal_Q <- sl3::Lrnr_hal9001$new(
  fit_type = "glmnet",
  n_folds = 3,
  use_min = TRUE
)
learner_list <- list(Y = hal_Q, A = hal_Q)
oatmle_fit <- tmle3::tmle3(tmle_spec, cpp, node_list, learner_list = learner_list)
# extract results
tmle3_psi <- oatmle_fit$summary$tmle_est
tmle3_se <- oatmle_fit$summary$se
tmle3_epsilon <- oatmle_fit$updater$epsilons[[1]]$Y
tmle3_psi
tmle3_se
tmle3_epsilon
