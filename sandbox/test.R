# devtools::load_all("~/tmle3")
remotes::install_github("tlverse/tmle3@devel")
remotes::install_github("tlverse/sl3@devel")
library(here)
library(tmle3)
library(data.table)
library(sl3)
library(SuperLearner)

# Load data
dat <- read.csv(here::here("get_data", "creditCardMod3.csv"))
dat <- data.table(dat)
covs <- colnames(dat[,-c("Y", "A")])

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
learner_list <- list(A = sl_A, Y = sl_Y) 
initial_likelihood <- tmle_spec$make_initial_likelihood(tmle_task, learner_list)

# initialize list of upper and lower bounds
lower_bounds <- c(1e-8, 1e-7, 1e-6, 1e-5, 1e-4, seq(1e-3, 9e-3, 5e-4), 
                    seq(1e-2, 9e-2, 5e-3), seq(.1, .3, 5e-3))
upper_bounds <- 1-lower_bounds
bounds <- list()
for(i in 1:length(lower_bounds)){
   bounds[[i]] <- c(lower_bounds[i], upper_bounds[i])
}

# subset to bounds in which truncation will occur (i.e. in range of preds)
task_g <- tmle_task$get_regression_task("A")
g_fit <- initial_likelihood$factor_list$A$learner
g_fit_range <- range(g_fit$predict())

bounds_in_range <- list()
for(i in 1:length(bounds)){
  if(g_fit_range[1] < bounds[[i]][1] | g_fit_range[2] > bounds[[i]][2]){
    bounds_in_range[[i]] <- bounds[[i]]
  } 
}
# incorporate bound of no truncation and in-range bounds
bounds_final <- c(list(bounds[[1]]), 
                  bounds_in_range[!sapply(bounds_in_range, is.null)])

# list of g_fits corresponding to different bounds
g_fits <- lapply(bounds_final, function(x){
  lrnr_bound <- Lrnr_bound$new(bound = x)
  bound_pipeline <- make_learner(Pipeline, g_fit, lrnr_bound)
  g_fit_bounded <- bound_pipeline$train(task_g)
  return(g_fit_bounded)
})

Q_fit <- initial_likelihood$factor_list$Y$learner


### testing generate_triplets function
tmle_fits <- list()
risks <- list()
n_clever_covs <- list() 

learner_list[[1]] <- list(A = g_fits[[1]], Y = Q_fit)
initial_likelihood <- tmle_spec$make_initial_likelihood(tmle_task,
                                                        learner_lists[[1]])
tmle_fits[[1]] <- do_tmle_fit(tmle_spec, tmle_task, initial_likelihood, 
                              cv_tmle, convergence_type)

## ERROR in do_tmle_fit (specifically fit_tmle3):
# Error in current_fit$chain_fold(current_task, fold_number) : 
# attempt to apply non-function
# Called from: learner$predict_fold(learner_task, fold_number)

# tmle_fit <- fit_tmle3(tmle_task, targeted_likelihood, tmle_params, updater)


