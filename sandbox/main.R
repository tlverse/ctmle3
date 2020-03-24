################################################################################

ctmle <- function(data, tmle_spec, tmle_task, node_list, Q_fit, g_fits, 
                  selection_criteria = c("cv", "revere"), ...){
  
  if(selection_criteria == "cv"){
    k <- criteria_cv(data, tmle_spec, tmle_task, node_list, Q_fit, g_fits, ...)
  }
  if(selection_criteria == "revere"){
    k <- criteria_revere(tmle_spec, tmle_task, Q_fit, g_fits, ...)
  }
  ctmle <- generate_candidates(tmle_spec, tmle_task, Q_fit, g_fits[1:k],
                               convergence_type, cv_tmle)
  tmle_fit <- ctmle[["tmle_fits"]][[k]]
  return(tmle_fit)
}

ctmle3_truncation <- function(tmle_spec, tmle_task, initial_likelihood, 
                              bounds = "default"){
  
  # initialize list of upper and lower bounds
  if(bounds == "default"){
    lower_bounds <- c(1e-8, 1e-7, 1e-6, 1e-5, 1e-4, seq(1e-3, 9e-3, 5e-4), 
                      seq(1e-2, 9e-2, 5e-3), seq(.1, .3, 5e-3))
    upper_bounds <- 1-lower_bounds
    bounds <- list()
    for(i in 1:length(lower_bounds)){
      bounds[[i]] <- c(lower_bounds[i], upper_bounds[i])
    }
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
  
  fit <- ctmle(data, tmle_spec, tmle_task, node_list, Q_fit, g_fits, 
               stopping_criteria = "cv_full", convergence_type = "sample_size", 
               cvtmle = TRUE)
}

