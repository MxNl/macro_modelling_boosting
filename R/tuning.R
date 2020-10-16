tune_model <-
  function(workflow, resampling_strategy, tuning_strategy) {
    all_cores <- parallel::detectCores(logical = FALSE)
    cluster <- makePSOCKcluster(all_cores)
    registerDoParallel(cluster)
    
    xgboost_tuned <-
      tune::tune_grid(
        object = workflow,
        resamples = resampling_strategy,
        grid = tuning_strategy,
        metrics = yardstick::metric_set(rmse, rsq, mae),
        control = tune::control_grid(verbose = TRUE)
      )
    
    stopCluster(cluster)
    registerDoSEQ()
    
    return(xgboost_tuned)
  }

get_best_model_params <-
  function(model_tuned) {
    # xgboost_tuned %>%
    #   tune::show_best(metric = "rmse")
    
    xgboost_best_params <-
      model_tuned %>%
      tune::select_best(PERFORMANCE_METRIC_FOR_BESTMODEL_SELECTION)
    
    # xgboost_best_params
    
    # xgboost_model_final <-
    #   model %>%
    #   finalize_model(xgboost_best_params)
    
    return(xgboost_best_params)
  }


make_final_workflow <- 
  function(workflow, model_params) {
    workflow %>% 
      finalize_workflow(model_params)
  }


make_final_model_fit <- 
  function(workflow, train_test_split_object) {
    workflow %>% 
      fit(training(train_test_split_object))
  }

predict_final_model_fit_on_testsplit <-
  function(workflow, train_test_split_object) {
    workflow %>%
      last_fit(train_test_split_object)
  }


# fit_best_model_on_training_split <- 
#   function(model, recipe, train_test_split_object, target_variable) {
#     #### Test
#     # model <- xgboost_model_final
#     # recipe <- preprocessing_recipe
#     # train_test_split_object <- train_test_split
#     # target_variable <- target_ca_mg_l
#     ###
#     
#     train_processed <- 
#       recipe %>% 
#       bake(new_data = training(train_test_split_object))
#     
#     train_prediction <- 
#       model %>%
#       fit(
#         formula = target_ca_mg_l ~ ., 
#         data    = train_processed
#       )
#     
#     return(train_prediction)
#   }