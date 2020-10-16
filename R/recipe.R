make_recipe <- 
  ######## Test
  # train_test_split_object <- tar_read(train_test_split)
  #####
  
  function(train_test_split_object, target_variable) {
    recipe(training(train_test_split_object)) %>% 
      update_role({{target_variable}}, new_role = "outcome") %>%
      update_role(station_id, new_role = "ID") %>%
      update_role(all_numeric(), -all_outcomes(), -station_id, new_role = "predictor") %>% 
      step_normalize(all_predictors()) %>%
      # step_log(all_outcomes(), offset = 1e-5, skip = TRUE) %>%
      step_corr(all_predictors()) %>%
      step_nzv(all_predictors()) %>%
      prep()
  }