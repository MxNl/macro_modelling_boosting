make_resampling_strategy <-
  function(train_test_split_object, target_variable) {
    train_test_split_object %>%
      training() %>%
      rsample::vfold_cv(
        v = NUMBER_OF_FOLDS,
        strata = {{target_variable}},
        breaks = NUMBER_OF_STRATA_BREAKS
      )
  }