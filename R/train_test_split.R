make_train_test_split <-
  function(data, target_variable) {
    data %>%
      initial_split(
        prop = TRAIN_TEST_SPLIT_PROPORTION,
        strata = {{target_variable}},
        breaks = 10
      )
  }
