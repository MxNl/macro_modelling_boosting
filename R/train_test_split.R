make_train_test_split <-
  function(data) {
    data %>%
      initial_split(
        prop = TRAIN_TEST_SPLIT_PROPORTION,
        strata = target,
        breaks = NUMBER_OF_STRATA_BREAKS
      )
  }
