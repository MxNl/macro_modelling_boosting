make_model <-
  function() {
    boost_tree(
      mode = "regression",
      trees = 1000,
      min_n = tune(),
      tree_depth = tune(),
      learn_rate = tune(),
      loss_reduction = tune()
    ) %>%
      set_engine("xgboost", objective = "reg:squarederror")
  }