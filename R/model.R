make_xgboost_model <-
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

make_neuralnet_model <-
  function() {
    mlp(
      mode = "regression",
      hidden_units = tune(),
      penalty = tune(),
      # dropout = tune(),
      epochs = tune()#,
      # activation = tune()
    ) %>%
      set_engine("nnet")
  }
