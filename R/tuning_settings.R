make_xgboost_tuning_parameter_set <-
  function() {
    dials::parameters(
      min_n(),
      tree_depth(),
      learn_rate(),
      loss_reduction()
    )
  }

make_nnet_tuning_parameter_set <-
  function() {
    dials::parameters(
      hidden_units(),
      penalty(),
      # dropout = sample_prop(),
      epochs()#,
      # activation()
    )
  }

make_xgboost_tuning_strategy <-
  function(tuning_parameter_set) {
    dials::grid_latin_hypercube(
      tuning_parameter_set,
      size = NUMBER_OF_HYPERPARAMETER_COMBINATIONS
    )
  }

make_nnet_tuning_strategy <-
  function(tuning_parameter_set) {
    dials::grid_latin_hypercube(
      tuning_parameter_set,
      size = NUMBER_OF_HYPERPARAMETER_COMBINATIONS_NNET
    )
  }