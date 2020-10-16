make_tuning_parameter_set <-
  function() {
    dials::parameters(
      min_n(),
      tree_depth(),
      learn_rate(),
      loss_reduction()
    )
  }

make_tuning_strategy <-
  function(tuning_parameter_set) {
    dials::grid_max_entropy(
      tuning_parameter_set,
      size = NUMBER_OF_HYPERPARAMETER_COMBINATIONS
    )
  }