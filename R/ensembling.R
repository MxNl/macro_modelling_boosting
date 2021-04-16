make_stack <- 
  function(xgboost_tuned, nnet_tuned) {
    stacks() %>% 
      add_candidates(xgboost_tuned) %>% 
      add_candidates(nnet_tuned)
  }