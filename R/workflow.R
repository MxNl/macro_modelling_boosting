make_workflow <-
  function(model, recipe) {
    workflow() %>%
      add_model(model) %>%
      add_recipe(recipe)
  }