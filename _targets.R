
# Settings ----------------------------------------------------------------

library(targets)
library(tarchetypes)
library(future)

source("R/import_functions.R")
source("R/plot_functions.R")
source("R/processing_functions.R")
source("R/train_test_split.R")
source("R/resampling.R")
source("R/recipe.R")
source("R/model.R")
source("R/tuning_settings.R")
source("R/workflow.R")
source("R/tuning.R")
source("R/config.R")

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("parallel",
                            "doParallel",
                            "assertr",
                            "rmarkdown",
                            "corrr",
                            "ggpubr",
                            "ggtext",
                            "glue",
                            "vip",
                            "ggpointdensity",
                            "tidymodels",
                            "tidyverse",
                            "sf"))


# plan(multisession)


# Define targets
targets <- list(


  # Data import -------------------------------------------------------------

  tar_target(
    filepath_back_vals_filter_sf,
    "../macro_data_preparation/data_processed/back_vals_filter_sf.Rds",
    format = "file"
  ),
  tar_target(
    back_vals_filter_sf,
    read_rds(filepath_back_vals_filter_sf)
  ),

  tar_target(
    filepath_features,
    "../macro_data_preparation/data_processed/features.Rds",
    format = "file"
  ),
  tar_target(
    data_features,
    read_rds(filepath_features)
  ),


  # Data preparation --------------------------------------------------------

  tar_target(
    data_features_wo_orientations,
    summarise_orientations(data_features)
  ),

  tar_target(
    back_vals_filter_allpositive_sf,
    remove_negative_concentration_values(back_vals_filter_sf)
  ),

  tar_target(
    data_features_depth_added,
    add_feature_depth(
      back_vals_filter_allpositive_sf,
      data_features_wo_orientations
    )
  ),

  tar_target(
    parameters_to_model,
    HYDROGEOCHEMICAL_PARAMS_MAJOR_IONS[c(1, 8)]
  ),
  
  tar_target(
    data_features_target,
    map(
      parameters_to_model,
      ~bind_target_to_features_and_filter_NA_rows(
      back_vals_filter_allpositive_sf,
      data_features_depth_added,
      .
      )
    )
  ),
  

  # Modelling ---------------------------------------------------------------

  tar_target(
    train_test_split,
    map(
      data_features_target,
      make_train_test_split
    ) %>% 
      set_names(parameters_to_model)
  ),

  tar_target(
    resampling_strategy_cv,
    map(
      train_test_split,
      make_resampling_strategy
    )
  ),

  tar_target(
    preprocessing_recipe,
    map(
      train_test_split,
      make_recipe
    ) %>% 
      set_names(parameters_to_model)
  ),

  tar_target(
    xgboost_model,
    make_model()
  ),

  tar_target(
    xgboost_params,
    make_tuning_parameter_set()
  ),

  tar_target(
    xgboost_grid,
    make_tuning_strategy(xgboost_params)
  ),

  tar_target(
    xgboost_workflow,
    map(
      preprocessing_recipe,
      ~make_workflow(xgboost_model, .)
    ) %>% 
      set_names(parameters_to_model)
  ),

  tar_target(
    xgboost_tuned,
    map2(
      xgboost_workflow,
      resampling_strategy_cv,
      ~tune_model(.x, .y, xgboost_grid)
    ) %>% 
      set_names(parameters_to_model)
  ),

  tar_target(
    xgboost_model_final_params,
    map(
      xgboost_tuned,
      get_best_model_params
    ) %>% 
      set_names(parameters_to_model)
  ),

  tar_target(
    xgboost_workflow_final,
    map2(
      xgboost_workflow,
      xgboost_model_final_params,
      make_final_workflow
    ) %>% 
      set_names(parameters_to_model)
  ),

  tar_target(
    xgboost_model_final_fit,
    map2(
      xgboost_workflow_final,
      train_test_split,
      make_final_model_fit
    ) %>% 
      set_names(parameters_to_model)
  ),

  tar_target(
    prediction_testsplit,
    map2(
      xgboost_workflow_final,
      train_test_split,
      predict_final_model_fit_on_testsplit
    ) %>% 
      set_names(parameters_to_model)
  ),
  
  ####
  # tar_target(
  #   xgboost_workflow_final,
  #   fit_best_model_on_training_split(xgboost_model_final, preprocessing_recipe, train_test_split)
  # ),
  

  # Plots -------------------------------------------------------------------

  
  
  tar_target(
    plot_violin_distribution_targets,
    make_plot_violin_distribution_targets(
      back_vals_filter_sf,
      parameters_to_model
    )
  ),
  
  tar_target(
    plot_histogram_distribution_features,
    make_plot_histogram_distribution_features(
      data_features_depth_added,
      20
    )
  ),
  
  tar_target(
    interactive_correlation_plot,
    make_interactive_correlation_plot(
      data_features_depth_added
    )
  ),


  tar_target(
    plot_train_test_split,
    imap(
      train_test_split,
      make_plot_train_test_split
    )
  ),

  tar_target(
    plot_feature_importance,
    imap(
      xgboost_model_final_fit,
      make_plot_feature_importance
    )
  ),

  tar_target(
    plot_observed_vs_predicted,
    imap(
      prediction_testsplit,
      make_plot_observed_vs_predicted
    )
  ),

  tar_target(
    plot_residuals_vs_predicted,
    imap(
      prediction_testsplit,
      make_plot_residuals_vs_predicted
    )
  ),

  # Report ------------------------------------------------------------------

  tar_render(
    report,
    "macro_modelling_boosting.Rmd"
  )

  
  
)

tar_pipeline(targets)


# targets::tar_make_future(workers = future::availableCores())
