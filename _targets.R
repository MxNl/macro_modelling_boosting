
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
    add_feature_depth_and_filter_NA_rows(
      back_vals_filter_allpositive_sf,
      data_features_wo_orientations
    )
  ),

  tar_target(
    data_features_target,
    bind_target_to_features_and_filter_NA_rows(
      back_vals_filter_allpositive_sf,
      data_features_depth_added,
      "ca_mg_l"
    )
  ),
  

  # Modelling ---------------------------------------------------------------

  tar_target(
    train_test_split,
    make_train_test_split(data_features_target, target_ca_mg_l)
  ),
  
  tar_target(
    resampling_strategy_cv,
    make_resampling_strategy(train_test_split, target_ca_mg_l)
  ),
  
  tar_target(
    preprocessing_recipe,
    make_recipe(train_test_split, target_ca_mg_l)
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
    make_workflow(xgboost_model, preprocessing_recipe)
  ),
  
  tar_target(
    xgboost_tuned,
    tune_model(xgboost_workflow, resampling_strategy_cv, xgboost_grid)
  ),
  
  tar_target(
    xgboost_model_final_params,
    get_best_model_params(xgboost_tuned)
  ),
  
  tar_target(
    xgboost_workflow_final,
    make_final_workflow(xgboost_workflow, xgboost_model_final_params)
  ),
  
  tar_target(
    xgboost_model_final_fit,
    make_final_model_fit(xgboost_workflow_final, train_test_split)
  ),
  
  tar_target(
    prediction_testsplit,
    predict_final_model_fit_on_testsplit(xgboost_workflow_final, train_test_split)
  ),
  
  
  # tar_target(
  #   xgboost_workflow_final,
  #   fit_best_model_on_training_split(xgboost_model_final, preprocessing_recipe, train_test_split)
  # ),
  

# Plots -------------------------------------------------------------------

  tar_target(
    plot_train_test_split,
    make_plot_train_test_split(train_test_split)
  ),

  tar_target(
    plot_feature_importance,
    make_plot_feature_importance(xgboost_model_final_fit)
  ),

  tar_target(
    plot_observed_vs_predicted,
    make_plot_observed_vs_predicted(prediction_testsplit, target_ca_mg_l)
  ),

  tar_target(
    plot_residuals_vs_predicted,
    make_plot_residuals_vs_predicted(prediction_testsplit, target_ca_mg_l)
  )

  
  
)

tar_pipeline(targets)


# targets::tar_make_future(workers = future::availableCores())
