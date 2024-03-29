
# Settings ----------------------------------------------------------------

library(targets)
library(tarchetypes)
library(future)

source("R/import_functions.R")
source("R/plot_functions.R")
source("R/ensembling.R")
source("R/processing_functions.R")
source("R/train_test_split.R")
source("R/resampling.R")
source("R/recipe.R")
source("R/model.R")
source("R/tuning_settings.R")
source("R/workflow.R")
source("R/tuning.R")
source("R/constants.R")

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
                            "xgboost",
                            "nnet",
                            "treesnip",
                            "stacks",
                            "ggpointdensity",
                            "tidymodels",
                            "tidyverse",
                            "sf"),
               # error = "continue",
               garbage_collection = TRUE,
               memory = "transient")


source("targets/report_targets.R")
# plan(multisession)


# Define targets
list(
  # Data import -------------------------------------------------------------


  ## Modelling ---------------------------------------------------------------

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
  
  ## Background Information ---------------------------------------------------------------

  tar_target(
    filepath_back_vals_2005,
    "J:/NUTZER/Noelscher.M/Studierende/Daten/hydrogeochemical_background_values/germany/multi_time/tabular/hintergrundwerte_bgr/data/point_data/reprojected/tbl_hgc_pkt_2005"
  ),
  tar_target(
    back_vals_2005,
    read_background_values(filepath_back_vals_2005)
  ),
  
  tar_target(
    back_vals_2005_clean_sf,
    make_background_values_clean(
      back_vals_2005
    )
  ),
  
  # Data preparation --------------------------------------------------------

  tar_target(
    data_features_cleaner_names,
    cleaner_feature_names(data_features)
  ),

  tar_target(
    back_vals_run_mode,
    filter_samples_run_mode(back_vals_filter_sf)
  ),
  
  tar_target(
    back_vals_filter_allpositive_sf,
    remove_negative_concentration_values(back_vals_run_mode)
  ),

  tar_target(
    data_features_depth_added,
    add_feature_depth(
      back_vals_filter_allpositive_sf,
      data_features_cleaner_names
    )
  ),

  tar_target(
    parameters_to_model,
      select_params_run_mode(HYDROGEOCHEMICAL_PARAMS_MAJOR_IONS)
  ),
  
  tar_target(
    data_features_target,
    bind_target_to_features_and_filter_NA_rows(
      back_vals_filter_allpositive_sf,
      data_features_depth_added,
      parameters_to_model
      ),
    pattern = map(parameters_to_model),
    iteration = "list"
  ),
  

  # Modelling ---------------------------------------------------------------

  ## Single Parameters -------------------------------------------------------

  tar_target(
    control_grid_stack,
    make_stack_control_grid()
  ), 

  tar_target(
    train_test_split,
    make_train_test_split(
      data_features_target
    ),
    pattern = map(data_features_target),
    iteration = "list"
  ),
  
  tar_target(
    resampling_strategy_cv,
    make_resampling_strategy(
      train_test_split
    ),
    pattern = map(train_test_split),
    iteration = "list"
  ),
  
  tar_target(
    preprocessing_recipe,
    make_recipe(
      train_test_split
    ),
    pattern = map(train_test_split),
    iteration = "list"
  ),
  
  ### Xgboost -----------------------------------------------------------------

  tar_target(
    xgboost_model,
    make_xgboost_model()
  ),

  tar_target(
    xgboost_params,
    make_xgboost_tuning_parameter_set()
  ),

  tar_target(
    xgboost_grid,
    make_xgboost_tuning_strategy(xgboost_params)
  ),

  tar_target(
    xgboost_workflow,
    make_workflow(
      xgboost_model,
      preprocessing_recipe
      ),
    pattern = map(preprocessing_recipe),
    iteration = "list"
  ),

  tar_target(
    xgboost_tuned,
    tune_model(
      xgboost_workflow, 
      resampling_strategy_cv, 
      xgboost_grid,
      control_grid_stack
      ),
    pattern = map(xgboost_workflow, resampling_strategy_cv),
    iteration = "list",
    deployment = "main"
  ),


  ### Neural Net --------------------------------------------------------------

  tar_target(
    nnet_model,
    make_neuralnet_model()
  ),
  
  tar_target(
    nnet_params,
    make_nnet_tuning_parameter_set()
  ),
  
  tar_target(
    nnet_grid,
    make_nnet_tuning_strategy(nnet_params)
  ),
  
  tar_target(
    nnet_workflow,
    make_workflow(
      nnet_model,
      preprocessing_recipe
    ),
    pattern = map(preprocessing_recipe),
    iteration = "list"
  ),
  
  tar_target(
    nnet_tuned,
    tune_model(
      nnet_workflow, 
      resampling_strategy_cv, 
      nnet_grid,
      control_grid_stack
    ),
    pattern = map(nnet_workflow, resampling_strategy_cv),
    iteration = "list",
    deployment = "main"
  ),

  ### Ensemble ----------------------------------------------------------------

  tar_target(
    data_stack,
    make_stack(xgboost_tuned, nnet_tuned),
    pattern = map(xgboost_tuned, nnet_tuned),
    iteration = "list"
  ),
  
  tar_target(
    model_stack,
    blend_predictions(data_stack),
    pattern = map(data_stack),
    iteration = "list",
    deployment = "main"
  ),
  

  ### Ensemble Finalization ---------------------------------------------------
  
  tar_target(
    model_stack_fitted,
    fit_members(model_stack),
    pattern = map(model_stack),
    iteration = "list",
    deployment = "main"
  ),

  tar_target(
    model_stack_predictions,
    train_test_split %>% 
      testing() %>% 
      predict(model_stack_fitted, .) %>% 
      bind_cols(),
    pattern = map(train_test_split, model_stack_fitted),
    iteration = "list",
    deployment = "main"
  ),
  
  ### XGBoost Finalization ---------------------------------------------------
  
  tar_target(
    xgboost_model_final_params,
    get_best_model_params(xgboost_tuned),
    pattern = map(xgboost_tuned),
    iteration = "list"
  ),

  tar_target(
    xgboost_workflow_final,
    make_final_workflow(
      xgboost_workflow,
      xgboost_model_final_params
      ),
    pattern = map(xgboost_workflow, xgboost_model_final_params),
    iteration = "list"
  ),

  tar_target(
    xgboost_model_final_fit,
    make_final_model_fit(
      xgboost_workflow_final,
      train_test_split
    ),
    pattern = map(xgboost_workflow_final, train_test_split),
    iteration = "list"
  ),

  tar_target(
    prediction_testsplit,
    predict_final_model_fit_on_testsplit(
      xgboost_workflow_final,
      train_test_split
    ),
    pattern = map(xgboost_workflow_final, train_test_split),
    iteration = "list"
  ),

  # Plots -------------------------------------------------------------------


  ## Base Plots --------------------------------------------------------------

  # tar_target(
  #   plot_locations_overview,
  #   make_locations_overview_plot(
  #     data_features_target,
  #     back_vals_filter_allpositive_sf,
  #     back_vals_2005_clean_sf,
  #     parameters_to_model
  #   )
  # ),
  
  ## Single XGBoost ----------------------------------------------------------

  tar_target(
    plot_xgb_violin_distribution_targets,
    make_plot_violin_distribution_targets(
      back_vals_filter_sf,
      parameters_to_model
    )
  ),
  
  tar_target(
    plot_xgb_histogram_distribution_features,
    make_plot_histogram_distribution_features(
      data_features_depth_added,
      20
    )
  ),
  
  tar_target(
    plot_xgb_interactive_correlation,
    make_interactive_correlation_plot(
      data_features_depth_added
    )
  ),


  tar_target(
    plot_xgb_train_test_split,
    imap(
      train_test_split %>% 
        set_names(parameters_to_model),
      make_plot_train_test_split
    )
  ),

  tar_target(
    plot_xgb_feature_importance,
    imap(
      xgboost_model_final_fit %>% 
        set_names(parameters_to_model),
      make_plot_feature_importance
    )
  ),

  tar_target(
    plot_xgb_observed_vs_predicted,
    imap(
      prediction_testsplit %>% 
        set_names(parameters_to_model),
      make_plot_observed_vs_predicted
    )
  ),

  tar_target(
    plot_xgb_residuals_vs_predicted,
    imap(
      prediction_testsplit %>% 
        set_names(parameters_to_model),
      make_plot_residuals_vs_predicted
    )
  ),

  ## Ensemble
  
  # tar_target(
  #   plot_ensemble_member_weights,
  #   make_ensemble_member_weights_plot(
  #     model_stack
  #   ),
  #   pattern = map(model_stack),
  #   iteration = "list"
  # ),
  
  report_targets

)



# targets::tar_make_future(workers = future::availableCores())
