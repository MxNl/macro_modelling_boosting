report_targets <- c(

  # Visualizations vEGU ----------------------------------------------------------

  tar_target(
    sampledatedistribution_plot,
    make_sampledatedistribution_plot(back_vals_2005_clean_sf)
  ),

  tar_target(
    sampledepthdistribution_plot,
    make_sampledepthdistribution_plot(back_vals_2005_clean_sf)
  ),

  tar_target(
    measurementdistribution_plot,
    make_measurementdistribution_plot(back_vals_2005_clean_sf)
  ),

  tar_target(
    samplesparameters_plot,
    make_samplesparameters_plot(back_vals_filter_allpositive_sf)
  ),

  tar_target(
    locations_plot,
    make_locations_plot(back_vals_filter_allpositive_sf)
  ),

  # Visualizations Current State ----------------------------------------------------------

  tar_target(
    sampledatedistribution_current_plot,
    make_sampledatedistribution_current_plot(back_vals_2005_clean_sf)
  ),
  
  tar_target(
    sampledepthdistribution_current_plot,
    make_sampledepthdistribution_current_plot(back_vals_2005_clean_sf)
  ),

  tar_target(
    measurementdistribution_current_plot,
    make_measurementdistribution_current_plot(back_vals_2005_clean_sf)
  ),

  tar_target(
    samplesparameters_current_plot,
    back_vals_2005_clean_sf %>% 
      select(all_of(names(back_vals_filter_allpositive_sf))) %>% 
      make_samplesparameters_current_plot()
  ),
  
  tar_target(
    locations_current_plot,
    make_locations_plot(back_vals_2005_clean_sf)
  ),

  # Report ------------------------------------------------------------------
  
  # tar_render(
  #   report,
  #   "macro_modelling_boosting.Rmd"
  # ),
  
  # tar_render(
  #   vegu_display_materials,
  #   "vEGU_display_material_maximilian_noelscher.Rmd"
  # ),

  tar_render(
    current_project_state,
    "current_project_state.Rmd"
  )
)