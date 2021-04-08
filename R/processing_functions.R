summarise_orientations <- 
  function(features){
    features %>% 
      pivot_longer(cols = -"station_id",
                   names_to = c("feature", "class", "orientation"),
                   names_pattern = "(.*)_(.*)_(.*)") %>% 
      group_by(station_id, feature, class) %>% 
      summarise(value = mean(value), .groups = "drop") %>% 
      arrange(station_id, feature, class) %>% 
      pivot_wider(names_from = c("feature", "class"))
  }


remove_negative_concentration_values <- 
  function(x){
    x %>% 
      mutate(across(one_of(HYDROGEOCHEMICAL_PARAMS), ~replace(.x, .x < 0, NA_real_)))
  }



bind_target_to_features_and_filter_NA_rows <- 
  function(sf_points, features, target_variable){
    #### Test
    # sf_points <- back_vals_2005_filter_sf
    # features <- all_features_model
    # target_variable <- "ca_mg_l"
    ###
    
    # new_variable_name <- paste0("target_", target_variable)
    
    sf_points %>% 
      st_drop_geometry() %>% 
      as_tibble() %>% 
      select(station_id, {{target_variable}}) %>%
      left_join(features, by = "station_id") %>% 
      rename(target= !!target_variable) %>% 
      drop_na(everything())
  }

add_feature_depth <- 
  function(sf_points, features){
    #### Test
    # sf_points <- back_vals_filter_sf
    # features <- tar_read(data_features)
    # target_variable <- "ca_mg_l"
    ###

    sf_points %>% 
      st_drop_geometry() %>% 
      as_tibble() %>% 
      select(station_id, sample_depth) %>% 
      # bind_cols(coords) %>% 
      left_join(features, by = "station_id") %>% 
      rename(sampledepth_sampledepth = sample_depth)
  }

tibble_to_sf <- 
  function(x, sf_points = tar_read(back_vals_filter_sf)){
    ######## Test
    # x <- data_test
    ####
    
    x %>% 
      left_join(sf_points) %>% 
      select(station_id, all_of(names(x)), geometry) %>% 
      st_as_sf()
  }

parameter_pretty_markdown <-
  function(parameter, with_unit = TRUE, markdown = TRUE, bold = FALSE) {
    ######## Test
    # parameter <- HYDROGEOCHEMICAL_PARAMS_MAJOR_IONS
    # with_unit <- FALSE
    # markdown <- TRUE
    # bold <- TRUE
    ###

    parameter <- parameter %>%
      str_to_title() %>%
      word(sep = "_") %>%
      str_replace_all("o", "O") %>%
      str_replace_all("c", "C")
    # str_replace_all("[:digit:]", "<sub>[:digit:]</sub>") %>%
    if (with_unit) {
      parameter <-
        parameter %>%
        str_c(" [mg/L]")
    } else if (markdown) {
      parameter <-
        parameter %>%
        sub("(\\d)", "<sub>\\1</sub>", .)
      if (bold) {
        parameter <-
          parameter %>%
          str_c("**", ., "**")
      } else {
        parameter
      }
    } else {
      parameter
    }

    return(parameter)
  }
