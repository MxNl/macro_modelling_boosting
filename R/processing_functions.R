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

cleaner_feature_names <- 
  function(features){
    feature_names <- 
      features %>% 
      names()
    
    features %>% 
      set_names(str_remove(feature_names, pattern = "_0$"))
  }

make_background_values_clean <-
  function(object) {
    object %>%
      janitor::clean_names() %>%
      janitor::remove_empty(c("rows", "cols")) %>%
      rename(
        date = sample_datet,
        x_coord = x,
        y_coord = y,
        sample_depth = filter_ok_m_u_gok,
        sample_id = stprj_stat_smp_id
      ) %>%
      mutate(station_id = str_c(!!sym(STATION_ID[1]), !!sym(STATION_ID[2]), sep = "_")) %>% 
      select(
        station_id,
        sample_id,
        date,
        x_coord,
        y_coord,
        sample_depth,
        stype,
        state,
        strati1,
        lage,
        one_of(HYDROGEOCHEMICAL_PARAMS)
      ) %>%
      mutate(station_id = as.character(station_id)) %>%
      mutate(date = lubridate::dmy(str_sub(date, end = 10))) %>%
      mutate(station_id = as.factor(station_id)) %>%
      mutate(date = na_if(date, as.Date("1900-01-01"))) %>% 
      mutate(date = replace_na(date, as.Date("2010-01-01"))) %>% 
      st_as_sf(coords = c("x_coord", "y_coord")) %>%
      st_sf(crs = 25832) %>% 
      st_transform(crs = CRS_REFERENCE)
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

filter_samples_run_mode <- 
  function(sf_points){
    if(RUN_MODE == "test") {
      sf_points %>% 
        slice_sample(prop = 0.1)
    } else if(RUN_MODE == "full") {
      sf_points
    } else {
      stop("Please provide a valid value for the run_mode in config.yml. Currently supported values are : 'test' and 'full'")
    }
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

select_params_run_mode <- 
  function(hydrogeochemical_params){
    if(RUN_MODE == "test") {
      hydrogeochemical_params[c(1, length(hydrogeochemical_params))]
    } else if(RUN_MODE == "full") {
      hydrogeochemical_params
    } else {
      stop("Please provide a valid value for the run_mode in config.yml. Currently supported values are : 'test' and 'full'")
    }
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

parameter_pretty_markdown <- #TODO rename to parameter_pretty_html
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

parameter_pretty_rmarkdown <-
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
        sub("(\\d)", "~\\1~", .)
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
