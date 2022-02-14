make_plot_violin_distribution_targets <- 
  function(x, parameters = NULL) {
    ##### TEst
    x <- tar_read(back_vals_filter_allpositive_sf)
    parameters <- tar_read(parameters_to_model)
    ####
    
  plot_data <- 
      x %>% 
      st_drop_geometry() %>% 
      as_tibble() %>% 
      select(all_of(parameters)) %>% 
      janitor::remove_empty("rows") %>% 
      pivot_longer(cols = everything()) %>% 
      mutate(value = replace(value, value == 0, 1e-5)) %>% 
      mutate(name = parameter_pretty_markdown(name)) %>% 
      mutate(name = str_replace_all(name, "\\[mg/L\\]", "")) %>% 
      mutate(name = str_replace_all(name, " ", ""))

    plot_data %>% 
      ggplot(aes(name, value)) +
      geom_violin(fill = COLOUR_HEX_BAR_FILL,
                  colour = COLOUR_HEX_BAR_FILL,
                  scale = "count",
                  bw = .1) +
      scale_y_log10() +
      theme_minimal() +
      labs(x = "Parameter",
           y = "Concentration [mg/L]") +
      theme(
        plot.subtitle = element_markdown(),
        plot.title = element_markdown(),
        axis.text.y = element_markdown()
        ) +
      coord_flip()
  }


make_plot_histogram_distribution_features <- 
  function(x, n_features) {
    ##### TEst
    # x <- tar_read(data_features_depth_added)
    # n_features <- 30
    ####

    plot1_data <- 
      x %>% 
      select(-contains("coord"), -station_id) %>% 
      select(-precipitation_precipitation, -seepage_seepage, -gwrecharge_gwrecharge, -temperature_temperature, -sampledepth_sampledepth) %>% 
      pivot_longer(cols = everything()) %>% 
      drop_na(value) %>% 
      filter(name %in% c(sample(unique(.$name), n_features))) %>% 
      mutate(feature = word(name, sep = "_"))

    plot2_data <- 
      x %>%
      select(precipitation_precipitation, seepage_seepage, gwrecharge_gwrecharge, temperature_temperature, sampledepth_sampledepth) %>% 
      pivot_longer(cols = everything()) %>% 
      drop_na(value) %>% 
      mutate(feature = word(name, sep = "_"))
      
    plot1 <- 
      plot1_data %>% 
      ggplot(aes(value)) +
      geom_histogram(aes(fill = feature),
                  boundary = 0,
                  alpha = ALPHA_BARS) +
      scale_y_log10() +
      theme_minimal() +
      labs(x = "Value",
           y = "Count",
           fill = "") +
      # scale_fill_manual(
      #   values = COLOUR_SCHEME
      # ) +
      theme(
        plot.subtitle = element_markdown(),
        plot.title = element_markdown(),
        axis.text.y = element_markdown(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position = "top",
        legend.justification='left',
        legend.direction='horizontal'
        ) +
      # coord_flip() +
      facet_wrap(~name, ncol = 3)
    
    plot2 <- 
      plot2_data %>% 
      ggplot(aes(value)) +
      geom_histogram(fill = COLOUR_HEX_BAR_FILL,
                  boundary = 0,
                  alpha = ALPHA_BARS) +
      # scale_y_log10() +
      theme_minimal() +
      labs(x = "Value",
           y = "Count",
           fill = "") +
      # scale_fill_manual(
      #   values = COLOUR_SCHEME
      # ) +
      theme(
        plot.subtitle = element_markdown(),
        plot.title = element_markdown(),
        axis.text.y = element_markdown(),
        # strip.background = element_blank(),
        # strip.text.x = element_blank(),
        legend.position = "top",
        legend.justification='left'
        ) +
      # coord_flip() +
      facet_wrap(~name, ncol = 1, scales = "free")
    
    ggpubr::ggarrange(plot1, plot2, ncol = 1) %>% 
      return()
  }


make_interactive_correlation_plot <-
  function(features, focus_n_maxcorr = 20) {
    #### Test
    # features <- tar_read(data_features_depth_added)
    # focus_n_maxcorr <- 20
    ###
    plot_data <- 
      features %>%
      select(-contains("coord"), -station_id) %>%
      drop_na(everything()) %>% 
      corrr::correlate()
    
    columns_to_keep <- 
      plot_data %>% 
      corrr::stretch() %>% 
      group_by(x) %>% 
      summarise(sum = sum(abs(r), na.rm = TRUE)) %>% 
      arrange(-sum) %>% 
      slice_head(n = focus_n_maxcorr) %>% 
      pull(x)
    
    plot <-
      plot_data %>% 
      # corrr::rearrange() %>%
      corrr::shave(upper = FALSE) %>% 
      corrr::stretch() %>% 
      filter(x %in% columns_to_keep & y %in% columns_to_keep) %>% 
      corrr::retract() %>%
      corrr::rplot(colours = c(COLOUR_HEX_DIVERGING_NEGATIVE, "white", COLOUR_HEX_DIVERGING_POSITIVE))
    
    plot <- 
      plot +
      # labs(title = glue("**Correlation Plot**"),
      #      subtitle = glue("of the {focus_n_maxcorr} highest correlated features")) +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
            plot.subtitle = element_markdown(),
            plot.title = element_markdown())
    
    
    plot %>%
      plotly::ggplotly() %>% 
      plotly::layout(title = list(text = str_c(glue("<b>Correlation Plot</b>"),
                                       "<br>",
                                       "<sup>",
                                       glue("of the {focus_n_maxcorr} highest correlated features"),
                                       "</sup>"),
                                  x = 0.3,
                                  xanchor = "left"))
  }


make_plot_train_test_split <-
  function(train_test_split_object, parameter = .y) {
    ######## Test
    # train_test_split_object <- tar_read(train_test_split)[[1]]
    #####
    
    ggplot() +
      geom_sf(
        data = training(train_test_split_object) %>%
          tibble_to_sf(),
        colour = COLOUR_HEX_TRAIN,
        alpha = .5,
        size = 1
      ) +
      geom_sf(
        data = testing(train_test_split_object) %>%
          tibble_to_sf(),
        colour = COLOUR_HEX_TEST,
        alpha = .5,
        size = 1
      ) +
      labs(
        title = str_c("**Locations of samples - ", parameter_pretty_markdown(parameter), "**"),
        subtitle = glue("...used for<span style='color:{COLOUR_HEX_TRAIN}'> training</span> and<span style='color:{COLOUR_HEX_TEST}'> testing </span>")
      ) +
      theme_minimal() +
      theme(
        plot.subtitle = element_markdown(),
        plot.title = element_markdown()
      )
  }

make_plot_feature_importance <-
  function(model_fit, parameter = .y) {
    plot <- model_fit %>%
      pull_workflow_fit() %>%
      vip(
        fill = COLOUR_HEX_BAR_FILL,
        alpha = ALPHA_BARS,
        num_features = 20
      )

    plot +
      theme_minimal() +
      theme(
        plot.subtitle = element_markdown(),
        plot.title = element_markdown()
      ) +
      labs(title = str_c("**Feature Importance Plot - ", parameter_pretty_markdown(parameter), "**"))
  }


make_plot_observed_vs_predicted <-
  function(prediction_testsplit, parameter = .y) {
    ######### Test
    # prediction_testsplit <- tar_read(prediction_testsplit) %>% chuck(1)
    # parameter <- tar_read(parameters_to_model) %>% chuck(1)
    #######

    plot_data <-
      prediction_testsplit %>%
      collect_predictions()

    metrics <-
      plot_data %>%
      yardstick::metrics(
        truth = target,
        estimate = .pred
      ) %>%
      mutate(.metric = str_to_upper(.metric)) %>%
      mutate(.metric = replace(.metric, .metric == "RSQ", "R<sup>2</sup>")) %>%
      mutate(.estimate = signif(.estimate, 3))
    
    axis_limits <-
      plot_data %>%
      pivot_longer(cols = -all_of(c("id", ".row", ".config"))) %>%
      pull(value) %>%
      range()
    
    label_r_square <-
      tibble(
        .pred = max(axis_limits),
        target = 0,
        label = str_c(as.character(glue("<span style='color:grey'> **{metrics$.metric} = {metrics$.estimate}** </span>")), collapse = "<br>"),
        alpha = .4,
        fill = "grey",
        hjust = .8,
        vjust = 0
      )

    plot_data %>%
      ggplot() +
      aes(.pred, target) +
      geom_abline(
        slope = 1,
        intercept = 0,
        linetype = "dashed",
        colour = "black",
        alpha = .3,
        size = 1
      ) +
      geom_pointdensity(
        alpha = ALPHA_POINTS,
        shape = 16
      ) +
      # scale_color_viridis_c()
    scale_color_gradientn(
        colours = COLOUR_SCHEME,
        breaks=c(1, 150),
        labels=c("low                   high density","")
        ) +
      guides(colour = guide_colorbar(ticks = FALSE)) +
      # scale_color_viridis_c(
      #   breaks=c(1, 150),
      #   labels=c("low","high density")
      #   ) +
      geom_richtext(
        data = label_r_square,
        aes(
          label = label,
          hjust = hjust,
          vjust = vjust
        ),
        label.color = NA,
        
      ) +
      coord_equal() +
      xlim(axis_limits) +
      ylim(axis_limits) +
      xlab("Predicted Values") +
      ylab("Observed Values") +
      labs(title = "**Predicted Values vs. Observed Values**",
           subtitle = parameter_pretty_markdown(parameter),
           colour = "") +
      theme_minimal() +
      theme(legend.position = "top",
            legend.justification='left',
            legend.direction='horizontal',
            plot.subtitle = element_markdown(),
            plot.title = element_markdown(),
            legend.text.align = 0)
  }

make_plot_residuals_vs_predicted <-
  function(prediction_testsplit, parameter = .y) {
    
    plot_data <- 
      prediction_testsplit %>%
      collect_predictions() %>%
      mutate(residual_percent = (target - .pred) / .pred)
    
    axis_limits <-
      plot_data %>%
      pull(residual_percent) %>%
      sd() %>%
      magrittr::multiply_by(10) %>%
      c(-., .)
      # range()
    
    plot_data %>%
      ggplot() +
      aes(.pred, y = residual_percent) +
      geom_pointdensity(
        alpha = ALPHA_POINTS,
        shape = 16
      ) +
      scale_color_gradientn(
        colours = COLOUR_SCHEME,
        breaks=c(1, 1200),
        labels=c("low","high density")
      ) +
      guides(colour = guide_colorbar(ticks = FALSE)) +
      xlab("Predicted Values") +
      ylab("Residual [%]") +
      labs(title = "**Residuals vs. Predicted Values**", 
           subtitle = parameter_pretty_markdown(parameter),
           colour = "") +
      scale_y_continuous(
        # labels = scales::percent,
        limits = axis_limits) +
      theme_minimal() +
      theme(legend.position = "none",
              legend.justification='left',
              legend.direction='horizontal',
            plot.subtitle = element_markdown(),
            plot.title = element_markdown()
            )
  }

make_locations_overview_plot <-
  function(model_data, sf_points, parameters) {
    model_data <- 
      model_data %>% 
      reduce(left_join, by = "station_id") %>% 
      select(station_id) %>% 
      inner_join(sf_points, .) %>% 
      select(station_id, all_of(parameters)) %>%
      pivot_longer(cols = parameters) %>% 
      drop_na(value) %>% 
      group_by(station_id) %>% 
      summarise(n = n()) %>% 
      inner_join(sf_points, .) %>% 
      select(station_id, n, all_of(parameters))
    
    # discarded_sites <- 
    #   original_sf_points %>% 
    #   as_tibble() %>% 
    #   anti_join(as_tibble(model_data), by = "station_id") %>% 
    #   inner_join(original_sf_points, .)
    
    map <- 
      model_data %>%
      # mapview::mapview(alpha.regions = c(0), alpha = .5, cex = 2, color = "red", layer.name = "Discarded sample sites during preprocessing") +
      mapview::mapview(zcol = "n", lwd = 0, alpha = .1, cex = 2, layer.name = "Sample sites for model training")
    
    map@map %>%  leaflet::addMiniMap()
  }

make_ensemble_member_weights_plot <-
  function(model_stack) {
    model_stack %>% 
      autoplot(type = "weights")
  }

make_sampledatedistribution_plot <-
  function(x) {
    x %>%
      as_tibble() %>%
      select(date) %>%
      mutate(
        date_cat = if_else(date >= MIN_SAMPLE_DATE, "kept", "discarded"),
        date_cat = factor(date_cat, levels = c("discarded", "kept"))
      ) %>%
      ggplot(aes(date, fill = date_cat)) +
      geom_histogram(
        alpha = .4,
        boundary = as.Date(MIN_SAMPLE_DATE)
      ) +
      geom_vline(
        xintercept = as.Date(MIN_SAMPLE_DATE),
        colour = "grey",
        linetype = "dashed"
      ) +
      geom_label(
        data = data.frame(x = as.Date(c("1975-11-27", "1975-11-27")), y = c(18598.5577000015, 18598.5577000015), label = c(
          MIN_SAMPLE_DATE,
          MIN_SAMPLE_DATE
        )), mapping = aes(x = x, y = y, label = label),
        label.padding = unit(0.25, "lines"), label.r = unit(
          0.1,
          "lines"
        ), inherit.aes = FALSE,
        colour = "grey"
      ) +
      geom_curve(data = data.frame(
        x = as.Date("1981-11-17"), y = 17123.0310652555,
        xend = as.Date("1988-04-02"), yend = 15336.8672442472
      ), mapping = aes(
        x = x,
        y = y, xend = xend, yend = yend
      ), arrow = arrow(30L, unit(
        0.1,
        "inches"
      ), "last", "closed"), inherit.aes = FALSE, colour = "grey") +
      theme_minimal() +
      theme(legend.position = "top", legend.direction = "horizontal") +
      labs(
        fill = "Filter on sample date",
        x = "Sample date",
        y = "Number of samples"
      )
  }

make_sampledepthdistribution_plot <-
  function(x) {
    x %>%
      as_tibble() %>%
      filter(date >= MIN_SAMPLE_DATE) %>%
      select(sample_depth) %>%
      mutate(
        depth_cat = if_else(sample_depth <= MAX_SAMPLE_DEPTH, "kept", "discarded"),
        depth_cat = factor(depth_cat, levels = c("discarded", "kept"))
      ) %>%
      ggplot(aes(sample_depth, fill = depth_cat)) +
      geom_histogram(
        alpha = .4,
        boundary = MAX_SAMPLE_DEPTH
      ) +
      scale_x_continuous(
        limits = c(-10, 500)) +
      geom_vline(
        xintercept = MAX_SAMPLE_DEPTH ,
        colour = "grey",
        linetype = "dashed"
      ) +
      geom_label(
        data = data.frame(x = c(200, 200), y = c(8000, 8000), label = c(
          str_c(MAX_SAMPLE_DEPTH, " m below ground level")
        )), mapping = aes(x = x, y = y, label = label),
        label.padding = unit(0.25, "lines"), label.r = unit(
          0.1,
          "lines"
        ), inherit.aes = FALSE,
        colour = "grey"
      ) +
      geom_curve(data = data.frame(
        x = 200, y = 7000,
        xend = 120, yend = 5000
      ), mapping = aes(
        x = x,
        y = y, xend = xend, yend = yend
      ), curvature = -0.305, arrow = arrow(30L, unit(
        0.1,
        "inches"
      ), "last", "closed"), inherit.aes = FALSE, colour = "grey") +
      theme_minimal() +
      theme(legend.position = "top", legend.direction = "horizontal") +
      labs(
        fill = "Filter on sample depth",
        x = "Sample depth",
        y = "Number of samples"
      )
  }

make_measurementdistribution_plot <-
  function(x) {
    x %>% 
      filter(date >= MIN_SAMPLE_DATE) %>% 
      filter(sample_depth <= MAX_SAMPLE_DEPTH | lage == 1) %>% 
      as_tibble() %>% 
      # slice(40000:45000) %>% 
      group_by(station_id) %>% 
      summarise(n = n()) %>% 
      mutate(
        n = factor(n, levels = min(n):max(n)),
        n_cat = if_else(as.character(n) == as.character(1), "unchanged", "aggregated")
        # n_cat = factor(n_cat, levels = c("aggregated", "unchanged"))
      ) %>%
      ggplot(aes(n)) +
      geom_histogram(
        aes(fill = n_cat),
        alpha = .4,
        stat = "count"
      ) +
      scale_y_log10() +
      theme_minimal() +
      theme(legend.position = "top", legend.direction = "horizontal") +
      labs(
        fill = "Treatment of multiple measurements per sample site",
        x = "Number of measurements per sample site",
        y = "Number of sample sites"
      )
  }

make_samplesparameters_plot <-
  function(x) {
    x %>%
      as_tibble() %>% 
      select(-station_id, -sample_depth, -geometry) %>% 
      pivot_longer(cols = everything()) %>% 
      drop_na(everything()) %>% 
      count(name) %>% 
      arrange(-n) %>% 
      mutate(n_cat = if_else(row_number() <= 13 & !(name %in% c("ph", "lf_u_scm", "nh4_mg_l")), "kept", "discarded")) %>% 
      ggplot(aes(reorder(name, n), n, fill = n_cat)) +
      geom_col(alpha = .4) +
      theme_minimal() +
      theme(legend.position = "top", legend.direction = "horizontal") +
      labs(
        fill = "",
        x = "Hydrogeochemical parameters",
        y = "Number of samples"
      ) +
      coord_flip()
  }

make_sampledatedistribution_current_plot <-
  function(x) {
    x %>%
      as_tibble() %>%
      select(date) %>%
      ggplot(aes(date)) +
      geom_histogram(
        fill = "#559FFF",
        colour = "#559FFF4D",
        alpha = .7,
        boundary = as.Date(MIN_SAMPLE_DATE)
      ) +
      theme_minimal() +
      labs(
        x = "Sample date",
        y = "Number of samples"
      )
  }


make_sampledepthdistribution_current_plot <-
  function(x) {
    x %>%
      as_tibble() %>%
      select(sample_depth) %>%
      ggplot(aes(sample_depth)) +
      geom_histogram(
        fill = "#559FFF",
        colour = "#559FFF4D",
        alpha = .7,
        boundary = as.Date(MIN_SAMPLE_DATE)
      ) +
      scale_x_continuous(
        limits = c(-10, 500)) +
      theme_minimal() +
      theme(legend.position = "top", legend.direction = "horizontal") +
      labs(
        x = "Sample depth",
        y = "Number of samples"
      ) +
      scale_x_reverse() +
      coord_flip()
  }

make_measurementdistribution_current_plot <-
  function(x) {
    x %>% 
      as_tibble() %>% 
      # slice(40000:45000) %>% 
      group_by(station_id) %>% 
      summarise(n = n()) %>% 
      mutate(
        n = factor(n, levels = min(n):max(n))
      ) %>%
      ggplot(aes(n)) +
      geom_histogram(
        fill = "#559FFF",
        colour = "#559FFF4D",
        alpha = .7,
        stat = "count"
      ) +
      scale_y_log10() +
      theme_minimal() +
      theme(legend.position = "top", legend.direction = "horizontal") +
      labs(
        x = "Number of measurements per sample site",
        y = "Number of sample sites"
      )
  }

make_samplesparameters_current_plot <-
  function(x) {
    x %>% 
      as_tibble() %>% 
      select(-station_id, -sample_depth, -geometry) %>% 
      pivot_longer(cols = everything()) %>% 
      drop_na(everything()) %>% 
      count(name) %>% 
      arrange(-n) %>% 
      mutate(n_cat = if_else(row_number() <= 13 & !(name %in% c("ph", "lf_u_scm", "nh4_mg_l")), "modelled", "considered for modelling later")) %>% 
      ggplot(aes(reorder(name, n), n, fill = n_cat)) +
      geom_col(alpha = .7) +
      theme_minimal() +
      scale_fill_manual(values = c("#F8766D", "#559FFF")) +
      theme(legend.position = "top", legend.direction = "horizontal") +
      labs(
        fill = "",
        x = "Hydrogeochemical parameters",
        y = "Number of samples"
      ) +
      coord_flip()
  }

make_locations_plot <-
  function(x) {
    plot <- 
      x %>% 
      st_coordinates() %>% 
      as_tibble() %>% 
      ggplot(aes(X, Y)) +
      ggplot2::stat_bin_hex(binwidth = 1E4) +
      geom_point(fill = NA, colour = NA) +
      scale_fill_viridis_c() +
      coord_equal() +
      theme_void() +
      theme(legend.position = "left") +
      labs(fill = "Number of samples\n per hexagon")
    
    ggExtra::ggMarginal(plot, type="density", fill = "grey", colour = NA, alpha = .3)
  }


