make_plot_violin_distribution_targets <- 
  function(x, parameters = NULL) {
    ##### TEst
    # x <- tar_read(back_vals_filter_allpositive_sf)
    # parameters <- tar_read(parameters_to_model)
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

    plot <- 
      plot_data %>% 
      ggplot(aes(name, value)) +
      geom_violin(fill = COLOUR_HEX_BAR_FILL,
                  colour = COLOUR_HEX_BAR_FILL) +
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
      # facet_wrap(~name, nrow = 1, scales = "free_y")
    
    return(plot)
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
      corrr::rearrange() %>% 
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
    # prediction_testsplit <- tar_read(prediction_testsplit)
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
      pivot_longer(cols = -all_of(c("id", ".row"))) %>%
      pull(value) %>%
      range()
    
    label_r_square <-
      tibble(
        .pred = max(axis_limits),
        target = max(axis_limits),
        label = str_c(as.character(glue("<span style='color:grey'> **{metrics$.metric} = {metrics$.estimate}** </span>")), collapse = "<br>"),
        alpha = .4,
        fill = "grey",
        hjust = .8,
        vjust = .8
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
          vjust = vjust,
          alpha = alpha
        ),
        label.color = NA,
        
      ) +
      coord_equal() +
      xlim(axis_limits) +
      ylim(axis_limits) +
      xlab("Predicted Values") +
      ylab("Observed Values") +
      labs(title = str_c("**Predicted Values vs. Observed Values - ", parameter_pretty_markdown(parameter), "**"),
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
      range()
    
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
      labs(title = str_c("**Residuals vs. Predicted Values - ", parameter_pretty_markdown(parameter), "**"),
           colour = "") +
      scale_y_continuous(labels = scales::percent,
                         limits = axis_limits) +
      theme_minimal() +
      theme(legend.position = "top",
              legend.justification='left',
              legend.direction='horizontal',
            plot.subtitle = element_markdown(),
            plot.title = element_markdown()
            )
  }
