make_plot_train_test_split <-
  function(train_test_split_object) {
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
        title = "**Locations of samples**",
        subtitle = glue("**...used for<span style='color:{COLOUR_HEX_TRAIN}'> training</span> and<span style='color:{COLOUR_HEX_TEST}'> testing </span>**")
      ) +
      theme_minimal() +
      theme(
        plot.subtitle = element_markdown(),
        plot.title = element_markdown()
      )
  }

make_plot_feature_importance <-
  function(model_fit) {
    plot <- model_fit %>%
      pull_workflow_fit() %>%
      vip(
        fill = COLOUR_HEX_BAR_FILL,
        alpha = .6,
        num_features = 20
      )

    plot +
      theme_minimal()
  }


make_plot_observed_vs_predicted <-
  function(prediction_testsplit, target_variable) {
    ######### Test
    # prediction_testsplit <- tar_read(prediction_testsplit)
    #######


    plot_data <-
      prediction_testsplit %>%
      collect_predictions() %>% 
      rename(target = {{target_variable}})

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
        fill = "grey",
        hjust = c(.8),
        vjust = c(.8)
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
        alpha = .3,
        shape = 16
      ) +
      scale_color_gradientn(
        colours = COLOUR_SCHEME,
        breaks=c(1, 150),
        labels=c("low","high density")
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
        label.color = NA
      ) +
      coord_equal() +
      xlim(axis_limits) +
      ylim(axis_limits) +
      xlab("Predicted Values") +
      ylab("Observed Values") +
      labs(title = "Predicted Values vs. Observed Values",
           colour = "") +
      theme_minimal() +
      theme(legend.position = "top",
            legend.justification='left',
            legend.direction='horizontal')
  }

make_plot_residuals_vs_predicted <-
  function(prediction_testsplit, target_variable) {
    
    plot_data <- 
      prediction_testsplit %>%
      collect_predictions() %>%
      mutate(residual_percent = ({{target_variable}} - .pred) / .pred)
    
    axis_limits <-
      plot_data %>%
      pull(residual_percent) %>%
      range()
    
    plot_data %>%
      ggplot() +
      aes(.pred, y = residual_percent) +
      geom_pointdensity(
        alpha = .3,
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
      labs(title = "Residuals vs. Predicted Values",
           colour = "") +
      scale_y_continuous(labels = scales::percent,
                         limits = axis_limits) +
      theme_minimal() +
      theme(legend.position = "top",
              legend.justification='left',
              legend.direction='horizontal')
  }
