library(targets)
library(glue)
library(ggpointdensity)
library(ggtext)
library(assertr)
library(parallel)
library(doParallel)
library(sf)
library(vip)
library(tidymodels)
library(tidyverse)


tar_read(data_features_wo_orientations)
data_features_target <- tar_read(data_features_target)
data_features <- tar_read(data_features)
back_vals_filter_sf <- tar_read(back_vals_filter_sf)
preprocessing_recipe <- tar_read(preprocessing_recipe)
xgboost_model_final <- tar_read(xgboost_model_final)
train_test_split <- tar_read(train_test_split)
resampling_strategy_cv <- tar_read(resampling_strategy_cv)
preprocessing_recipe <- tar_read(preprocessing_recipe)
xgboost_workflow_final <- tar_read(xgboost_workflow_final)
xgboost_model_final_fit <- tar_read(xgboost_model_final_fit)
prediction_testsplit <- tar_read(prediction_testsplit)
tar_read(xgboost_model_final_params)
xgboost_tuned <- tar_read(xgboost_tuned)
tar_read(interactive_correlation_plot)
tar_read(plot_train_test_split)
tar_read(plot_feature_importance)
tar_read(plot_observed_vs_predicted) %>%
  map(function(x) {
    x +
      scale_x_log10() +
      scale_y_log10()
  })
tar_read(plot_observed_vs_predicted) %>% 
   ggpubr::ggarrange(plotlist = ., common.legend = TRUE)
   cowplot::plot_grid(plotlist = .)

tar_read(plot_residuals_vs_predicted) %>% 
   imap(~ggsave(str_c("C:/Noelscher.M/Desktop/plots/residuals_", .y, ".png"), .x))
tar_read(xgboost_model_final)
tar_read(preprocessing_recipe)

tar_read(train_test_split)

data_features_target %>% 
  pull(target_ca_mg_l) %>% 
  range()


tar_read(data_features) %>% 
   select(contains("temperature")) %>% 
   summarise()
 
prediction_testsplit %>% 
   collect_metrics()
 

"J:/NUTZER/Noelscher.M/Studierende/Daten/hydrogeochemical_background_values/germany/multi_time/tabular/hintergrundwerte_bgr/data/point_data/reprojected/tbl_hgc_pkt_2005.csv" %>% 
   read_csv2() %>% 
   janitor::clean_names() %>% 
   select(one_of(HYDROGEOCHEMICAL_PARAMS_MAJOR_IONS)) %>% 
   mutate(across(everything(), as.character)) %>% 
   pivot_longer(cols = everything()) %>% 
   filter(str_detect(value, "n"))

 predict(xgboost_model_final_fit, new_data = bake(preprocessing_recipe, new_data = training(train_test_split)))
 
 
 df <- tibble(
   label = c(
     "Some text **in bold.**",
     "Linebreaks<br>Linebreakssdf<br>Linebreaks",
     "*x*<sup>2</sup> + 5*x* + *C*<sub>*i*</sub>",
     "Some <span style='color:blue'>blue text **in bold.**</span><br>And *italics text.*<br>
    And some <span style='font-size:18pt; color:black'>large</span> text."
   ),
   x = c(.2, .1, .5, .9),
   y = c(.8, .4, .1, .5),
   hjust = c(0.5, 0, 0, 1),
   vjust = c(0.5, 1, 0, 0.5),
   angle = c(0, 0, 45, -45),
   color = c("black", "blue", "black", "red"),
   fill = c("cornsilk", "white", "lightblue1", "white")
 )
 
 
 ggplot(df) +
   aes(
     x, y, label = label, angle = angle, color = color, fill = fill,
     hjust = hjust, vjust = vjust
   ) +
   geom_richtext() +
   geom_point(color = "black", size = 2) +
   scale_color_identity() +
   scale_fill_identity() +
   xlim(0, 1) + ylim(0, 1)
 