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
xgboost_workflow_final <- tar_read(xgboost_workflow_final)
xgboost_model_final_fit <- tar_read(xgboost_model_final_fit)
prediction_testsplit <- tar_read(prediction_testsplit)
xgboost_tuned <- tar_read(xgboost_tuned)
tar_read(plot_train_test_split)
tar_read(plot_feature_importance)
tar_read(plot_observed_vs_predicted) +
  scale_x_log10() +
  scale_y_log10()
tar_read(plot_residuals_vs_predicted)
tar_read(xgboost_model_final)
tar_read(preprocessing_recipe)

tar_read(train_test_split)

data_features_target %>% 
  pull(target_ca_mg_l) %>% 
  range()

 
prediction_testsplit %>% 
   collect_metrics()
 

 

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
 