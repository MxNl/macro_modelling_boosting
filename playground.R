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
library(stacks)
library(leafgl)
library(leaflet)
library(tidyverse)


tar_read(data_features_wo_orientations)
data_features_target <- tar_read(data_features_target)
features <- tar_read(data_features)
back_vals_filter_sf <- tar_read(batarck_vals_filter_sf)
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


tar_read(plot_train_test_split)
tar_read(back_vals_filter_allpositive_sf) %>% 
   select(station_id, all_of(tar_read(parameters_to_model))) %>% 
   filter(station_id %in% pull(tar_read(data_features_depth_added), station_id)) %>% 
   drop_na(ca_mg_l)
   

tar_read(data_features_target) %>% 
   reduce(left_join, by = "station_id") %>% 
   select(station_id) %>% 
   inner_join(tar_read(back_vals_filter_allpositive_sf), .) %>% 
   select(station_id, all_of(tar_read(parameters_to_model))) %>%
   pivot_longer(cols = tar_read(parameters_to_model)) %>% 
   drop_na(value) %>% 
   group_by(station_id) %>% 
   summarise(n = n()) %>% 
   inner_join(tar_read(back_vals_filter_allpositive_sf), .) %>% 
   select(station_id, n, all_of(tar_read(parameters_to_model))) %>% 
   mapview::mapview(zcol = "n", lwd = 0, alpha = .1, cex = 2)
   


data_features_target <- tar_read(data_features_target)[[1]]
library(DALEX)
library(DALEXtra)

dalex_test <- tar_read(xgboost_model_final_fit) %>% 
   chuck(1) %>% 
   explain_tidymodels(data = data_features_target, y = data_features_target$target)

dalex_test %>% 
   model_performance() %>% 
   plot()

test <- tar_read(model_stack) %>% 
   chuck(1) %>% 
   blend_predictions()

test %>% 
   autoplot(type = "weights")

xgb <- tar_read(xgboost_tuned) %>% 
   chuck(1)

nnet <- tar_read(nnet_tuned) %>% 
   chuck(1)
   
stacks::stacks() %>% 
   stacks::add_candidates(xgb) %>%
   stacks::add_candidates(nnet)
   
plot_data <- 
   tar_read(prediction_testsplit) %>% 
   set_names(tar_read(parameters_to_model)) %>% 
   chuck(1) %>% 
   collect_predictions()


   yardstick::metrics(
      truth = target,
      estimate = .pred
   ) %>%
   mutate(.metric = str_to_upper(.metric)) %>%
   mutate(.metric = replace(.metric, .metric == "RSQ", "R<sup>2</sup>")) %>%
   mutate(.estimate = signif(.estimate, 3))

xgb %>% 
   slice(1) %>% 
   pull(.notes) %>% 
   chuck(1) %>% 
   slice(1) %>% 
   pull(.notes)

options(viewer = NULL) # view in browser

points <- tar_read(back_vals_filter_sf) %>% 
   # as_tibble() %>% 
   # select(all_of(HYDROGEOCHEMICAL_PARAMS_MAJOR_IONS), contains("geometry")) %>% 
   # mutate(geometry = as.character(geometry)) %>% 
   # pivot_longer(cols = all_of(HYDROGEOCHEMICAL_PARAMS_MAJOR_IONS)) %>% 
   # drop_na(value) %>% 
   # group_by(geometry) %>% 
   # mutate(n = n()) %>% 
   # slice(1) %>% 
   # ungroup() %>% 
   # slice(1) %>%
   # st_point(eval(expression(geometry)))
   # st_as_sfc() %>% 
   st_transform(4326)

leaflet() %>%
   addProviderTiles(provider = providers$CartoDB.DarkMatter) %>%
   addCircleMarkers(data = points,
               radius = 1,
               fillOpacity = .7,
               stroke = FALSE)

test_rset <- 
   tar_read(resampling_strategy_cv)[[1]]


key_table_coords <- 
   tar_read(back_vals_filter_sf) %>% 
   select(station_id) %>% 
   bind_cols(as_tibble(st_coordinates(.))) %>% 
   st_drop_geometry() %>% 
   as_tibble()

data_sf <- 
   tar_read(back_vals_filter_sf) %>% 
   bind_cols(as_tibble(st_coordinates(.))) %>% 
   st_drop_geometry() %>% 
   as_tibble() %>% 
   st_as_sf(coords = c("X", "Y")) %>% 
   st_sf(crs = CRS_REFERENCE)

test_blockcv <-
  data_sf %>%
  blockCV::spatialBlock(k = 5,
                        species = "ca_mg_l",
                        selection = "random",
                        theRange = 70000)

test_spatialcv <- 
   tar_read(train_test_split) %>% 
   pluck(1) %>% 
   training() %>% 
   left_join(key_table_coords, by = "station_id") %>% 
   sperrorest::partition_kmeans(coords = c("X", "Y"), nfold = 5)
   
   
   
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
 