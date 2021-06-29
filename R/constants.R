# Program Configuration
YML_CONFIG <- yaml::read_yaml('config.yml')

RUN_MODE <- purrr::chuck(YML_CONFIG, "run_mode")


# Hard Copied from MOHP Data Preparation
MIN_SAMPLE_DATE <- "1990-01-01"
MAX_SAMPLE_DEPTH <- 100
STATION_ID <- c("stprj_id", "stat_id")


CRS_REFERENCE <- 25832
CRS_LEAFLET <- 3857
TRAIN_TEST_SPLIT_PROPORTION <- .8
COLOUR_HEX_TRAIN <- "#F8766D" #"#F8766D"
COLOUR_HEX_TEST <- "#00BFC4" #"#00BFC4"
COLOUR_HEX_BAR_FILL <- "#374776"
COLOUR_HEX_POINT_FILL <- "#374776"
COLOUR_HEX_DIVERGING_POSITIVE <- "#74ADAF"
COLOUR_HEX_DIVERGING_NEGATIVE <- "#EF7C98"
COLOUR_HEX_DIVERGING_ZERO <- "#FAE8DA"
ALPHA_BARS <- .7
ALPHA_POINTS <- .3
NUMBER_OF_FOLDS <- 5
NUMBER_OF_STRATA_BREAKS <- 10

if(RUN_MODE == "test") {
  NUMBER_OF_HYPERPARAMETER_COMBINATIONS <- 3
  NUMBER_OF_HYPERPARAMETER_COMBINATIONS_NNET <- 3
} else if(RUN_MODE == "full") {
  NUMBER_OF_HYPERPARAMETER_COMBINATIONS <- 50
  NUMBER_OF_HYPERPARAMETER_COMBINATIONS_NNET <- 50
} else {
  stop("Please provide a valid value for the run_mode in config.yml. Currently supported values are : 'test' and 'full'")
}


PERFORMANCE_METRIC_FOR_BESTMODEL_SELECTION <- "rmse"
COLOUR_SCHEME <- 
  c(
    "#74ADAF",
    "#A7D0C2",
    "#FAE8DA",
    "#EF7C98",
    "#374776"
  )

FEATURE_NAMES <- 
  list(
  "feature_lulc",
  "feature_gw_recharge",
  "feature_seepage",
  "feature_temperature",
  "feature_precipitation",
  # "feature_humus",
  "feature_hydrounits",
  "feature_geology",
  "feature_soilunits",
  "feature_hydrogeology_gc",
  "feature_hydrogeology_kf",
  "feature_hydrogeology_ga",
  "feature_hydrogeology_ha"
)

NUMERICAL_FEATURES <- 
  c(
    "aspect", "elevation", "sampledepth", "seepage", "gwrecharge", "precipitation", "slope"
  )

HYDROGEOCHEMICAL_PARAMS <-
  c(
    "ag_mg_l", "al_mg_l", "ar_mg_l", "as_mg_l",
    "b_mg_l", "ba_mg_l", "bi_mg_l", "br_mg_l",
    "ca_mg_l", "cd_mg_l", "cl_mg_l", "cn_mg_l",
    "co_mg_l", "co2_mg_l", "co3_mg_l", "cr_mg_l",
    "csb_mg_l", "cu_mg_l", "doc_mg_l", "f_mg_l",
    "fe_mg_l_original", "fe_2_mg_l", "fe_3_mg_l",
    "fe_tot_mg_l", "fe_mg_l_zus", "fe_mg_l", "ghaerte_mg",
    "ghaerte_mmol_l", "hco3_mg_l", "hg_mg_l", "h2s_mg_l",
    "i_mg_l", "k_mg_l", "kbase_43_mmol_l", "kbase_82_mmol_l",
    "kmno4_mg_l", "ksaeure_43_mmol_l", "ksaeure_82_mmol_l",
    "lf_u_scm", "li_mg_l", "mg_mg_l", "mn_mg_l", "mo_mg_l",
    "na_mg_l", "nh4_mg_l", "ni_mg_l", "no2_mg_l", "no3_mg_l",
    "o2_mg_l", "pb_mg_l", "ph", "po4_mg_l", "redox_m_v",
    "sb_mg_l", "se_mg_l", "si_mg_l", "sio2_mg_l", "sn_mg_l",
    "so4_mg_l", "sr_mg_l", "t_grad_c", "te_mg_l", "tl_mg_l",
    "tn_mg_l", "toc_mg_l", "tp_mg_l", "u_mg_l", "v_mg_l",
    "zn_mg_l"
  )

HYDROGEOCHEMICAL_PARAMS_SUBSET <-
  c(
    "as_mg_l", "ca_mg_l", "cl_mg_l", "fe_mg_l",
    "hco3_mg_l", "k_mg_l", "lf_u_scm",
    "mg_mg_l", "mn_mg_l", "na_mg_l", "no3_mg_l",
    "o2_mg_l", "ph", "po4_mg_l", "so4_mg_l"
  )
HYDROGEOCHEMICAL_PARAMS_MAJOR_IONS <-
  c(
    "ca_mg_l", "cl_mg_l", "fe_mg_l", "hco3_mg_l",
    "k_mg_l", "mg_mg_l", "mn_mg_l", "na_mg_l",
    "no3_mg_l", "so4_mg_l"
  )