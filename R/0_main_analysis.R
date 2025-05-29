main_analysis <- function(subset_n = 1) {
  ## library ###################################################################
  suppressMessages(library(ggplot2))
  suppressMessages(library(ggdist))
  suppressMessages(library(dplyr))
  suppressMessages(library(sf))
  
  ## analysis of one dataset ###################################################
  # subset of data
  datatype    <- as.character(df_subset$DATA_TYPE[subset_n])
  period      <- as.character(df_subset$DATA_PERIOD[subset_n])
  averaging   <- as.logical(df_subset$AVG[subset_n])
  
  # prepare input for clustering
  source("4_data_into_list.R")
  df_list <- data_into_list(df_data, datatype, period, averaging, plotting)
  
  # clustering using different seeds
  source("5_cluster_seeds.R")
  cl_list <- cluster_seeds(df_list, k_max, seeds, datatype, period, averaging, plotting)
  
  # selecting number of clusters
  source("6_cluster_select.R")
  df_tibble <- cluster_select(cl_list, datatype, period, averaging, plotting)
  
  # analysis of peak weeks (only ILINet | WHO_NREVSS)
  if (datatype != "ILINet_WHO_NREVSS" & averaging == TRUE) {
    
    # peak weeks of mean across seasons
    source("4a_peak_week.R")
    df_peak <- peak_week(df_list, datatype, period, averaging, plotting)
    
    # spatial autocorrelation of peak weeks
    source("4b_spatial_autocorr.R")
    df_moran <- spatial_autocorr(df_peak, datatype, period, averaging, plotting)
    
    # add columns to clustering results
    df_tibble <- df_tibble %>%
      left_join(df_peak %>%
                  select(STUSPS, WEEK_SEASON_PEAK) %>%
                  sf::st_drop_geometry(), by = "STUSPS") %>%
      left_join(df_moran %>%
                  select(STUSPS, MORAN_I_LOCAL, MORAN_I_GLOBAL, MORAN_I_GLOBAL_p), by = "STUSPS") %>%
      bind_cols(df_subset[subset_n, ])
    
  } else {
    
    # add columns to clustering results
    df_tibble <- df_tibble %>%
      mutate(WEEK_SEASON_PEAK = NA) %>%
      mutate(MORAN_I_LOCAL = NA,
             MORAN_I_GLOBAL = NA,
             MORAN_I_GLOBAL_p = NA) %>%
      bind_cols(df_subset[subset_n, ])
    
  }
  
  return(df_tibble)
}
