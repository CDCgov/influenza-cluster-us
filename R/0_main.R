## new script ##################################################################
cat("\014")    # clear the console
rm(list=ls())  # remove all variables
graphics.off() # close all plots
set.seed(2023) # fix randomness
suppressMessages(library(ggplot2))
suppressMessages(library(ggdist))
suppressMessages(library(dplyr))

## new folder ##################################################################
unlink(    paste0("figure-temp/"), recursive = TRUE) # Delete the folder and its contents
dir.create(paste0("figure-temp/"), recursive = TRUE) # Create the new folder
plotting <- TRUE

## map #########################################################################
# custom map
source("fig_map.R")
# load shapefiles for map
source("fig_shapefiles.R")
shapefile_FULL <- fig_shapefiles(fig_map, plotting)

# define spatial neighborhood
source("spatial_nb.R")
spatial_nb_list <- spatial_nb(shapefile_FULL, "STATEFP", plotting)

## parameters ##################################################################
threshold <- 0.5 # threshold to discard seasons and states (used in 2_data_process.R)
k_max     <- 10  # max number of clusters (used in 5_cluster_seeds.R)
seeds     <- 100 # number of replication to count duplicated results (used in 5_cluster_seeds.R)

## three datasets ##############################################################
df_data <- list()
for(datatype in c("ILINet",                               # ILI data
                  "WHO_NREVSS_Combined_prior_to_2015_16", # test positivity data
                  "WHO_NREVSS_Clinical_Labs")             # test positivity data
) {
  # raw data
  source("1_data_raw.R")
  df_main <- data_raw(datatype, plotting)
  
  # processing data (imputation and smoothing)
  source("2_data_process.R")
  df_main <- data_process(df_main, threshold, plotting)
  
  # add column
  df_data[[datatype]] <- df_main %>%
    mutate(DATA_TYPE  = datatype) %>%
    mutate(
      DATA_TYPE = case_when(
        (DATA_TYPE == "WHO_NREVSS_Combined_prior_to_2015_16") ~ "WHO_NREVSS",
        (DATA_TYPE == "WHO_NREVSS_Clinical_Labs")             ~ "WHO_NREVSS",
        TRUE ~ DATA_TYPE # Keep other values unchanged
      )
    )
}

# Combine the data frames of all datasets
df_data <- data.table::rbindlist(df_data)

# plot processed data
source("2a_plot_processed_data.R")
plot_processed_data(df_data, plotting)

# define subset of data
source("3_data_subset.R")
df_subset <- data_subset()

## analysis in parallel ########################################################
# Create a cluster
tic <- Sys.time()
cl <- parallel::makeCluster(parallel::detectCores(), outfile = "")              # Create a cluster with the detected number of CPU cores
parallel::clusterExport(cl, c(ls(), lsf.str()))                                 # Export the all variables and functions to the cluster
# Main parallel
source("0_main_analysis.R")
df_result <- parallel::parLapply(cl, 1:nrow(df_subset), main_analysis)          # Use parLapply to apply the function in parallel
# Stop the cluster
parallel::stopCluster(cl)                                                       # Stop the cluster when done
print(toc <- Sys.time() - tic)

# Combine the data frames of all scenarios
df_result <- data.table::rbindlist(df_result)
# save results
save(df_result,
     file = paste0("figure-temp/df_result.RData"))

## handling the results ########################################################
# Moran's I: spatial autocorrelation of peak weeks
df_result %>%
  group_by(DATA_SUBSET) %>%
  filter(!is.na(MORAN_I_GLOBAL)) %>%
  summarise(
    count = n(),
    moran_i_mean = first(MORAN_I_GLOBAL),
    moran_i_p = first(MORAN_I_GLOBAL_p),
    all_same_mean = n_distinct(MORAN_I_GLOBAL) == 1,
    all_same_p = n_distinct(MORAN_I_GLOBAL_p) == 1
  ) %>%
  filter(all_same_mean & all_same_p) %>%
  mutate(
    moran_i_mean = sprintf("%.2f", moran_i_mean),
    moran_i_p = ifelse(moran_i_p < 0.05, "<0.05", sprintf("%.2f", moran_i_p))
  ) %>%
  as.data.frame()

# selected clusters
source("7_cluster_summary.R")
df_scn_cluster_count <- cluster_summary(df_result, plotting)
df_scn_cluster_count

# combine all maps
source("8_cluster_maps.R")
cluster_maps(df_subset, plotting)

# find significant differences between clusters
source("9_significant_diff.R")
significant_diff(df_result, plotting)
