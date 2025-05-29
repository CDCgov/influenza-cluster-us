cluster_summary <- function(df_result, plotting = T) {
  ## count cluster size ########################################################
  df_scn_cluster_count = df_result %>%
    group_by(DATA_SUBSET, CLUSTER) %>%
    summarise(count = n())
  
  ## sort STUSPS by latitude ###################################################
  df_scn_lat <- df_result %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs = 4326) %>%
    mutate(centroid = sf::st_centroid(geometry)) %>%
    arrange(sf::st_coordinates(centroid)[, 2]) %>%
    mutate(STUSPS = factor(STUSPS, levels = unique(STUSPS)))
  
  ## plot all clustering #######################################################
  # rename
  source("name_variable.R")
  name_variables <- name_variable(df_scn_lat$DATA_TYPE, df_scn_lat$DATA_PERIOD, df_scn_lat$AVG)
  df_scn_lat$DATA_TYPE   <- name_variables$name_datatype
  df_scn_lat$DATA_PERIOD <- name_variables$name_period
  df_scn_lat$AVG         <- name_variables$name_averaging
  # Plot all clustering results
  p <- df_scn_lat %>%
    ggplot() +
    geom_tile(aes(x = DATA_PERIOD, y = STUSPS, fill = as.factor(CLUSTER)),
              col = "black") +
    facet_grid(DATA_TYPE ~ AVG + (DATA_SEASON > 1), scales = "free_x", space = "free_x",
               labeller = labeller(`(DATA_SEASON > 1)` = c("TRUE" = "Multiple seasons",
                                                           "FALSE" = "Single season"))) +
    # scale_fill_brewer(palette = "Dark2") +
    scale_fill_manual(values = setNames(colorRampPalette(c("#0057b7", RColorBrewer::brewer.pal(8, "Dark2")))(k_max + 1), 1:(k_max + 1))) +
    labs(x = "Period",
         y = "Jurisdiction",
         fill = "Cluster") +
    theme_bw(base_size = 15) +
    theme(axis.text.x  = element_text(angle = 90, hjust = 1, vjust = 0.5),
          axis.text.y  = element_text(size = rel(0.6)), 
          strip.text.y = element_text(size = rel(0.9)))
  # save figure
  print( name_plot <- paste0("figure-temp/", "figure7-cluster-tile", ".png") )
  if(plotting) ggplot2::ggsave(file = name_plot, plot = p, width = 16, height = 9*2, type = "cairo")
  
  return(df_scn_cluster_count)
}
