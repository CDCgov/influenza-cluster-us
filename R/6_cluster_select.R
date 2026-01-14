cluster_select <- function(cl_list, datatype = "ILINet", period = "preCOVID19_2010_2020", averaging = T, plotting = T) {
  ## silhouette width ############################################################
  # initializes a clustering family
  fam <- new("tsclustFamily",
             dist = "L1",
             control = dtwclust::partitional_control(nrep = 100))
  # maximum number of clusters
  k_max <- length(cl_list) + 1
  # distance matrix
  for(k in 2:k_max) {
    cl_list[[k-1]]@distmat <- fam@dist(cl_list[[k-1]]@datalist)
  }
  # silhouette index
  cl_list_sil <- lapply(cl_list, function(cl_list) {
    cluster::silhouette(cl_list@cluster, cl_list@distmat)
  })
  
  # merge all lists
  cl_list_sil_all <- bind_rows(
    lapply(seq_along(cl_list_sil)+1, function(k) {
      as_tibble(cl_list_sil[[k-1]][,]) %>%
        arrange(cluster, desc(sil_width)) %>%
        mutate(order = rev(seq_along(cluster)),
               cluster = as.factor(cluster),
               k = k)  # Add a column for k
    })
  )
  
  # add mean and variance of silhouette width
  cl_list_sil_all <- cl_list_sil_all %>%
    group_by(k) %>%
    mutate(sil_width_mean = mean(sil_width),
           sil_width_var = var(sil_width))
  
  # check mean and variance of silhouette width
  cl_list_sil_all %>%
    group_by(k) %>%
    summarise(sil_width_mean = mean(sil_width),
              sil_width_var = var(sil_width))
  
  # plot silhouette width
  p <- cl_list_sil_all %>%
    ggplot(aes(x = order, y = sil_width, fill = cluster)) +
    geom_col() +
    geom_hline(aes(yintercept = sil_width_mean), linetype = "dashed", col = "black") +
    geom_hline(aes(yintercept = sil_width_var),  linetype = "dashed", col = "blue") +
    # scale_fill_brewer(palette = "Dark2") +
    scale_fill_manual(values = setNames(colorRampPalette(c("#0057b7", RColorBrewer::brewer.pal(8, "Dark2")))(k_max + 1), 1:(k_max + 1))) +
    coord_flip() +
    facet_wrap(k ~ ., scales = "fixed", labeller = "label_both") +
    labs(x = "Order of jurisdictions",
         y = "Silhouette width",
         fill = "Cluster") +
    theme_bw(base_size = 18)
  # save figure
  print( name_plot <- paste0("figure-temp/", "figure6-", datatype, "-", period, "-mean", averaging, "-cluster-silhouette", ".png") )
  if(plotting) ggplot2::ggsave(file = name_plot, plot = p, width = 16, height = 9, type = "cairo")
  
  ## selected number of clusters #################################################
  # cluster validity indices
  cl_cvi <- sapply(cl_list, dtwclust::cvi, type = "internal")
  # optimal k, selected by maximum averaged silhouette width
  k_opt <- which.max(cl_cvi["Sil", ]) + 1
  # optimal k, selected by minimum variance of silhouette width
  # k_opt <- which.min(sapply(cl_list_sil, function(x) var(x[, 3]))) + 1
  
  # selected cluster
  k <- k_opt
  
  # clustering with k = 2 had the best silhouette, extract results
  df_tibble <- tibble(STUSPS = names(cl_list[[k-1]]@datalist),
                      CLUSTER    = as.character(cl_list[[k-1]]@cluster),
                      SIL        = cl_list_sil[[k-1]][,3],
                      CL_DIST    = cl_list[[k-1]]@cldist[,1],
  ) %>%
    group_by(CLUSTER) %>%
    mutate(sd_dist   = sd(CL_DIST),
           mean_dist = mean(CL_DIST),
           sd_sil    = sd(SIL),
           mean_sil  = mean(SIL),
    ) %>%
    ungroup() %>%
    mutate(cluster_class = case_when(
      CL_DIST >= (mean_dist + sd_dist) ~ paste0(CLUSTER, "-like"),
      TRUE ~ CLUSTER
    )) %>%
    arrange(cluster_class)
  
  ## plot clusters (map) #########################################################
  # join with shapefile
  df_tibble <- left_join(shapefile_FULL, df_tibble, by = c("STUSPS"))
  
  # plot map (cluster)
  p0 <- df_tibble %>%
    ggplot() +
    geom_sf(aes(fill = CLUSTER)) +
    geom_sf_text(aes(label = STUSPS), col = "black") +
    # scale_fill_brewer(palette = "Dark2") +
    scale_fill_manual(values = setNames(colorRampPalette(c("#0057b7", RColorBrewer::brewer.pal(8, "Dark2")))(k_max + 1), 1:(k_max + 1))) +
    theme_void()
  # map title
  source("name_variable.R")
  name_variables <- name_variable(datatype, period, averaging)
  name_datatype  <- name_variables$name_datatype
  name_period    <- name_variables$name_period
  name_averaging <- name_variables$name_averaging
  name_map_title <- name_variables$name_map_title
  # custom map
  if(!grepl("season", period)) # multiple seasons
    p <- fig_map(p0, shapefile_FULL) +
    labs(title = name_map_title,
         fill = "Cluster") +
    theme(plot.title = element_text(size = rel(3)),
          legend.title = element_text(size = rel(3)),
          legend.text = element_text(size = rel(3)))
  if(grepl("season", period)) # single season
    p <- fig_map(p0, shapefile_FULL) +
    labs(title = name_period,
         fill = "Cluster") +
    theme(plot.title = element_text(size = rel(5)),
          legend.title = element_text(size = rel(3)),
          legend.text = element_text(size = rel(3)))
  # save figure
  print( name_plot <- paste0("figure-temp/", "figure6-", datatype, "-", period, "-mean", averaging, "-cluster-map", ".png") )
  if(plotting) ggplot2::ggsave(file = name_plot, plot = p, width = 14, height = 9, type = "cairo")
  
  ## plot clusters (time series) #################################################
  # plot clustering distribution
  fig <- list(p,
              plot(cl_list[[k-1]], plot = FALSE)) # not shown as output
  # combine plots
  p <- cowplot::plot_grid(plotlist = fig,
                          ncol = 1,
                          rel_heights = c(0.6, 0.4))
  # save figure
  print( name_plot <- paste0("figure-temp/", "figure6-", datatype, "-", period, "-mean", averaging, "-cluster-map_and_line", ".png") )
  if(plotting) ggplot2::ggsave(file = name_plot, plot = p, width = 16, height = 9*1.5, type = "cairo")
  
  ## plot distance (map) ###############################################################
  # plot map (distance)
  fig <- list()
  for (i in sort(unique(df_tibble$cluster_class[!is.na(df_tibble$cluster_class)]))) {
    # plot map
    p0 <- df_tibble %>%
      filter(cluster_class == i) %>%
      ggplot() +
      geom_sf(data = shapefile_FULL, fill = "white") +
      geom_sf(aes(fill = CL_DIST)) +
      geom_sf_text(aes(label = STUSPS), col = "black") +
      scale_fill_gradient(low = "#0057b7", high = "#9a3b25", limits = range(df_tibble$CL_DIST, na.rm = T)) +
      guides(fill = guide_colorbar(barwidth = 1, barheight = 35)) +
      theme_void()
    
    # each clusters
    fig[[i]] <- fig_map(p0, shapefile_FULL) +
      labs(title = paste0("Cluster ", i),
           fill = "Distance") +
      theme(plot.title = element_text(size = rel(3)),
            legend.title = element_text(size = rel(3)),
            legend.text = element_text(size = rel(3)))
  }
  
  # plot all clusters
  p <- cowplot::plot_grid(plotlist = fig,
                          ncol = 2,
                          # labels = names(fig),
                          hjust = 0,
                          label_size = 24, label_fontface = "plain",
                          align = "hv", axis = "t")
  # save figure
  print( name_plot <- paste0("figure-temp/", "figure6-", datatype, "-", period, "-mean", averaging, "-cluster-map-distance", ".png") )
  if(plotting) ggplot2::ggsave(file = name_plot, plot = p, width = 16*2, height = 9*2, type = "cairo")
  
  return(df_tibble)
}
