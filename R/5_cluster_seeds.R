cluster_seeds <- function(df_list, k_max = 10, seeds = 100, datatype = "ILINet", period = "preCOVID19_2010_2020", averaging = T, plotting = T) {
  
  # maximum number of clusters
  k_max
  # number of seeds
  seeds
  
  # clustering using different seeds
  df_sil <- matrix(NA, nrow = k_max - 1, ncol = seeds,
                   dimnames = list(2:k_max, 1:seeds))
  df_clusters <- matrix(NA, nrow = length(df_list), ncol = seeds,
                        dimnames = list(names(df_list), 1:seeds))
  cl_list <- list()
  for(seed_i in 1:seeds){
    # simple partitional clustering with Euclidean distance and k-means
    # time series clustering with z-score adjustment
    cl_list[[seed_i]] <- dtwclust::tsclust(df_list,
                                           type = "partitional",
                                           k = 2:k_max,
                                           preproc = dtwclust::zscore,
                                           distance = "L1",
                                           centroid = "mean",
                                           seed = seed_i,
                                           trace = FALSE, # output regarding the progress is printed to screen
                                           dtwclust::partitional_control(nrep = 100))
    
    # cluster validity indices
    cl_cvi <- sapply(cl_list[[seed_i]], dtwclust::cvi, type = "internal")
    # optimal k, selected by maximum averaged silhouette width
    k_opt <- which.max(cl_cvi["Sil", ]) + 1
    # optimal k, selected by minimum variance of silhouette width
    # k_opt <- which.min(sapply(cl_list_sil, function(x) var(x[, 3]))) + 1
    # selected cluster
    k <- k_opt
    
    # remove those results with single-element clusters
    for (k_i in 2:k_max) {
      if (1 %in% table(cl_list[[seed_i]][[k_i-1]]@cluster)) {
        cl_cvi["Sil", k_i-1] <- NA
      }
    }
    df_sil[, seed_i] <- cl_cvi["Sil", ]
    
    # sort cluster index in decreasing order
    df_clusters[, seed_i] <-
      cl_list[[seed_i]][[k-1]]@cluster <-
      as.integer(as.character(factor(cl_list[[seed_i]][[k-1]]@cluster,
                                     levels = 1:k,
                                     labels = names(sort(table(cl_list[[seed_i]][[k-1]]@cluster), decreasing = TRUE)))))
  }
  
  # count the duplicated ones
  df_clusters_text <- apply(t(df_clusters), 1, paste, collapse = ",")
  dup_table <- table(df_clusters_text)
  dup_max <- names(dup_table)[which.max(dup_table)]
  # sort by the most duplicated ones first
  dup_index_sort <- as.numeric(names(sort(factor(df_clusters_text, levels = names(sort(dup_table, decreasing = TRUE))))))
  # index of the most duplicated ones
  dup_index <- which(df_clusters_text == dup_max)
  # take one result
  cl_list <- cl_list[[dup_index[1]]]
  
  # # take the result with maximum averaged silhouette width, instead of most duplicated
  # k_seed_max <- which(df_sil == max(df_sil, na.rm = TRUE), arr.ind = TRUE)
  # cl_list <- cl_list[[k_seed_max[1, "col"]]]
  
  # plot clustering using different seeds
  p <- df_clusters[, dup_index_sort] %>%
    as.table %>%
    as.data.frame() %>%
    rename(STUSPS = Var1, SEED = Var2, CLUSTER = Freq) %>%
    ggplot() +
    geom_tile(aes(x = SEED, y = STUSPS, fill = as.factor(CLUSTER)),
              col = "black") +
    # scale_fill_brewer(palette = "Dark2") +
    scale_fill_manual(values = setNames(colorRampPalette(c("#0057b7", RColorBrewer::brewer.pal(8, "Dark2")))(k_max + 1), 1:(k_max + 1))) +
    theme_bw(base_size = 15) +
    theme(axis.text.x  = element_text(angle = 90, hjust = 1, vjust = 0.5))
  # save figure
  print( name_plot <- paste0("figure-temp/", "figure5-", datatype, "-", period, "-mean", averaging, "-cluster-seeds", ".png") )
  if(plotting) ggplot2::ggsave(file = name_plot, plot = p, width = 16, height = 9, type = "cairo")
  
  ## cluster of each state #######################################################
  # cluster of each state
  df_k <-
    lapply(2:k_max, function(k) {
      # cluster of each state
      cl_k   <- data.frame(STUSPS = names(cl_list[[k-1]]@datalist),
                           CLUSTER = as.character(cl_list[[k-1]]@cluster)) %>%
        mutate(k = k)
      return(cl_k)
    }) %>%
    bind_rows() %>%
    mutate(CLUSTER = factor(CLUSTER,
                            levels = 1:k_max))
  
  # plot cluster
  p <- ggplot() +
    geom_tile(data = df_k, aes(x = as.factor(k), y = STUSPS, fill = CLUSTER), col = "black") +
    geom_text(data = df_k, aes(x = as.factor(k), y = STUSPS, label = CLUSTER), col = "black", size = 4) +
    # scale_fill_brewer(palette = "Dark2") +
    scale_fill_manual(values = setNames(colorRampPalette(c("#0057b7", RColorBrewer::brewer.pal(8, "Dark2")))(k_max + 1), 1:(k_max + 1))) +
    theme_bw(base_size = 15)
  # save figure
  print( name_plot <- paste0("figure-temp/", "figure5-", datatype, "-", period, "-mean", averaging, "-cluster-k", ".png") )
  if(plotting) ggplot2::ggsave(file = name_plot, plot = p, width = 16, height = 9, type = "cairo")
  
  ## centroids ###################################################################
  if (ncol(df_list[[1]]) == 1) { # plot centroids (only ILINet | WHO_NREVSS)
    # centroids of all k
    cl_centroids <-
      lapply(2:k_max, function(k) {
        cl_centroids <- simplify2array(cl_list[[k-1]]@centroids) %>%
          as.data.frame() %>%
          rename_with(~as.character(1:k)) %>%
          tibble::rownames_to_column(var = "week") %>%
          mutate(week = as.numeric(week)) %>%
          tidyr::gather(key = "CLUSTER", value = "value", -week) %>%
          mutate(k = k,
                 CLUSTER = CLUSTER)
        return(cl_centroids)
      }) %>%
      bind_rows() %>%
      mutate(CLUSTER = factor(CLUSTER,
                              levels = 1:k_max))
    
    # plot all centroids
    p <- cl_centroids %>%
      ggplot(aes(x = week, y = value, col = as.factor(k), group = CLUSTER)) +
      geom_line() +
      # scale_color_brewer(palette = "Dark2") +
      scale_color_manual(values = setNames(colorRampPalette(c("#0057b7", RColorBrewer::brewer.pal(8, "Dark2")))(k_max + 1), 1:(k_max + 1))) +
      facet_wrap(k ~ ., scales = "fixed", labeller = "label_both") +
      theme_bw(base_size = 18)
    # save figure
    print( name_plot <- paste0("figure-temp/", "figure5-", datatype, "-", period, "-mean", averaging, "-cluster-centroids", ".png") )
    if(plotting) ggplot2::ggsave(file = name_plot, plot = p, width = 16, height = 9, type = "cairo")
    
    # datalist and cluster of all k
    df_datalist <-
      lapply(2:k_max, function(k) {
        # datalist
        cl_datalist_k <- cl_list[[k-1]]@datalist
        # cluster
        cl_cluster_k  <- as.character(cl_list[[k-1]]@cluster)
        # datalist and cluster
        cl_datalist   <- simplify2array(cl_datalist_k) %>%
          as.data.frame() %>%
          tibble::rownames_to_column(var = "week") %>%
          mutate(week = as.numeric(week)) %>%
          tidyr::gather(key = "STUSPS", value = "value", -week) %>%
          mutate(k = k) %>%
          mutate(CLUSTER = factor(STUSPS,
                                  levels = names(cl_datalist_k),
                                  labels = cl_cluster_k))
        return(cl_datalist)
      }) %>%
      bind_rows() %>%
      mutate(CLUSTER = factor(CLUSTER,
                              levels = 1:k_max))
    
    # plot all centroids with datalist and cluster
    p <- ggplot() +
      geom_line(data = df_datalist,  aes(x = week, y = value, group = STUSPS), col = "black", alpha = 0.8) +
      geom_line(data = cl_centroids, aes(x = week, y = value, col = as.factor(k), group = CLUSTER), alpha = 0.8, linewidth = 2) +
      # scale_color_brewer(palette = "Dark2") +
      scale_color_manual(values = setNames(colorRampPalette(c("#0057b7", RColorBrewer::brewer.pal(8, "Dark2")))(k_max + 1), 1:(k_max + 1))) +
      facet_grid(k ~ CLUSTER, scales = "fixed", labeller = "label_both") +
      theme_bw(base_size = 15) +
      theme(axis.text.x  = element_text(angle = 90, hjust = 1, vjust = 0.5))
    # save figure
    print( name_plot <- paste0("figure-temp/", "figure5-", datatype, "-", period, "-mean", averaging, "-cluster-datalist", ".png") )
    if(plotting) ggplot2::ggsave(file = name_plot, plot = p, width = 16, height = 9, type = "cairo")
  }
  
  return(cl_list)
}
