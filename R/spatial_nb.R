spatial_nb <- function(shapefile_FULL, MAIN_COL = "STATEFP", plotting = T) {
  ## define spatial neighborhood ###############################################
  # Filter out geometries without neighbors (e.g., territories)
  shapefile_FULL_nb <- shapefile_FULL %>%
    filter(!STUSPS %in% c("AK", "HI", "PR", "VI", "GU", "MP", "AS"))
  
  # Filter out those (e.g., FL) without data to avoid error in adjacency matrix W
  shapefile_FULL_nb_W <- shapefile_FULL_nb %>%
    filter(!is.na(.data[[MAIN_COL]]))
  
  # The data
  Y <- shapefile_FULL_nb_W[[MAIN_COL]]
  
  # Compute neighbors using poly2nb (Queen's case)
  nb_list <- spdep::poly2nb(shapefile_FULL_nb_W, queen = TRUE, row.names = shapefile_FULL_nb_W$STUSPS)
  
  # Check if "NYC" exists
  if ("NYC" %in% shapefile_FULL_nb_W$STUSPS) {
    # Index of NYC, NY and NJ
    index_NYC <- which(shapefile_FULL_nb_W$STUSPS == "NYC")
    index_NY  <- which(shapefile_FULL_nb_W$STUSPS == "NY")
    index_NJ  <- which(shapefile_FULL_nb_W$STUSPS == "NJ")
    # Manually add neighbors (NY and NJ) of NYC
    nb_list[[index_NYC]] <- as.integer(c(index_NY, index_NJ))
    nb_list[[index_NY]]  <- as.integer(c(nb_list[[index_NY]], index_NYC))
    nb_list[[index_NJ]]  <- as.integer(c(nb_list[[index_NJ]], index_NYC))
  }
  
  # Convert the neighbors list to a spatial weights list with row-standardized weights
  nb_list_W <- spdep::nb2listw(nb_list, style = "W", zero.policy = TRUE)
  # Convert the neighbors list to a row standardized adjacency matrix
  nb_mat_W <- spdep::nb2mat(nb_list, style = "W", zero.policy = TRUE)
  # Set column and row names for clarity in the matrix
  colnames(nb_mat_W) <- rownames(nb_mat_W)
  
  ## neighborhood visualization (matrix) #######################################
  # Convert the neighbors list to a spatial weights list with binary weights
  nb_list_B <- spdep::nb2listw(nb_list, style = "B", zero.policy = TRUE)
  # Convert the neighbors list to a binary adjacency matrix
  nb_mat_B <- spdep::nb2mat(nb_list, style = "B", zero.policy = TRUE)
  # Set column and row names for clarity in the matrix
  colnames(nb_mat_B) <- rownames(nb_mat_B)
  
  # Add row and column sums to the matrix
  nb_mat_sum <- addmargins(nb_mat_B)
  
  # Plot the adjacency matrix using ggplot2
  p <- reshape2::melt(nb_mat_B) %>%
    ggplot() +
    geom_tile(aes(x = Var1, y = Var2, fill = as.factor(value))) +
    scale_fill_brewer(palette = "Dark2") +
    # scale_fill_manual(values = setNames(colorRampPalette(c("#0057b7", RColorBrewer::brewer.pal(8, "Dark2")))(k_max + 1), 1:(k_max + 1))) +
    theme_bw(base_size = 15) +
    theme(axis.text.x  = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    coord_equal()
  # save figure
  print( name_plot <- paste0("figure-temp/", "figure0-matrix-nb", ".png") )
  if(plotting) ggplot2::ggsave(file = name_plot, plot = p, width = 16, height = 9, type = "cairo")
  
  ## neighborhood visualization (map) ##########################################
  # Compute centroids of each geometry
  shapefile_FULL_nb_W <- shapefile_FULL_nb_W %>%
    mutate(centroid = sf::st_centroid(geometry))
  
  # Initialize an empty data frame to store edge connections
  edges_list <- list()
  # Loop through each polygon to create edges based on neighbors
  for (i in seq_along(nb_list)) {
    # Check if there are valid neighbors
    if (length(nb_list[[i]]) > 1) {
      for (j in nb_list[[i]]) {
        # Append the edge information to the edges data frame
        edges_list[[length(edges_list) + 1]] <- data.frame(
          i = i,
          j = j,
          lon1 = sf::st_coordinates(shapefile_FULL_nb_W$centroid)[i, 1],
          lat1 = sf::st_coordinates(shapefile_FULL_nb_W$centroid)[i, 2],
          lon2 = sf::st_coordinates(shapefile_FULL_nb_W$centroid)[j, 1],
          lat2 = sf::st_coordinates(shapefile_FULL_nb_W$centroid)[j, 2]
        )
      }
    }
  }
  # Combine all rows into a single data frame
  df_edges <- do.call(rbind, edges_list)
  
  ## plot neighbors in map #####################################################
  # plot neighbors in map
  p <- shapefile_FULL_nb_W %>%
    ggplot() +
    geom_sf(data = shapefile_FULL_nb, fill = "gray50") +
    geom_sf(aes(geometry = geometry, fill = .data[[MAIN_COL]])) + # Plot the geometries
    geom_sf(aes(geometry = centroid), color = "black", size = 3, shape = 1, stroke = 2) + # Plot centroids
    geom_segment(data = df_edges, aes(x = lon1, y = lat1, xend = lon2, yend = lat2), color = "darkred", linewidth = 1) + # Plot edges
    theme_void()
  # save figure
  print( name_plot <- paste0("figure-temp/", "figure0-map-nb", ".png") )
  if(plotting) ggplot2::ggsave(file = name_plot, plot = p, width = 16, height = 9, type = "cairo")
  
  return(list(
    nb_list   = nb_list,
    nb_list_W = nb_list_W,
    nb_mat_W  = nb_mat_W,
    nb_list_B = nb_list_B,
    nb_mat_B  = nb_mat_B,
    Y         = Y
  ))
}
