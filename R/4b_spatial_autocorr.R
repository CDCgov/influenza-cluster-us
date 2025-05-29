spatial_autocorr <- function(df_peak, datatype = "ILINet", period = "preCOVID19_2010_2020", averaging = T, plotting = T) {
  ## define spatial neighborhood ###############################################
  # define spatial neighborhood and exclude those without data
  spatial_nb_list <- spatial_nb(df_peak, "WEEK_SEASON_PEAK", plotting = F)
  #
  nb_list   = spatial_nb_list$nb_list
  nb_list_W = spatial_nb_list$nb_list_W
  nb_mat_W  = spatial_nb_list$nb_mat_W
  nb_list_B = spatial_nb_list$nb_list_B
  nb_mat_B  = spatial_nb_list$nb_mat_B
  Y         = spatial_nb_list$Y
  
  ## Moran's I (global dependence) #############################################
  # Moran's plot: against their spatially lagged values
  if(FALSE) spdep::moran.plot(Y, nb_list_W)
  
  # Compute the deviation from the mean for each value
  r <- Y - mean(Y)
  # Calculate Moran's I statistic = (r' * W * r) / (sum(r^2) * n / sum(W))
  moran_I_formula <- sum(r %*% t(r) * nb_mat_W) / (sum(r^2) * nrow(nb_mat_W) / sum(nb_mat_W))
  
  # equivalent to the slope of the regression model
  moran_I_regression <- coef(lm(nb_mat_W %*% Y ~ Y))[2]
  
  # Moran's I test with Gaussian assumptions (default)
  moran_I_norm <- spdep::moran.test(Y, nb_list_W, randomisation = FALSE)
  # Moran's I test under non-Gaussian assumptions with randomization
  moran_I_random <- spdep::moran.test(Y, nb_list_W, randomisation = TRUE)
  
  # number of permutations
  N <- 10000
  # Moran's I permutation test
  moran_I_perm <- spdep::moran.mc(Y, nb_list_W, nsim = N)
  
  # Plot the histogram of Moran's I under the null hypothesis
  if(FALSE)
    ggplot(data.frame(res = moran_I_perm$res), aes(x = res)) +
    geom_histogram() +
    geom_vline(aes(xintercept = moran_I_formula)) +
    theme_bw()
  
  # Create a data frame with all the methods
  df_moran_global <- data.frame(
    METHOD = c("Formula",
               "Regression Model",
               "Gaussian Test",
               "Non-Gaussian Test",
               "Permutation Test"),
    MORAN_I_GLOBAL = c(moran_I_formula,
                       moran_I_regression,
                       moran_I_norm$estimate[1],
                       moran_I_random$estimate[1],
                       moran_I_perm$statistic),
    MORAN_I_GLOBAL_p = c(NA,
                         NA,
                         moran_I_norm$p.value,
                         moran_I_random$p.value,
                         moran_I_perm$p.value) # NA for methods that do not provide a p-value
  )
  df_moran_global
  
  ## Moran's I (local dependence) ##############################################
  # # Compute Local Moran's I statistics with Gaussian assumptions (replaced by the permutation test below)
  # moran_I_local <- spdep::localmoran(Y, nb_list_B, alternative = "greater") %>%
  #   as.data.frame() %>%
  #   tibble::rownames_to_column(var = "STUSPS") %>%
  #   mutate(Significant_norm = ifelse(`Pr(z > E(Ii))` < 0.05, "Yes", "No"))
  
  # Compute Local Moran's I statistics with permutation test
  moran_I_local <- spdep::localmoran_perm(Y, nb_list_W, nsim = N, alternative = "greater") %>%
    cbind(as.data.frame(attr(., "quadr"))) %>%
    tibble::rownames_to_column(var = "STUSPS") %>%
    mutate(Significant_norm = ifelse(`Pr(z > E(Ii))` < 0.05, "Yes", "No")) %>%
    mutate(`Pr(z > E(Ii)) Sim` = 1 - `Pr(z > E(Ii)) Sim`) %>% # fix bug
    mutate(Significant_perm = ifelse(`Pr(z > E(Ii)) Sim` < 0.05, "Yes", "No"))
  
  # permutation test
  moran_I_local_perm <- matrix(NA, ncol = N, nrow = length(Y))
  for (i in 1:N) {
    permuted_Y <- sample(Y, length(Y), replace = TRUE)
    moran_I_local_perm[, i] <- spdep::localmoran(permuted_Y, nb_list_B)[, "Ii"]
  }
  
  # Check normality of Local Moran's I
  if(FALSE)
    moran_I_local_perm %>%
    as.data.frame() %>%
    mutate(STUSPS = moran_I_local$STUSPS) %>%
    reshape2::melt(id.vars = "STUSPS") %>%
    ggplot(aes(sample = value)) +
    stat_qq() +
    stat_qq_line() +
    facet_wrap(~ STUSPS) +
    theme_bw()
  
  # Plot the histogram of Local Moran's I under the null hypothesis
  if(FALSE)
    moran_I_local_perm %>%
    as.data.frame() %>%
    mutate(STUSPS = moran_I_local$STUSPS) %>%
    reshape2::melt(id.vars = "STUSPS") %>%
    ggplot() +
    geom_histogram(aes(x = value)) +
    geom_vline(data = moran_I_local, aes(xintercept = Ii)) +
    facet_wrap(~ STUSPS) +
    theme_bw()
  
  # Calculate p-value by the proportion where Local Moran's I exceeds observed values
  # P(Local Moran's I > observed | Null)
  moran_I_local$p_hist <- apply(sweep(moran_I_local_perm, 1, moran_I_local[, "Ii"], ">"), 1, mean)
  # Adjust p-values for cases where all permutations were less than the observed value
  moran_I_local$p_hist[moran_I_local$p_hist == 0] <- 1 / N
  # Add significance column based on p-value threshold
  moran_I_local$Significant_hist = ifelse(moran_I_local$p_hist < 0.05, "Yes", "No")
  
  # join with shapefile
  df_moran_local <- left_join(df_peak, moran_I_local, by = c("STUSPS")) %>%
    filter(!STUSPS %in% c("AK", "HI", "PR", "VI", "GU", "MP", "AS"))
  
  ## plot local Moran's I in map ###############################################
  # plot using mapview
  p_map_I   <- mapview::mapview(df_moran_local, zcol = "Ii")
  p_map_p   <- mapview::mapview(df_moran_local, zcol = "Pr(z > E(Ii)) Sim")
  p_map_py  <- mapview::mapview(df_moran_local, zcol = "pysal")
  p_map_p_n <- mapview::mapview(df_moran_local, zcol = "Significant_norm")
  p_map_p_p <- mapview::mapview(df_moran_local, zcol = "Significant_perm")
  p_map_p_h <- mapview::mapview(df_moran_local, zcol = "Significant_hist")
  if(FALSE) leafsync::sync(p_map_I, p_map_p, p_map_py, p_map_p_n, p_map_p_p, p_map_p_h)
  
  # List of variables to plot
  variables <- c("Ii",
                 "`Pr(z > E(Ii)) Sim`",
                 "pysal",
                 "Significant_norm",
                 "Significant_perm",
                 "Significant_hist")
  
  # List to store the individual plots
  figs <- list()
  # Loop through the variables and create plots
  for (i in seq_along(variables)) {
    figs[[i]] <- df_moran_local %>%
      ggplot(aes_string(fill = variables[i])) +
      geom_sf() +
      theme_void() +
      ggtitle(variables[i])  # Title based on the variable name
  }
  
  # Combine all the plots into one figure using plot_grid
  p = cowplot::plot_grid(plotlist = figs,
                         nrow = 2,
                         # labels = "AUTO",
                         hjust = 0,
                         label_size = 28, label_fontface = "plain",
                         align = "hv", axis = "b")
  
  
  # plot in map (local Moran's I)
  p0 <- df_moran_local %>%
    ggplot() +
    geom_sf(aes(fill = Ii)) +
    geom_sf_text(aes(label = STUSPS), col = "white") +
    # scale_fill_brewer(palette = "Dark2") +
    # scale_fill_manual(values = setNames(colorRampPalette(c("#0057b7", RColorBrewer::brewer.pal(8, "Dark2")))(k_max + 1), 1:(k_max + 1))) +
    theme_void()
  # map title
  source("name_variable.R")
  name_map_title <- name_variable(datatype, period, averaging)$name_map_title
  # custom map
  p <- fig_map(p0, shapefile_FULL) +
    labs(title = name_map_title,
         fill = "Local Moran's I") +
    guides(fill = guide_colorbar(barwidth = 1, barheight = 35)) +
    theme(plot.title = element_text(size = rel(3)),
          legend.title = element_text(size = rel(2)),
          legend.text = element_text(size = rel(2)))
  
  # save figure
  print( name_plot <- paste0("figure-temp/", "figure4b-", datatype, "-", period, "-mean", averaging, "-moran_local-map", ".png") )
  if(plotting) ggplot2::ggsave(file = name_plot, plot = p, width = 16, height = 9, type = "cairo")
  
  ## global and local Moran's I as output ######################################
  # Moran's I statistics with permutation test
  df_moran <- moran_I_local %>%
    rename(MORAN_I_LOCAL = Ii) %>%
    mutate(df_moran_global %>%
             filter(METHOD == "Permutation Test") %>%
             select(MORAN_I_GLOBAL, MORAN_I_GLOBAL_p))
  
  return(df_moran)
}
