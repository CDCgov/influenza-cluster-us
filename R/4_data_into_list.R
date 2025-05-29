data_into_list <- function(df_data, datatype = "ILINet", period = "preCOVID19_2010_2020", averaging = T, plotting = T) {
  ## subset of data ############################################################
  # ILINet / WHO_NREVSS
  df_data_subset <- df_data %>%
    filter(stringr::str_detect(datatype, DATA_TYPE))
  
  # split the (string) period by "_"
  period1 <- as.numeric(strsplit(period, "_")[[1]][2])
  period2 <- as.numeric(strsplit(period, "_")[[1]][3])
  
  # selected seasons only
  df_data_subset <- df_data_subset %>%
    # filter(YEAR_SEASON == period) %>%
    filter(YEAR > period1 | (YEAR == period1 & WEEK >= 40)) %>% # from the year (period1)
    filter(YEAR < period2 | (YEAR == period2 & WEEK <  40)) %>% # to   the year (period2)
    filter(YEAR_SEASON != "2024-2025")
  
  # mean across seasons or full time series
  if (averaging == T) {
    ## mean ####################################################################
    # remove the 53rd week each year
    if(!grepl("season", period)) {
      df_data_subset <- df_data_subset %>%
        filter(WEEK_SEASON != 53)
    }
    
    # calculate summary statistics across seasons
    # (count, mean, standard deviation, standard error of the mean, and 95% confidence interval)
    df_summary <- Rmisc::summarySE(
      data       = df_data_subset,
      measurevar = "MAIN_SMOOTH",
      groupvars  = c("STUSPS", "STATEFP", "NAME_FULL", "WEEK_SEASON", "DATA_TYPE")
    ) %>%
      rename(mean     = MAIN_SMOOTH,
             N_SEASON = N)
    
    # Reshape and organize the data
    df_list <- df_summary %>%
      select(STUSPS, WEEK_SEASON, DATA_TYPE, mean) %>%
      tidyr::pivot_wider(names_from = DATA_TYPE, values_from = mean) %>% # wide format
      arrange(STUSPS, WEEK_SEASON) %>% # ensure correct order
      split(.$STUSPS) # split into list
    
    # Convert each dataframe to a matrix
    df_list <- df_list %>%
      lapply(function(df) {
        df %>%
          select(-STUSPS) %>%
          tibble::column_to_rownames(var = "WEEK_SEASON") %>%
          as.matrix()
      })
    
    # Remove states with NA
    df_list <- df_list %>%
      purrr::keep(~ !any(is.na(.)))
    
    ## plot mean across seasons ################################################
    # add columns of summary statistics
    df_data_subset <- left_join(df_data_subset, df_summary, by = c("STUSPS", "STATEFP", "NAME_FULL", "WEEK_SEASON", "DATA_TYPE"))
    
    if (length(unique(df_data_subset$DATA_TYPE)) == 1) {
      # plot mean across seasons (only ILINet | WHO_NREVSS)
      p <- df_data_subset %>%
        ggplot(aes(x = WEEK_SEASON, linetype = DATA_TYPE)) +
        geom_line(aes(y = MAIN,        group = as.factor(YEAR_SEASON)), alpha = 0.2, col = "black") +
        geom_line(aes(y = MAIN_SMOOTH, group = as.factor(YEAR_SEASON)), alpha = 0.2, col = "red") +
        geom_line(data = df_summary, aes(y = mean, col = se), linewidth = 1)
    } else {
      # plot mean across seasons (both ILINet & WHO_NREVSS)
      p <- df_summary %>%
        ggplot() +
        geom_line(aes(x = WEEK_SEASON, y = mean, linetype = DATA_TYPE))
    }
    # plot style
    p <- p +
      scale_x_continuous(expand = c(0, 0)) +
      facet_wrap(NAME_FULL ~ ., scales = "free_y") +
      theme_bw(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    # save figure
    print( name_plot <- paste0("figure-temp/", "figure4-", datatype, "-", period, "-mean", averaging, "-line-season", ".png") )
    if(plotting) ggplot2::ggsave(file = name_plot, plot = p, width = 16, height = 9, type = "cairo")
    
  } else {
    ## full time series without averaging ######################################
    # Reshape and organize the data
    df_list <- df_data_subset %>%
      select(STUSPS, DATE, DATA_TYPE, MAIN_SMOOTH) %>%
      tidyr::pivot_wider(names_from = DATA_TYPE, values_from = MAIN_SMOOTH) %>% # wide format
      arrange(STUSPS, DATE) %>% # ensure correct order
      split(.$STUSPS) # split into list
    
    # Convert each dataframe to a matrix
    df_list <- df_list %>%
      lapply(function(df) {
        df %>%
          select(-STUSPS) %>%
          tibble::column_to_rownames(var = "DATE") %>%
          as.matrix()
      })
    
    # Remove states with NA
    df_list <- df_list %>%
      purrr::keep(~ !any(is.na(.)))
    
    # Remove states with different length
    df_list <- purrr::keep(df_list, ~ length(.x) == max(sapply(df_list, length)))
  }
  
  return(df_list)
}
