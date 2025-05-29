data_subset <- function() {
  ## Define subset of data #####################################################
  # Define study periods
  years   <- 2010:2023
  seasons <- paste0(years, "_", years + 1)
  periods <- c(#"full_2010_2024",
               "preCOVID19_2010_2020",
               # "preCOVID19_2010_2015",  # ILINet, WHO_NREVSS_Combined_prior_to_2015_16
               # "preCOVID19_2015_2020",  # ILINet, WHO_NREVSS_Clinical_Labs
               # "COVID19era_2020_2024",
               "postCOVID19_2022_2024",
               paste0("season_", seasons) # one season each
  )
  
  # Create the cross product of surveillance system and periods
  df_subset <- bind_rows(
    expand.grid(
      DATA_TYPE = c("ILINet", "WHO_NREVSS", "ILINet_WHO_NREVSS"),
      DATA_PERIOD = periods,
      AVG = TRUE
    )
  )
  
  # # Add periods more than one season without averaging
  # df_subset <- df_subset %>%
  #   bind_rows(
  #     df_subset %>%
  #       filter(!grepl("season", DATA_PERIOD)) %>%
  #       mutate(AVG = FALSE)
  #   )
  
  # Add columns
  df_subset <- df_subset %>%
    mutate(DATA_PERIOD.3 = as.character(DATA_PERIOD)) %>%
    tidyr::separate(DATA_PERIOD.3, into = c("DATA_PERIOD_COVID", "DATA_YEAR0", "DATA_YEAR1"), sep = "_", fill = "right") %>%
    mutate(DATA_SEASON = as.numeric(DATA_YEAR1) - as.numeric(DATA_YEAR0))
  
  # Add column
  df_subset <- df_subset %>%
    mutate(DATA_SUBSET = paste(DATA_TYPE, DATA_PERIOD, AVG, sep = "-"))
  
  return(df_subset)
}
