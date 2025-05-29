name_variable <- function(datatype, period, averaging) {
  ## rename variables for plotting #############################################
  # name for plotting
  name_datatype <- case_when(
    datatype == "ILINet" ~ "The unweighted weekly proportions of outpatient visits for ILI",
    datatype == "WHO_NREVSS" ~ "The weekly percentages of specimens testing positive for influenza",
    datatype == "ILINet_WHO_NREVSS" ~ "Both datasets",
    TRUE ~ datatype  # Default case if no match
  ) %>%
    factor(levels = c("Both datasets",
                      "The unweighted weekly proportions of outpatient visits for ILI",
                      "The weekly percentages of specimens testing positive for influenza"))
  
  # name for plotting
  name_period <- case_when(
    # single season (e.g., season_2010_2011 -> "2010/2011")
    grepl("^season_", period) ~ gsub("season_(\\d{4})_(\\d{4})", "\\1/\\2", period),
    # multiple seasons (e.g., preCOVID19_2010_2020 -> "2010/2011-2019/2020 (preCOVID19)")
    grepl("^[a-zA-Z0-9]+_\\d{4}_\\d{4}", period) ~ {
      # extract years
      year0 <- as.integer(sub(".*_(\\d{4})_.*", "\\1", period))
      year1 <- as.integer(sub(".*_(\\d{4})$", "\\1", period))
      # output name
      paste0(year0, "/", year0 + 1, "-", year1 - 1, "/", year1, " (", sub("_\\d{4}_\\d{4}", "", period), ")")
      
    },
    TRUE ~ period # Default case if no match
  )
  
  # name for plotting
  name_averaging <- case_when(
    averaging == TRUE ~ "Averaged",
    averaging == FALSE ~ "Unaveraged"
  ) %>%
    factor(levels = c("Averaged", "Unaveraged"))
  
  # name for plotting
  name_map_title <- paste0(name_datatype, "\n", name_period)
  # name_map_title <- paste0(name_datatype, ": ", name_period, " (", name_averaging, ")")
  
  return(data.frame(name_datatype  = name_datatype,
                    name_period    = name_period,
                    name_averaging = name_averaging,
                    name_map_title = name_map_title))
}
