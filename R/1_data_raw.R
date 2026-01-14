data_raw <- function(datatype = "ILINet", plotting = T) {
  ## load raw data #############################################################
  # load data downloaded from https://gis.cdc.gov/grasp/fluview/fluportaldashboard.html
  df_csv <- read.csv(paste0("FluViewPhase2Data/", datatype, ".csv"),
                     skip = 1)
  
  # check columns
  colnames(df_csv)
  
  # replace by NA
  df_main <- df_csv %>%
    replace(. == "X", NA)
  
  # data preprocessing: replace by NA, take 3 columns, and remove other columns
  # df_main <- df_csv %>%
  #   replace(. == "X", NA) %>%
  #   mutate(
  #     ILI_UNWEIGHTED = as.numeric(X.UNWEIGHTED.ILI),
  #     ILITOTAL         = as.numeric(ILITOTAL),
  #     TOTAL.PATIENTS   = as.numeric(TOTAL.PATIENTS),
  #   ) %>%
  #   select(-c("AGE.0.4", "AGE.25.49", "AGE.25.64", "AGE.5.24", "AGE.50.64", "AGE.65",
  #             "REGION.TYPE", "NUM..OF.PROVIDERS",
  #             "X..WEIGHTED.ILI", "X.UNWEIGHTED.ILI"))
  
  # Combine "New York" and "New York City" into "New York"
  # df_main <- df_main %>%
  #   mutate(
  #     TOTAL.PATIENTS = case_when(
  #       REGION == "New York" ~ TOTAL.PATIENTS + TOTAL.PATIENTS[REGION == "New York City"],
  #       TRUE ~ TOTAL.PATIENTS # Keep other values unchanged
  #     ),
  #     ILITOTAL = case_when(
  #       REGION == "New York" ~ ILITOTAL + ILITOTAL[REGION == "New York City"],
  #       TRUE ~ ILITOTAL # Keep other values unchanged
  #     ),
  #     ILI_UNWEIGHTED = ILITOTAL / TOTAL.PATIENTS * 100
  #   ) %>%
  #   filter(REGION != "New York City") # Exclude rows with "New York City"
  
  # take the main column for analysis
  if (datatype == "ILINet")                               df_main <- df_main %>% mutate(MAIN = as.numeric(X.UNWEIGHTED.ILI))
  if (datatype == "WHO_NREVSS_Combined_prior_to_2015_16") df_main <- df_main %>% mutate(MAIN = as.numeric(PERCENT.POSITIVE))
  if (datatype == "WHO_NREVSS_Clinical_Labs")             df_main <- df_main %>% mutate(MAIN = as.numeric(PERCENT.POSITIVE))
  
  # take important columns only
  df_main <- df_main %>%
    select(c("REGION", "YEAR", "WEEK", "MAIN"))
  
  # rename regions https://www2.census.gov/geo/docs/reference/state.txt
  df_main <- df_main %>%
    mutate(
      NAME = as.character(as.factor(case_when(
        REGION == "Virgin Islands" ~ "U.S. Virgin Islands",
        REGION == "Commonwealth of the Northern Mariana Islands" ~ "Northern Mariana Islands",
        TRUE ~ REGION # Keep other values unchanged
      ))),
      STUSPS = as.character(factor(NAME,
                                   levels = c(tidycensus::fips_codes$state_name, "New York City"),
                                   labels = c(tidycensus::fips_codes$state,      "NYC"))),
      STATEFP  = as.character(factor(NAME,
                                     levels = c(tidycensus::fips_codes$state_name, "New York City"),
                                     labels = c(tidycensus::fips_codes$state_code, "99"))),
      NAME_FULL = paste0("(", STATEFP, ") ", STUSPS, ": ", NAME),
    ) %>%
    select(-c("REGION"))
  
  # add date variable
  # Note: MMWRweek uses epi week, (https://www.cmmcp.org/sites/g/files/vyhlif2966/f/uploads/epiweekcalendar2015.pdf)
  df_main <- df_main %>%
    mutate(DATE = MMWRweek::MMWRweek2Date(MMWRyear = YEAR, MMWRweek = WEEK, MMWRday = 1))
  # mutate(DATE = ISOweek::ISOweek2date(paste0(YEAR, "-W", sprintf("%02d", WEEK), "-1")))
  # error in duplicated dates in 2015 using ISOweek::ISOweek2date, because of Week 53
  # unique(df_main$YEAR[df_main$WEEK==53]) # [1] 2014 2020
  
  # shifting weeks to match the flu season starting from week 40
  df_main <- df_main %>%
    mutate(YEAR_SEASON = if_else(WEEK < 40, paste0(YEAR - 1, "-", YEAR), paste0(YEAR, "-", YEAR + 1))) %>%
    group_by(STUSPS, YEAR_SEASON) %>%
    mutate(WEEK_SEASON = seq_along(YEAR_SEASON)) %>%
    # filter(WEEK_SEASON != 53) %>%
    ungroup() %>%
    arrange(STUSPS, YEAR_SEASON, WEEK_SEASON)
  
  ## plot raw data #############################################################
  # plot time series
  p <- df_main %>%
    ggplot(aes(x = DATE, y = MAIN)) +
    geom_line() +
    scale_x_date(expand = c(0, 0), date_breaks = "1 year", date_labels = "%b %Y") +
    facet_wrap(NAME_FULL ~ ., ncol = 6, scales = "free_y") +
    labs(x = "Date",
         y = "Weekly value") +
    theme_bw(base_size = 12) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  # save figure
  print( name_plot <- paste0("figure-temp/", "figure1-", datatype, "-line-raw", ".png") )
  if(plotting) ggplot2::ggsave(file = name_plot, plot = p, width = 16, height = 9, type = "cairo")
  
  # plot tile by season
  p <- df_main %>%
    ggplot(aes(x = DATE, y = NAME_FULL, fill = MAIN)) +
    geom_tile() +
    scale_x_date(expand = c(0, 0), date_labels = "%b %Y") +
    facet_grid(. ~ YEAR_SEASON, scales = "free", space = "free") +
    labs(x = "Date",
         y = "Jurisdiction",
         fill = "Weekly value") +
    theme_bw(base_size = 12) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  # save figure
  print( name_plot <- paste0("figure-temp/", "figure1-", datatype, "-tile-raw", ".png") )
  if(plotting) ggplot2::ggsave(file = name_plot, plot = p, width = 16, height = 9, type = "cairo")
  
  return(df_main)
}
