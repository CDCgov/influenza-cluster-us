significant_diff <- function(df_result, plotting = T) {
  ## proportion of influenza A and B / H1 and H3 #################################
  df_aggregated <- list()
  for (datatype in c("WHO_NREVSS_Combined_prior_to_2015_16", # test positivity data
                     "WHO_NREVSS_Public_Health_Labs")        # test positivity data
  ) {
    
    # load data downloaded from https://gis.cdc.gov/grasp/fluview/fluportaldashboard.html
    df_csv <- read.csv(paste0("FluViewPhase2Data/", datatype, ".csv"),
                       skip = 1)
    
    # check columns
    colnames(df_csv)
    
    # replace by NA
    df_main <- df_csv %>%
      replace(. == "X", NA)
    
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
    
    if (datatype == "WHO_NREVSS_Combined_prior_to_2015_16") {
      # add date variable
      # Note: MMWRweek uses epi week, (https://www.cmmcp.org/sites/g/files/vyhlif2966/f/uploads/epiweekcalendar2015.pdf)
      df_main <- df_main %>%
        mutate(DATE = MMWRweek::MMWRweek2Date(MMWRyear = YEAR, MMWRweek = WEEK, MMWRday = 1))
      # mutate(DATE = ISOweek::ISOweek2date(paste0(YEAR, "-W", sprintf("%02d", WEEK), "-1")))
      # error in duplicated dates in 2015 using ISOweek::ISOweek2date, because of Week 53
      # unique(df_main$YEAR[df_main$WEEK==53]) # [1] 2014 2020
      
      # shifting weeks to match the flu season starting from week 40
      df_main <- df_main %>%
        mutate(YEAR_SEASON = if_else(WEEK < 40, paste0(YEAR - 1, "-", YEAR), paste0(YEAR, "/", YEAR + 1))) %>%
        group_by(STUSPS, YEAR_SEASON) %>%
        mutate(WEEK_SEASON = seq_along(YEAR_SEASON)) %>%
        # filter(WEEK_SEASON != 53) %>%
        ungroup() %>%
        arrange(STUSPS, YEAR_SEASON, WEEK_SEASON)
      
    } else {
      
      # rename YEAR_SEASON
      df_main$YEAR_SEASON <- gsub("Season (\\d{4})-(\\d{2})", "\\1/20\\2", df_main$SEASON_DESCRIPTION)
      df_main$YEAR_SEASON <- trimws(df_main$YEAR_SEASON) # remove whitespace
    }
    
    # Convert to numeric
    df_main$A..2009.H1N1.               <- as.numeric(df_main$A..2009.H1N1.)
    df_main$A..H3.                      <- as.numeric(df_main$A..H3.)
    df_main$A..Subtyping.not.Performed. <- as.numeric(df_main$A..Subtyping.not.Performed.)
    
    # Calculate TOTAL.A and TOTAL.B
    df_main$TOTAL.A <- rowSums(cbind(df_main$A..2009.H1N1.,
                                     df_main$A..H3.,
                                     df_main$A..Subtyping.not.Performed.), na.rm = TRUE)
    df_main$TOTAL.B <- as.numeric(df_main$B)
    
    # Aggregate data
    df_aggregated[[datatype]] <- df_main %>%
      group_by(YEAR_SEASON, STUSPS) %>%
      summarise(
        TOTAL_H1 = sum(A..2009.H1N1., na.rm = TRUE),
        TOTAL_H3 = sum(A..H3.,        na.rm = TRUE),
        TOTAL_A  = sum(TOTAL.A,       na.rm = TRUE),
        TOTAL_B  = sum(TOTAL.B,       na.rm = TRUE),
        .groups  = "drop"
      ) %>%
      mutate(
        PERCENT_H1 = TOTAL_H1 / (TOTAL_H1 + TOTAL_H3),
        PERCENT_H3 = TOTAL_H3 / (TOTAL_H1 + TOTAL_H3),
        PERCENT_A = TOTAL_A / (TOTAL_A + TOTAL_B),
        PERCENT_B = TOTAL_B / (TOTAL_A + TOTAL_B)
      )
  }
  
  # Combine the data frames
  df_aggregated <- data.table::rbindlist(df_aggregated)
  
  ## merge with clustering results ##########################################################
  # reload results
  if (!exists("df_result")) load(file = paste0("figure-temp/df_result.RData"))
  
  # filter and merge
  df_merge <- df_result %>%
    filter(DATA_PERIOD_COVID == "season") %>%
    filter(DATA_YEAR1 <= 2020) %>%
    mutate(YEAR_SEASON = paste0(DATA_YEAR0, "/", DATA_YEAR1)) %>%
    merge(df_aggregated, by = c("STUSPS", "YEAR_SEASON"), all.x = TRUE)
  
  # rename
  source("name_variable.R")
  name_variables <- name_variable(df_merge$DATA_TYPE, df_merge$DATA_PERIOD, df_merge$AVG)
  df_merge$DATA_TYPE   <- name_variables$name_datatype
  df_merge$DATA_PERIOD <- name_variables$name_period
  df_merge$AVG         <- name_variables$name_averaging
  
  ## analysis for each variable #####################################################
  aov_results <- list() # perform aov for each variable
  aov_comparison <- list() # perform multiple comparison analysis testing for each variable
  variables <- c("WEEK_SEASON_PEAK" = "Peak week",
                 "MORAN_I_LOCAL"    = "Local Moran's I",
                 "PERCENT_A"        = "Proportion of all influenza A and B detections that were influenza A viruses",
                 "PERCENT_H1"       = "Proportion of all influenza A detections that were influenza A/H1 viruses")
  for (var in names(variables)){
    ## plot clustering results ###########################################
    p <- df_merge %>%
      filter(!is.na(CLUSTER)) %>%
      filter(!is.na(!!sym(var))) %>%
      ggplot(aes(y = !!sym(var), x = CLUSTER, col = CLUSTER)) +
      geom_boxplot() +
      geom_jitter(alpha = 0.5) +
      facet_grid(DATA_TYPE ~ YEAR_SEASON, scales = "free",
                 labeller = labeller(DATA_TYPE = function(x) stringr::str_wrap(x, width = 40))) +
      # scale_color_brewer(palette = "Dark2") +
      scale_color_manual(values = setNames(colorRampPalette(c("#0057b7", RColorBrewer::brewer.pal(8, "Dark2")))(k_max + 1), 1:(k_max + 1))) +
      labs(y = variables[[var]],
           x = "Cluster",
           col = "Cluster") +
      theme_bw(base_size = 15) +
      theme(strip.text.y = element_text(size = rel(0.8)))
    # save figure
    print( name_plot <- paste0("figure-temp/", "figure9-cluster-", var, ".png") )
    if(plotting) ggplot2::ggsave(file = name_plot, plot = p, width = 16, height = 9, type = "cairo")
    
    ## perform aov ########################################################
    aov_model  <- aov(get(var) ~ CLUSTER * YEAR_SEASON * DATA_TYPE, data = df_merge)
    qqnorm(residuals(aov_model)) # QQ plots for checking normality
    aov_results[[var]] <- summary(aov_model)
    aov_comparison[[var]] <- TukeyHSD(aov_model)
  }
  
  # print aov results
  aov_results
  # summary(aov_comparison)
  aov_comparison$MORAN_I_LOCAL$CLUSTER
  aov_comparison$MORAN_I_LOCAL$DATA_TYPE
  aov_comparison$PERCENT_H1$CLUSTER
  aov_comparison$PERCENT_H1$DATA_TYPE
}
