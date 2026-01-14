data_process <- function(df_main, threshold = 0.5, plotting = T) {
  ## preprocessing: imputation and smoothing ###################################
  # check states with entirely NA or entirely zero
  df_main %>%
    group_by(STUSPS) %>%
    summarize(MAIN_percent_NA   = mean(is.na(MAIN)),
              MAIN_percent_zero = mean(MAIN == 0, na.rm = T)) %>%
    filter(MAIN_percent_NA   == 1 |
             MAIN_percent_zero == 1) %>%
    ungroup()
  
  # remove states with entirely NA or entirely zero
  df_main <- df_main %>%
    group_by(STUSPS) %>%
    filter(mean(is.na(MAIN)) != 1 &
             mean(MAIN == 0, na.rm = T) != 1) %>%
    ungroup()
  
  # # remove FL and MP
  # df_main <- df_main %>%
  #   filter(
  #     !(STATE_ABBR %in% c("FL") & is.na(MAIN)) &
  #       !STATE_ABBR %in% c("MP")
  #   )
  
  # check NA
  df_main %>%
    filter(is.na(MAIN))
  
  # imputation, for those NA values
  df_main <- df_main %>%
    arrange(STUSPS, DATE) %>%
    group_by(STUSPS) %>%
    mutate(MAIN_IMPUTE = imputeTS::na_interpolation(MAIN, option = "stine")) %>%
    ungroup()
  
  # replace NA by the imputed values and keep other values
  df_main <- df_main %>%
    mutate(
      MAIN_IMPUTE = case_when(
        is.na(MAIN) ~ MAIN_IMPUTE,
        TRUE ~ MAIN # Keep other values unchanged
      )
    )
  
  # smoothing
  df_main <- df_main %>%
    group_by(STUSPS) %>%
    mutate(MAIN_SMOOTH = ksmooth(x = time(DATE),
                                 y = MAIN_IMPUTE,
                                 kernel = "normal",
                                 bandwidth = 4,
                                 n.points = length(DATE))$y
    ) %>%
    ungroup()
  
  # replace negative smoothed values by zeros
  df_main <- df_main %>%
    mutate(
      MAIN_SMOOTH = case_when(
        (MAIN_SMOOTH < 0) ~ 0,
        TRUE ~ MAIN_SMOOTH # Keep other values unchanged
      )
    )
  
  ## preprocessing: discard state-season pairs #################################
  # check states with not enough data
  df_main %>%
    group_by(STUSPS, YEAR_SEASON) %>%
    summarize(MAIN_percent_NA = mean(is.na(MAIN))) %>%
    filter(MAIN_percent_NA > threshold) %>%
    ungroup()
  
  # remove states with not enough data
  df_main <- df_main %>%
    group_by(STUSPS, YEAR_SEASON) %>%
    filter(mean(is.na(MAIN)) < threshold) %>%
    ungroup()
  
  ## plot preprocessing data ###################################################
  # plot time series
  p <- df_main %>%
    ggplot() +
    geom_line(aes(x = DATE, y = MAIN_SMOOTH), linewidth = 1.0, col = "red") +
    geom_point(aes(x = DATE, y = MAIN),       size = 0.1,      col = "black") +
    scale_x_date(expand = c(0, 0), date_breaks = "1 year", date_labels = "%b %Y") +
    facet_wrap(NAME_FULL ~ ., ncol = 6, scales = "free_y") +
    labs(x = "Date",
         y = "Weekly value") +
    theme_bw(base_size = 12) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  # save figure
  print( name_plot <- paste0("figure-temp/", "figure2-", datatype, "-line-smooth", ".png") )
  if(plotting) ggplot2::ggsave(file = name_plot, plot = p, width = 16, height = 9, type = "cairo")
  
  # plot tile by season
  p <- df_main %>%
    ggplot(aes(x = DATE, y = NAME_FULL, fill = MAIN_SMOOTH)) +
    geom_tile() +
    scale_x_date(expand = c(0, 0), date_labels = "%b %Y") +
    facet_grid(. ~ YEAR_SEASON, scales = "free", space = "free") +
    labs(x = "Date",
         y = "Jurisdiction",
         fill = "Weekly value") +
    theme_bw(base_size = 12) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  # save figure
  print( name_plot <- paste0("figure-temp/", "figure2-", datatype, "-tile-smooth", ".png") )
  if(plotting) ggplot2::ggsave(file = name_plot, plot = p, width = 16, height = 9, type = "cairo")
  
  return(df_main)
}
