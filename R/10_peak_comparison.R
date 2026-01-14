peak_comparison <- function(df_result, plotting = T) {
  
  ## data preparation ###########################################################
  
  # peak time of averaged time series
  df_averged <-
    df_result %>%
    filter(!is.na(WEEK_SEASON_PEAK)) %>%
    filter(DATA_TYPE != "ILINet_WHO_NREVSS") %>%
    filter(!grepl("season", DATA_PERIOD)) %>%
    mutate(WEEK_SEASON_PEAK_text = as.character(WEEK_SEASON_PEAK)) %>%
    mutate(TIME_SERIES = "AVERAGED")
  
  # peak time of SINGLE season time series
  df_single <-
    df_result %>%
    filter(!is.na(WEEK_SEASON_PEAK)) %>%
    filter(DATA_TYPE != "ILINet_WHO_NREVSS") %>%
    filter(DATA_PERIOD_COVID == "season") %>%
    mutate(
      DATA_PERIOD_COVID = case_when(
        as.integer(DATA_YEAR1) <= 2020 ~ paste0("preCOVID19"),
        as.integer(DATA_YEAR0) >= 2022 ~ paste0("postCOVID19"),
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(DATA_PERIOD_COVID)) %>%
    group_by(STUSPS, DATA_TYPE, DATA_PERIOD_COVID) %>%
    summarise(
      count                 = n(),
      WEEK_SEASON_PEAK_q25  = quantile(WEEK_SEASON_PEAK, 0.25),
      WEEK_SEASON_PEAK_q75  = quantile(WEEK_SEASON_PEAK, 0.75),
      WEEK_SEASON_PEAK      = median(WEEK_SEASON_PEAK),
      WEEK_SEASON_PEAK_text = paste0(round(WEEK_SEASON_PEAK, 1), " (",
                                     round(WEEK_SEASON_PEAK_q25, 1), "–",
                                     round(WEEK_SEASON_PEAK_q75, 1), ")"),
      .groups = "drop"
    ) %>%
    mutate(TIME_SERIES = "SINGLE")
  
  # combine two data frames
  df_both <-
    bind_rows(df_averged, df_single) %>%
    left_join(
      df_data %>% distinct(STUSPS, NAME_FULL),
      by = "STUSPS"
    ) %>%
    mutate(DATA_PERIOD_COVID = factor(DATA_PERIOD_COVID, levels = c("preCOVID19", "postCOVID19")))
  
  ## figures #####################################################################
  
  # rename
  source("name_variable.R")
  name_variables <- name_variable(df_both$DATA_TYPE, df_both$DATA_PERIOD, df_both$AVG)
  df_both$DATA_TYPE   <- name_variables$name_datatype
  df_both$DATA_PERIOD <- name_variables$name_period
  df_both$AVG         <- name_variables$name_averaging
  
  # fix color scale from 1 to 53
  map_color_scale <- scales::gradient_n_pal(c("#9a3b24", "#f0e442", "#0057b7"))(seq(0, 1, length.out = 53))
  map_color_scale <- map_color_scale[min(df_both$WEEK_SEASON_PEAK, na.rm = TRUE):max(df_both$WEEK_SEASON_PEAK, na.rm = TRUE)]
  # integer breaks at color scale
  peak_range <- range(df_both$WEEK_SEASON_PEAK, na.rm = TRUE)
  peak_break <- seq(peak_range[1], peak_range[2], by = ifelse(diff(peak_range) + 1 > 35, 2, 1))
  
  # plot heatmap
  p <- df_both %>%
    ggplot(aes(x = TIME_SERIES, y = NAME_FULL)) +
    geom_tile(aes(fill = WEEK_SEASON_PEAK)) +
    geom_text(aes(label = WEEK_SEASON_PEAK_text), size = 4, color = "black") +
    scale_fill_gradientn(colors = map_color_scale,
                         breaks = peak_break) +
    scale_x_discrete(labels = function(x) stringr::str_wrap(c("AVERAGED" = "Averaged time series",
                                                              "SINGLE"   = "Seasonal median (IQR)"
    )[x], width = 15)) +
    facet_grid(~ DATA_TYPE + DATA_PERIOD_COVID,
               labeller = labeller(DATA_TYPE = function(x) stringr::str_wrap(x, width = 40))) +
    guides(fill = guide_colorbar(barwidth = 1, barheight = 35)) +
    theme_bw(base_size = 15) +
    labs(x = NULL,
         y = "Jurisdiction",
         fill = "Peak week")
  # save figure
  print( name_plot <- paste0("figure-temp/", "figure10-averaged_peak-tile", ".png") )
  if(plotting) ggplot2::ggsave(file = name_plot, plot = p, width = 16, height = 9, type = "cairo")
  
  # plot scatter
  p <- df_both %>%
    select(STUSPS, DATA_TYPE, DATA_PERIOD_COVID, WEEK_SEASON_PEAK, TIME_SERIES) %>%
    tidyr::pivot_wider(
      names_from  = TIME_SERIES,
      values_from = WEEK_SEASON_PEAK
    ) %>%
    ggplot(aes(x = AVERAGED, y = SINGLE)) +
    geom_point(size = 2) +
    geom_abline(slope = 1, intercept = 0, linewidth = 1, linetype = "dashed") +
    coord_equal() +
    facet_grid(DATA_TYPE ~ DATA_PERIOD_COVID,
               labeller = labeller(DATA_TYPE = function(x) stringr::str_wrap(x, width = 40))) +
    theme_bw(base_size = 15) +
    labs(
      x = "Averaged time series",
      y = "Seasonal median",
      title = "Comparison of peak timing estimated from averaged and seasonal time series")
  # save figure
  print( name_plot <- paste0("figure-temp/", "figure10-averaged_peak-scatter", ".png") )
  if(plotting) ggplot2::ggsave(file = name_plot, plot = p, width = 16, height = 9, type = "cairo")
  
  # plot distribution
  p <- df_both %>%
    select(STUSPS, DATA_TYPE, DATA_PERIOD_COVID, WEEK_SEASON_PEAK, TIME_SERIES) %>%
    tidyr::pivot_wider(
      names_from  = TIME_SERIES,
      values_from = WEEK_SEASON_PEAK
    ) %>%
    mutate(DIFF = AVERAGED - SINGLE) %>%
    ggplot(aes(x = DIFF)) +
    geom_density(linewidth = 1) +
    geom_point(aes(y = 0), position = position_jitter(height = 0.002), alpha = 0.6, size = 1.5) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1) +
    facet_grid(DATA_TYPE ~ DATA_PERIOD_COVID,
               labeller = labeller(DATA_TYPE = function(x) stringr::str_wrap(x, width = 40))) +
    theme_bw(base_size = 15) +
    labs(
      x = "Difference in peak week (Averaged time series − Seasonal median)",
      y = "Density",
      title = "Comparison of peak timing estimated from averaged and seasonal time series")
  # save figure
  print( name_plot <- paste0("figure-temp/", "figure10-averaged_peak-density", ".png") )
  if(plotting) ggplot2::ggsave(file = name_plot, plot = p, width = 16, height = 9, type = "cairo")
}
