peak_week <- function(df_list, datatype = "ILINet", period = "preCOVID19_2010_2020", averaging = T, plotting = T) {
  ## peak time of mean across seasons ##########################################
  # convert list back into a data frame
  df_summary <- do.call(rbind, lapply(names(df_list), function(name) {
    data.frame(
      STUSPS = name,
      WEEK_SEASON = as.numeric(rownames(df_list[[name]])),
      mean = as.vector(df_list[[name]])
    )
  }))
  
  # peak weeks
  df_summary_peak <- df_summary %>%
    group_by(STUSPS) %>%
    filter(mean == max(mean)) %>%
    rename(PEAK_MEAN = mean) %>%
    rename(WEEK_SEASON_PEAK = WEEK_SEASON) %>%
    ungroup()
  
  # join with shapefile
  df_peak <- left_join(shapefile_FULL, df_summary_peak, by = c("STUSPS"))
  
  ## plot peak time in map #####################################################
  # fix color scale from 1 to 53
  map_color_scale <- scales::gradient_n_pal(c("#9a3b24", "#f0e442", "#0057b7"))(seq(0, 1, length.out = 53))
  map_color_scale <- map_color_scale[min(df_peak$WEEK_SEASON_PEAK, na.rm = TRUE):max(df_peak$WEEK_SEASON_PEAK, na.rm = TRUE)]
  # integer breaks at color scale
  peak_range <- range(df_peak$WEEK_SEASON_PEAK, na.rm = TRUE)
  peak_break <- seq(peak_range[1], peak_range[2], by = ifelse(diff(peak_range) + 1 > 35, 2, 1))
  # plot peak time in map
  p0 <- df_peak %>%
    ggplot() +
    geom_sf(aes(fill = WEEK_SEASON_PEAK)) +
    geom_sf_text(aes(label = STUSPS), color = "white") +
    scale_fill_gradientn(colors = map_color_scale,
                         breaks = peak_break) +
    theme_void()
  # map title
  source("name_variable.R")
  name_map_title <- name_variable(datatype, period, averaging)$name_map_title
  # custom map
  p <- fig_map(p0, shapefile_FULL) +
    labs(title = name_map_title,
         fill = "Peak week") +
    guides(fill = guide_colorbar(barwidth = 1, barheight = 35)) +
    theme(plot.title = element_text(size = rel(3)),
          legend.title = element_text(size = rel(2)),
          legend.text = element_text(size = rel(2)))
  # save figure
  print( name_plot <- paste0("figure-temp/", "figure4a-", datatype, "-", period, "-mean", averaging, "-peak-map", ".png") )
  if(plotting) ggplot2::ggsave(file = name_plot, plot = p, width = 16, height = 9, type = "cairo")
  
  return(df_peak)
}
