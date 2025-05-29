plot_processed_data <- function(df_data, plotting = T) {
  ## plot processed data #######################################################
  # exclude the last season
  df_data <- df_data %>%
    filter(YEAR_SEASON != "2024-2025") %>%
    mutate(NAME_FULL = factor(NAME_FULL, levels = sort(unique(NAME_FULL))))
  
  for(datatype in c("ILINet",     # ILI data
                    "WHO_NREVSS") # test positivity data
  ) {
    # figure title
    source("name_variable.R")
    name_datatype <- name_variable(datatype, period = NA, averaging = NA)$name_datatype
    
    # plot time series
    p <- df_data %>%
      filter(DATA_TYPE == datatype) %>%
      ggplot() +
      geom_line(aes(x = DATE, y = MAIN),        col = "black") +
      geom_line(aes(x = DATE, y = MAIN_SMOOTH), col = "red") +
      scale_x_date(expand = c(0, 0), date_labels = "%b %Y") +
      facet_wrap(NAME_FULL ~ ., scales = "free_y") +
      labs(title = name_datatype,
           x = "Date",
           y = "Weekly value") +
      theme_bw(base_size = 12) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    # save figure
    print( name_plot <- paste0("figure-temp/", "figure2a-", datatype, "-line-smooth", ".png") )
    if(plotting) ggplot2::ggsave(file = name_plot, plot = p, width = 16, height = 9, type = "cairo")
    
    # plot tile by season
    p <- df_data %>%
      filter(DATA_TYPE == datatype) %>%
      ggplot(aes(x = DATE, y = NAME_FULL, fill = MAIN_SMOOTH)) +
      geom_tile() +
      scale_x_date(expand = c(0, 0), date_labels = "%b %Y") +
      scale_y_discrete(drop = FALSE) +  # This ensures that all factor levels are displayed
      facet_grid(. ~ YEAR_SEASON, scales = "free", space = "free") +
      labs(title = name_datatype,
           x = "Date",
           y = "Jurisdiction",
           fill = "Weekly value") +
      guides(fill = guide_colorbar(barwidth = 1, barheight = 35)) +
      theme_bw(base_size = 12) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
            plot.title = element_text(size = rel(2)),
            legend.title = element_text(size = rel(1.5)),
            legend.text = element_text(size = rel(1.5)))
    # save figure
    print( name_plot <- paste0("figure-temp/", "figure2a-", datatype, "-tile-smooth", ".png") )
    if(plotting) ggplot2::ggsave(file = name_plot, plot = p, width = 16, height = 9, type = "cairo")
  }
  
  # load files
  print( file_names <- list.files(path = "figure-temp", pattern = "figure2a-.*-tile-smooth\\.png$", full.names = TRUE) )
  # check file existence
  file_names <- file_names[file.exists(file_names)]
  # all figures
  rl = lapply(file_names, png::readPNG)
  gl = lapply(rl, grid::rasterGrob)
  # plot
  p = cowplot::plot_grid(plotlist = gl,
                         nrow = 2,
                         # labels = "AUTO",
                         hjust = 0,
                         label_size = 28, label_fontface = "plain",
                         align = "hv", axis = "b")
  # save figure
  print( name_plot <- paste0("figure-temp/", "figure2a-", "tile-smooth", ".png") )
  if(plotting) ggplot2::ggsave(file = name_plot, plot = p, width = 16, height = 9*2, type = "cairo")
}
