cluster_maps <- function(df_subset, plotting = T) {
  ## combine png ###############################################################
  combine_pngs <- function(df_subset_select, target = "cluster", ncol = 1) {
    # subset of data
    datatypes    <- as.character(df_subset_select$DATA_TYPE)
    periods      <- as.character(df_subset_select$DATA_PERIOD)
    averagings   <- as.logical(df_subset_select$AVG)
    # load files
    if(target == "peak")
      print( file_names <- c(paste0("figure-temp/", "figure4a-", datatypes, "-", periods, "-mean", averagings, "-", target, "-map", ".png")) )
    if(target == "moran_local")
      print( file_names <- c(paste0("figure-temp/", "figure4b-", datatypes, "-", periods, "-mean", averagings, "-", target, "-map", ".png")) )
    if(target == "cluster")
      print( file_names <- c(paste0("figure-temp/", "figure6-", datatypes, "-", periods, "-mean", averagings, "-", target, "-map", ".png")) )
    
    # check file existence
    file_names <- file_names[file.exists(file_names)]
    # all figures
    rl = lapply(file_names, png::readPNG)
    gl = lapply(rl, grid::rasterGrob)
    # reorder for multiple seasons
    if(!grepl("season", periods[1]))
      gl = gl[as.vector(t(matrix(seq_len(length(gl)), ncol = ncol)))[1:length(gl)]]
    # plot
    p = cowplot::plot_grid(plotlist = gl,
                           ncol = ncol,
                           # labels = "AUTO",
                           hjust = 0,
                           label_size = 28, label_fontface = "plain",
                           align = "hv", axis = "b")
    
    return(p)
  }
  
  ## cluster and peak week #####################################################
  for(target in c("peak", "moran_local", "cluster")) { # plotting maps of cluster and peak week
    
    ## single season #############################################################
    if(target %in% c("peak", "moran_local"))
      datatypes <- c("ILINet", "WHO_NREVSS")
    if(target == "cluster")
      datatypes <- c("ILINet", "WHO_NREVSS", "ILINet_WHO_NREVSS")
    for(datatype in datatypes) { # univariate & multivariate analysis
      ## full ##################################################################
      # subset of data
      df_subset_select <- df_subset %>%
        filter(DATA_TYPE == datatype) %>%
        filter(grepl("season", DATA_PERIOD)) %>%
        filter(AVG == TRUE)
      
      # combine png
      p <- combine_pngs(df_subset_select, target, ncol = 3)
      # rename
      source("name_variable.R")
      name_datatype <- name_variable(datatype, NA, NA)$name_datatype
      # add title
      p <- cowplot::ggdraw() +
        cowplot::draw_label(name_datatype, x = 0.01, y = 0.98, hjust = 0, size = 36) +
        cowplot::draw_plot(p, y = 0, height = 0.97)
      # save figure
      print( name_plot <- paste0("figure-temp/", "figure8-", datatype, "-", "full_period", "-mean", "TRUE", "-", target, "-map", ".png") )
      if(plotting) ggplot2::ggsave(file = name_plot, plot = p, width = 14/2*3, height = 9/2*5, type = "cairo")
      
      # ## preCOVID19 ##################################################################
      # # subset of data
      # df_subset_select <- df_subset %>%
      #   filter(DATA_TYPE == datatype) %>%
      #   filter(grepl("season", DATA_PERIOD)) %>%
      #   filter(AVG == TRUE) %>%
      #   filter(DATA_YEAR0 < 2020)
      #
      # # combine png
      # p <- combine_pngs(df_subset_select, target, ncol = 2)
      # # save figure
      # print( name_plot <- paste0("figure-temp/", "figure8-", datatype, "-", "preCOVID19", "-mean", "TRUE", "-", target, "-map", ".png") )
      # if(plotting) ggplot2::ggsave(file = name_plot, plot = p, width = 14/2*5, height = 9/2*2, type = "cairo")
      #
      # ## COVID19era #################################################################
      # # subset of data
      # df_subset_select <- df_subset %>%
      #   filter(DATA_TYPE == datatype) %>%
      #   filter(grepl("season", DATA_PERIOD)) %>%
      #   filter(AVG == TRUE) %>%
      #   filter(DATA_YEAR0 >= 2020)
      #
      # # combine png
      # p <- combine_pngs(df_subset_select, target, ncol = 1)
      # # save figure
      # print( name_plot <- paste0("figure-temp/", "figure8-", datatype, "-", "COVID19era", "-mean", "TRUE", "-", target, "-map", ".png") )
      # if(plotting) ggplot2::ggsave(file = name_plot, plot = p, width = 14/2*4, height = 9/2*1, type = "cairo")
    }
    
    ## multiple seasons ##########################################################
    if(target %in% c("peak", "moran_local"))
      averagings <- c(TRUE)
    if(target == "cluster")
      averagings <- unique(as.logical(df_subset$AVG)) # c(TRUE, FALSE)
    for(averaging in averagings) { # with / without averaging
      
      ## univariate & multivariate analysis ########################################
      # subset of data
      df_subset_select <- df_subset %>%
        filter(!grepl("season", DATA_PERIOD)) %>%
        filter(AVG == averaging)
      
      # combine png
      p <- combine_pngs(df_subset_select, target, ncol = 2)
      # save figure
      print( name_plot <- paste0("figure-temp/", "figure8-", "all_datatypes", "-", "full_period", "-mean", averaging, "-", target, "-map", ".png") )
      if(target %in% c("peak", "moran_local"))
        if(plotting) ggplot2::ggsave(file = name_plot, plot = p, width = 14*2, height = 9*2, type = "cairo")
      if(target == "cluster")
        if(plotting) ggplot2::ggsave(file = name_plot, plot = p, width = 14*2, height = 9*3, type = "cairo")
    }
  }
}
