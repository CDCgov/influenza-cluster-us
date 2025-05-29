fig_shapefiles <- function(fig_map, plotting = T) {
  ## shapefiles for map ########################################################
  # read shapefiles
  # shapefile_USA <- sf::st_read("shapefile/usstateandterr/usstateandterr.shp") # large file: 34 MB
  shapefile_USA <- sf::st_read("shapefile/cb_2018_us_state_500k/cb_2018_us_state_500k.shp") # small file: 5 MB
  shapefile_USA <- shapefile_USA %>%
    filter(!STUSPS %in% c("MP", "GU", "AS"))
  # only the columns needed
  shapefile_USA <- shapefile_USA %>%
    select(STATEFP, STUSPS, NAME, geometry)
  
  # read shapefiles
  shapefile_NYC <- sf::st_read("shapefile/nybb_23d/nybb.shp")
  # unioned geometry for NYC
  shapefile_NYC <- sf::st_sf(STATEFP = "99",
                             STUSPS = "NYC",
                             NAME = "New York City",
                             geometry = sf::st_union(shapefile_NYC$geometry))
  # transform and make valid
  shapefile_NYC <- sf::st_transform(shapefile_NYC, sf::st_crs(shapefile_USA))
  shapefile_NYC <- sf::st_make_valid(shapefile_NYC)
  
  # combine the two shapefiles
  shapefile_FULL <- sf::st_sf(bind_rows(shapefile_USA, shapefile_NYC))
  # transform to non-flat map
  shapefile_FULL <- sf::st_transform(shapefile_FULL,
                                     crs = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")
  
  ## plot custom map ###########################################################
  # plot custom map
  p0 <- shapefile_FULL %>%
    ggplot() +
    geom_sf(aes(fill = STATEFP)) +
    geom_sf_text(aes(label = STUSPS)) +
    theme_void()
  # custom map
  p <- fig_map(p0, shapefile_FULL)
  # save figure
  print( name_plot <- paste0("figure-temp/", "figure0-map", ".png") )
  if(plotting) ggplot2::ggsave(file = name_plot, plot = p, width = 16, height = 9, type = "cairo")
  
  return(shapefile_FULL)
}
