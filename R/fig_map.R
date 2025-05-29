fig_map <- function(p0, shapefile_FULL) {
  ## custom map ################################################################
  # check boundaries
  shapefile_FULL %>%
    filter(!STUSPS %in% c("AK", "HI", "PR", "VI", "MP", "GU","AS")) %>%
    # filter(STUSPS %in% c("AK")) %>%
    # filter(STUSPS %in% c("HI")) %>%
    # filter(STUSPS %in% c("PR")) %>%
    # filter(STUSPS %in% c("VI")) %>%
    # filter(STUSPS %in% c("MP")) %>%
    # filter(STUSPS %in% c("GU")) %>%
    # filter(STUSPS %in% c("AS")) %>%
    # filter(STUSPS %in% c("DC")) %>%
    # filter(STUSPS %in% c("NYC")) %>%
    pull(geometry)
  
  # continental
  p1    <- p0 + coord_sf(xlim = c(-2031905,  2516374), ylim = c( -2116976,  732352.2), expand = F)
  # others
  p_AK  <- p0 + coord_sf(xlim = c(-4358710, -1512170), ylim = c(  1466024,   3909687), expand = F) + theme(legend.position = "none")
  p_HI  <- p0 + coord_sf(xlim = c(-5761986, -5451969), ylim = c( -1051923,   -441867), expand = F) + theme(legend.position = "none")
  p_PR  <- p0 + coord_sf(xlim = c( 3398354,  3664446), ylim = c( -2360368,  -2225484), expand = F) + theme(legend.position = "none")
  p_VI  <- p0 + coord_sf(xlim = c( 3676372,  3745421), ylim = c( -2281796,  -2200183), expand = F) + theme(legend.position = "none")
  # p_MP  <- p0 + coord_sf(xlim = c(-8370000, -8320000), ylim = c(  4400000,   4420000), expand = F) + theme(legend.position = "none")
  # p_GU  <- p0 + coord_sf(xlim = c(-8511607, -8473122), ylim = c(  4401359,   4436953), expand = F) + theme(legend.position = "none")
  # p_AS  <- p0 + coord_sf(xlim = c(-8048451, -8020000), ylim = c( -3535000,  -3510000), expand = F) + theme(legend.position = "none")
  p_DC  <- p0 + coord_sf(xlim = c( 1950825,  1969386), ylim = c(-415971.5, -393994.3), expand = F) + theme(legend.position = "none")
  p_NYC <- p0 + coord_sf(xlim = c( 2133783,  2172514), ylim = c(-162348.7, -107456.4), expand = F) + theme(legend.position = "none")
  # all states and territories
  p <- p1 +
    # annotate("rect", xmin = -2030000, xmax = -1000000, ymin = -2110000, ymax = -1500000, col = "black", fill = "transparent", linewidth = 0.1) + # AK
    # annotate("rect", xmin = -1000000, xmax =  -700000, ymin = -2110000, ymax = -1500000, col = "black", fill = "transparent", linewidth = 0.1) + # HI
    # annotate("rect", xmin =   900000, xmax =  1300000, ymin = -1930000, ymax = -1750000, col = "black", fill = "transparent", linewidth = 0.1) + # PR
    # annotate("rect", xmin =  1300000, xmax =  1700000, ymin = -2110000, ymax = -1750000, col = "black", fill = "transparent", linewidth = 0.1) + # VI
    # annotate("rect", xmin =   500000, xmax =   900000, ymin = -1930000, ymax = -1750000, col = "black", fill = "transparent", linewidth = 0.1) + # MP
    # annotate("rect", xmin =   500000, xmax =   900000, ymin = -2110000, ymax = -1930000, col = "black", fill = "transparent", linewidth = 0.1) + # GU
    # annotate("rect", xmin =   900000, xmax =  1300000, ymin = -2110000, ymax = -1930000, col = "black", fill = "transparent", linewidth = 0.1) + # AS
    # annotate("rect", xmin =  2300000, xmax =  2510000, ymin =  -750000, ymax =  -500000, col = "black", fill = "transparent", linewidth = 0.1) + # DC
    # annotate("rect", xmin =  2300000, xmax =  2510000, ymin =  -500000, ymax =  -250000, col = "black", fill = "transparent", linewidth = 0.1) + # NYC
    # geom_rect(aes(xmin = -2030000, xmax = -1000000, ymin = -2110000, ymax = -1500000), col = "black", fill = "transparent", linewidth = 0.1) + # AK
    # geom_rect(aes(xmin = -1000000, xmax =  -700000, ymin = -2110000, ymax = -1500000), col = "black", fill = "transparent", linewidth = 0.1) + # HI
    # geom_rect(aes(xmin =   900000, xmax =  1300000, ymin = -1930000, ymax = -1750000), col = "black", fill = "transparent", linewidth = 0.1) + # PR
    # geom_rect(aes(xmin =  1300000, xmax =  1700000, ymin = -2110000, ymax = -1750000), col = "black", fill = "transparent", linewidth = 0.1) + # VI
    # geom_rect(aes(xmin =   500000, xmax =   900000, ymin = -1930000, ymax = -1750000), col = "black", fill = "transparent", linewidth = 0.1) + # MP
    # geom_rect(aes(xmin =   500000, xmax =   900000, ymin = -2110000, ymax = -1930000), col = "black", fill = "transparent", linewidth = 0.1) + # GU
    # geom_rect(aes(xmin =   900000, xmax =  1300000, ymin = -2110000, ymax = -1930000), col = "black", fill = "transparent", linewidth = 0.1) + # AS
    # geom_rect(aes(xmin =  2300000, xmax =  2510000, ymin =  -750000, ymax =  -500000), col = "black", fill = "transparent", linewidth = 0.1) + # DC
    # geom_rect(aes(xmin =  2300000, xmax =  2510000, ymin =  -500000, ymax =  -250000), col = "black", fill = "transparent", linewidth = 0.1) + # NYC
    annotation_custom(ggplotGrob(p_AK), xmin = -2110000, xmax = -1000000, ymin = -2110000, ymax = -1500000) +
    annotation_custom(ggplotGrob(p_HI), xmin = -1000000, xmax =  -700000, ymin = -2110000, ymax = -1500000) +
    annotation_custom(ggplotGrob(p_PR), xmin =   900000, xmax =  1300000, ymin = -1930000, ymax = -1750000) +
    annotation_custom(ggplotGrob(p_VI), xmin =  1300000, xmax =  1700000, ymin = -2110000, ymax = -1750000) +
    # annotation_custom(ggplotGrob(p_MP), xmin =   500000, xmax =   900000, ymin = -1930000, ymax = -1750000) +
    # annotation_custom(ggplotGrob(p_GU), xmin =   500000, xmax =   900000, ymin = -2110000, ymax = -1930000) +
    # annotation_custom(ggplotGrob(p_AS), xmin =   900000, xmax =  1300000, ymin = -2110000, ymax = -1930000) +
    annotation_custom(ggplotGrob(p_DC), xmin =  2300000, xmax =  2510000, ymin =  -750000, ymax =  -500000) +
    annotation_custom(ggplotGrob(p_NYC), xmin = 2300000, xmax =  2510000, ymin =  -500000, ymax =  -250000)
  
  return(p)
}
