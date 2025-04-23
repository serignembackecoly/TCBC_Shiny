# Spatial distributionof consecutive dry days in Burkina Faso

# Librairies
library(ggplot2)
library(raster)
library(rgdal)
library(sf)
library(Kendall)
library(cowplot)
library(ggpubr)
library(ggarrange)

# Load shapefile
regions_burkina <- st_read("data/regions.shp")


# Load data
cwd_burkina <- stack("E:/Etude/Donnees/historical/cwd/cwd_historical_1981_2014.nc")
cwd_burkina_ssp2 <- stack("E:/Etude/Donnees/projections/ssp2_25_50/cwd/cwd_2025-2050.nc")
cwd_burkina_ssp5 <- stack("E:/Etude/Donnees/projections/ssp5_25_50/cwd/cwd_ssp5_2025-2050.nc")

cdd_burkina <- stack("E:/Etude/Donnees/historical/cdd/cdd_historical_1981_2014.nc")
cdd_burkina_ssp2 <- stack("E:/Etude/Donnees/projections/ssp2_25_50/cdd/cdd_ssp2_2025_2050.nc")
cdd_burkina_ssp5 <- stack("E:/Etude/Donnees/projections/ssp5_25_50/cdd/cdd_ssp5_2025_2050.nc")

sdii_burkina <- stack("E:/Etude/Donnees/historical/sdii/sdii_historical_1981_2014.nc")
sdii_burkina_ssp2 <- stack("E:/Etude/Donnees/projections/ssp2_25_50/sdii/sdii_ssp2_2025_2050.nc")
sdii_burkina_ssp5 <- stack("E:/Etude/Donnees/projections/ssp5_25_50/sdii/sdii_ssp5_2025_2050.nc")

r10mm_burkina <- stack("E:/Etude/Donnees/historical/r10mm/r10mm_historical_1981_2014.nc")
r10mm_burkina_ssp2 <- stack("E:/Etude/Donnees/projections/ssp2_25_50/r10mm/r10mm_ssp2_2025_2050.nc")
r10mm_burkina_ssp5 <- stack("E:/Etude/Donnees/projections/ssp5_25_50/r10mm/r10mm_ssp5_2025_2050.nc")

r20mm_burkina <- stack("E:/Etude/Donnees/historical/r20mm/r20mm_historical_1981_2014.nc")
r20mm_burkina_ssp2 <- stack("E:/Etude/Donnees/projections/ssp2_25_50/r20mm/r20mm_ssp2_2025_2050.nc")
r20mm_burkina_ssp5 <- stack("E:/Etude/Donnees/projections/ssp5_25_50/r20mm/r20mm_ssp5_2025_2050.nc")

# The main function
make_plot_indices <- function(data, scenario, periode, indice, unit) {
  # Check CRS
  crs_raster <- st_crs(data)
  crs_shapefile <- st_crs(regions_burkina)
  
  # If CRS are different, reproject shapefile to match raster
  if (crs_raster != crs_shapefile) {
    regions_burkina <- st_transform(regions_burkina, crs_raster)
  }
  cdd_burkina <- mask(data, regions_burkina)
  cdd_burkina <- crop(cdd_burkina, regions_burkina)
  
  
  # Convert to dataframe (ggplot purpose)
  cdd_df <- as.data.frame(cdd_burkina, xy = TRUE) 
  colnames(cdd_df) <- c('x','y','value')
  cdd_dff <- cdd_df[complete.cases(cdd_df), ]
  
  # Plot the raster and shapefile
  p1 <- ggplot() +
    geom_raster(data = cdd_dff, aes(x = x, y = y, fill = value)) +
    geom_sf(data = regions_burkina,
            fill = NA, color = "black",
            linewidth = 1) +
    scale_fill_viridis_c(name = paste0(indice, " ", "(", unit,")"),
                         option = "turbo" ) + # "plasma"
    theme_light() +
    labs(x = NULL, y = NULL, 
         title = paste0(scenario," ","(", periode, ")")) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      panel.border = element_rect(color = "black"),
      panel.grid.major = element_blank(),
      axis.text  = element_text(colour = "navy",
                                face = "bold",
                                size = 14),
      legend.position = c(0.7,0.1),
      legend.direction = "horizontal",
      legend.frame = element_rect(colour = "black"),
      legend.title = element_text(size = 12),    # Customize title size
      legend.text = element_text(size = 10),      # Customize text size
      legend.key.size = unit(1.5, "lines"),      # Size of legend keys
      legend.key.width = unit(0.5, "lines"),     # Width of legend keys
      legend.background = element_rect(fill = "transparent"),  # Transparent background
      legend.margin = margin(10, 10, 10, 10), # Margins around legend
      legend.box.margin = margin(10, -10, 10, -10),
      legend.ticks = element_line(colour = "white"),
      legend.ticks.length = unit(0.1, 'cm')) +
    guides(
      fill = guide_colorbar(
        barwidth = unit(20, "lines"),   # Width of the color bar
        barheight = unit(1, "lines"), # Height of the color bar
        title.position = "top",         # Position of the title
        reverse = FALSE
      )
    ) +
    coord_sf(expand = 0, ylim = c(9.3, 15.2), xlim = c(-5.7,2.5))
  
  # Spatial trends and significance
  
  source("E:/CMIP6/Data/Burkina/results/cdd/MKtrend.R")
  mk_cdd <- MKraster(cdd_burkina, type = "both")
  
  # Convert to dataframe (ggplot purpose)
  trend_cdd_df <- as.data.frame(mk_cdd[[1]], xy = TRUE) 
  colnames(trend_cdd_df) <- c('x','y','value')
  
  # Significance
  sig_cdd_df <- as.data.frame(mk_cdd[[2]], xy = TRUE) 
  colnames(sig_cdd_df) <- c('x','y','value')
  sig_cdd_dff <- sig_cdd_df %>% 
    mutate(significance = ifelse(value<0.05, "p < 0.05",  "p > 0.05"))
  sig_cdd_dff <- sig_cdd_dff[complete.cases(sig_cdd_dff),]
  
  # Plot the raster and shapefile
  p2 <- ggplot() +
    geom_raster(data = trend_cdd_df, aes(x = x, y = y, fill = value)) +
    geom_point(data = sig_cdd_dff, aes(x = x, y = y, color = significance),
               size = 5, shape = 3, stroke = 1) +
    scale_color_manual(name = "Sig.", values = c("p < 0.05" = "white", "p > 0.05" = "transparent"),
                       breaks = "p < 0.05") +
    geom_sf(data = regions_burkina,
            fill = NA, color = "black",
            linewidth = 1) +
    scale_fill_viridis_c(name = "Trend", option = "plasma",
                         #limits = c(-0.3,0.3),
                         na.value = "white") + # "plasma"
    theme_light() +
    labs(x = NULL, y = NULL, 
         title = paste0(scenario, periode)) +
    theme(
      plot.title = element_text(colour = "white"),
      panel.border = element_rect(colour = "black"),
      panel.grid = element_blank(),
      axis.text  = element_text(colour = "navy",
                                face = "bold",
                                size = 14),
      legend.frame = element_rect(colour = "black"),
      legend.position = c(0.7,0.1),
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.title = element_text(size = 12),    # Customize title size
      legend.text = element_text(size = 10),      # Customize text size
      legend.key.size = unit(1.5, "lines"),      # Size of legend keys
      legend.key.width = unit(1.5, "lines"),     # Width of legend keys
      legend.key = element_rect(fill = "gray"),
      legend.background = element_rect(fill = "transparent"),  # Transparent background
      legend.margin = margin(10, 10, 10, 10), # Margins around legend
      legend.ticks = element_line(colour = "white")) +
    guides(
      fill = guide_colorbar(
        barwidth = unit(15, "lines"),   # Width of the color bar
        barheight = unit(1, "lines"), # Height of the color bar
        ticks = TRUE,                  # Show ticks
        ticks.linewidth = 0.5,         # Line width of ticks
        title.position = "top",       # Position of the title
        draw.ulim = TRUE,
        reverse = FALSE),
      color = guide_legend(title.position = "top")
    ) +
    coord_sf(expand = 0, ylim = c(9.3, 15.2), xlim = c(-5.7,2.5))

  return(list(p1,p2))
}

# Indice cwd
results_his <- make_plot_indices(cwd_burkina, "Historical", "1950 - 2014",
                                 "CWD", "Jours")
plot_cwd_his <- results_his[[1]]
plot_cwd_trend_his <- results_his[[2]]

results_ssp2 <- make_plot_indices(cwd_burkina_ssp2, "Future - SSP2",
                                  "2025 - 2050","CWD", "Jours")
plot_cwd_ssp2 <- results_ssp2[[1]]
plot_cwd_trend_ssp2 <- results_ssp2[[2]]

results_ssp5 <- make_plot_indices(cwd_burkina_ssp5, "Future - SSP5",
                                  "2025 - 2050","CWD", "Jours")
plot_cwd_ssp5 <- results_ssp5[[1]]
plot_cwd_trend_ssp5 <- results_ssp5[[2]]

# Indice cdd
results_cdd_his <- make_plot_indices(cdd_burkina, "Historical", "1981 - 2014",
                                 "CDD", "Jours")

plot_cdd_his <- results_cdd_his[[1]]
plot_cdd_trend_his <- results_cdd_his[[2]]

results_cdd_ssp2 <- make_plot_indices(cdd_burkina_ssp2, "Future - SSP2",
                                  "2025 - 2050","CDD", "Jours")
plot_cdd_ssp2 <- results_cdd_ssp2[[1]]
plot_cdd_trend_ssp2 <- results_cdd_ssp2[[2]]

results_cdd_ssp5 <- make_plot_indices(cdd_burkina_ssp5, "Future - SSP5",
                                  "2025 - 2050","CWD", "Jours")
plot_cdd_ssp5 <- results_cdd_ssp5[[1]]
plot_cdd_trend_ssp5 <- results_cdd_ssp5[[2]]

# Indice sdii 
results_sdii_his <- make_plot_indices(sdii_burkina, "Historical", "1950 - 2014",
                                 "CWD", "Jours")
plot_sdii_his <- results_sdii_his[[1]]
plot_sdii_trend_his <- results_sdii_his[[2]]

results_sdii_ssp2 <- make_plot_indices(sdii_burkina_ssp2, "Future - SSP2",
                                  "2025 - 2050","CWD", "Jours")
plot_sdii_ssp2 <- results_sdii_ssp2[[1]]
plot_sdii_trend_ssp2 <- results_sdii_ssp2[[2]]

results_sdii_ssp5 <- make_plot_indices(sdii_burkina_ssp5, "Future - SSP5",
                                  "2025 - 2050","CWD", "Jours")
plot_sdii_ssp5 <- results_sdii_ssp5[[1]]
plot_sdii_trend_ssp5 <- results_sdii_ssp5[[2]]

# Indices r10mm
results_r10mm_his <- make_plot_indices(r10mm_burkina, "Historical", "1950 - 2014",
                                       "R10mm", "Jours")
plot_r10mm_his <- results_r10mm_his[[1]]
plot_r10mm_trend_his <- results_r10mm_his[[2]]

results_r10mm_ssp2 <- make_plot_indices(r10mm_burkina_ssp2, "Future - SSP2",
                                        "2025 - 2050","R10mm", "Jours")
plot_r10mm_ssp2 <- results_r10mm_ssp2[[1]]
plot_r10mm_trend_ssp2 <- results_r10mm_ssp2[[2]]

results_r10mm_ssp5 <- make_plot_indices(r10mm_burkina_ssp5, "Future - SSP5",
                                        "2025 - 2050","R10mm", "Jours")
plot_r10mm_ssp5 <- results_r10mm_ssp5[[1]]
plot_r10mm_trend_ssp5 <- results_r10mm_ssp5[[2]]

# Indices r20mm
results_r20mm_his <- make_plot_indices(r20mm_burkina, "Historical", "1950 - 2014",
                                 "R20mm", "Jours")
plot_r20mm_his <- results_r20mm_his[[1]]
plot_r20mm_trend_his <- results_r20mm_his[[2]]

results_r20mm_ssp2 <- make_plot_indices(r20mm_burkina_ssp2, "Future - SSP2",
                                  "2025 - 2050","R20mm", "Jours")
plot_r20mm_ssp2 <- results_r20mm_ssp2[[1]]
plot_r20mm_trend_ssp2 <- results_r20mm_ssp2[[2]]

results_r20mm_ssp5 <- make_plot_indices(r20mm_burkina_ssp5, "Future - SSP5",
                                  "2025 - 2050","R20mm", "Jours")
plot_r20mm_ssp5 <- results_r20mm_ssp5[[1]]
plot_r20mm_trend_ssp5 <- results_r20mm_ssp5[[2]]

##### Making the plots ####
# simple grid with labels and aligned plots
final_plot_cwd <- plot_grid(plot_cwd_his + theme(plot.margin=grid::unit(c(10,0,0,-10), "mm")) ,NULL,plot_cwd_trend_his  + theme(plot.margin=grid::unit(c(10,0,0,-10), "mm")),
                        plot_cwd_ssp2+ theme(plot.margin=grid::unit(c(10,0,0,-10), "mm")),NULL, plot_cwd_trend_ssp2+ theme(plot.margin=grid::unit(c(10,0,0,-10), "mm")),
                        plot_cwd_ssp5+ theme(plot.margin=grid::unit(c(10,0,0,-10), "mm")), NULL, plot_cwd_trend_ssp5+ theme(plot.margin=grid::unit(c(10,0,0,-10), "mm")),
                        nrow  = 3,
                        align = "hv",
                        rel_widths = c(3,-1.8,3))

final_plot_cdd <- plot_grid(plot_cdd_his + theme(plot.margin=grid::unit(c(10,0,0,-10), "mm")) ,NULL,plot_cdd_trend_his  + theme(plot.margin=grid::unit(c(10,0,0,-10), "mm")),
                            plot_cdd_ssp2+ theme(plot.margin=grid::unit(c(10,0,0,-10), "mm")),NULL, plot_cdd_trend_ssp2+ theme(plot.margin=grid::unit(c(10,0,0,-10), "mm")),
                            plot_cdd_ssp5+ theme(plot.margin=grid::unit(c(10,0,0,-10), "mm")), NULL, plot_cdd_trend_ssp5+ theme(plot.margin=grid::unit(c(10,0,0,-10), "mm")),
                            nrow  = 3,
                            align = "hv",
                            rel_widths = c(3,-1.8,3))

final_plot_sdii <- plot_grid(plot_sdii_his + theme(plot.margin=grid::unit(c(10,0,0,-10), "mm")) ,NULL,plot_sdii_trend_his  + theme(plot.margin=grid::unit(c(10,0,0,-10), "mm")),
                            plot_sdii_ssp2+ theme(plot.margin=grid::unit(c(10,0,0,-10), "mm")),NULL, plot_sdii_trend_ssp2+ theme(plot.margin=grid::unit(c(10,0,0,-10), "mm")),
                            plot_sdii_ssp5+ theme(plot.margin=grid::unit(c(10,0,0,-10), "mm")), NULL, plot_sdii_trend_ssp5+ theme(plot.margin=grid::unit(c(10,0,0,-10), "mm")),
                            nrow  = 3,
                            align = "hv",
                            rel_widths = c(3,-1.8,3))

final_plot_r10mm <- plot_grid(plot_r10mm_his + theme(plot.margin=grid::unit(c(10,0,0,-10), "mm")) ,NULL,plot_r10mm_trend_his  + theme(plot.margin=grid::unit(c(10,0,0,-10), "mm")),
                            plot_r10mm_ssp2+ theme(plot.margin=grid::unit(c(10,0,0,-10), "mm")),NULL, plot_r10mm_trend_ssp2+ theme(plot.margin=grid::unit(c(10,0,0,-10), "mm")),
                            plot_r10mm_ssp5+ theme(plot.margin=grid::unit(c(10,0,0,-10), "mm")), NULL, plot_r10mm_trend_ssp5+ theme(plot.margin=grid::unit(c(10,0,0,-10), "mm")),
                            nrow  = 3,
                            align = "hv",
                            rel_widths = c(3,-1.8,3))

final_plot_r20mm <- plot_grid(plot_r20mm_his + theme(plot.margin=grid::unit(c(10,0,0,-10), "mm")) ,NULL,plot_r20mm_trend_his  + theme(plot.margin=grid::unit(c(10,0,0,-10), "mm")),
                            plot_r20mm_ssp2+ theme(plot.margin=grid::unit(c(10,0,0,-10), "mm")),NULL, plot_r20mm_trend_ssp2+ theme(plot.margin=grid::unit(c(10,0,0,-10), "mm")),
                            plot_r20mm_ssp5+ theme(plot.margin=grid::unit(c(10,0,0,-10), "mm")), NULL, plot_r20mm_trend_ssp5+ theme(plot.margin=grid::unit(c(10,0,0,-10), "mm")),
                            nrow  = 3,
                            align = "hv",
                            rel_widths = c(3,-1.8,3))

ggsave("final_plot_c.png", plot = final_plot,
       dpi = 400, height = 20, width = 30, bg = "white")

ggsave("final_plot_cdd.png", plot = final_plot_cdd,
       dpi = 400, height = 20, width = 30, bg = "white")

ggsave("final_plot_r10mm.png", plot = final_plot_r10mm,
       dpi = 400, height = 20, width = 30, bg = "white")

ggsave("final_plot_r20mm.png", plot = final_plot_r20mm,
       dpi = 400, height = 20, width = 30, bg = "white")
