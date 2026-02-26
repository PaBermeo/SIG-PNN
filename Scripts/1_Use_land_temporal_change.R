
### Land use PNN Las Hermosas ####

#Authors: Bermeo, Paula & Rico, Mauricio


# Load libraries ----------------------------------------------------------
pacman::p_load(dplyr, cowplot, geobr, ggplot2, ggpattern, ggspatial, gridGraphics, sf, sp, tmap, terra, raster, readr, rnaturalearth, stars,viridis)


# # Land use raster loads -------------------------------------------------


LU_86 <- rast("Raster/Mapbiomas/INTEGRACION-COLOMBIA-COL3-1986.tif")
crs(LU_86) <- "epsg:4326"

LU_24 <- rast("Raster/Mapbiomas/INTEGRACION-COLOMBIA-COL3-2024.tif")

#Load shapefile
Fincas <-  "Vectorial/PREDIOS_KFW_SSC_Ganaderia/Predios_KFW_SSC_Ganaderia_POL.shp"
Fincas1 = vect(Fincas)
crs(Fincas1) <- "EPSG:9377"

#Project rasters
Fincas1 <- project(Fincas1, crs(LU_86))

F1 <- Fincas1[Fincas1$DIRECCION == "LA FLORESTA", ] 
F1_sf <- sf::st_as_sf(F1)

#Mask variables to the ca (Calibration Area)
F1_86 <- mask(crop(LU_86, F1), F1)
F1_24 <- mask(crop(LU_24, F1), F1)
#plot(F1_86)
#res(F1_86)



# Map land use ------------------------------------------------------------

# change to tmap mode
tmap::tmap_mode(mode = "plot")
tmap_options(check.and.fix = TRUE)

Leg <- c("3" = "Bosque", "13"= "Bosque", "21"= "Pastos") ##Tener en cuenta 13 se dejará como bosque

palet <- c("3" = "#1F8D49", "13"= "#1F8D49",  "21" = "#FFEFC3")

expand_bbox <- function(x, frac = 0.2){
  e <- terra::ext(x)
  
  xpad <- (e[2] - e[1]) * frac
  ypad <- (e[4] - e[3]) * frac
  
  c(e[1]-xpad, e[2]+xpad, e[3]-ypad, e[4]+ypad)
}

bbox_exp <- expand_bbox(F1_86, frac = 0.3)

    tm_shape(F1_86) + #, bbox=bbox_exp) +
    tm_raster(style = "cat", palette = palet, labels = Leg, title = 'Uso de suelo') +
      tm_shape(F1_sf) +
      tm_borders(col = 'black', lwd = 1, lty=2) +
      tm_compass(position = c(0.05, 0.65), size= 1.5) + 
      tm_scale_bar(text.size = 0.6, position = c(0.5, 0), width = 0.15) + #scale bar
      #tm_graticules(lines = F, labels.rot = c(0, 90), labels.size = 0.55) +
      tm_layout(main.title= "Año 1986")
    Map_1986 <- grid.grab()
    
    tm_shape(F1_24) + #, bbox=bbox_exp) +
      tm_raster(style = "cat", palette = palet, labels = Leg, title = 'Uso de suelo') +
      tm_shape(F1_sf) +
      tm_borders(col = 'black', lwd = 1, lty=2) +
      #tm_compass(position = c(0.05, 0.65), size= 1.5) + 
      #tm_scale_bar(text.size = 0.6, position = c(0.5, 0), width = 0.15) + #scale bar
      tm_layout(main.title= "Año 2024")
  Map_2024 <- grid.grab()
  
  merge_plot <-  plot_grid(Map_1986, Map_2024, align = "h")
  
  ggsave(plot=merge_plot, "La_Floresta.png", width = 14, height = 5.3, units = "in", dpi= 600)
   


# Generar shapefile de pérdida/ganancia -----------------------------------
  
 Bosq_84 <- F1_86 %in% c(3, 13) 
 Bosq_24 <-  F1_24 %in% c(3, 13)

 Loss <-  Bosq_84 & !Bosq_24
 Gain <-  !Bosq_84 & Bosq_24
 
 #Ploteando pérdidas y ganancias
 par(mfrow = c(2, 2), cex = 0.5, mar = rep(0.6, 4))
 plot(Bosq_84); legend("topleft", legend = "1984", border = "white")
 plot(Bosq_24); legend("topleft", legend = "2024", bty = "n")
 plot(Loss); legend("topleft", legend = "Area_loss", bty = "n")
 plot(Gain); legend("topleft", legend = "Area_gain", bty = "n")
 dev.off()
 
 
 Loss1 <- st_as_stars(Loss, proxy=T) 
 L <- st_as_sf(Loss1, merge = T)
 LossF <- L[L$classification_1986 == 1, ]
 
 st_write(LossF, "Vectorial/Gananc_perdd_finca/PerdidaBosq_Fincaname.shp",  overwrite = TRUE)

#Gnancia entre el año   
 Gain1 <- st_as_stars(Gain, proxy=T) 
 G <- st_as_sf(Gain1, merge = T)
 GainF <- G[G$classification_1986 == 1, ]
 
 st_write(GainF, "Vectorial/Gananc_perdd_finca/GanaBosq_Fincaname.shp",  overwrite = TRUE)
 
#Cálculo de áreas de pérdida y ganancia por finca
 
#Recortar loss/gain de acuerdo al shapefile de cada finca

 Cambio_m <- project(Loss, "EPSG:3116")
 F1_m <- project(F1, "EPSG:3116")
 
# Aumentar resolución
Cambio_fino <- disagg(Cambio_m, fact=40)
 
# Ahora aplicar mask
Cambio_rec <- mask(crop(Cambio_fino, F1_m), F1_m)
plot(Cambio_rec)

#cálculo de areas de acuerdo con script PMP ENM 

