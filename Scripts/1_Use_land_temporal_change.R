
### Land use PNN Las Hermosas ####

#Authors: Bermeo, Paula & Rico, Mauricio

#Load libraries
pacman::p_load(dplyr, cowplot, geobr, ggplot2, ggpattern, ggspatial, gridGraphics, sf, sp, tmap, terra, raster, readr, rnaturalearth, viridis)

# Land use raster loads

LU_86 <- rast("Raster/Mapbiomas/INTEGRACION-COLOMBIA-COL3-1986.tif")
crs(LU_86) <- "epsg:4326"

LU_24 <- rast("Raster/Mapbiomas/INTEGRACION-COLOMBIA-COL3-2024.tif")

#Load shapefile
Fincas <-  "Vectorial/PREDIOS_KFW_SSC_Ganaderia/Predios_KFW_SSC_Ganaderia_POL.shp"
Fincas1 = vect(Fincas)
crs(Fincas1) <- "epsg:4326"

#Project rasters
Fincas1 <- project(Fincas1, crs(LU_86))

F1 <- Fincas1[Fincas1$DIRECCION == "LA FLORESTA", ] 

#Mask variables to the ca (Calibration Area)
F1_86 <- mask(crop(LU_86, F1), F1)
#plot(F1_86)
#res(F1_86)

# change to tmap mode
tmap::tmap_mode(mode = "plot")
tmap_options(check.and.fix = TRUE)

expand_bbox <- function(x, digits = 3){
  
  e <- ext(x)
  
  mult <- 10^digits
  
  xmin <- floor(e[1] * mult) / mult
  xmax <- ceiling(e[2] * mult) / mult
  ymin <- floor(e[3] * mult) / mult
  ymax <- ceiling(e[4] * mult) / mult
  
  c(xmin, ymin, xmax, ymax)
}

#Expandir bbox
bbox_exp <- expand_bbox(F1_86, digits = 3)
bbox_exp

Leg <- c("3" = "Bosque", "21"= "Pastos")
palet <- c("3" = "#228B22", "21" = "lightyellow")

    tm_shape(F1_86, bbox_exp) +  ###deben ser tipo c(xmin, ymin, ...)!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    tm_raster(style = "cat", palette = palet, labels = Leg, title = 'Uso de suelo') +
      #tm_shape(F1) +
      #tm_borders(col = 'white', lwd = 0.05, lty=2) +
      tm_compass(position = c(0.05, 0.65), size= 1.5) + 
      tm_scale_bar(text.size = 0.6, position = c(0.5, 0), width = 0.15) + #scale bar
      tm_graticules(lines = F, labels.rot = c(0, 90), labels.size = 0.55) +
      tm_layout(main.title= "Año 1986")

