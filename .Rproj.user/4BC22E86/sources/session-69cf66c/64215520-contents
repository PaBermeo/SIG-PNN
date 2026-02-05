
### Land use PNN Las Hermosas ####

#Authors: Bermeo, Paula & Rico, Mauricio

#Load libraries
pacman::p_load(dplyr, cowplot, geobr, ggplot2, ggpattern, ggspatial, gridGraphics, sf, sp, tmap, terra, raster, readr, rnaturalearth, viridis)

# Land use raster loads

LU_86 <- rast("Raster/Mapbiomas/INTEGRACION-COLOMBIA-COL3-1986.tif")

LU_24 <- rast("Raster/Mapbiomas/INTEGRACION-COLOMBIA-COL3-2024.tif")

#Load shapefile
Fincas <-  st_read("Vectorial/PREDIOS_KFW_SSC_Ganaderia/Predios_KFW_SSC_Ganaderia_POL.shp")

#Project
t_crs <- crs(LU_86, proj = TRUE) 
Fincas1 <- st_transform(Fincas, t_crs)

F1 <- Fincas[Fincas$DIRECCION == "LA FLORESTA", ] 

# change to tmap mode
tmap::tmap_mode(mode = "plot")
tmap_options(check.and.fix = TRUE)

palet <- 

    tm_raster(F1, bbox = c(-480000, -31, -36.6, -8), style = "cat", #legend.reverse= T, palette = palet,
           title = 'Uso de suelo') +
    Tm_shape()
      #4704277 ymin: 1985876 xmax: 4705115 ymax: 1986412

