
### Land use PNN Las Hermosas ####

#Authors: Bermeo, Paula & Rico, Mauricio

# Load libraries ----------------------------------------------------------
pacman::p_load(dplyr, cowplot, geobr, ggplot2, ggpattern, ggspatial, gridGraphics, sf, sp, tmap, terra, raster, readr, rnaturalearth, stars,viridis, writexl)

Fincas_marzo <- c( "GRANJA AGROECOLOGICA BRISAS AMOYA")
  
Finca mar3 <-   Finca c("EL MIRADOR")

Fincas_mar2 <- c("EL TRIUNFO", "LAS PALMAS", "ALTOS DEL JARDIN", "EL RECREO", "LAS BRISAS", "LAS AURAS", "LA LAGUNA", "LOS OLIVOS", "LA FLORESTA")

# Land use raster loads -------------------------------------------------
# 
# LU_86 <- rast("Raster/Mapbiomas/INTEGRACION-COLOMBIA-COL3-1986.tif")
# crs(LU_86) <- "epsg:4326"
# 
# LU_24 <- rast("Raster/Mapbiomas/INTEGRACION-COLOMBIA-COL3-2024.tif")
# 
# #Load shaefile Chaparral
# Col <-  "Vectorial/Servicios_Públicos_-_Municipios_2005.shp"
# Col1 = vect(Col)
# Ch <- Col1[Col1$MPIO_CNMBR ==  "CHAPARRAL"]
# crs(Ch) <- "EPSG:3857"
# 
# #Project raster
# Ch <- project(Ch, LU_86)
# 
# #Enmascarar sólo para Chaparral
# LU_86Ch <- mask(crop(LU_86, Ch), Ch)
# LU_24Ch <- mask(crop(LU_24, Ch), Ch)
# #plot(F1_86)
# #res(F1_86)
# 
# LU_86p <- project(LU_86Ch, "EPSG:4326", method="near")
# LU_86f <- disagg(LU_86p, fact= 40)
# LU_86p
# 
# raster::writeRaster(LU_86f, "Raster/Land_use_Chaparral_1986.tiff", overwrite=TRUE)
# 
# LU_24p <- project(LU_24Ch, "EPSG:4326", method="near")
# LU_24f <- disagg(LU_24p, fact= 40)
# LU_24f
# 
# raster::writeRaster(LU_24f, "Raster/Land_use_Chaparral_2024.tiff", overwrite=TRUE)


# Loop mapas 1984 2024 por finca ------------------------------------------

LU_86 <- rast("Raster/Land_use_Chaparral_1986.tiff")
crs(LU_86) <- "EPSG:4326"

LU_24 <- rast("Raster/Land_use_Chaparral_2024.tiff")
crs(LU_24) <- "EPSG:4326"

#Load shapefile
Fincas <-  "Vectorial/PREDIOS_KFW_SSC_Ganaderia/Predios_KFW_SSC_Ganaderia_POL.shp"
Fincas1 = vect(Fincas)
crs(Fincas1) <- "EPSG:9377"

#Project rasters
#Fincas1 <- project(Fincas1, crs(LU_86))


#Map land use -

# change to tmap mode
tmap::tmap_mode(mode = "plot")
tmap_options(check.and.fix = TRUE)

# Loop fincas

for (finca in 1: length(Fincas_marzo))

{ 
  finca <- 1
  
  F1 <- Fincas1[Fincas1$DIRECCION == Fincas_marzo[finca], ] 
  plot(F1)
  
  F1_m <- project(F1, "EPSG:4326")
  F1sf <- sf::st_as_sf(F1_m, crs= st_crs(4326))

# expand_bbox <- function(x, frac = 0.2){
#   e <- terra::ext(x)
#   
#   xpad <- (e[2] - e[1]) * frac
#   ypad <- (e[4] - e[3]) * frac
#   
#   c(e[1]-xpad, e[2]+xpad, e[3]-ypad, e[4]+ypad)
# }
# 
# bbox_exp <- expand_bbox(F1_86, frac = 0.3)

# Ahora aplicar mask
F1_86b <- mask(crop(LU_86, F1_m), F1_m)
plot(F1_86b)

Leg <- c("3" = "Bosque", "13"= "", "21"= "Pastos") ##Tener en cuenta 13 se dejará como bosque

palet <- c("3" = "#1F8D49", "13"= "#1F8D49",  "21" = "#FFEFC3")

    tm_shape(F1_86b) + #, bbox= c(-75, -75.56, 3.74, 3.758)) +
    tm_raster(style = "cat", palette = palet, labels = Leg, title = ' ') +
    tm_shape(F1sf) +
    tm_borders(col = 'black', lwd = 1, lty=2) +
    tm_compass(position = c(0.9, 0.9), size= 2) + 
      tm_scale_bar(text.size = 0.5, position = c(0.1, 0), width = 0.38) + 
     # tm_graticules(lines = F, labels.rot = c(0, 90), labels.size = 0.55) +
    tm_layout(main.title= "Año 1986",
                #legend.position = c(0.7, 0.8),
               legend.outside = T
                ) +
    tm_add_legend(type = "line", 
                  labels = "Perímetro finca", 
                  col = "black", 
                  lwd = 1,
                  lty=2,
                  size = 0.9)
    
    Map_1986 <- grid.grab()
    
    
#Mapeando mapa 2024 

F1_24b <- mask(crop(LU_24, F1_m), F1_m)
plot(F1_24b)  
    
      tm_shape(F1_24b) +
        tm_raster(style = "cat", palette = palet, labels = Leg, title = ' ') +
        tm_shape(F1sf) +
        tm_borders(col = 'black', lwd = 1, lty=2) +
        tm_compass(position = c(0.9, 0.9), size= 2) + 
        tm_scale_bar(text.size = 0.5, position = c(0.1, 0), width = 0.38) + #scale bar
        #tm_graticules(lines = F, labels.rot = c(0, 90), labels.size = 0.55) +
        tm_layout(main.title= "Año 2024",
                  #legend.position = c(0.7, 0.75
                  legend.outside = T
        ) +
        tm_add_legend(type = "line", 
                      labels = "Perímetro finca", 
                      col = "black", 
                      lwd = 1,
                      lty=2,
                      size = 1.2)
  
  Map_2024 <- grid.grab()
  
  merge_plot <-  plot_grid(Map_1986, Map_2024, align = "h")
  
  ggsave(plot=merge_plot, paste0("Entrega_fase_I/Mapas_perdd_gananc/", Fincas_marzo[finca], ".png"), width = 23, height = 15, units = "cm", dpi= 600)
  
}

# Generar shapefile de pérdida/ganancia -----------------------------------

LU_86 <- rast("Raster/Land_use_Chaparral_1986.tiff")
crs(LU_86) <- "EPSG:4326"

LU_24 <- rast("Raster/Land_use_Chaparral_2024.tiff")
crs(LU_24) <- "EPSG:4326"

#Load shapefile
Fincas <-  "Vectorial/PREDIOS_KFW_SSC_Ganaderia/Predios_KFW_SSC_Ganaderia_POL.shp"
Fincas1 = vect(Fincas)
crs(Fincas1) <- "EPSG:9377"
  

for (finca in 1: length(Fincas_marzo))
  
{
  F1 <- Fincas1[Fincas1$DIRECCION == Fincas_marzo[finca], ] 
  
  if(nrow(F1) == 0){
    cat("No encontrada:", Fincas_marzo[finca], "\n")
    next
  }
  
  F1_m <- project(F1, "EPSG:4326")
  F1sf <- sf::st_as_sf(F1_m, crs= st_crs(4326))

  F1_86b <- mask(crop(LU_86, F1_m), F1_m)
  F1_24b <- mask(crop(LU_24, F1_m), F1_m)
 
  Bosq_84 <- F1_86b %in% c(3, 13) 
  Bosq_24 <-  F1_24b %in% c(3, 13)

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
 
 st_write(LossF, paste0("Vectorial/Perdida_bosque/", Fincas_marzo[finca], ".shp"),  overwrite = T)

#Ganancia entre 1985 a 2024   
 Gain1 <- st_as_stars(Gain, proxy=T) 
 G <- st_as_sf(Gain1, merge = T)
 GainF <- G[G$classification_1986 == 1, ]
 
 st_write(GainF, paste0("Vectorial/Ganancia_bosque/", Fincas_marzo[finca], ".shp"),  overwrite = T)
 

}


# #Cálculo de áreas de pérdida y ganancia por finca -----------------------

#Recortar loss/gain de acuerdo al shapefile de cada finca

 LU_86 <- rast("Raster/Land_use_Chaparral_1986.tiff")
 crs(LU_86) <- "EPSG:4326"
 #LU_86 <- project(LU_86, "EPSG:3116")
 
 LU_24 <- rast("Raster/Land_use_Chaparral_2024.tiff")
 crs(LU_24) <- "EPSG:4326"
 #LU_24 <- project(LU_24, "EPSG:3116")

#Load shapefile
 Fincas <-  "Vectorial/PREDIOS_KFW_SSC_Ganaderia/Predios_KFW_SSC_Ganaderia_POL.shp"
 Fincas1 = vect(Fincas)
 crs(Fincas1) <- "EPSG:9377"
 Fincas1 <- project(Fincas1, "EPSG:4326")

 for (finca in 1: length(Fincas_marzo))
   
 { 
 
 F1 <- Fincas1[Fincas1$DIRECCION == Fincas_marzo[finca], ]  #Fincas_marzo[finca]

 F1sf <- sf::st_as_sf(F1) #, crs= st_crs(4326))

 F1_86b <- mask(crop(LU_86, F1), F1)
 F1_24b <- mask(crop(LU_24, F1), F1)

 Bosq_84 <- F1_86b %in% c(3, 13) 
 Bosq_24 <-  F1_24b %in% c(3, 13)

 Loss <-  Bosq_84 & !Bosq_24
 Gain <-  !Bosq_84 & Bosq_24 
 
 area_finca <- round(as.numeric(sf::st_area(F1_sf) / 10000), 2)
 #area_finca
 
 #cálculo de areas de acuerdo con script PMP ENM 

 area_p <- cellSize(Loss, unit = "ha")

 area_per <- round(global(area_p * (Loss==1), sum, na.rm = F), 2)[1,1] 

 #area_per #Área perdida en ha entre el año 1985 a 2024


#Cálculo de porcentaje de área ganada

#cálculo de areas de acuerdo con script PMP ENM 

area_g <- cellSize(Gain, unit = "ha")

area_gain <- round(global(area_g * (Gain==1), sum, na.rm = TRUE), 2)[1,1]

#area_gain #Área ganada de bosque en ha entre el año 1985 a 2024

# porcentaje
perc_loss <- (area_per / area_finca) * 100
perc_gain <- (area_gain / area_finca) * 100

tabla_r <- data.frame(
  Categoria = c("Area finca", "Perdida de Bosque", "Ganancia de Bosque"),
  Hectareas = c(area_finca, area_per, area_gain),
  Porcentaje = c("100%", 
                 paste0(round(perc_loss,0), "%"),
                 paste0(round(perc_gain,0), "%"))
)

tabla_r
 
write.csv(tabla_r, paste0("Entrega_fase_I/Tabla_perdidas_ganancias/", Fincas_marzo[finca],".csv")) 
}
