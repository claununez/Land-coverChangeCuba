# Protect: Change of Cuba vegetation land cover
# Authors: Claudia Nunez- Penichet, Juan Maita-Chamba, and Jorge Soberon
# Date: October 9, 2023

# Loading packages
library(terra)

# Defining working directory
setwd("WORKING DIRECTORY")

# Reading the layer
layer1985 <- rast("cuba_1985_rfL5_rc5_byte_majority.tif")
layer2020 <- rast("cuba_2021_rfL8_rc5_byte_majority.tif")

# Changing values of the categories
layer1985 <- layer1985 * 1
layer2020 <- layer2020 * 1
layer2020 <- layer2020 * 100

both <- layer1985 + layer2020

writeRaster(both, "layer_both_10_9_23.tif", overwrite = T)

# 1, 100 = Agriculture
# 2, 200 = Forest and Shrubbery
# 3, 300 = Mangrove
# 4, 400 = Pine forest
# 5, 500 = Soils without vegetation cover (SWVC)
# 6, 600 = Water
# 7, 700 = Wetlands

# 101 = Agriculture stable
# 102 = Forest and Shrubbery to Agriculture
# 103 = Mangrove to Agriculture
# 104 = Pine forest to Agriculture
# 105 = SWVC to Agriculture
# 106 = Water to Agriculture
# 107 = Wetland to Agriculture

# 201 = Agriculture to Forest and Shrubbery
# 202 = Forest and Shrubbery stable
# 203 = Mangrove to Forest and Shrubbery
# 204 = Pine forest to Forest and Shrubbery
# 205 = SWVC to Forest and Shrubbery
# 206 = Water to Forest and Shrubbery
# 207 = Wetland to Forest and Shrubbery

# 301 = Agriculture to Mangrove
# 302 = Forest and Shrubbery to Mangrove
# 303 = Mangrove stable
# 304 = Pine forest to Mangrove
# 305 = SWVC to Mangrove
# 306 = Water to Mangrove
# 307 = Wetland to Mangrove

# 401 = Agriculture to Pine forest
# 402 = Forest and Shrubbery to Pine forest
# 403 = Mangrove to Pine forest
# 404 = Pine forest stable
# 405 = SWVC to Pine forest
# 406 = Water to Pine forest
# 407 = Wetland to Pine forest

# 501 = Agriculture to SWVC
# 502 = Forest and Shrubbery to SWVC
# 503 = Mangrove to SWVC
# 504 = Pine forest to SWVC
# 505 = SWVC stable
# 506 = Water to SWVC
# 507 = Wetland to SWVC

# 601 = Agriculture to Water
# 602 = Forest and Shrubbery to Water
# 603 = Mangrove to Water
# 604 = Pine forest to Water
# 605 = SWVC to Water
# 606 = Water stable
# 607 = Wetland to Water

# 701 = Agriculture to Wetland
# 702 = Forest and Shrubbery to Wetland
# 703 = Mangrove to Wetland
# 704 = Pine forest to Wetland
# 705 = SWVC to Wetland
# 706 = Water to Wetland
# 707 = Wetland stable


# Statistics (All Cuba)
val_1985 <- terra::na.omit(layer1985[])
val_2020 <- terra::na.omit(layer2020[])
val_both <- terra::na.omit(both[])

count1985 <- table(val_1985)
count2020 <- table(val_2020)
countboth <- table(val_both)

write.csv(count1985, "count1985_10_9_23.csv")
write.csv(count2020, "count2020_10_9_23.csv")
write.csv(countboth, "countboth_10_9_23.csv")

# Protecting areas in Cuba
prot_areas <- vect("AreasProtegidasSNAP2014_2020_region.shp")

prot_areas <- project(prot_areas, layer1985)

prot_ar_layer1985 <- terra::crop(layer1985, prot_areas, mask = TRUE)
prot_ar_layer2020 <- terra::crop(layer2020, prot_areas, mask = TRUE)
prot_ar_both <- terra::crop(both, prot_areas, mask = TRUE)

writeRaster(prot_ar_layer1985, "prot_ar_layer1985_10_9_23.tif", overwrite = T)
writeRaster(prot_ar_layer2020, "prot_ar_layer2020_10_9_23.tif", overwrite = T)
writeRaster(prot_ar_both, "prot_ar_both_10_9_23.tif", overwrite = T)

# Statistics (Protecting areas in all Cuba)
prot_val_1985 <- na.omit(prot_ar_layer1985[])
prot_val_2020 <- na.omit(prot_ar_layer2020[])
prot_val_both <- na.omit(prot_ar_both[])

prot_count1985 <- table(prot_val_1985)
prot_count2020 <- table(prot_val_2020)
prot_countboth <- table(prot_val_both)

write.csv(prot_count1985, "prot_count1985_10_9_23.csv")
write.csv(prot_count2020, "prot_count2020_10_9_23.csv")
write.csv(prot_countboth, "prot_countboth_10_9_23.csv")

# Considering Omphalea potential distribution
# Reading Omphalea raster
omp <- rast("modelo_omphalea.tif") 
omp[omp[] == 0] <- NA
#omp1 <- rasterToPolygons(omp, dissolve = TRUE)
omp1 <- as.polygons(omp, round = TRUE, aggregate = TRUE, values = TRUE,
                    na.rm = TRUE, na.all = FALSE, extent = FALSE, digits = 0)
writeVector(omp1, filename = "omp1_10_9_2023", overwrite = T)

omp_layer1985 <- terra::crop(layer1985, omp1, mask = T)
omp_layer2020 <- terra::crop(layer2020, omp1, mask = T)
omp_both <- terra::crop(both, omp1, mask = T)

writeRaster(omp_layer1985, "omp_layer1985_10_9_23.tif", overwrite = T)
writeRaster(omp_layer2020, "omp_layer2020_10_9_23.tif", overwrite = T)
writeRaster(omp_both, "omp_both_10_9_23.tif", overwrite = T)

# Statistics (in Omphalea potential distribution)
omp_val_1985 <- na.omit(omp_layer1985[])
omp_val_2020 <- na.omit(omp_layer2020[])
omp_val_both <- na.omit(omp_both[])

omp_count1985 <- table(omp_val_1985)
omp_count2020 <- table(omp_val_2020)
omp_countboth <- table(omp_val_both)

write.csv(omp_count1985, "omp_count1985_10_9_23.csv")
write.csv(omp_count2020, "omp_count2020_10_9_23.csv")
write.csv(omp_countboth, "omp_countboth_10_9_23.csv")

# Potential distribution of Omphalea inside protective areas
omp_prot_ar_layer1985 <- mask(crop(omp_layer1985, prot_areas), prot_areas)
omp_prot_ar_layer2020 <- mask(crop(omp_layer2020, prot_areas), prot_areas)
omp_prot_ar_both <- mask(crop(omp_both, prot_areas), prot_areas)

writeRaster(omp_prot_ar_layer1985, "omp_prot_ar_layer1985_10_9_23.tif", 
            overwrite = T)
writeRaster(omp_prot_ar_layer2020, "omp_prot_ar_layer2020_10_9_23.tif", 
            overwrite = T)
writeRaster(omp_prot_ar_both, "omp_prot_ar_both_10_9_23.tif", overwrite = T)

# Statistics (in Protective areas inside Omphalea potential distribution)
omp_prot_val_1985 <- na.omit(omp_prot_ar_layer1985[])
omp_prot_val_2020 <- na.omit(omp_prot_ar_layer2020[])
omp_prot_val_both <- na.omit(omp_prot_ar_both[])

omp_prot_count1985 <- table(omp_prot_val_1985)
omp_prot_count2020 <- table(omp_prot_val_2020)
omp_prot_countboth <- table(omp_prot_val_both)

write.csv(omp_prot_count1985, "omp_prot_count1985_10_9_23.csv")
write.csv(omp_prot_count2020, "omp_prot_count2020_10_9_23.csv")
write.csv(omp_prot_countboth, "omp_prot_countboth_10_9_23.csv")
