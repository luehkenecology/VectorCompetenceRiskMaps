# load libraries----------------------------------------
library(maptools)
library(raster)

# set working directory----------------------------------------
#setwd("C:/Users/RenkeLuehken/Google Drive/Project/VectorCompetenceRiskMaps")
setwd("G:/NeuAll/Project/VectorCompetenceRiskMaps")

# read shapefiles of European countries----------------------------------------
# all countries in Europe
european_countries <- c("BEL", "BGR", "DNK", "DEU", "EST", "FIN",
                 "FRA", "GRC", "IRL", "ITA", "HRV", "LTU",
                 "LUX", "MLT", "NLD", "AUT", "POL", "PRT",
                 "ROU", "SWE", "ESP", "SVK", "SVN", "CZE",
                 "HUN", "GBR", "ALB", "BIH", "NOR", "VAT",
                 "LVA","LIE","CHE","MNE","MKD","ALB","SRB", "Kosovo")

country_shapes_nuts0_list <- lapply(1:38, function(x) getData("GADM", path = "output",country = as.character(european_countries[x]), level = 0))
country_shapes_nuts0_all <- do.call(rbind, country_shapes_nuts0_list)

# read nuts shapefile----------------------------------------
nuts_all <- readShapeSpatial("data/NUTS_2013_01M_SH/data/NUTS_RG_01M_2013") # read NUTS shape file
nuts_all_3 <- subset(nuts_all, STAT_LEVL_ == 3) # only level 0
#nuts_0 <- subset(nuts_all, STAT_LEVL_ == 0) # only level 0
crs(nuts_0) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" # define projection 
crs(nuts_all) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" # define projection 
crs(nuts_all_3) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" # define projection 

# mosquito distribution----------------------------------------
# read data provided by ECDC
invasive_species_distribution <- read.csv("data/Invasive species.csv", sep = ";")

# only established Aedes albopictus populations
invasive_species_distribution_sub <- subset(invasive_species_distribution, vectorspecies == "Aedes_albopictus" & statusDescription == "Established")

# select nuts-shapes
positive_nuts <- subset(nuts_all, 
                        (NUTS_ID %in% c(unique(as.character(invasive_species_distribution_sub$geoID)))))

# established populations in the EU-candidate states
alb <- getData("GADM", country = "ALB", level = 2)
alb2 <- alb[-c(4,5,15,19,20,21,22,23,25),]
mne <- getData("GADM", country = "MNE", level = 1)
mne2 <- mne[c(4,7,8,9,10,12,14,18,21),]
bih <- getData("GADM", country = "BIH", level = 1)

# cropping
cropping_info <- c(-9, 25, 36, 49.8)
country_shapes_nuts0_all_crop <- crop(country_shapes_nuts0_all, cropping_info)
positive_nuts_crop <- crop(positive_nuts, cropping_info)

png(file = "figs/distribution_aedes_albopictus.png",
    width = 7, height = 4.3, units = 'in', res = 1000)
par(mar=c(2.5,2.5,1.5,1.5))
plot(country_shapes_nuts0_all_crop, cex.axis=0.9, axes = T)
plot(positive_nuts_crop, border = "gray", col = "red", add = T)
plot(alb2, border = "gray", col = "red", add = T)
plot(mne2, border = "gray", col = "red", add = T)
plot(bih, border = "gray", col = "red", add = T)
plot(country_shapes_nuts0_all_crop, col = NA, add = T)
points(cbind(7.843,	48.016), lwd = 2, col = "green", cex = 2.6)
points(cbind(8.652,	49.411), lwd = 2, col = "orange", cex = 2.6)
points(cbind(16.837,	39.473), lwd = 2, col = "blue", cex = 2.6)
dev.off()

# temperature data----------------------------------------
temp_14d_21C <- raster("output/zika_mean_map_14_21.grd")
temp_14d_21C <- crop(temp_14d_21C, cropping_info)
country_shapes_nuts0_all_crop <- crop(country_shapes_nuts0_all, cropping_info)

testst <- lapply(2007:2016, function(x) stack(paste("G:/NeuAll/Research_projects/Extract_E_OBS_gridded_dataset/output/tg_0.25deg_reg_v14.0_europe_", 
                                                    x, ".grd", sep = "")))
testst <- lapply(2007:2016, function(x) stack(paste("C:/Users/RenkeLuehken/Google Drive/Research_projects/Extract_E_OBS_gridded_dataset/output/tg_0.25deg_reg_v14.0_europe_", 
                                                    x, ".grd", sep = "")))
testst2 <- stack(testst)
testst3 <- crop(testst2, cropping_info)
testst3[[1]][is.na((testst3[[1]]))] <- 240

#temp_14d_21C_crop <- crop(temp_14d_21C, cropping_info)

temp_14d_21C_creece <- crop(temp_14d_21C, country_shapes_nuts0_all)
temp_14d_21C[is.na((temp_14d_21C))] <- 240
temp_14d_21C_creece2 <- mask(temp_14d_21C_creece,country_shapes_nuts0_all)

clip<-function(raster,shape) {
  cr <- crop(raster, extent(shape), snap="out")                    
  fr <- rasterize(shape, cr, getCover = T)   
  lr <- mask(x=cr, mask=fr)
}


temp_14d_21C_mask_all <- mask(temp_14d_21C, country_shapes_nuts0_all_crop)
eb <- stack(temp_14d_21C_mask_all, testst3[[1]])
temp_14d_21C_mask_all <- max(eb)
temp_14d_21C_mask_positive <- mask(temp_14d_21C, positive_nuts_crop)

# NAs to zero
temp_14d_21C_mask_all2 <- reclassify(temp_14d_21C_mask_all,
                                         cbind(NA, 0))
temp_14d_21C_mask_positive2 <- reclassify(temp_14d_21C_mask_positive,
                                         cbind(NA, 0))

m <- c(-Inf,0,0,
       0, 30, 1,
       30, 60, 2,
       60, 90, 3,
       90, 120, 4,
       120, 160, 5,
       160, 250, 6)

rclmat <- matrix(m, ncol=3, byrow=TRUE)

temp_14d_21C_mask_all3 <- reclassify(temp_14d_21C_mask_all2, 
                                         rclmat)
temp_14d_21C_mask_positive3 <- reclassify(temp_14d_21C_mask_positive2, 
                                     rclmat)

rbPal <- colorRampPalette(c("yellow","red","purple"))

png(file = "figs/transmission_period.png",
    width = 7, height = 4.2, units = 'in', res = 1000)
par(mar=c(0,0,0,0))
plot(country_shapes_nuts0_all_crop, cex.axis=0.9, axes = F)
plot(temp_14d_21C_mask_all3, legend = F, add = T, col = c("white",
                                      rbPal(5), "gray"))
plot(country_shapes_nuts0_all_crop, add = T, col = NA)
legend(-10,49.7, legend = c("NA", "0 d",
                            ">0-30 d",
                            ">30-60 d",
                            ">60-90 d",
                            ">90-120 d",
                            ">120-160 d"),horiz = F, 
       cex = 1, bty = "n",
       fill = c("gray", "white", rbPal(5)))
points(cbind(7.843,	48.016), lwd = 2, 
       col = "green", bg = rbPal(5)[extract(temp_14d_21C_mask_all3, cbind(7.843,	48.016))], cex = 2.6, pch = 21)
points(cbind(8.652,	49.411), lwd = 2, 
       col = "orange", bg = rbPal(5)[extract(temp_14d_21C_mask_all3, cbind(8.652,	49.411))], cex = 2.6, pch = 21)
points(cbind(16.837,	39.473), lwd = 2, 
       col = "black", bg = rbPal(5)[extract(temp_14d_21C_mask_all3, cbind(16.837,	39.473))], cex = 2.6, pch = 21)
dev.off()



#positive_nuts_crop, temp_14d_21C
clip <- function(shape_dat, raster_dat){
  raster_dat_2 <- raster_dat
  fr <- rasterize(shape_dat, raster_dat_2, getCover = T)   
  raster_dat_2[fr==0] <- 0 
  raster_dat_3 <- reclassify(raster_dat_2,
             cbind(NA, 0))
}

temp_14d_21C_mask_reclass2 <- clip(positive_nuts_crop, temp_14d_21C_mask_all3)
alb5 <- clip(alb2, temp_14d_21C_mask_all3)
mne5 <- clip(mne2, temp_14d_21C_mask_all3)
bih5 <- clip(bih, temp_14d_21C_mask_all3)

eb <- stack(temp_14d_21C_mask_reclass2, alb5, mne5, bih5)
er <- max(eb)


png(file = "figs/merge_albopictus_distribution-transmission_period.png",
    width = 7, height = 4.2, units = 'in', res = 1000)
par(mar=c(0,0,0,0))
plot(country_shapes_nuts0_all_crop, cex.axis=0.9, axes = F)
plot(er, add = T, 
     col = c("white", rbPal(5), "gray"), legend = F)
#plot(alb5, legend = F, add = T, col = c("white",
#                                        rbPal(5)[1:3]))
#plot(mne5, legend = F, add = T, col = c("white",
#                                        rbPal(5)[1:3]))
#plot(bih5, legend = F, add = T, col = c("white",
#                                        rbPal(5)[1:3]))
plot(positive_nuts_crop, border = "gray", add = T, col = NA)
plot(alb2, border = "gray", add = T, col = NA)
plot(mne2, border = "gray", add = T, col = NA)
plot(bih, border = "gray", add = T, col = NA)

points(cbind(8.652,	49.411), lwd = 2, col = "orange", cex = 1.5)
points(cbind(7.843,	48.016), lwd = 2, col = "green", cex = 1.5)
points(cbind(16.837,	39.473), lwd = 2, col = "blue", cex = 1.5)
plot(country_shapes_nuts0_all_crop, add = T, col = NA)
legend(-10,49.7, legend = c("NA", "0 d",
                             ">0-30 d",
                             ">30-60 d",
                             ">60-90 d",
                             ">90-120 d",
                             ">120-160 d"),horiz = F, 
       cex = 1, bty = "n",
       fill = c("gray", "white", rbPal(5)))
points(cbind(7.843,	48.016), lwd = 2, 
       col = "green", bg = rbPal(5)[extract(temp_14d_21C_mask_all3, cbind(7.843,	48.016))], cex = 2.6, pch = 21)
points(cbind(8.652,	49.411), lwd = 2, 
       col = "orange", bg = rbPal(5)[extract(temp_14d_21C_mask_all3, cbind(8.652,	49.411))], cex = 2.6, pch = 21)
points(cbind(16.837,	39.473), lwd = 2, 
       col = "black", bg = rbPal(5)[extract(temp_14d_21C_mask_all3, cbind(16.837,	39.473))], cex = 2.6, pch = 21)
dev.off()