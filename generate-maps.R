# clear memory
rm(list = ls())

# load libraries
#library(dplyr)
#library(ggplot2)
#library(eurostat)
#library(raster)
#library(dismo)
#library(plyr)
#library(ROCR)
#library(gtools)
#library(spatstat)
#library(ecospat)
#library(gamm4)
#library(mgcv)  

library(maptools)
library(raster)

# set working directory
<<<<<<< HEAD
setwd("C:/Users/RenkeLuehken/Google Drive/Project/VectorCompetenceRiskMaps")
#setwd("G:/NeuAll/Project/VecCompRiskMap")

# read shapefiles of European countries----------------------------------------
# countries in Europe
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
cropping_info <- c(-9, 23.5, 35, 49.84)
country_shapes_nuts0_all_crop <- crop(country_shapes_nuts0_all, cropping_info)
positive_nuts_crop <- crop(positive_nuts, cropping_info)

png(file = "figs/distribution_aedes_albopictus.png",
    width = 7.2, height = 5, units = 'in', res = 1000)
par(mar=c(2.5,2.5,1.5,1.5))
plot(country_shapes_nuts0_all_crop, cex.axis=0.7, axes = T)
plot(positive_nuts_crop, border = "gray", col = "red", add = T)
plot(alb2, border = "gray", col = "red", add = T)
plot(mne2, border = "gray", col = "red", add = T)
plot(bih, border = "gray", col = "red", add = T)
plot(country_shapes_nuts0_all_crop, col = NA, add = T)
points(cbind(7.843,	48.016), lwd = 2, col = "green", cex = 2.8)
points(cbind(8.652,	49.411), lwd = 2, col = "orange", cex = 2.8)
points(cbind(16.837,	39.473), lwd = 2, col = "blue", cex = 2.8)
dev.off()

# temperature data----------------------------------------
temp_14d_21C <- raster("output/zika_mean_map_14_21.grd")
cropping_info <- c(-8.8, 21.5, 35.1, 49.5)
countrylist232 <- crop(countrylist23, cropping_info)
temp_14d_21C_crop <- crop(temp_14d_21C, cropping_info)

clip<-function(raster,shape) {
  a1_crop<-crop(raster,shape)
  mask(a1_crop,shape)}
=======
temp_14d_21C_crop <- mask(temp_14d_21C, nuts_0_crop)
temp_14d_21C_mask <- mask(temp_14d_21C, positive_nuts_crop)

# NAs to zero
temp_14d_21C_crop_reclass1 <- reclassify(temp_14d_21C_crop,
                                         cbind(NA, 0))
temp_14d_21C_mask_reclass1 <- reclassify(temp_14d_21C_mask,
                                         cbind(NA, 0))
>>>>>>> cf088ae9983db7a4f07e7771ce250b2fda5a34af

temp_14d_21C_crop2 <- clip(temp_14d_21C_crop, countrylist232)
temp_14d_21C_crop_reclass1 <- reclassify(temp_14d_21C_crop2,
                                         cbind(NA, 0))
m <- c(-Inf,0,0,
       0, 30, 1,
       30, 60, 2,
       60, 90, 3,
       90, 120, 4,
       120, 160, 5)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
<<<<<<< HEAD
temp_14d_21C_crop_reclass2 <- reclassify(temp_14d_21C_crop_reclass1, 
                                         rclmat, right=NA)

rbPal <- colorRampPalette(c("yellow","red","purple"))

png(file = "figs/transmission_period.png",
    width = 5.3, height = 3.8, units = 'in', res = 1000)
par(mar=c(2.5,2.5,1.5,1.5))
plot(countrylist232, cex.axis=1, axes = T)
plot(temp_14d_21C_crop_reclass2, legend = F, add = T, col = c("white",
                                                              rbPal(5)))
plot(countrylist232, add = T, col = NA)
=======
temp_14d_21C_crop_reclass2 <- reclassify(temp_14d_21C_crop_reclass1, rclmat, right=NA)
temp_14d_21C_mask_reclass2 <- reclassify(temp_14d_21C_mask_reclass1, rclmat, right=NA)

rbPal <- colorRampPalette(c("yellow","red"))

png(file = "figs/transmission_period.png",
    width = 7.2, height = 5.4, units = 'in', res = 1000)
plot(nuts_0_crop, cex.axis=0.7, axes = T)
plot(temp_14d_21C_crop_reclass2, legend = F, add = T, col = c("white",
                                      rbPal(5)))
plot(nuts_0_crop, add = T, col = NA)
points(cbind(7.843,	48.016), lwd = 2, col = "green", cex = 1.5)
points(cbind(8.652,	49.411), lwd = 2, col = "orange", cex = 1.5)
points(cbind(16.837,	39.473), lwd = 2, col = "blue", cex = 1.5)
>>>>>>> cf088ae9983db7a4f07e7771ce250b2fda5a34af
legend("bottom", legend = c("0 d",
                            ">0-30 d",
                            ">30-60 d",
                            ">60-90 d",
                            ">90-120 d",
                            ">120-160 d"),horiz = T, 
<<<<<<< HEAD
       cex = 0.5, bg = "white",
       fill = c("white", rbPal(6)))
points(cbind(7.843,	48.016), lwd = 2, 
       col = "green", bg = rbPal(5)[extract(temp_14d_21C_crop_reclass2, cbind(7.843,	48.016))], cex = 2.8, pch = 21)
points(cbind(8.652,	49.411), lwd = 2, 
       col = "orange", bg = rbPal(5)[extract(temp_14d_21C_crop_reclass2, cbind(8.652,	49.411))], cex = 2.8, pch = 21)
points(cbind(16.837,	39.473), lwd = 2, 
       col = "black", bg = rbPal(5)[extract(temp_14d_21C_crop_reclass2, cbind(16.837,	39.473))], cex = 2.8, pch = 21)
dev.off()


temp_14d_21C_albo <- clip(temp_14d_21C, positive_nuts_crop)

alb3 <- clip(temp_14d_21C, alb2)
mne3 <- clip(temp_14d_21C, mne2)
bih3 <- clip(temp_14d_21C, bih)

# NAs to zero

temp_14d_21C_mask_reclass2 <- reclassify(temp_14d_21C_albo,
                                         cbind(NA, 0))
alb4 <- reclassify(alb3,
                                         cbind(NA, 0))
mne4 <- reclassify(mne3,
                                         cbind(NA, 0))
bih4 <- reclassify(bih3,
                                         cbind(NA, 0))


temp_14d_21C_mask_reclass2 <- reclassify(temp_14d_21C_mask_reclass2, 
                                         rclmat, right=NA)
alb5 <- reclassify(alb4, 
                                         rclmat, right=NA)
mne5 <- reclassify(mne4, 
                                         rclmat, right=NA)
bih5 <- reclassify(bih4, 
                                         rclmat, right=NA)





#plot(g5, border = "blue", add = T,col = c("white",
#                                          rbPal(5)[1]), legend = F)
#plot(g5, add = T, border = "blue")

dev.off()


=======
       cex = 0.75,
       fill = c("white", rbPal(6)))
dev.off()

>>>>>>> cf088ae9983db7a4f07e7771ce250b2fda5a34af
png(file = "figs/merge_albopictus_distribution-transmission_period.png",
    width = 7.2, height = 5.4, units = 'in', res = 1000)
plot(nuts_0_crop, cex.axis=0.7, axes = T)
plot(temp_14d_21C_mask_reclass2, add = T, 
     col = c("white", rbPal(5)), legend = F)
<<<<<<< HEAD
plot(alb5, legend = F, add = T, col = c("white",
                                        rbPal(5)[1:3]))
plot(mne5, legend = F, add = T, col = c("white",
                                        rbPal(5)[1:3]))
plot(bih5, legend = F, add = T, col = c("white",
                                        rbPal(5)[1:3]))
plot(positive_nuts_crop, border = "gray", add = T, col = NA)
=======
plot(positive_nuts_crop, border = "gray", add = T, col = NA)
plot(nuts_0_crop, add = T, col = NA)
points(cbind(8.652,	49.411), lwd = 2, col = "orange", cex = 1.5)
points(cbind(7.843,	48.016), lwd = 2, col = "green", cex = 1.5)
points(cbind(16.837,	39.473), lwd = 2, col = "blue", cex = 1.5)
>>>>>>> cf088ae9983db7a4f07e7771ce250b2fda5a34af
legend("bottom", legend = c("0 d",
                             ">0-30 d",
                             ">30-60 d",
                             ">60-90 d",
                             ">90-120 d",
                             ">120-160 d"),horiz = T, 
       cex = 0.75,
<<<<<<< HEAD
       fill = c("white", rbPal(5)[3]))
plot(nuts_0_crop, add = T, col = NA)
plot(alb0, col = NA, add = T)
plot(mne0, col = NA, add = T)
plot(bih0, col = NA, add = T)
plot(srb0, col = NA, add = T)
points(cbind(7.843,	48.016), lwd = 2, 
       col = "green", bg = "yellow", cex = 1.5, pch = 21)
points(cbind(8.652,	49.411), lwd = 2, 
       col = "orange", bg = "yellow", cex = 1.5, pch = 21)
points(cbind(16.837,	39.473), lwd = 2, 
       col = "blue", bg = rbPal(5)[3], cex = 1.5, pch = 21)
dev.off() 

=======
       fill = c("white", rbPal(6)))
dev.off() 
>>>>>>> cf088ae9983db7a4f07e7771ce250b2fda5a34af
