# Camera Trap Species Distribution Model Testing

# Load packages
library(ctmm)  # continuous-time movement modeling
library(ggplot2)  # figures

# Import data
# Data for 7 large mammal species in Washington state
# Bassing, Sarah; DeVivo, Melia; Ganz, Taylor; Kertson, Brian; Prugh, Laura; Roussin, Trent; 
# Satterfield, Lauren; Windell, Rebecca; Wirsing, Aaron; Gardner, Beth. (2022).
# Data from: Are we telling the same story? [Dataset]. Dryad.
# https://doi.org/10.5061/dryad.g4f4qrfsv
# Related publication: Bassing et al. (2022). https://doi.org/10.1002/eap.2745
## Raw data provided directly from authors
load("data/bassing_etal_2022_data/all_spp_camtrap.RData")  # camera trap (CT) all species
load("data/bassing_etal_2022_data/all_spp_tracks.RData")  # telemetry all species
dem30 <- raster::raster("data/bassing_etal_2022_data/spatial data/DEM_30m.tif")  # SRTM digital elevation model, 30m res
roads <- raster::raster("data/bassing_etal_2022_data/spatial data/road_density_1km.tif")  # total length of roads per 1 km^2, 1000m res
slope <- raster::raster("data/bassing_etal_2022_data/spatial data/slope_aspect.tif")  # 0.00027 degree slope derived from DEM, 30m res
forest18 <- raster::raster("data/bassing_etal_2022_data/spatial data/perc_forestmix_2018.tif")  # % mixed forest w/in 250m radius of observation, 2018
forest19 <- raster::raster("data/bassing_etal_2022_data/spatial data/perc_forestmix_2019.tif")  # % mixed forest w/in 250m radius of observation, 2019
shrub18 <- raster::raster("data/bassing_etal_2022_data/spatial data/perc_shrub_2018.tif")  # % xeric shrub w/in 250m radius of observation, 2018
shrub19 <- raster::raster("data/bassing_etal_2022_data/spatial data/perc_shrub_2019.tif")  # % xeric shrub w/in 250m radius of observation, 2019
grass18 <- raster::raster("data/bassing_etal_2022_data/spatial data/perc_grass_2018.tif")  # % xeric grass w/in 250m radius of observation, 2018
grass19 <- raster::raster("data/bassing_etal_2022_data/spatial data/perc_grass_2019.tif")  # % xeric grass w/in 250m radius of observation, 2019

# NOTE: Authors obtained road density and landcover habitat covariates from Cascadia Biodiversity Watch TerrAdapt: Cascadia tool (30m res)
## Road density calculated as total road length per 1 km (incl. highways, residential roads, service roads)
## Categorical landcover (19 classes) reclassified into 6 landcover classes (forest, xeric shrub, xeric grass, mesic grass, developed, water)
## Used a moving window analysis to calculate the % of each landcover class w/in 250 m radius of each observation (CT observation?)
## (incl. % mixed forest, % xeric grass, % xeric shrub in their analyses because they made up the bulk of the classes in the study areas
## Standardized all habitat covariate data (centered on 0, SD = 1)


# Data wrangling ----

## Rasters ----

## For full raster data cleaning and wrangling, see `population_rsf.R`

# Named list of raster layers (read all layers into memory)
# save(R, file = "data/bassing_etal_2022_data/habitat_vars_rasterlist.rda")
load(file = "data/bassing_etal_2022_data/habitat_vars_rasterlist.rda")

# Load rasters with NAs replaced by 0s
# save(R, file = "data/bassing_etal_2022_data/habitat_vars_rasterlist_noNA.rda")
load(file = "data/bassing_etal_2022_data/habitat_vars_rasterlist_noNA.rda")

# Plot raster data
## Non-landcover covariates (DEM, roads, slope)
png(file = "figures/bassing_etal_2022/habitat_vars_nonlandcover.png", 
    width = 6000, height = 2000, res = 600)
par(mfrow = c(1,3))
raster::plot(R$DEM, main = "Elevation")
raster::plot(R$road_density, main = "Road Density")
raster::plot(R$slope, main = "Slope")
dev.off()

## Landcover covariates
png(file = "figures/bassing_etal_2022/habitat_vars_landcover.png", 
    width = 6000, height = 3400, res = 600)
par(mfrow = c(2,3), oma = c(1, 1, 1, 3) + 0.1)
raster::plot(R$percforest2018, main = "% Mixed Forest (2018)")
raster::plot(R$percshrub2018, main = "% Shrubland (2018)")
raster::plot(R$percgrass2018, main = "% Grassland (2018)")
raster::plot(R$percforest2019, main = "% Mixed Forest (2019)")
raster::plot(R$percshrub2019, main = "% Shrubland (2019)")
raster::plot(R$percgrass2019, main = "% Grassland (2019)")
dev.off()

# Load raster list for Year 1 (2018) for testing
# save(R_short, file = "data/bassing_etal_2022_data/habitat_vars_rasterlist_year1.rda")
load(file = "data/bassing_etal_2022_data/habitat_vars_rasterlist_year1.rda")

# Load standardized raster list
# save(R, file = "data/bassing_etal_2022_data/habitat_vars_rasterlist_standardized.rda")
load(file = "data/bassing_etal_2022_data/habitat_vars_rasterlist_standardized.rda")

# Load standardized raster list for Year 1 (2018)
# save(R_short, file = "data/bassing_etal_2022_data/habitat_vars_rasterlist_year1_standardized.rda")
load(file = "data/bassing_etal_2022_data/habitat_vars_rasterlist_year1_standardized.rda")  # standardized


## CT data ----

# TODO:
## Manually fix individual track colors to color-match overlapping species across seasons
## Filter/clean CT data by temporal autocorrelation of detection/non-detection (variogram)

# Load cleaned telemetry data for reference
load(file = "data/bassing_etal_2022_data/tracks_all_cleaned.rda")

# Make consistent naming scheme across telemetry and camera trap data
## Change colnames to match format needed for `telemetry`
colnames(all_spp_camtrap)
colnames(all_spp_camtrap)[2] <- "Lat"
colnames(all_spp_camtrap)[3] <- "Long"
colnames(all_spp_camtrap)[5] <- "timestamp"
colnames(all_spp_camtrap)[7] <- "Img_Time"
# Change species column to match tracking data naming scheme
all_spp_camtrap$Species <- tolower(all_spp_camtrap$Species)  # lowercase
all_spp_camtrap$Species[all_spp_camtrap$Species == "white-tailed deer"] <- "wtd"
all_spp_camtrap$Species[all_spp_camtrap$Species == "mule deer"] <- "md"
species <- sort(unique(all_spp_camtrap$Species))  # 7 species, alphabetical order

# Add indicator columns for study year to telemetry data
## Add an empty indicator column for each year
all_spp_camtrap$Year1 <- NA
all_spp_camtrap$Year2 <- NA

# Indicate the year of each location point (1 = yes, 0 = no, per column)
all_spp_camtrap$Year1[grepl("2018", all_spp_camtrap$Date)] <- TRUE
all_spp_camtrap$Year2[grepl("2018", all_spp_camtrap$Date)] <- FALSE
all_spp_camtrap$Year2[grepl("2019", all_spp_camtrap$Date)] <- TRUE
all_spp_camtrap$Year1[grepl("2019", all_spp_camtrap$Date)] <- FALSE
all_spp_camtrap$Year2[grepl("2020", all_spp_camtrap$Date)] <- TRUE  # assign 2020 data points to Year 2
all_spp_camtrap$Year1[grepl("2020", all_spp_camtrap$Date)] <- FALSE

# # Convert to factor
# all_spp_camtrap$Year1 <- as.factor(all_spp_camtrap$Year1)
# all_spp_camtrap$Year2 <- as.factor(all_spp_camtrap$Year2)

# Create column for study area to match telemetry data
all_spp_camtrap$StudyArea <- ifelse(grepl("NE", all_spp_camtrap$CameraLocation), "NE", "OK")

# Separate by study site for data cleaning
camtrap_NE <- all_spp_camtrap[all_spp_camtrap$StudyArea == "NE",]
camtrap_OK <- all_spp_camtrap[all_spp_camtrap$StudyArea == "OK",]

# Separate CT data into individual `telemetry` objects by species and season
## Separate summers and winters for each year
ct_seasons <- list(all_spp_camtrap[grepl("Summer", all_spp_camtrap$Season),], 
                   all_spp_camtrap[grepl("Winter", all_spp_camtrap$Season),])
## `telemetry` object for each species
# ct_all <- list()
# for (sp in species) {
#   ct_all[[paste(sp, "summer", sep = "_")]] <- as.telemetry(ct_seasons[[1]][ct_seasons[[1]]$Species == sp,],
#                                                            keep = c("Count", "Season", "StudyArea", "CameraLocation", 
#                                                                     "AF", "AM", "AU", "OS", "UNK"))
#   ct_all[[paste(sp, "winter", sep = "_")]] <- as.telemetry(ct_seasons[[2]][ct_seasons[[2]]$Species == sp,],
#                                                            keep = c("Count", "Season", "StudyArea", "CameraLocation", 
#                                                                     "AF", "AM", "AU", "OS", "UNK"))
# }
# ct_all <- ct_all[order(names(ct_all))]  # alphabetical order

# # Separate data by species, season, and year
# ct_all_seasons <- list()
# for (sp in species) {
#   for (season in unique(all_spp_camtrap$Season)) {
#     ct_all_seasons[[paste(sp, season, sep = "_")]] <- as.telemetry(
#       all_spp_camtrap[all_spp_camtrap$Season == season & all_spp_camtrap$Species == sp,],
#       keep = c("Count", "Season", "StudyArea", "CameraLocation", "AF", "AM", "AU", "OS", "UNK", "Year1", "Year2"))
#   }
# }
# ct_all_seasons <- ct_all_seasons[order(names(ct_all_seasons))]  # alphabetical order


## TEST for CT independent detections

# Northeast study area
diff_NE <- data.frame(diff = NULL, count = NULL, cam = NULL, species = NULL)
for (ne in unique(camtrap_NE$CameraLocation)) {
  
  # Subset data from each NE camera to extract detections
  camtrap_NE1 <- camtrap_NE[camtrap_NE$CameraLocation == ne,]
  camtrap_NE1 <- camtrap_NE1[order(camtrap_NE1$timestamp),]  # sort by time to avoid negative diffs
  diff <- data.frame(diff = c(1000,diff(camtrap_NE1$timestamp)), count = camtrap_NE1$Count, 
                      cam = camtrap_NE1$CameraLocation, species = camtrap_NE1$Species)
  # diff <- diff[diff$diff > 3,]  # remove < 3 seconds (within the same sequence)
  diff$minutes <- diff$diff/60  # convert to mins
  diff <- diff[diff$minutes <= 60,]  # less than or equal to 1 hours (in mins)
  diff_NE <- rbind(diff_NE, diff)
}
range(diff_NE$minutes)

par(mfrow = c(2,1))
for (sp in species) {
  hist(diff_NE$minutes[diff_NE$species == sp], 
       breaks = seq(0, 60, by = 0.5),  # max(diff$minutes)/2000,  # divide by mins/day
       main = sp, xlab = "mins")
  plot(acf(diff_NE$minutes[diff_NE$species == sp], plot = FALSE, lag.max = 15), main = paste(sp, "NE"))
}  # ~2 mins should be sufficient for individual detection

# Okanogan study area
diff_OK <- data.frame(diff = NULL, count = NULL, cam = NULL, species = NULL)
for (ok in unique(camtrap_OK$CameraLocation)) {
  
  # Subset data from each OK camera to extract detections
  camtrap_OK1 <- camtrap_OK[camtrap_OK$CameraLocation == ok,]
  camtrap_OK1 <- camtrap_OK1[order(camtrap_OK1$timestamp),]  # sort by time to avoid negative diffs
  diff <- data.frame(diff = c(1000,diff(camtrap_OK1$timestamp)), count = camtrap_OK1$Count, 
                     cam = camtrap_OK1$CameraLocation, species = camtrap_OK1$Species)
  # diff <- diff[diff$diff > 3,]  # remove < 3 seconds (within the same sequence)
  diff$minutes <- diff$diff/60  # convert to mins
  diff <- diff[diff$minutes <= 30,]  # less than or equal to 1.5 hours (in mins)
  diff_OK <- rbind(diff_OK, diff)
}
range(diff_OK$minutes)  

par(mfrow = c(2,1))
for (sp in species) {
  hist(diff_OK$minutes[diff_OK$species == sp], 
       breaks = seq(0, 30, by = 0.5),  # max(diff$minutes)/2000,  # divide by mins/day
       main = sp, xlab = "mins")
  plot(acf(diff_OK$minutes[diff_OK$species == sp], plot = FALSE, lag.max = 15), main = paste(sp, "OK"))
}  # ~2 mins should be sufficient for individual detection


# Separate by species and season
ct_all <- list()
for (sp in species) {
  ct_all[[paste(sp, "summer", sep = "_")]] <- ct_seasons[[1]][ct_seasons[[1]]$Species == sp,]
  ct_all[[paste(sp, "winter", sep = "_")]] <- ct_seasons[[2]][ct_seasons[[2]]$Species == sp,]
}
ct_all <- ct_all[order(names(ct_all))]  # alphabetical order

# Convert to telemetry objects
ct_all2 <- ct_all
ct_all2 <- lapply(ct_all2, function(x) { 
  as.telemetry(x, keep = c("Count", "Season", "StudyArea", "CameraLocation", 
                           "AF", "AM", "AU", "OS", "UNK", "Year1", "Year2"))})

# Match projections to tracking data
for (i in 1: length(ct_all2)) {
  projection(ct_all2[[i]]) <- projection(tracks_all[[i]])
}

# Thin data to independent detections (separated by 2 mins) for each species
for (i in 1:length(ct_all)) {
  ct_all[[i]] <- ct_all[[i]][order(ct_all[[i]]$timestamp),]  # ensure chronological order
  ct_all[[i]]$diff <- c(1000, diff(ct_all[[i]]$timestamp))
  ct_all[[i]] <- ct_all[[i]][ct_all[[i]]$diff > 120,]  # keep only entries >2 min apart
  
  # Convert to telemetry object
  ct_all[[i]] <- as.telemetry(ct_all[[i]], 
                              keep = c("Count", "Season", "StudyArea", "CameraLocation", 
                                       "AF", "AM", "AU", "OS", "UNK", "Year1", "Year2"))
}

# Match projections to tracking data
for (i in 1: length(ct_all)) {
  projection(ct_all[[i]]) <- projection(tracks_all[[i]])
}


### Preliminary visualization of CT data ----
# par(mfrow = c(2,1))  # Compare seasons

## Bobcat
png(file = "figures/bassing_etal_2022/bobcat/bobcat_ct.png",
    width = 4800, height = 6000, res = 600)
par(mfrow = c(2,1), mai = c(1,0.8,0.8,0.3), omi = c(0,0.15,0.15,0.15))  # Compare seasons
plot(ct_all$bobcat_summer, xlim = c(-150000,150000), ylim = c(-70000,70000), 
     R = dem30, col.R = "gray2", error = FALSE, pch = 16, cex = 0.65)
title(main = "Summers 2018, 2019", line = 0.5)
mtext("Bobcat Camera Trap", line = 2, cex = 1.5, font = 2)
plot(ct_all$bobcat_winter, xlim = c(-150000,150000), ylim = c(-70000,70000),
     R = dem30, col.R = "gray2", error = FALSE, pch = 16, cex = 0.65)
title(main = "Winter 2018/19, 2019/20", line = 0.5)
dev.off()

## Cougar
png(file = "figures/bassing_etal_2022/cougar/cougar_ct.png",
    width = 4800, height = 6000, res = 600)
par(mfrow = c(2,1), mai = c(1,0.8,0.8,0.3), omi = c(0,0.15,0.15,0.15))  # Compare seasons
plot(ct_all$cougar_summer, xlim = c(-150000,150000), ylim = c(-70000,70000), 
     R = dem30, col.R = "gray2", error = FALSE, pch = 16, cex = 0.65)
title(main = "Summers 2018, 2019", line = 0.5)
mtext("Cougar Camera Trap", line = 2, cex = 1.5, font = 2)
plot(ct_all$cougar_winter, xlim = c(-150000,150000), ylim = c(-70000,70000),
     R = dem30, col.R = "gray2", error = FALSE, pch = 16, cex = 0.65)
title(main = "Winter 2018/19", line = 0.5)
dev.off()

## Coyote
png(file = "figures/bassing_etal_2022/coyote/coyote_ct.png",
    width = 4800, height = 6000, res = 600)
par(mfrow = c(2,1), mai = c(1,0.8,0.8,0.3), omi = c(0,0.15,0.15,0.15))  # Compare seasons
plot(ct_all$coyote_summer, xlim = c(-150000,150000), ylim = c(-70000,70000), 
     R = dem30, col.R = "gray2", error = FALSE, pch = 16, cex = 0.65)
title(main = "Summers 2018, 2019", line = 0.5)
mtext("Coyote Camera Trap", line = 2, cex = 1.5, font = 2)
plot(ct_all$coyote_winter, xlim = c(-150000,150000), ylim = c(-70000,70000),
     R = dem30, col.R = "gray2", error = FALSE, pch = 16, cex = 0.65)
title(main = "Winter 2018/19", line = 0.5)
dev.off()

## Elk
png(file = "figures/bassing_etal_2022/elk/elk_ct.png",
    width = 4800, height = 6000, res = 600)
par(mfrow = c(2,1), mai = c(1,0.8,0.8,0.3), omi = c(0,0.15,0.15,0.15))  # Compare seasons
plot(ct_all$elk_summer, xlim = c(-150000,150000), ylim = c(-70000,70000), 
     R = dem30, col.R = "gray2", error = FALSE, pch = 16, cex = 0.65)
title(main = "Summers 2018, 2019", line = 0.5)
mtext("Elk Camera Trap", line = 2, cex = 1.5, font = 2)
plot(ct_all$elk_winter, xlim = c(-150000,150000), ylim = c(-70000,70000),
     R = dem30, col.R = "gray2", error = FALSE, pch = 16, cex = 0.65)
title(main = "Winter 2018/19", line = 0.5)
dev.off()

## Mule deer
png(file = "figures/bassing_etal_2022/mule_deer/md_ct.png",
    width = 4800, height = 6000, res = 600)
par(mfrow = c(2,1), mai = c(1,0.8,0.8,0.3), omi = c(0,0.15,0.15,0.15))  # Compare seasons
plot(ct_all$md_summer, xlim = c(-150000,150000), ylim = c(-70000,70000), 
     R = dem30, col.R = "gray2", error = FALSE, pch = 16, cex = 0.65)
title(main = "Summers 2018, 2019", line = 0.5)
mtext("Mule Deer Camera Trap", line = 2, cex = 1.5, font = 2)
plot(ct_all$md_winter, xlim = c(-150000,150000), ylim = c(-70000,70000),
     R = dem30, col.R = "gray2", error = FALSE, pch = 16, cex = 0.65)
title(main = "Winter 2018/19", line = 0.5)
dev.off()

## Wolf
png(file = "figures/bassing_etal_2022/wolf/wolf_ct.png",
    width = 4800, height = 6000, res = 600)
par(mfrow = c(2,1), mai = c(1,0.8,0.8,0.3), omi = c(0,0.15,0.15,0.15))  # Compare seasons
plot(ct_all$wolf_summer, xlim = c(-150000,150000), ylim = c(-70000,70000), 
     R = dem30, col.R = "gray2", error = FALSE, pch = 16, cex = 0.65)
title(main = "Summers 2018, 2019", line = 0.5)
mtext("Gray Wolf Camera Trap", line = 2, cex = 1.5, font = 2)
plot(ct_all$wolf_winter, xlim = c(-150000,150000), ylim = c(-70000,70000),
     R = dem30, col.R = "gray2", error = FALSE, pch = 16, cex = 0.65)
title(main = "Winter 2018/19", line = 0.5)
dev.off()

## White-tailed deer
png(file = "figures/bassing_etal_2022/wtd/wtd_ct.png",
    width = 4800, height = 6000, res = 600)
par(mfrow = c(2,1), mai = c(1,0.8,0.8,0.3), omi = c(0,0.15,0.15,0.15))  # Compare seasons
plot(ct_all$wtd_summer, xlim = c(-150000,150000), ylim = c(-70000,70000), 
     R = dem30, col.R = "gray2", error = FALSE, pch = 16, cex = 0.65)
title(main = "Summers 2018, 2019", line = 0.5)
mtext("White-Tailed Deer Camera Trap", line = 2, cex = 1.5, font = 2)
plot(ct_all$wtd_winter, xlim = c(-150000,150000), ylim = c(-70000,70000),
     R = dem30, col.R = "gray2", error = FALSE, pch = 16, cex = 0.65)
title(main = "Winter 2018/19", line = 0.5)
dev.off()


# Summer 2018, 2019 ----

## Bobcat ----

# Camera trap SDM

# Include year indicator from CT data for landcover covariates
formula <- as.formula("~ DEM + road_density + slope + Year1:percforest2018 + 
                      Year2:percforest2019 + Year1:percshrub2018 + Year2:percshrub2019 + 
                      Year1:percgrass2018 + Year2:percgrass2019")

tictoc::tic()
# Fit SDM
bobcat_sdm_s <- sdm.fit(ct_all$bobcat_summer, R = R, formula = formula, trace = TRUE) # integrator = "Riemann", 
tictoc::toc()  # 113.695 sec elapsed (~2 min)
summary(bobcat_sdm_s)
# $name
# [1] "inactive"
# 
# $DOF
# mean      area diffusion     speed 
# 0         0         0         0 
# 
# $CI
#                                         low          est      high
# percgrass2019 (1/percgrass2019)   -5.9937798 -0.113857236 5.7660653
# percgrass2018 (1/percgrass2018)   -5.8833193  0.115188009 6.1136953
# percshrub2019 (1/percshrub2019)   -4.4034623 -0.108988245 4.1854859
# percshrub2018 (1/percshrub2018)   -3.9309239  0.104752466 4.1404289
# percforest2019 (1/percforest2019) -0.7640081 -0.005001089 0.7540059
# percforest2018 (1/percforest2018) -0.9372944  0.017542879 0.9723802
# slope (1/slope)                   -0.2561488 -0.008036673 0.2400755
# road_density (1/road_density)     -0.4095359  0.036502735 0.4825414
# DEM (1/DEM)                       -0.2281425  0.006850720 0.2418439
# area (square meters)               0.0000000          Inf       Inf  <-- unable to get an area estimate

# Add telemetry iRSF results for individuals
load(file = "data/bassing_etal_2022_data/outputs/bobcat/RSF_bobcat_summer_OK_tel.rda")  # indiv iRSFs OK (telemetry)
load(file = "data/bassing_etal_2022_data/outputs/bobcat/RSF_bobcat_summer_NE_tel.rda")  # indiv iRSFs NE (telemetry)

# Extract betas and CIs
bobcat_sdm_s_dat <- data.frame(index = seq(1:9),
                               low = rev(summary(bobcat_sdm_s)$CI[,1])[-1], 
                               est = bobcat_sdm_s$beta,
                               high = rev(summary(bobcat_sdm_s)$CI[,3])[-1],
                               OK1 = c(RSF_bobcat_OKs[[1]]$beta, rep(NA,3)),
                               OK2 = c(RSF_bobcat_OKs[[2]]$beta, rep(NA,3)),
                               OK3 = c(RSF_bobcat_OKs[[3]]$beta, rep(NA,3)),
                               OK4 = c(RSF_bobcat_OKs[[4]]$beta, rep(NA,3)),
                               OK5 = c(RSF_bobcat_OKs[[5]]$beta, rep(NA,3)),
                               NE1 = c(RSF_bobcat_NEs[[1]]$beta, rep(NA,3)),
                               NE2 = c(RSF_bobcat_NEs[[2]]$beta, rep(NA,3)),
                               NE3 = c(RSF_bobcat_NEs[[3]]$beta, rep(NA,3)),
                               NE4 = c(RSF_bobcat_NEs[[4]]$beta, rep(NA,3)),
                               NE5 = c(RSF_bobcat_NEs[[5]]$beta, rep(NA,3)),
                               row.names = c("Elevation", "Road density", "Slope",
                                             "% Forest 2018", "% Forest 2019",
                                             "% Shrub 2018", "% Shrub 2019",
                                             "% Grass 2018", "% Grass 2019"))
COLOR <- rainbow(10)  # Color index for individuals

# Plot selection coefficients and CIs
ggplot(data = bobcat_sdm_s_dat) +
  geom_vline(xintercept = 0, color = "red2", linetype = "dashed") +
  geom_point(aes(x = OK1, y = index), col = COLOR[1], alpha = 0.75) +
  geom_point(aes(x = OK2, y = index), col = COLOR[2], alpha = 0.75) +
  geom_point(aes(x = OK3, y = index), col = COLOR[3], alpha = 0.75) +
  geom_point(aes(x = OK4, y = index), col = COLOR[4], alpha = 0.75) +
  geom_point(aes(x = OK5, y = index), col = COLOR[5], alpha = 0.75) +
  geom_point(aes(x = NE1, y = index), col = COLOR[6], alpha = 0.75) +
  geom_point(aes(x = NE2, y = index), col = COLOR[7], alpha = 0.75) +
  geom_point(aes(x = NE3, y = index), col = COLOR[8], alpha = 0.75) +
  geom_point(aes(x = NE4, y = index), col = COLOR[9], alpha = 0.75) +
  geom_point(aes(x = NE5, y = index), col = COLOR[10], alpha = 0.75) +
  geom_point(aes(x = est, y = index)) +
  geom_errorbar(aes(x = est, y = index, xmin = low, xmax = high)) +
  labs(x = "Selection coefficient (betas)", 
       title = "Bobcat Summer Resource Selection (both sites)") +
  scale_y_continuous(name = "", breaks = 1:9, labels = rownames(bobcat_sdm_s_dat)) +
  theme_bw()
ggsave(file = "figures/bassing_etal_2022/bobcat/bobcat_selection_summer.png")


# Try akde on the CT data
bobcat_ctrange_s <- akde(data = ct_all$bobcat_summer, bobcat_sdm, R = R,
                       grid = list(dr = c(100,100)))
## Can't fit due to DOF[area] == 0

# Model selection across covariates
bobcat_sdm.select_s <- sdm.select(ct_all$bobcat_summer, R = R, formula = formula, verbose = TRUE) # integrator = "Riemann", 
# Error in eigen(M) : 0 x 0 matrix
# In addition: Warning messages:
#   1: In min(t.lo, t.up, na.rm = TRUE) :
#   no non-missing arguments to min; returning Inf
# 2: In min(t.lo, t.up, na.rm = TRUE) :
#   no non-missing arguments to min; returning Inf
# Error in eigen(M) : 0 x 0 matrix
# Error in `[<-`(`*tmp*`, , i, value = Daprox[, 1]) : 
#   subscript out of bounds
# In addition: Warning message:
#   In min(t.lo, t.up, na.rm = TRUE) :
#   no non-missing arguments to min; returning Inf


### Year 1 Landcover only ----
# Model selection across covariates
bobcat_sdm.select_s <- sdm.select(ct_all$bobcat_summer, R = R_short, verbose = TRUE) # integrator = "Riemann", 
# Error in eigen(M) : 0 x 0 matrix
# In addition: Warning messages:
#   1: In min(t.lo, t.up, na.rm = TRUE) :
#   no non-missing arguments to min; returning Inf
# 2: In min(t.lo, t.up, na.rm = TRUE) :
#   no non-missing arguments to min; returning Inf
# Error in eigen(M) : 0 x 0 matrix
# Error in `[<-`(`*tmp*`, , i, value = Daprox[, 1]) : 
#   subscript out of bounds
# In addition: Warning message:
#   In min(t.lo, t.up, na.rm = TRUE) :
#   no non-missing arguments to min; returning Inf


# ## DEM layer only
# test <- raster::readAll(dem30)  # large object
# tictoc::tic()
# bobcat_sdm <- sdm.fit(ct_all$bobcat_summer, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
# tictoc::toc()  # 62.834 sec elapsed
# ## ERROR: in if (sqrt(sum((RESCALE - par)^2)) > DIM * .Machine$double.eps) { : missing value where TRUE/FALSE needed
# ### only for list of raster layers without `raster::readAll`
# ## WARNING: In cov.loglike(hess, grad) : MLE is near a boundary or optimizer failed.
# summary(bobcat_sdm)
# ## Weird output (due to warning?)
# 
# any(is.na(raster::values(dem30)))  # not due to any NAs in the raster values
# any(raster::values(dem30) == 0)  # there are zeros in the raster values
# which(raster::values(dem30) == 0)  # only 5 zeros
# 
# ## TEST
# # Try akde on the CT data
# bobcat_ctrange <- akde(data = ct_all$bobcat_summer, bobcat_sdm, R = list(dem30 = test), 
#                        grid = list(dr = c(100,100)))
# ## Can't plot due to DOF[area] == 0


### Okanogan site ----

# Subset indivs from Okanogan site
sites <- ct_all$bobcat_summer$StudyArea  # identify sites
bobcat_OKs <- ct_all$bobcat_summer[which(sites == "OK"),]  # subset CT data

# Fit SDM
bobcat_sdm_OKs <- sdm.fit(bobcat_OKs, R = R, formula = formula, trace = TRUE) # integrator = "Riemann", 
summary(bobcat_sdm_OKs)
# $name
# [1] "inactive"
# 
# $DOF
# mean      area diffusion     speed 
# 0         0         0         0 
# 
# $CI
#                                           low          est       high
# percgrass2019 (1/percgrass2019)   -14.1535122 -0.254215930 13.6450803
# percgrass2018 (1/percgrass2018)   -13.8914732  0.260107295 14.4116878
# percshrub2019 (1/percshrub2019)    -5.1826004  0.064232109  5.3110646
# percshrub2018 (1/percshrub2018)    -5.1235030 -0.051203842  5.0210953
# percforest2019 (1/percforest2019)  -1.0016905 -0.014876858  0.9719368
# percforest2018 (1/percforest2018)  -1.3147672  0.036147206  1.3870616
# slope (1/slope)                    -0.4434842  0.009655702  0.4627956
# road_density (1/road_density)      -1.0163786 -0.021505093  0.9733684
# DEM (1/DEM)                        -0.6009545 -0.020134034  0.5606864
# area (square meters)                0.0000000          Inf        Inf  <-- unable to get an area estimate

# Extract betas and CIs
bobcat_sdm_OKs_dat <- data.frame(index = seq(1:9),
                               low = rev(summary(bobcat_sdm_OKs)$CI[,1])[-1], 
                               est = bobcat_sdm_OKs$beta,
                               high = rev(summary(bobcat_sdm_OKs)$CI[,3])[-1],
                               OK1 = c(RSF_bobcat_OKs[[1]]$beta, rep(NA,3)),
                               OK2 = c(RSF_bobcat_OKs[[2]]$beta, rep(NA,3)),
                               OK3 = c(RSF_bobcat_OKs[[3]]$beta, rep(NA,3)),
                               OK4 = c(RSF_bobcat_OKs[[4]]$beta, rep(NA,3)),
                               OK5 = c(RSF_bobcat_OKs[[5]]$beta, rep(NA,3)),
                               row.names = c("Elevation", "Road density", "Slope",
                                             "% Forest 2018", "% Forest 2019",
                                             "% Shrub 2018", "% Shrub 2019",
                                             "% Grass 2018", "% Grass 2019"))
COLOR <- rainbow(10)  # Color index for individuals

# Plot selection coefficients and CIs
ggplot(data = bobcat_sdm_OKs_dat) +
  geom_vline(xintercept = 0, color = "red2", linetype = "dashed") +
  geom_point(aes(x = OK1, y = index), col = COLOR[1], alpha = 0.75) +
  geom_point(aes(x = OK2, y = index), col = COLOR[2], alpha = 0.75) +
  geom_point(aes(x = OK3, y = index), col = COLOR[3], alpha = 0.75) +
  geom_point(aes(x = OK4, y = index), col = COLOR[4], alpha = 0.75) +
  geom_point(aes(x = OK5, y = index), col = COLOR[5], alpha = 0.75) +
  geom_point(aes(x = est, y = index)) +
  geom_errorbar(aes(x = est, y = index, xmin = low, xmax = high)) +
  labs(x = "Selection coefficient (betas)", 
       title = "Bobcat Summer Resource Selection (Okanogan)") +
  scale_y_continuous(name = "", breaks = 1:9, labels = rownames(bobcat_sdm_OKs_dat)) +
  theme_bw()
ggsave(file = "figures/bassing_etal_2022/bobcat/bobcat_selection_summer_OK.png")


# Model selection across covariates
bobcat_sdm.select_OKs <- sdm.select(bobcat_OKs, R = R, formula = formula, verbose = TRUE) # integrator = "Riemann", 
# Error in eigen(M) : 0 x 0 matrix
# In addition: Warning messages:
#   1: In min(t.lo, t.up, na.rm = TRUE) :
#   no non-missing arguments to min; returning Inf
# 2: In min(t.lo, t.up, na.rm = TRUE) :
#   no non-missing arguments to min; returning Inf
# Error in eigen(M) : 0 x 0 matrix
# Error in `[<-`(`*tmp*`, , i, value = Daprox[, 1]) : 
#   subscript out of bounds
# In addition: Warning message:
#   In min(t.lo, t.up, na.rm = TRUE) :
#   no non-missing arguments to min; returning Inf


# ## DEM layer only
# test <- raster::readAll(dem30)  # large object
# bobcat_sdm_OKs <- sdm.fit(bobcat_OKs, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
# # 34 Warnings: In FUN(X[[i]], ...) : Objective function failure at c(=0, =7.13150492285974e-05)
# summary(bobcat_sdm_OKs)  # infinite area
# 
# # Range distribution
# bobcat_range_OKs <- akde(data = bobcat_OKs, CTMM = bobcat_sdm_OKs, R = list(dem30 = test))
# ## Warning: Fit object returned. DOF[area] = 0


### Northeast site ----

# Subset indivs from Northeast site
bobcat_NEs <- ct_all$bobcat_summer[which(sites == "NE"),]  # subset CT data
# saveRDS(bobcat_NEs, file = "data/bassing_etal_2022_data/bobcat_ct_NEs.rds")  # save for easy access (debug)

# Fit SDM
bobcat_sdm_NEs <- sdm.fit(bobcat_NEs, R = R, formula = formula, trace = TRUE) # integrator = "Riemann", 
# Warning message:
#   In cov.loglike(hess, grad) : MLE is near a boundary or optimizer failed.
summary(bobcat_sdm_NEs)  # output may not be reliable
# $name
# [1] "inactive"
# 
# $DOF
# mean      area diffusion     speed 
# 0         0         0         0 
# 
# $CI
#                                             low          est         high
# percgrass2019 (1/percgrass2019)     -486.488411 -0.152133740   486.184144
# percgrass2018 (1/percgrass2018)     -560.131320  0.093663458   560.318647
# percshrub2019 (1/percshrub2019)   -17549.086769 -0.030485810 17549.025797
# percshrub2018 (1/percshrub2018)     -882.605935 -0.005625307   882.594685
# percforest2019 (1/percforest2019)    -41.066813 -0.033450563    40.999912
# percforest2018 (1/percforest2018)    -66.836768  0.014172995    66.865114
# slope (1/slope)                       -6.652750 -0.020916158     6.610918
# road_density (1/road_density)         -5.125100  0.058257419     5.241615
# DEM (1/DEM)                           -8.885235  0.018795399     8.922826
# area (square meters)                   0.000000          Inf          Inf

# Extract betas and CIs
bobcat_sdm_NEs_dat <- data.frame(index = seq(1:9),
                               low = rev(summary(bobcat_sdm_NEs)$CI[,1])[-1], 
                               est = bobcat_sdm_NEs$beta,
                               high = rev(summary(bobcat_sdm_NEs)$CI[,3])[-1],
                               NE1 = c(RSF_bobcat_NEs[[1]]$beta, rep(NA,3)),
                               NE2 = c(RSF_bobcat_NEs[[2]]$beta, rep(NA,3)),
                               NE3 = c(RSF_bobcat_NEs[[3]]$beta, rep(NA,3)),
                               NE4 = c(RSF_bobcat_NEs[[4]]$beta, rep(NA,3)),
                               NE5 = c(RSF_bobcat_NEs[[5]]$beta, rep(NA,3)),
                               row.names = c("Elevation", "Road density", "Slope",
                                             "% Forest 2018", "% Forest 2019",
                                             "% Shrub 2018", "% Shrub 2019",
                                             "% Grass 2018", "% Grass 2019"))
COLOR <- rainbow(10)  # Color index for individuals

# Plot selection coefficients and CIs
ggplot(data = bobcat_sdm_NEs_dat) +
  geom_vline(xintercept = 0, color = "red2", linetype = "dashed") +
  geom_point(aes(x = NE1, y = index), col = COLOR[6], alpha = 0.75) +
  geom_point(aes(x = NE2, y = index), col = COLOR[7], alpha = 0.75) +
  geom_point(aes(x = NE3, y = index), col = COLOR[8], alpha = 0.75) +
  geom_point(aes(x = NE4, y = index), col = COLOR[9], alpha = 0.75) +
  geom_point(aes(x = NE5, y = index), col = COLOR[10], alpha = 0.75) +
  geom_point(aes(x = est, y = index)) +
  geom_errorbar(aes(x = est, y = index, xmin = low, xmax = high)) +
  labs(x = "Selection coefficient (betas)", 
       title = "Bobcat Summer Resource Selection (Northeast)") +
  scale_y_continuous(name = "", breaks = 1:9, labels = rownames(bobcat_sdm_NEs_dat)) +
  theme_bw()
ggsave(file = "figures/bassing_etal_2022/bobcat/bobcat_selection_summer_NE.png")


# Model selection across covariates
bobcat_sdm.select_NEs <- sdm.select(bobcat_NEs, R = R, formula = formula, verbose = TRUE) # integrator = "Riemann", 
# Error in eigen(M) : 0 x 0 matrix
# In addition: Warning messages:
#   1: In min(t.lo, t.up, na.rm = TRUE) :
#   no non-missing arguments to min; returning Inf
# 2: In min(t.lo, t.up, na.rm = TRUE) :
#   no non-missing arguments to min; returning Inf
# Error in eigen(M) : 0 x 0 matrix
# Error in `[<-`(`*tmp*`, , i, value = Daprox[, 1]) : 
#   subscript out of bounds
# In addition: Warning message:
#   In min(t.lo, t.up, na.rm = TRUE) :
#   no non-missing arguments to min; returning Inf


# ## DEM layer only
# test <- raster::readAll(dem30)  # large object
# tictoc::tic()
# bobcat_sdm_NEs <- sdm.fit(bobcat_NEs, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
# tictoc::toc()  # 64.034 sec elapsed
# # 47 Warnings: In FUN(X[[i]], ...) : Objective function failure at c(=0, =3.56658396845821e-05)
# summary(bobcat_sdm_NEs)  # infinite area
# 
# # Range distribution
# bobcat_range_NEs <- akde(data = bobcat_NEs, CTMM = bobcat_sdm_NEs, R = list(dem30 = test))
# ## Warning: Fit object returned. DOF[area] = 0


## Cougar ----

# Camera trap SDM

# Fit SDM
cougar_sdm <- sdm.fit(ct_all$cougar_summer, R = R, formula = formula, trace = TRUE) # integrator = "Riemann", 
summary(cougar_sdm)


# Try akde on the CT data
cougar_ctrange <- akde(data = ct_all$cougar_summer, cougar_sdm, R = R,
                       grid = list(dr = c(100,100)))
## Can't fit due to DOF[area] == 0

# Model selection across covariates
cougar_sdm.select <- sdm.select(ct_all$cougar_summer, R = R, formula = formula, verbose = TRUE) # integrator = "Riemann", 


# ### Year 1 Landcover only
# # Model selection across covariates
# cougar_sdm.select <- sdm.select(ct_all$cougar_summer, R = R_short, verbose = TRUE) # integrator = "Riemann", 


### Okanogan site ----

# Subset indivs from Okanogan site
sites <- ct_all$cougar_summer$StudyArea  # identify sites
cougar_OKs <- ct_all$cougar_summer[which(sites == "OK"),]  # subset CT data

# Fit SDM
cougar_sdm_OKs <- sdm.fit(cougar_OKs, R = R, formula = formula, trace = TRUE) # integrator = "Riemann", 
summary(cougar_sdm_OKs)


# Model selection across covariates
cougar_sdm.select_OKs <- sdm.select(cougar_OKs, R = R, formula = formula, verbose = TRUE) # integrator = "Riemann", 


### Northeast site ----

# Subset indivs from Northeast site
cougar_NEs <- ct_all$cougar_summer[which(sites == "NE"),]  # subset CT data

# Fit SDM
cougar_sdm_NEs <- sdm.fit(cougar_NEs, R = R, formula = formula, trace = TRUE) # integrator = "Riemann", 
# Warning message:
#   In cov.loglike(hess, grad) : MLE is near a boundary or optimizer failed.
summary(cougar_sdm_NEs)  # output may not be reliable


# Model selection across covariates
cougar_sdm.select_NEs <- sdm.select(cougar_NEs, R = R, formula = formula, verbose = TRUE) # integrator = "Riemann", 


## Coyote ----

# Camera trap SDM

## DEM layer only
# test <- raster::readAll(dem30)  # large object
tictoc::tic()
coyote_sdm <- sdm.fit(ct_all$coyote_summer, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 64.243 sec elapsed
# 3 Warnings: In FUN(X[[i]], ...) : Objective function failure at c(=0, =6.54485665384555e-05)
# Warning: In cov.loglike(hess, grad) : MLE is near a boundary or optimizer failed.
summary(coyote_sdm)  # infinite area

coyote_range_s <- akde(data = ct_all$coyote_summer, CTMM = coyote_sdm, R = list(dem30 = test))
## Warning: Fit object returned. DOF[area] = 0


## TEST: Separate study sites

# Okanogan
coyote_OKs <- ct_all$coyote_summer[ct_all$coyote_summer$StudyArea == "OK",]

## DEM layer only
# test <- raster::readAll(dem30)  # large object
tictoc::tic()
coyote_sdm_OKs <- sdm.fit(coyote_OKs, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 62.53 sec elapsed
# 5 Warnings: In FUN(X[[i]], ...) : Objective function failure at c(=0, =4.50894399464998e-05)
# Warning: In cov.loglike(hess, grad) : MLE is near a boundary or optimizer failed.
summary(coyote_sdm_OKs)  # infinite area

# Range distribution
coyote_range_OKs <- akde(data = coyote_OKs, CTMM = coyote_sdm_OKs, R = list(dem30 = test))
## Warning: Fit object returned. DOF[area] = 0


# Northeast
coyote_NEs <- ct_all$coyote_summer[ct_all$coyote_summer$StudyArea == "NE",]

## DEM layer only
# test <- raster::readAll(dem30)  # large object
tictoc::tic()
coyote_sdm_NEs <- sdm.fit(coyote_NEs, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 64.06 sec elapsed
# 2 Warnings: In FUN(X[[i]], ...) : Objective function failure at c(=0, =8.5117365384514e-05)
summary(coyote_sdm_NEs)  # infinite area

# Range distribution
coyote_range_NEs <- akde(data = coyote_NEs, CTMM = coyote_sdm_NEs, R = list(dem30 = test))
## Warning: Fit object returned. DOF[area] = 0


## Elk ----

# Camera trap SDM

## DEM layer only
# test <- raster::readAll(dem30)  # large object
tictoc::tic()
elk_sdm <- sdm.fit(ct_all$elk_summer, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 63.733 sec elapsed
# 2 Warnings: In FUN(X[[i]], ...) : Objective function failure at c(=0, =6.54485665384555e-05)
summary(elk_sdm)  # infinite area

elk_range_s <- akde(data = ct_all$elk_summer, CTMM = elk_sdm, R = list(dem30 = test))
## Warning: Fit object returned. DOF[area] = 0


## TEST: Separate study sites

# Okanogan
elk_OKs <- ct_all$elk_summer[ct_all$elk_summer$StudyArea == "OK",]

## DEM layer only
# test <- raster::readAll(dem30)  # large object
tictoc::tic()
elk_sdm_OKs <- sdm.fit(elk_OKs, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 64.493 sec elapsed
# 2 Warnings: In FUN(X[[i]], ...) : Objective function failure at c(=0, =6.54485665384555e-05)
summary(elk_sdm_OKs)  # infinite area

# Range distribution
elk_range_OKs <- akde(data = elk_OKs, CTMM = elk_sdm_OKs, R = list(dem30 = test))
## Warning: Fit object returned. DOF[area] = 0


# Northeast
elk_NEs <- ct_all$elk_summer[ct_all$elk_summer$StudyArea == "NE",]

## DEM layer only
# test <- raster::readAll(dem30)  # large object
tictoc::tic()
elk_sdm_NEs <- sdm.fit(elk_NEs, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 63.894 sec elapsed
# 9 Warnings: In FUN(X[[i]], ...) : Objective function failure at c(=0, =5.72450269275729e-05)
summary(elk_sdm_NEs)  # infinite area

# Range distribution
elk_range_NEs <- akde(data = elk_NEs, CTMM = elk_sdm_NEs, R = list(dem30 = test))
## Warning: Fit object returned. DOF[area] = 0


## Mule Deer ----

# Camera trap SDM

# Include year indicator from CT data for landcover covariates
formula <- as.formula("~ DEM + road_density + slope + Year1:percforest2018 + 
                      Year2:percforest2019 + Year1:percshrub2018 + Year2:percshrub2019 + 
                      Year1:percgrass2018 + Year2:percgrass2019")

tictoc::tic()
# Fit SDM
md_sdm_s <- sdm.fit(ct_all$md_summer, R = R, formula = formula, trace = TRUE) # integrator = "Riemann", 
tictoc::toc()  # 118.804 sec elapsed (~2 min)
summary(md_sdm_s)
# $name
# [1] "inactive"
# 
# $DOF
# mean      area diffusion     speed 
# 0         0         0         0 
# 
# $CI
# low           est       high
# Year2:percgrass2019 (1/Year2:percgrass2019)   -0.01171772  0.0321335207 0.07598476
# Year1:percgrass2018 (1/Year1:percgrass2018)   -0.04712778  0.0033481549 0.05382409
# Year2:percshrub2019 (1/Year2:percshrub2019)    0.10363893  0.1747821788 0.24592542
# Year1:percshrub2018 (1/Year1:percshrub2018)    0.01764151  0.0871443463 0.15664719
# Year2:percforest2019 (1/Year2:percforest2019) -0.04720925  0.0565079125 0.16022507
# Year1:percforest2018 (1/Year1:percforest2018) -0.08455017  0.0317811552 0.14811248
# slope (1/slope)                               -0.04228691  0.0006103077 0.04350752
# road_density (1/road_density)                 -0.21468955 -0.0790561618 0.05657723
# DEM (1/DEM)                                   -0.04823399  0.0186022020 0.08543839


# Add telemetry iRSF results for individuals
load(file = "data/bassing_etal_2022_data/outputs/mule_deer/RSF_md_summer_OK_tel.rda")  # indiv iRSFs OK (telemetry)
load(file = "data/bassing_etal_2022_data/outputs/mule_deer/RSF_md_summer_NE_tel.rda")  # indiv iRSFs NE (telemetry)

# Extract betas and CIs
md_sdm_s_dat <- data.frame(index = seq(1:9),
                               low = rev(summary(md_sdm_s)$CI[,1])[-1], 
                               est = md_sdm_s$beta,
                               high = rev(summary(md_sdm_s)$CI[,3])[-1],
                               # OK1 = c(RSF_md_OKs[[1]]$beta, rep(NA,3)),
                               # OK2 = c(RSF_md_OKs[[2]]$beta, rep(NA,3)),
                               # OK3 = c(RSF_md_OKs[[3]]$beta, rep(NA,3)),
                               # OK4 = c(RSF_md_OKs[[4]]$beta, rep(NA,3)),
                               # OK5 = c(RSF_md_OKs[[5]]$beta, rep(NA,3)),
                               # NE1 = c(RSF_md_NEs[[1]]$beta, rep(NA,3)),
                               # NE2 = c(RSF_md_NEs[[2]]$beta, rep(NA,3)),
                               # NE3 = c(RSF_md_NEs[[3]]$beta, rep(NA,3)),
                               # NE4 = c(RSF_md_NEs[[4]]$beta, rep(NA,3)),
                               # NE5 = c(RSF_md_NEs[[5]]$beta, rep(NA,3)),
                               row.names = c("Elevation", "Road density", "Slope",
                                             "% Forest 2018", "% Forest 2019",
                                             "% Shrub 2018", "% Shrub 2019",
                                             "% Grass 2018", "% Grass 2019"))
# COLOR <- rainbow(10)  # Color index for individuals

# Plot selection coefficients and CIs
ggplot(data = md_sdm_s_dat) +
  geom_vline(xintercept = 0, color = "red2", linetype = "dashed") +
  # geom_point(aes(x = OK1, y = index), col = COLOR[1], alpha = 0.75) +
  # geom_point(aes(x = OK2, y = index), col = COLOR[2], alpha = 0.75) +
  # geom_point(aes(x = OK3, y = index), col = COLOR[3], alpha = 0.75) +
  # geom_point(aes(x = OK4, y = index), col = COLOR[4], alpha = 0.75) +
  # geom_point(aes(x = OK5, y = index), col = COLOR[5], alpha = 0.75) +
  # geom_point(aes(x = NE1, y = index), col = COLOR[6], alpha = 0.75) +
  # geom_point(aes(x = NE2, y = index), col = COLOR[7], alpha = 0.75) +
  # geom_point(aes(x = NE3, y = index), col = COLOR[8], alpha = 0.75) +
  # geom_point(aes(x = NE4, y = index), col = COLOR[9], alpha = 0.75) +
  # geom_point(aes(x = NE5, y = index), col = COLOR[10], alpha = 0.75) +
  geom_point(aes(x = est, y = index)) +
  geom_errorbar(aes(x = est, y = index, xmin = low, xmax = high)) +
  labs(x = "Selection coefficient (betas)", 
       title = "Mule Deer Summer Resource Selection (both sites)") +
  scale_y_continuous(name = "", breaks = 1:9, labels = rownames(md_sdm_s_dat)) +
  theme_bw()
ggsave(file = "figures/bassing_etal_2022/mule_deer/md_SDM_selection_summer.png")


# Model selection across covariates
md_sdm.select_s <- sdm.select(ct_all$md_summer, R = R, formula = formula, verbose = TRUE) # integrator = "Riemann", 
save(md_sdm.select_s, file = "data/bassing_etal_2022_data/outputs/mule_deer/SDM_md_summer_full_select.rda")
load(file = "data/bassing_etal_2022_data/outputs/mule_deer/SDM_md_summer_full_select.rda")
summary(md_sdm.select_s)  # all models tested

# Compare AICc
AICdat <- data.frame(model = names(md_sdm.select_s),  # model formulas
                     AICc = sapply(md_sdm.select_s, function(x) {x$AICc}),  # AICc
                     delta_AICc = sapply(md_sdm.select_s, 
                                         function(x) {x$AICc-md_sdm.select_s[[1]]$AICc}),
                     row.names = NULL)
                     
summary(md_sdm.select_s[[1]])  # isotropic ~ Year2:percshrub2019
## landcover (shrub and grass), road density most strongly supported



### Year 1 Landcover only ----

# Year 1 landcover only
formula_y1 <- as.formula("~ DEM + road_density + slope + percforest2018 + percshrub2018 + percgrass2018")

tictoc::tic()
# Global SDM fit
md_sdm_s_y1 <- sdm.fit(ct_all$md_summer, R = R, formula = formula_y1, trace = TRUE) # integrator = "Riemann", 
tictoc::toc()  # 110.052 sec elapsed (~2 min)
summary(md_sdm_s_y1)
# $name
# [1] "inactive"
# 
# $DOF
# mean      area diffusion     speed 
# 0         0         0         0 
# 
# $CI
# low          est       high
# percgrass2018 (1/percgrass2018)   -0.02406285  0.013396148 0.05085514
# percshrub2018 (1/percshrub2018)    0.06193484  0.118472871 0.17501090
# percforest2018 (1/percforest2018) -0.04412956  0.036989131 0.11810782
# slope (1/slope)                   -0.05453416 -0.002279586 0.04997498
# road_density (1/road_density)     -0.23589867 -0.103654257 0.02859016
# DEM (1/DEM)                       -0.05586113  0.011209125 0.07827938
# area (square meters)               0.00000000          Inf        Inf

# Extract betas and CIs
md_sdm_s_y1dat <- data.frame(index = seq(1:6),
                             low = rev(summary(md_sdm_s_y1)$CI[,1])[-1], 
                             est = md_sdm_s_y1$beta,
                             high = rev(summary(md_sdm_s_y1)$CI[,3])[-1],
                             row.names = c("Elevation", "Road density", "Slope",
                                           "% Forest 2018", "% Shrub 2018", "% Grass 2018"))
# COLOR <- rainbow(10)  # Color index for individuals

# Plot selection coefficients and CIs
ggplot(data = md_sdm_s_y1dat) +
  geom_vline(xintercept = 0, color = "red2", linetype = "dashed") +
  geom_point(aes(x = est, y = index)) +
  geom_errorbar(aes(x = est, y = index, xmin = low, xmax = high)) +
  labs(x = "Selection coefficient (betas)", 
       title = "Mule Deer Summer Resource Selection (both sites)") +
  scale_y_continuous(name = "", breaks = 1:6, labels = rownames(md_sdm_s_y1dat)) +
  theme_bw()
ggsave(file = "figures/bassing_etal_2022/mule_deer/md_selection_summer_y1.png")


# TEST with non-thinned data
tictoc::tic()
# Fit SDM
md_sdm_s_full <- sdm.fit(ct_all2$md_summer, R = R_short, trace = TRUE) # integrator = "Riemann", 
tictoc::toc()  # 112.499 sec elapsed (~2 min)
summary(md_sdm_s_full)
# $name
# [1] "inactive"
# 
# $DOF
# mean      area diffusion     speed 
# 0         0         0         0 
# 
# $CI
# low          est         high
# percgrass2018 (1/percgrass2018)    0.003460498  0.010573398  0.017686298
# percshrub2018 (1/percshrub2018)    0.014672846  0.027679855  0.040686865
# percforest2018 (1/percforest2018) -0.048223013 -0.031487544 -0.014752076
# slope (1/slope)                   -0.038384926 -0.026872348 -0.015359770
# road_density (1/road_density)     -0.051568859 -0.022752636  0.006063587
# DEM (1/DEM)                       -0.006687261  0.002759768  0.012206797
# area (square meters)               0.000000000          Inf          Inf

## still using the "inactive model" and very low effect sizes?

# Extract betas and CIs
md_sdm_s_fulldat <- data.frame(index = seq(1:6),
                             low = rev(summary(md_sdm_s_full)$CI[,1])[-1], 
                             est = md_sdm_s_full$beta,
                             high = rev(summary(md_sdm_s_full)$CI[,3])[-1],
                             row.names = c("Elevation", "Road density", "Slope",
                                           "% Forest 2018", "% Shrub 2018", "% Grass 2018"))
# COLOR <- rainbow(10)  # Color index for individuals

# Plot selection coefficients and CIs
ggplot(data = md_sdm_s_fulldat) +
  geom_vline(xintercept = 0, color = "red2", linetype = "dashed") +
  geom_point(aes(x = est, y = index)) +
  geom_errorbar(aes(x = est, y = index, xmin = low, xmax = high)) +
  labs(x = "Selection coefficient (betas)", 
       title = "Mule Deer Summer Resource Selection (both sites)") +
  scale_y_continuous(name = "", breaks = 1:6, labels = rownames(md_sdm_s_fulldat)) +
  theme_bw()
ggsave(file = "figures/bassing_etal_2022/mule_deer/md_selection_summer_unthinned.png")


# Model selection across covariates
tictoc::tic()
md_sdm.select_s2 <- sdm.select(ct_all$md_summer, R = R, formula = formula_y1, verbose = TRUE) # integrator = "Riemann", 
tictoc::toc()
# save(md_sdm.select_s2, file = "data/bassing_etal_2022_data/outputs/mule_deer/SDM_md_summer_short_select.rda")
load(file = "data/bassing_etal_2022_data/outputs/mule_deer/SDM_md_summer_short_select.rda")
summary(md_sdm.select_s2)  # all models tested
## Landcover (mostly shrub) and road density most supported, maybe DEM

summary(md_sdm.select_s2[[1]])
# $name
# [1] "inactive"
# 
# $DOF
# mean      area diffusion     speed 
# 0         0         0         0 
# 
# $CI
# low         est       high
# percshrub2018 (1/percshrub2018)  0.06649716  0.10118307 0.13586897
# road_density (1/road_density)   -0.21451665 -0.09738326 0.01975013
# area (square meters)             0.00000000         Inf        Inf


### Okanogan site ----

# Subset indivs from Okanogan site
sites <- ct_all$md_summer$StudyArea  # identify sites
md_OKs <- ct_all$md_summer[which(sites == "OK"),]  # subset CT data

# Global SDM fit
md_sdm_OKs <- sdm.fit(md_OKs, R = R, formula = formula, trace = TRUE) # integrator = "Riemann", 
summary(md_sdm_OKs)

# Extract betas and CIs
md_sdm_OKs_dat <- data.frame(index = seq(1:9),
                                 low = rev(summary(md_sdm_OKs)$CI[,1])[-1], 
                                 est = md_sdm_OKs$beta,
                                 high = rev(summary(md_sdm_OKs)$CI[,3])[-1],
                                 # OK1 = c(RSF_md_OKs[[1]]$beta, rep(NA,3)),
                                 # OK2 = c(RSF_md_OKs[[2]]$beta, rep(NA,3)),
                                 # OK3 = c(RSF_md_OKs[[3]]$beta, rep(NA,3)),
                                 # OK4 = c(RSF_md_OKs[[4]]$beta, rep(NA,3)),
                                 # OK5 = c(RSF_md_OKs[[5]]$beta, rep(NA,3)),
                                 row.names = c("Elevation", "Road density", "Slope",
                                               "% Forest 2018", "% Forest 2019",
                                               "% Shrub 2018", "% Shrub 2019",
                                               "% Grass 2018", "% Grass 2019"))
COLOR <- rainbow(10)  # Color index for individuals

# Plot selection coefficients and CIs
ggplot(data = md_sdm_OKs_dat) +
  geom_vline(xintercept = 0, color = "red2", linetype = "dashed") +
  # geom_point(aes(x = OK1, y = index), col = COLOR[1], alpha = 0.75) +
  # geom_point(aes(x = OK2, y = index), col = COLOR[2], alpha = 0.75) +
  # geom_point(aes(x = OK3, y = index), col = COLOR[3], alpha = 0.75) +
  # geom_point(aes(x = OK4, y = index), col = COLOR[4], alpha = 0.75) +
  # geom_point(aes(x = OK5, y = index), col = COLOR[5], alpha = 0.75) +
  geom_point(aes(x = est, y = index)) +
  geom_errorbar(aes(x = est, y = index, xmin = low, xmax = high)) +
  labs(x = "Selection coefficient (betas)", 
       title = "Mule Deer Summer Resource Selection (Okanogan)") +
  scale_y_continuous(name = "", breaks = 1:9, labels = rownames(md_sdm_OKs_dat)) +
  theme_bw()
ggsave(file = "figures/bassing_etal_2022/mule_deer/md_selection_summer_OK.png")


# Model selection across covariates
md_sdm.select_OKs <- sdm.select(md_OKs, R = R, formula = formula, verbose = TRUE) # integrator = "Riemann", 
# save(md_sdm.select_OKs, file = "data/bassing_etal_2022_data/outputs/mule_deer/SDM_md_summer_full_select_OK.rda")
load(file = "data/bassing_etal_2022_data/outputs/mule_deer/SDM_md_summer_full_select_OK.rda")
summary(md_sdm.select_OKs)  # all models tested
## Landcover mostly (shrub and forest), road density and DEM
summary(md_sdm.select_OKs[[1]])  # best fit
# $name
# [1] "inactive"
# 
# $DOF
# mean      area diffusion     speed 
# 0         0         0         0 
# 
# $CI
#                                                   low        est       high
# Year2:percshrub2019 (1/Year2:percshrub2019) 0.04769169 0.07025434 0.09281699
# area (square meters)                        0.00000000        Inf        Inf


# Year 1 Landcover only  ## CURRENTLY RUNNING ##
## Global SDM fit
md_sdm_s_y1_OKs <- sdm.fit(md_OKs, R = R, formula = formula_y1, trace = TRUE) # integrator = "Riemann", 
summary(md_sdm_s_y1_OKs)
# $name
# [1] "inactive"
# 
# $DOF
# mean      area diffusion     speed 
# 0         0         0         0 
# 
# $CI
#                                           low           est       high
# percgrass2018 (1/percgrass2018)     -284.5286  0.0135144768   284.5556
# percshrub2018 (1/percshrub2018)     -521.8873  0.1330363517   522.1534
# percforest2018 (1/percforest2018)   -501.5533  0.0506793009   501.6547
# slope (1/slope)                   -17753.6132 -0.0001878932 17753.6128
# road_density (1/road_density)      -1099.3575 -0.0966792739  1099.1642
# DEM (1/DEM)                         -120.4115  0.0255097725   120.4625
# area (square meters)                   0.0000           Inf        Inf


## Model selection across covariates
md_sdm.select_OKs2 <- sdm.select(md_OKs, R = R, formula = formula_y1, verbose = TRUE) # integrator = "Riemann", 
# save(md_sdm.select_OKs2, file = "data/bassing_etal_2022_data/outputs/mule_deer/SDM_md_summer_short_select_OK.rda")
load(file = "data/bassing_etal_2022_data/outputs/mule_deer/SDM_md_summer_short_select_OK.rda")
summary(md_sdm.select_OKs2)  # all models tested
## Landcover mostly (shrub and forest), road density and DEM


### Northeast site ----

# Subset indivs from Northeast site
md_NEs <- ct_all$md_summer[which(sites == "NE"),]  # subset CT data
# saveRDS(md_NEs, file = "data/bassing_etal_2022_data/md_ct_NEs.rds")  # save for easy access (debug)

# Fit SDM
md_sdm_NEs <- sdm.fit(md_NEs, R = R, formula = formula, trace = TRUE) # integrator = "Riemann", 
# Warning message:
#   In cov.loglike(hess, grad) : MLE is near a boundary or optimizer failed.
# Error in if (reference == "auto") { : argument is of length zero
summary(md_sdm_NEs)  # output may not be reliable


# Extract betas and CIs
md_sdm_NEs_dat <- data.frame(index = seq(1:9),
                                 low = rev(summary(md_sdm_NEs)$CI[,1])[-1], 
                                 est = md_sdm_NEs$beta,
                                 high = rev(summary(md_sdm_NEs)$CI[,3])[-1],
                                 # NE1 = c(RSF_md_NEs[[1]]$beta, rep(NA,3)),
                                 # NE2 = c(RSF_md_NEs[[2]]$beta, rep(NA,3)),
                                 # NE3 = c(RSF_md_NEs[[3]]$beta, rep(NA,3)),
                                 # NE4 = c(RSF_md_NEs[[4]]$beta, rep(NA,3)),
                                 # NE5 = c(RSF_md_NEs[[5]]$beta, rep(NA,3)),
                                 row.names = c("Elevation", "Road density", "Slope",
                                               "% Forest 2018", "% Forest 2019",
                                               "% Shrub 2018", "% Shrub 2019",
                                               "% Grass 2018", "% Grass 2019"))
COLOR <- rainbow(10)  # Color index for individuals

# Plot selection coefficients and CIs
ggplot(data = md_sdm_NEs_dat) +
  geom_vline(xintercept = 0, color = "red2", linetype = "dashed") +
  # geom_point(aes(x = NE1, y = index), col = COLOR[6], alpha = 0.75) +
  # geom_point(aes(x = NE2, y = index), col = COLOR[7], alpha = 0.75) +
  # geom_point(aes(x = NE3, y = index), col = COLOR[8], alpha = 0.75) +
  # geom_point(aes(x = NE4, y = index), col = COLOR[9], alpha = 0.75) +
  # geom_point(aes(x = NE5, y = index), col = COLOR[10], alpha = 0.75) +
  # geom_point(aes(x = est, y = index)) +
  geom_errorbar(aes(x = est, y = index, xmin = low, xmax = high)) +
  labs(x = "Selection coefficient (betas)", 
       title = "Mule Deer Summer Resource Selection (Northeast)") +
  scale_y_continuous(name = "", breaks = 1:9, labels = rownames(md_sdm_NEs_dat)) +
  theme_bw()
ggsave(file = "figures/bassing_etal_2022/mule_deer/md_selection_summer_NE.png")


# Model selection across covariates 
md_sdm.select_NEs <- sdm.select(md_NEs, R = R, formula = formula, verbose = TRUE) # integrator = "Riemann", 
# save(md_sdm.select_NEs, file = "data/bassing_etal_2022_data/outputs/mule_deer/SDM_md_summer_full_select_NE.rda")
load(file = "data/bassing_etal_2022_data/outputs/mule_deer/SDM_md_summer_full_select_NE.rda")
summary(md_sdm.select_NEs)  # all models tested
## isotropic ~ 0 selected?? (followed by individual landcover and other rasters)
summary(md_sdm.select_NEs[[1]])  # best fit


# Year 1 Landcover only
## Global SDM fit
md_sdm_s_y1_NEs <- sdm.fit(md_NEs, R = R, formula = formula_y1, trace = TRUE) # integrator = "Riemann", 
summary(md_sdm_s_y1_NEs)
# $DOF
# mean      area diffusion     speed 
# 0         0         0         0 
# 
# $CI
# low         est      high
# percgrass2018 (1/percgrass2018)   -0.1740465  0.02263249 0.2193114
# percshrub2018 (1/percshrub2018)   -1.2368388  0.69812215 2.6330831
# percforest2018 (1/percforest2018) -0.5228426 -0.02283896 0.4771647
# slope (1/slope)                   -0.3578554  0.12242927 0.6027140
# road_density (1/road_density)     -0.7458810  0.04557521 0.8370314
# DEM (1/DEM)                       -0.7969510 -0.07213856 0.6526739
# area (square meters)               0.0000000         Inf       Inf


## Model selection across covariates  ## CURRENTLY RUNNING ##
md_sdm.select_NEs2 <- sdm.select(md_NEs, R = R, formula = formula_y1, verbose = TRUE) # integrator = "Riemann", 
# save(md_sdm.select_NEs2, file = "data/bassing_etal_2022_data/outputs/mule_deer/SDM_md_summer_short_select_NE.rda")
load(file = "data/bassing_etal_2022_data/outputs/mule_deer/SDM_md_summer_short_select_NE.rda")
summary(md_sdm.select_NEs2)  # all models tested
## isotropic ~ 0 selected?? (followed by individual landcover and other rasters)
summary(md_sdm.select_NEs2[[1]])


## Wolf ----

# Camera trap SDM

## DEM layer only
# test <- raster::readAll(dem30)  # large object
tictoc::tic()
wolf_sdm <- sdm.fit(ct_all$wolf_summer, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 65.628 sec elapsed
# 2 Warnings: In FUN(X[[i]], ...) : Objective function failure at c(=0, =5.06370756612765e-05)
summary(wolf_sdm)  # infinite area

wolf_range_s <- akde(data = ct_all$wolf_summer, CTMM = wolf_sdm, R = list(dem30 = test))
## Warning: Fit object returned. DOF[area] = 0


## TEST: Separate study sites

# Okanogan
wolf_OKs <- ct_all$wolf_summer[ct_all$wolf_summer$StudyArea == "OK",]

## DEM layer only
# test <- raster::readAll(dem30)  # large object
tictoc::tic()
wolf_sdm_OKs <- sdm.fit(wolf_OKs, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 65.695 sec elapsed
# Warning: In FUN(X[[i]], ...) : Objective function failure at c(=0, =0.000125127023115934)
summary(wolf_sdm_OKs)  # infinite area

# Range distribution
wolf_range_OKs <- akde(data = wolf_OKs, CTMM = wolf_sdm_OKs, R = list(dem30 = test))
## Warning: Fit object returned. DOF[area] = 0


# Northeast
wolf_NEs <- ct_all$wolf_summer[ct_all$wolf_summer$StudyArea == "NE",]

## DEM layer only
# test <- raster::readAll(dem30)  # large object
tictoc::tic()
wolf_sdm_NEs <- sdm.fit(wolf_NEs, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 66.994 sec elapsed
# 48 Warnings: In FUN(X[[i]], ...) : Objective function failure at c(=0, =1.93474438866856e-05)
summary(wolf_sdm_NEs)  # infinite area

# Range distribution
wolf_range_NEs <- akde(data = wolf_NEs, CTMM = wolf_sdm_NEs, R = list(dem30 = test))
## Warning: Fit object returned. DOF[area] = 0


## White-Tailed Deer ----

# Camera trap SDM

## DEM layer only
# test <- raster::readAll(dem30)  # large object
tictoc::tic()
wtd_sdm <- sdm.fit(ct_all$wtd_summer, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 65.201 sec elapsed
# 5 Warnings: In FUN(X[[i]], ...) : Objective function failure at c(=0, =7.75282135915763e-05)
summary(wtd_sdm)  # infinite area

wtd_range_s <- akde(data = ct_all$wtd_summer, CTMM = wtd_sdm, R = list(dem30 = test))
## Warning: Fit object returned. DOF[area] = 0


## TEST: Separate study sites

# Okanogan
wtd_OKs <- ct_all$wtd_summer[ct_all$wtd_summer$StudyArea == "OK",]

## DEM layer only
# test <- raster::readAll(dem30)  # large object
tictoc::tic()
wtd_sdm_OKs <- sdm.fit(wtd_OKs, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 64.111 sec elapsed
# 9 Warnings: In FUN(X[[i]], ...) : Objective function failure at c(=0, =7.14119995708735e-05)
summary(wtd_sdm_OKs)  # infinite area

# Range distribution
wtd_range_OKs <- akde(data = wtd_OKs, CTMM = wtd_sdm_OKs, R = list(dem30 = test))
## Warning: Fit object returned. DOF[area] = 0


# Northeast
wtd_NEs <- ct_all$wtd_summer[ct_all$wtd_summer$StudyArea == "NE",]

## DEM layer only
# test <- raster::readAll(dem30)  # large object
tictoc::tic()
wtd_sdm_NEs <- sdm.fit(wtd_NEs, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 64.966 sec elapsed
# 3 Warnings: In FUN(X[[i]], ...) : Objective function failure at c(=0, =7.79949590373665e-05)
summary(wtd_sdm_NEs)  # infinite area

# Range distribution
wtd_range_NEs <- akde(data = wtd_NEs, CTMM = wtd_sdm_NEs, R = list(dem30 = test))
## Warning: Fit object returned. DOF[area] = 0


# Winter 2018/19, 2019/20 ----

## Bobcat ----

# Camera trap SDM

# Fit SDM
bobcat_sdm_w <- sdm.fit(ct_all$bobcat_winter, R = R, formula = formula, trace = TRUE) # integrator = "Riemann", 
# Warning message:
#   In cov.loglike(hess, grad) : MLE is near a boundary or optimizer failed.
## Too little data?
summary(bobcat_sdm_w)
# $name
# [1] "inactive"
# 
# $DOF
# mean      area diffusion     speed 
# 0         0         0         0 
# 
# $CI
#                                    low est high
# percgrass2019 (1/percgrass2019)   -Inf NaN  Inf
# percgrass2018 (1/percgrass2018)   -Inf NaN  Inf
# percshrub2019 (1/percshrub2019)   -Inf NaN  Inf
# percshrub2018 (1/percshrub2018)   -Inf NaN  Inf
# percforest2019 (1/percforest2019) -Inf NaN  Inf
# percforest2018 (1/percforest2018) -Inf NaN  Inf
# slope (1/slope)                   -Inf NaN  Inf
# road_density (1/road_density)     -Inf NaN  Inf
# DEM (1/DEM)                       -Inf NaN  Inf
# area (square meters)                 0 Inf  Inf  <-- unable to get ANY parameter estimates


# Try akde on the CT data
bobcat_ctrange_w <- akde(data = ct_all$bobcat_winter, bobcat_sdm_w, R = R,
                         grid = list(dr = c(100,100)))
## Can't fit due to DOF[area] == 0

# Model selection across covariates
bobcat_sdm.select_w <- sdm.select(ct_all$bobcat_winter, R = R, formula = formula, verbose = TRUE) # integrator = "Riemann", 
# Error in eigen(M) : 0 x 0 matrix
# In addition: Warning messages:
#   1: In min(t.lo, t.up, na.rm = TRUE) :
#   no non-missing arguments to min; returning Inf
# 2: In min(t.lo, t.up, na.rm = TRUE) :
#   no non-missing arguments to min; returning Inf
# Error in eigen(M) : 0 x 0 matrix
# Error in `[<-`(`*tmp*`, , i, value = Daprox[, 1]) : 
#   subscript out of bounds
# In addition: Warning message:
#   In min(t.lo, t.up, na.rm = TRUE) :
#   no non-missing arguments to min; returning Inf

# ### Year 1 Landcover only
# # Model selection across covariates
# bobcat_sdm.select <- sdm.select(ct_all$bobcat_summer, R = R_short, verbose = TRUE) # integrator = "Riemann", 


### Okanogan site ----

# Subset indivs from Okanogan site
sites <- ct_all$bobcat_winter$StudyArea  # identify sites
bobcat_OKw <- ct_all$bobcat_winter[which(sites == "OK"),]  # subset CT data

# Fit SDM
bobcat_sdm_OKw <- sdm.fit(bobcat_OKw, R = R, formula = formula, trace = TRUE) # integrator = "Riemann",
# Warning message:
#   In cov.loglike(hess, grad) : MLE is near a boundary or optimizer failed.
summary(bobcat_sdm_OKw)
# $name
# [1] "inactive"
# 
# $DOF
# mean      area diffusion     speed 
# 0         0         0         0 
# 
# $CI
#                                    low est high
# percgrass2019 (1/percgrass2019)   -Inf NaN  Inf
# percgrass2018 (1/percgrass2018)   -Inf NaN  Inf
# percshrub2019 (1/percshrub2019)   -Inf NaN  Inf
# percshrub2018 (1/percshrub2018)   -Inf NaN  Inf
# percforest2019 (1/percforest2019) -Inf NaN  Inf
# percforest2018 (1/percforest2018) -Inf NaN  Inf
# slope (1/slope)                   -Inf NaN  Inf
# road_density (1/road_density)     -Inf NaN  Inf
# DEM (1/DEM)                       -Inf NaN  Inf
# area (square meters)                 0 Inf  Inf  <-- unable to get ANY parameter estimates

# Model selection across covariates
bobcat_sdm.select_OKw <- sdm.select(bobcat_OKw, R = R, formula = formula, verbose = TRUE) # integrator = "Riemann", 
# Error in eigen(M) : 0 x 0 matrix
# In addition: Warning messages:
#   1: In min(t.lo, t.up, na.rm = TRUE) :
#   no non-missing arguments to min; returning Inf
# 2: In min(t.lo, t.up, na.rm = TRUE) :
#   no non-missing arguments to min; returning Inf
# Error in eigen(M) : 0 x 0 matrix
# Error in `[<-`(`*tmp*`, , i, value = Daprox[, 1]) : 
#   subscript out of bounds
# In addition: Warning message:
#   In min(t.lo, t.up, na.rm = TRUE) :
#   no non-missing arguments to min; returning Inf

### Northeast site ----

# Subset indivs from Northeast site
bobcat_NEw <- ct_all$bobcat_winter[which(sites == "NE"),]  # subset CT data

# Fit SDM
bobcat_sdm_NEw <- sdm.fit(bobcat_NEw, R = R, formula = formula, trace = TRUE) # integrator = "Riemann",
# Warning message:
#   In cov.loglike(hess, grad) : MLE is near a boundary or optimizer failed.
summary(bobcat_sdm_NEw)  # output may not be reliable
# $name
# [1] "inactive"
# 
# $DOF
# mean      area diffusion     speed 
# 0         0         0         0 
# 
# $CI
# low est         high
# percgrass2019 (1/percgrass2019)   -5.588910e+08   0 5.588910e+08
# percgrass2018 (1/percgrass2018)            -Inf NaN          Inf
# percshrub2019 (1/percshrub2019)   -8.834909e+10   0 8.834909e+10
# percshrub2018 (1/percshrub2018)   -8.901987e+10   0 8.901987e+10
# percforest2019 (1/percforest2019) -1.734289e+06   0 1.734289e+06
# percforest2018 (1/percforest2018) -2.501898e+07   0 2.501898e+07
# slope (1/slope)                   -2.291356e+05   0 2.291356e+05
# road_density (1/road_density)     -2.562477e+05   0 2.562477e+05
# DEM (1/DEM)                       -2.643001e+05   0 2.643001e+05
# area (square meters)               0.000000e+00 Inf          Inf

# Model selection across covariates
bobcat_sdm.select_NEw <- sdm.select(bobcat_NEw, R = R, formula = formula, verbose = TRUE) # integrator = "Riemann", 
# Error in eigen(M) : 0 x 0 matrix
# In addition: Warning messages:
#   1: In min(t.lo, t.up, na.rm = TRUE) :
#   no non-missing arguments to min; returning Inf
# 2: In min(t.lo, t.up, na.rm = TRUE) :
#   no non-missing arguments to min; returning Inf
# Error in eigen(M) : 0 x 0 matrix
# Error in `[<-`(`*tmp*`, , i, value = Daprox[, 1]) : 
#   subscript out of bounds
# In addition: Warning message:
#   In min(t.lo, t.up, na.rm = TRUE) :
#   no non-missing arguments to min; returning Inf


## Cougar ----

# Camera trap SDM

## DEM layer only
# test <- raster::readAll(dem30)  # large object
tictoc::tic()
cougar_sdm_w <- sdm.fit(ct_all$cougar_winter, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 64.81 sec elapsed
# No errors, no area
summary(cougar_sdm_w)  # infinite area

cougar_range_w <- akde(data = ct_all$cougar_winter, CTMM = cougar_sdm_w, R = list(dem30 = test))
## Warning: Fit object returned. DOF[area] = 0


## TEST: Separate study sites

# Okanogan
cougar_OKw <- ct_all$cougar_winter[ct_all$cougar_winter$StudyArea == "OK",]

## DEM layer only
# test <- raster::readAll(dem30)  # large object
tictoc::tic()
cougar_sdm_OKw <- sdm.fit(cougar_OKw, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 63.848 sec elapsed
# 50 Warnings: In FUN(X[[i]], ...) : Objective function failure at c(=0, =2.91087194698241e-05)
summary(cougar_sdm_OKw)  # infinite area

# Range distribution
cougar_range_OKw <- akde(data = cougar_OKw, CTMM = cougar_sdm_OKw, R = list(dem30 = test))
## Warning: Fit object returned. DOF[area] = 0


# Northeast
cougar_NEw <- ct_all$cougar_winter[ct_all$cougar_winter$StudyArea == "NE",]

## DEM layer only
# test <- raster::readAll(dem30)  # large object
tictoc::tic()
cougar_sdm_NEw <- sdm.fit(cougar_NEw, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 63.699 sec elapsed
# 2 Warnings: In FUN(X[[i]], ...) : Objective function failure at c(=0, =7.18437142492834e-05)
summary(cougar_sdm_NEw)  # infinite area

# Range distribution
cougar_range_NEw <- akde(data = cougar_NEw, CTMM = cougar_sdm_NEw, R = list(dem30 = test))
## Warning: Fit object returned. DOF[area] = 0


## Coyote ----

# Camera trap SDM

## DEM layer only
# test <- raster::readAll(dem30)  # large object
tictoc::tic()
coyote_sdm_w <- sdm.fit(ct_all$coyote_winter, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 65.047 sec elapsed
# 10 Warnings: In FUN(X[[i]], ...) : Objective function failure at c(=0, =9.16331003966925e-05)
summary(coyote_sdm_w)  # infinite area

coyote_range_w <- akde(data = ct_all$coyote_winter, CTMM = coyote_sdm_w, R = list(dem30 = test))
## Warning: Fit object returned. DOF[area] = 0


## TEST: Separate study sites

# Okanogan
coyote_OKw <- ct_all$coyote_winter[ct_all$coyote_winter$StudyArea == "OK",]

## DEM layer only
# test <- raster::readAll(dem30)  # large object
tictoc::tic()
coyote_sdm_OKw <- sdm.fit(coyote_OKw, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 64.828 sec elapsed
# 5 Warnings: In FUN(X[[i]], ...) : Objective function failure at c(=0, =9.87077783419526e-05)
# Warning: In cov.loglike(hess, grad) : MLE is near a boundary or optimizer failed.
summary(coyote_sdm_OKw)  # infinite area

# Range distribution
coyote_range_OKw <- akde(data = coyote_OKw, CTMM = coyote_sdm_OKw, R = list(dem30 = test))
## Warning: Fit object returned. DOF[area] = 0


# Northeast
coyote_NEw <- ct_all$coyote_winter[ct_all$coyote_winter$StudyArea == "NE",]

## DEM layer only
# test <- raster::readAll(dem30)  # large object
tictoc::tic()
coyote_sdm_NEw <- sdm.fit(coyote_NEw, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 63.746 sec elapsed
# 8 Warnings: In FUN(X[[i]], ...) : Objective function failure at c(=0, =8.27789935574364e-05)
summary(coyote_sdm_NEw)  # infinite area

# Range distribution
coyote_range_NEw <- akde(data = coyote_NEw, CTMM = coyote_sdm_NEw, R = list(dem30 = test))
## Warning: Fit object returned. DOF[area] = 0


## Elk ----

# Camera trap SDM

## DEM layer only
# test <- raster::readAll(dem30)  # large object
tictoc::tic()
elk_sdm_w <- sdm.fit(ct_all$elk_winter, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 62.018 sec elapsed
# 2 Warnings: In FUN(X[[i]], ...) : Objective function failure at c(=0, =6.07743285798374e-05)
summary(elk_sdm_w)  # infinite area

elk_range_w <- akde(data = ct_all$elk_winter, CTMM = elk_sdm_w, R = list(dem30 = test))
## Warning: Fit object returned. DOF[area] = 0


## TEST: Separate study sites

# # Okanogan --> Very few detections (ignore in dataset)
# elk_OKw <- ct_all$elk_winter[ct_all$elk_winter$StudyArea == "OK",]
# 
# ## DEM layer only
# # test <- raster::readAll(dem30)  # large object
# tictoc::tic()
# elk_sdm_OKw <- sdm.fit(elk_OKw, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
# tictoc::toc()  # 62.514 sec elapsed
# # Warning: In rsf.fit(data, UD = UD, R = R, fomula = formula, integrated = integrated,: Raster resolution is Inf× coarse compared to home-range size.
# # Warning: In cov.loglike(hess, grad) : MLE is near a boundary or optimizer failed.
# summary(elk_sdm_OKw)  # infinite area
# 
# # Range distribution
# elk_range_OKw <- akde(data = elk_OKw, CTMM = elk_sdm_OKw, R = list(dem30 = test))
# ## Warning: Fit object returned. DOF[area] = 0


# Northeast
elk_NEw <- ct_all$elk_winter[ct_all$elk_winter$StudyArea == "NE",]

## DEM layer only
# test <- raster::readAll(dem30)  # large object
tictoc::tic()
elk_sdm_NEw <- sdm.fit(elk_NEw, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 65.727 sec elapsed
# 2 Warnings: In FUN(X[[i]], ...) : Objective function failure at c(=0, =6.12607036556539e-05)
summary(elk_sdm_NEw)  # infinite area

# Range distribution
elk_range_NEw <- akde(data = elk_NEw, CTMM = elk_sdm_NEw, R = list(dem30 = test))
## Warning: Fit object returned. DOF[area] = 0


## Mule Deer ----

# Camera trap SDM

## DEM layer only
# test <- raster::readAll(dem30)  # large object
tictoc::tic()
md_sdm_w <- sdm.fit(ct_all$md_winter, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 64.696 sec elapsed
# 7 Warnings: In FUN(X[[i]], ...) : Objective function failure at c(=0, =7.33587968562326e-05)
summary(md_sdm_w)  # infinite area

md_range_w <- akde(data = ct_all$md_winter, CTMM = md_sdm_w, R = list(dem30 = test))
## Warning: Fit object returned. DOF[area] = 0


## TEST: Separate study sites

# Okanogan 
md_OKw <- ct_all$md_winter[ct_all$md_winter$StudyArea == "OK",]

## DEM layer only
# test <- raster::readAll(dem30)  # large object
tictoc::tic()
md_sdm_OKw <- sdm.fit(md_OKw, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 63.911 sec elapsed
# 4 Warnings: In FUN(X[[i]], ...) : Objective function failure at c(=0, =7.47276354709733e-05)
summary(md_sdm_OKw)  # infinite area

# Range distribution
md_range_OKw <- akde(data = md_OKw, CTMM = md_sdm_OKw, R = list(dem30 = test))
## Warning: Fit object returned. DOF[area] = 0


# # Northeast  --> Very few detections (ignore in dataset)
# md_NEw <- ct_all$md_winter[ct_all$md_winter$StudyArea == "NE",]
# 
# ## DEM layer only
# # test <- raster::readAll(dem30)  # large object
# tictoc::tic()
# md_sdm_NEw <- sdm.fit(md_NEw, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
# tictoc::toc()  # 65.727 sec elapsed
# # 2 Warnings: In FUN(X[[i]], ...) : Objective function failure at c(=0, =6.12607036556539e-05)
# summary(md_sdm_NEw)  # infinite area
# 
# # Range distribution
# md_range_NEw <- akde(data = md_NEw, CTMM = md_sdm_NEw, R = list(dem30 = test))
# ## Warning: Fit object returned. DOF[area] = 0


## Wolf ----

# Camera trap SDM

## DEM layer only
# test <- raster::readAll(dem30)  # large object
tictoc::tic()
wolf_sdm_w <- sdm.fit(ct_all$wolf_winter, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 63.993 sec elapsed
# 2 Warnings: In FUN(X[[i]], ...) : Objective function failure at c(=0, =2.5612545108799e-05)
summary(wolf_sdm_w)  # infinite area

wolf_range_w <- akde(data = ct_all$wolf_winter, CTMM = wolf_sdm_w, R = list(dem30 = test))
## Warning: Fit object returned. DOF[area] = 0


## TEST: Separate study sites

# Okanogan 
wolf_OKw <- ct_all$wolf_winter[ct_all$wolf_winter$StudyArea == "OK",]

## DEM layer only
# test <- raster::readAll(dem30)  # large object
tictoc::tic()
wolf_sdm_OKw <- sdm.fit(wolf_OKw, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 63.035 sec elapsed
# Warning: In FUN(X[[i]], ...) : Objective function failure at c(=0, =7.47276354709733e-05)
summary(wolf_sdm_OKw)  # infinite area

# Range distribution
wolf_range_OKw <- akde(data = wolf_OKw, CTMM = wolf_sdm_OKw, R = list(dem30 = test))
## Warning: Fit object returned. DOF[area] = 0


# Northeast 
wolf_NEw <- ct_all$wolf_winter[ct_all$wolf_winter$StudyArea == "NE",]

## DEM layer only
# test <- raster::readAll(dem30)  # large object
tictoc::tic()
wolf_sdm_NEw <- sdm.fit(wolf_NEw, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 62.391 sec elapsed
# 2 Warnings: In FUN(X[[i]], ...) : Objective function failure at c(=0, =6.12607036556539e-05)
summary(wolf_sdm_NEw)  # infinite area

# Range distribution
wolf_range_NEw <- akde(data = wolf_NEw, CTMM = wolf_sdm_NEw, R = list(dem30 = test))
## Warning: Fit object returned. DOF[area] = 0


## White-Tailed Deer ----

# Camera trap SDM

## DEM layer only
# test <- raster::readAll(dem30)  # large object
tictoc::tic()
wtd_sdm_w <- sdm.fit(ct_all$wtd_winter, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 63.88 sec elapsed
# 2 Warnings: In FUN(X[[i]], ...) : Objective function failure at c(=0, =9.97820275539682e-05)
summary(wtd_sdm_w)  # infinite area

wtd_range_w <- akde(data = ct_all$wtd_winter, CTMM = wtd_sdm_w, R = list(dem30 = test))
## Warning: Fit object returned. DOF[area] = 0


## TEST: Separate study sites

# # Okanogan --> Very few detections (ignore in dataset)
# wtd_OKw <- ct_all$wtd_winter[ct_all$wtd_winter$StudyArea == "OK",]
# 
# ## DEM layer only
# # test <- raster::readAll(dem30)  # large object
# tictoc::tic()
# wtd_sdm_OKw <- sdm.fit(wtd_OKw, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
# tictoc::toc()  # 62.514 sec elapsed
# # Warning: In rsf.fit(data, UD = UD, R = R, fomula = formula, integrated = integrated,: Raster resolution is Inf× coarse compared to home-range size.
# # Warning: In cov.loglike(hess, grad) : MLE is near a boundary or optimizer failed.
# summary(wtd_sdm_OKw)  # infinite area
# 
# # Range distribution
# wtd_range_OKw <- akde(data = wtd_OKw, CTMM = wtd_sdm_OKw, R = list(dem30 = test))
# ## Warning: Fit object returned. DOF[area] = 0


# Northeast
wtd_NEw <- ct_all$wtd_winter[ct_all$wtd_winter$StudyArea == "NE",]

## DEM layer only
# test <- raster::readAll(dem30)  # large object
tictoc::tic()
wtd_sdm_NEw <- sdm.fit(wtd_NEw, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 65.647 sec elapsed
# 2 Warnings: In FUN(X[[i]], ...) : Objective function failure at c(=0, =9.89154099343271e-05)
summary(wtd_sdm_NEw)  # infinite area

# Range distribution
wtd_range_NEw <- akde(data = wtd_NEw, CTMM = wtd_sdm_NEw, R = list(dem30 = test))
## Warning: Fit object returned. DOF[area] = 0


