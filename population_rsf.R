# Population-Level Resource Selection from Telemetry

# Load packages
library(ctmm)  # continuous-time movement modeling

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

# Named list of raster layers
## Read all raster layers into memory
R <- list(DEM = raster::readAll(dem30), 
          road_density = raster::readAll(roads), 
          slope = raster::readAll(slope),  # slope is a large file
          percforest2018 = raster::readAll(forest18), 
          percforest2019 = raster::readAll(forest19), 
          percshrub2018 = raster::readAll(shrub18), 
          percshrub2019 = raster::readAll(shrub19), 
          percgrass2018 = raster::readAll(grass18), 
          percgrass2019 = raster::readAll(grass19))
# save(R, file = "data/bassing_etal_2022_data/habitat_vars_rasterlist.rda")
load(file = "data/bassing_etal_2022_data/habitat_vars_rasterlist.rda")
# R_stack <- raster::stack(dem30, roads, slope, forest18, forest19, shrub18, shrub19, grass18, grass19)
## diff extents and projections (WGS84 vs. GRS80)

# Check for NAs
any(is.na(raster::getValues(R$DEM)))  # FALSE
any(is.na(raster::getValues(R$road_density)))  # TRUE
any(is.na(raster::getValues(R$slope)))  # TRUE
any(is.na(raster::getValues(R$percforest2018)))  # TRUE
any(is.na(raster::getValues(R$percforest2019)))  # TRUE
any(is.na(raster::getValues(R$percshrub2018)))  # TRUE
any(is.na(raster::getValues(R$percshrub2019)))  # TRUE
any(is.na(raster::getValues(R$percgrass2018)))  # TRUE
any(is.na(raster::getValues(R$percgrass2019)))  # TRUE

# Replace NAs with 0 for now
for (i in 2:length(R)) {
  R[[i]][is.na(R[[i]])] <- 0
}
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

# Calculate % change between 2018 and 2019 land cover
## (relative difference in raster values, % cells with differece > 5%)
## % Mixed forest
forestdiff <- abs(R$percforest2019 - R$percforest2018) / raster::maxValue(R$percforest2018, 
                                                                          R$percforest2019)
# How many cells changed >5%?
raster::ncell(forestdiff[forestdiff > 0.05]) / raster::ncell(forestdiff) * 100   # 9.65%

## % Xeric shrub
shrubdiff <- abs(R$percshrub2019 - R$percshrub2018) / raster::maxValue(R$percshrub2018, 
                                                                       R$percshrub2019)
# How many cells changed >5%?
raster::ncell(shrubdiff[shrubdiff > 0.05]) / raster::ncell(shrubdiff) * 100  # 3.49%

## % Xeric grass
grassdiff <- abs(R$percgrass2019 - R$percgrass2018) / raster::maxValue(R$percgrass2018, 
                                                                       R$percgrass2019)
# How many cells changed >5%?
raster::ncell(grassdiff[grassdiff > 0.05]) / raster::ncell(grassdiff) * 100   # 3.06%


# Extract rasters with only 2018 landcover for testing
R_short <- list(DEM = R[[1]], road_density = R[[2]], slope = R[[3]],
                percforest2018 = R[[4]], percshrub2018 = R[[6]], percgrass2018 = R[[8]])
# save(R_short, file = "data/bassing_etal_2022_data/habitat_vars_rasterlist_year1.rda")
load(file = "data/bassing_etal_2022_data/habitat_vars_rasterlist_year1.rda")  # not standardized

# Check for collinearity in landcover rasters for each year
## If all sum to 100% for each cell, will need to remove one as reference layer
landcover18 <- raster::stack(R$percforest2018, R$percshrub2018, R$percgrass2018)  # stack by year
landcover19 <- raster::stack(R$percforest2019, R$percshrub2019, R$percgrass2019)
sum_landcov18 <- raster::calc(landcover18, sum)  # check sums
sum_landcov19 <- raster::calc(landcover19, sum)
## Do not sum to 1 (100%)

dat_lc <- data.frame(forest18 = raster::getValues(R$percforest2018), 
                     shrub18 = raster::getValues(R$percshrub2018),
                     grass18 = raster::getValues(R$percgrass2018),
                     forest19 = raster::getValues(R$percforest2019), 
                     shrub19 = raster::getValues(R$percshrub2019),
                     grass19 = raster::getValues(R$percgrass2019))

# Pairwise correlation check
cor(dat_lc$forest18, dat_lc$shrub18)
cor(dat_lc$forest18, dat_lc$grass18)
cor(dat_lc$grass18, dat_lc$shrub18)
cor(dat_lc$forest19, dat_lc$shrub19)
cor(dat_lc$forest19, dat_lc$grass19)
cor(dat_lc$grass19, dat_lc$shrub19)
## Low pairwise correlation coefficients

# Check for collinearity between DEM and slope
cor(raster::getValues(R$DEM), raster::getValues(R$slope))  # acceptably low correlation

# Standardize habitat covariates for RSF formula
## Save mean and standard deviation for each covariate
MEANS <- lapply(R, function(x) { mean(raster::getValues(x))})
SDS <- lapply(R, function(x) {sd(raster::getValues(x))})

for (i in 1:length(R)) {
  raster::values(R[[i]]) <- (raster::values(R[[i]])-MEANS[[i]]) / SDS[[i]]  # standardize
}
# save(R, file = "data/bassing_etal_2022_data/habitat_vars_rasterlist_standardized.rda")
load(file = "data/bassing_etal_2022_data/habitat_vars_rasterlist_standardized.rda")

# Extract rasters with only 2018 landcover
R_short <- list(DEM = R[[1]], road_density = R[[2]], slope = R[[3]],
                percforest2018 = R[[4]], percshrub2018 = R[[6]], percgrass2018 = R[[8]])
# save(R_short, file = "data/bassing_etal_2022_data/habitat_vars_rasterlist_year1_standardized.rda")
load(file = "data/bassing_etal_2022_data/habitat_vars_rasterlist_year1_standardized.rda")  # standardized


## Telemetry ----

# Make consistent naming scheme across telemetry and camera trap data

names(all_spp_tracks)  # list of tracking data grouped by species and season

# Shorten deer names for consistent naming scheme: "species_season"
names(all_spp_tracks)[1] <- "md_summer"  # "mule_deer_summer" -> "md_summer"
names(all_spp_tracks)[2] <- "md_winter"
names(all_spp_tracks)[5] <- "wtd_summer"  # "whitetailed_deer_summer" -> "wtd_summer"
names(all_spp_tracks)[6] <- "wtd_winter"

all_spp_tracks <- all_spp_tracks[order(names(all_spp_tracks))]  # alphabetical order

# Change CT species column to match tracking data naming scheme
all_spp_camtrap$Species <- tolower(all_spp_camtrap$Species)  # lowercase
all_spp_camtrap$Species[all_spp_camtrap$Species == "white-tailed deer"] <- "wtd"
all_spp_camtrap$Species[all_spp_camtrap$Species == "mule deer"] <- "md"
species <- sort(unique(all_spp_camtrap$Species))  # 7 species, alphabetical order

# Add indicator columns for study year to telemetry data
for (i in 1:length(all_spp_tracks)) {
  
  # Add an empty indicator column for each year
  all_spp_tracks[[i]]$Year1 <- NA
  all_spp_tracks[[i]]$Year2 <- NA
  
  # Indicate the year of each location point
  all_spp_tracks[[i]]$Year1[grepl("2018", all_spp_tracks[[i]]$time)] <- TRUE
  all_spp_tracks[[i]]$Year2[grepl("2018", all_spp_tracks[[i]]$time)] <- FALSE
  all_spp_tracks[[i]]$Year2[grepl("2019", all_spp_tracks[[i]]$time)] <- TRUE
  all_spp_tracks[[i]]$Year1[grepl("2019", all_spp_tracks[[i]]$time)] <- FALSE
  all_spp_tracks[[i]]$Year2[grepl("2020", all_spp_tracks[[i]]$time)] <- TRUE  # assign 2020 data points to Year 2
  all_spp_tracks[[i]]$Year1[grepl("2020", all_spp_tracks[[i]]$time)] <- FALSE
  
  # # Convert to factor
  # all_spp_tracks[[i]]$Year1 <- as.factor(all_spp_tracks[[i]]$Year1)
  # all_spp_tracks[[i]]$Year2 <- as.factor(all_spp_tracks[[i]]$Year2)
}

# Convert to listed dataframes to `telemetry` objects
tracks_all <- lapply(all_spp_tracks, 
                     function(x) {as.telemetry(x, keep = c("Sex", "Season", "StudyArea", 
                                                           "Year1", "Year2"))})
for (i in 1:length(tracks_all)) {
  tracks_all[[i]] <- tracks_all[[i]][order(names(tracks_all[[i]]))]  # alphabetize IDs
}
## Some individuals were tracks across seasons, while others were only tracked for 1 or 2

# Match projections for species across seasons
for (i in 1:length(tracks_all)) {
  projection(tracks_all[[i]]) <- median(tracks_all[[1]])
}
# save(tracks_all, file = "data/bassing_etal_2022_data/tracks_all_cleaned.rda")
load(file = "data/bassing_etal_2022_data/tracks_all_cleaned.rda")

# Match individuals across seasons for plotting colors
## Combine data from summer and winter
tracks_species <- list()
for (i in seq(1, 13, by = 2)) { # for each species
  
  # Combine tracking data for each species
  tracks_species[[i]] <- rbind(all_spp_tracks[[i]], all_spp_tracks[[i+1]])
  tracks_species[[i]] <- tracks_species[[i]][order(tracks_species[[i]]$time),] 
}
tracks_species <- Filter(Negate(is.null), tracks_species)
names(tracks_species) <- species

# Convert to listed dataframes to `telemetry` objects
tracks_species <- lapply(tracks_species, 
                         function(x) {as.telemetry(x, keep = c("Sex", "Season", "StudyArea",
                                                               "Year1", "Year2"))})
# for (i in 1:length(tracks_all)) {
#   tracks_all[[i]] <- tracks_all[[i]][order(names(tracks_all[[i]]))]  # alphabetize IDs
# }
## Some individuals were tracks across seasons, while others were only tracked for 1 or 2

# Match projections for species across seasons
for (i in 1:length(tracks_species)) {
  projection(tracks_species[[i]]) <- projection(tracks_all[[1]])
}


### Preliminary visualization of tracking data ----
# par(mfrow = c(2,1), mai = c(1,0.8,0.8,0.3), omi = c(0,0.15,0.15,0.15))  # Compare seasons

# TODO:
## Fix individual track colors to color-match overlapping species across seasons

## Bobcat

png(file = "figures/bassing_etal_2022/bobcat/bobcat_tracks.png",
    width = 4800, height = 6000, res = 600)
par(mfrow = c(2,1), mai = c(1,0.8,0.8,0.3), omi = c(0,0.15,0.15,0.15))  # Compare seasons
plot(tracks_all$bobcat_summer, xlim = c(-150000,150000), ylim = c(-70000,70000), # xlim = c(-130000,110000), ylim = c(-45000,57000), 
     col = color(tracks_species$bobcat, by = "individual"),  # color-match indivs
     R = dem30, col.R = "gray2")
title(main = "Summers 2018, 2019", line = 0.5)
mtext("Bobcat Telemetry", line = 2, cex = 1.5, font = 2)  # bold
plot(tracks_all$bobcat_winter, xlim = c(-150000,150000), ylim = c(-70000,70000), # xlim = c(-130000,110000), ylim = c(-45000,57000),
     col = color(tracks_species$bobcat, by = "individual"),
     R = dem30, col.R = "gray2")
title(main = "Winter 2018/19, 2019/20", line = 0.5)
dev.off()

png(file = "figures/bassing_etal_2022/bobcat/bobcat_tracks_noerror.png",  # no error for clarity
    width = 4800, height = 6000, res = 600)
par(mfrow = c(2,1), mai = c(1,0.8,0.8,0.3), omi = c(0,0.15,0.15,0.15))  # Compare seasons
plot(tracks_all$bobcat_summer, xlim = c(-150000,150000), ylim = c(-70000,70000), # xlim = c(-130000,110000), ylim = c(-45000,57000), 
     col = colors_bobcat,  # color-match indivs
     R = dem30, col.R = "gray2", error = FALSE, pch = 16)
title(main = "Summers 2018, 2019", line = 0.5)
mtext("Bobcat Telemetry", line = 2, cex = 1.5, font = 2)  # bold
plot(tracks_all$bobcat_winter, xlim = c(-150000,150000), ylim = c(-70000,70000), # xlim = c(-130000,110000), ylim = c(-45000,57000),
     col = colors_bobcat,
     R = dem30, col.R = "gray2", error = FALSE, pch = 16)
title(main = "Winter 2018/19, 2019/20", line = 0.5)
dev.off()

## Cougar
png(file = "figures/bassing_etal_2022/cougar/cougar_tracks.png",
    width = 4800, height = 6000, res = 600)
par(mfrow = c(2,1), mai = c(1,0.8,0.8,0.3), omi = c(0,0.15,0.15,0.15))  # Compare seasons
plot(tracks_all$cougar_summer, xlim = c(-150000,150000), ylim = c(-70000,85000), 
     col = color(tracks_species$cougar, by = "individual"),  # color-match indivs
     R = dem30, col.R = "gray2")
title(main = "Summers 2018, 2019", line = 0.5)
mtext("Cougar Telemetry", line = 2, cex = 1.5, font = 2)
plot(tracks_all$cougar_winter, xlim = c(-150000,150000), ylim = c(-70000,85000), 
     col = color(tracks_species$cougar, by = "individual"),
     R = dem30, col.R = "gray2")
title(main = "Winter 2018/19, 2019/20", line = 0.5)
dev.off()

png(file = "figures/bassing_etal_2022/cougar/cougar_tracks_noerror.png",
    width = 4800, height = 6000, res = 600)
par(mfrow = c(2,1), mai = c(1,0.8,0.8,0.3), omi = c(0,0.15,0.15,0.15))  # Compare seasons
plot(tracks_all$cougar_summer, xlim = c(-150000,150000), ylim = c(-70000,85000), 
     col = color(tracks_species$cougar, by = "individual"),  # color-match indivs
     R = dem30, col.R = "gray2", error = FALSE, pch = 16, cex = 0.25)
title(main = "Summers 2018, 2019", line = 0.5)
mtext("Cougar Telemetry", line = 2, cex = 1.5, font = 2)
plot(tracks_all$cougar_winter, xlim = c(-150000,150000), ylim = c(-70000,85000), 
     col = color(tracks_species$cougar, by = "individual"),
     R = dem30, col.R = "gray2", error = FALSE, pch = 16, cex = 0.25)
title(main = "Winter 2018/19, 2019/20", line = 0.5)
dev.off()

## Coyote
png(file = "figures/bassing_etal_2022/coyote/coyote_tracks.png",
    width = 4800, height = 6000, res = 600)
par(mfrow = c(2,1), mai = c(1,0.8,0.8,0.3), omi = c(0,0.15,0.15,0.15))  # Compare seasons
plot(tracks_all$coyote_summer, xlim = c(-150000,150000), ylim = c(-70000,70000), # xlim = c(-140000,110000), ylim = c(-50000,70000), 
     col = color(tracks_species$coyote, by = "individual"),  # color-match indivs
     R = dem30, col.R = "gray2")
title(main = "Summers 2018, 2019", line = 0.5)
mtext("Coyote Telemetry", line = 2, cex = 1.5, font = 2)
plot(tracks_all$coyote_winter, xlim = c(-150000,150000), ylim = c(-70000,70000), # xlim = c(-140000,110000), ylim = c(-50000,70000),
     col = color(tracks_species$coyote, by = "individual"),
     R = dem30, col.R = "gray2")
title(main = "Winter 2018/19, 2019/20", line = 0.5)
dev.off()

png(file = "figures/bassing_etal_2022/coyote/coyote_tracks_noerror.png",
    width = 4800, height = 6000, res = 600)
par(mfrow = c(2,1), mai = c(1,0.8,0.8,0.3), omi = c(0,0.15,0.15,0.15))  # Compare seasons
plot(tracks_all$coyote_summer, xlim = c(-150000,150000), ylim = c(-70000,70000), 
     col = color(tracks_species$coyote, by = "individual"),  # color-match indivs
     R = dem30, col.R = "gray2", error = FALSE, pch = 16, cex = 0.25)
title(main = "Summers 2018, 2019", line = 0.5)
mtext("Coyote Telemetry", line = 2, cex = 1.5, font = 2)
plot(tracks_all$coyote_winter, xlim = c(-150000,150000), ylim = c(-70000,70000), 
     col = color(tracks_species$coyote, by = "individual"),
     R = dem30, col.R = "gray2", error = FALSE, pch = 16, cex = 0.25)
title(main = "Winter 2018/19, 2019/20", line = 0.5)
dev.off()

## Elk
png(file = "figures/bassing_etal_2022/elk/elk_tracks.png",
    width = 4800, height = 6000, res = 600)
par(mfrow = c(2,1), mai = c(1,0.8,0.8,0.3), omi = c(0,0.15,0.15,0.15))  # Compare seasons
plot(tracks_all$elk_summer, xlim = c(-150000,150000), ylim = c(-70000,70000), #xlim = c(-140000,110000), ylim = c(-50000,70000), 
     col = color(tracks_species$elk, by = "individual"),  # color-match indivs
     R = dem30, col.R = "gray2")
title(main = "Summers 2018, 2019", line = 0.5)
mtext("Elk Telemetry", line = 2, cex = 1.5, font = 2)
plot(tracks_all$elk_winter, xlim = c(-150000,150000), ylim = c(-70000,70000), #xlim = c(-140000,110000), ylim = c(-50000,70000),
     col = color(tracks_species$elk, by = "individual"),
     R = dem30, col.R = "gray2")
title(main = "Winter 2018/19, 2019/20", line = 0.5)
dev.off()

png(file = "figures/bassing_etal_2022/elk/elk_tracks_noerror.png",
    width = 4800, height = 6000, res = 600)
par(mfrow = c(2,1), mai = c(1,0.8,0.8,0.3), omi = c(0,0.15,0.15,0.15))  # Compare seasons
plot(tracks_all$elk_summer, xlim = c(-150000,150000), ylim = c(-70000,70000), 
     col = color(tracks_species$elk, by = "individual"),  # color-match indivs
     R = dem30, col.R = "gray2", error = FALSE, pch = 16, cex = 0.25)
title(main = "Summers 2018, 2019", line = 0.5)
mtext("Elk Telemetry", line = 2, cex = 1.5, font = 2)
plot(tracks_all$elk_winter, xlim = c(-150000,150000), ylim = c(-70000,70000), 
     col = color(tracks_species$elk, by = "individual"),
     R = dem30, col.R = "gray2", error = FALSE, pch = 16, cex = 0.25)
title(main = "Winter 2018/19, 2019/20", line = 0.5)
dev.off()

## Mule deer

## TEST:
### Mule deer used to test performance of `ctmm::color` for 100+ individuals with high overlap
### `tictoc` used to track computation time

png(file = "figures/bassing_etal_2022/mule_deer/md_tracks.png",
    width = 4800, height = 6000, res = 600)
par(mfrow = c(2,1), mai = c(1,0.8,0.8,0.3), omi = c(0,0.15,0.15,0.15))  # Compare seasons
plot(tracks_all$md_summer, xlim = c(-150000,150000), ylim = c(-70000,90000), #xlim = c(-150000,150000), ylim = c(-50000,80000), # xlim = c(-140000,110000), ylim = c(-50000,70000), 
     col = color(tracks_species$md, by = "individual"),  # color-match indivs
     R = dem30, col.R = "gray2")
title(main = "Summers 2018, 2019", line = 0.5)
mtext("Mule Deer Telemetry", line = 2, cex = 1.5, font = 2)
# tictoc::tic()
plot(tracks_all$md_winter, xlim = c(-150000,150000), ylim = c(-70000,90000), #xlim = c(-150000,150000), ylim = c(-50000,80000), # xlim = c(-140000,110000), ylim = c(-50000,70000),
     col = color(tracks_species$md, by = "individual"),
     R = dem30, col.R = "gray2")
title(main = "Winter 2018/19, 2019/20", line = 0.5)
dev.off()
# tictoc::toc()  # 62.28 sec elapsed

# For testing purposes (w/out raster layer):
tictoc::tic()
plot(tracks_all$md_winter, xlim = c(-150000,150000), ylim = c(-70000,90000), #xlim = c(-150000,150000), ylim = c(-50000,80000), # xlim = c(-140000,110000), ylim = c(-50000,70000),
     col = color(tracks_species$md, by = "individual"))
title(main = "Winter 2018/19, 2019/20", line = 0.5)
tictoc::toc()  # 13.65 sec elapsed

# For testing purposes (w/out raster layer), using IID KDEs:
tictoc::tic()
# IID_md <- lapply(tracks_species$md, ctmm.fit)
# KDE_md <- akde(tracks_species$md, IID_md, grid = list(dr = c(100,100)))  # VERY SLOW
# names(KDE_md) <- names(tracks_species$md)
# save(KDE_md, file = "data/bassing_etal_2022_data/outputs/mule_deer/IID_KDE_md.rda")
png(file = "figures/bassing_etal_2022/mule_deer/md_tracks_iid_kde_dem.png",
    width = 4800, height = 6000, res = 600)
par(mfrow = c(2,1), mai = c(1,0.8,0.8,0.3), omi = c(0,0.15,0.15,0.15))  # Compare seasons
plot(tracks_all$md_summer, xlim = c(-150000,150000), ylim = c(-70000,90000), #xlim = c(-150000,150000), ylim = c(-50000,80000), # xlim = c(-140000,110000), ylim = c(-50000,70000), 
     col = color(KDE_md, by = "individual"),  # is the IID KDE too constrained to separate the colors? Many overlapping tracks have similar colors
     R = dem30, col.R = "gray2")
title(main = "Summers 2018, 2019", line = 0.5)
mtext("Mule Deer Telemetry", line = 2, cex = 1.5, font = 2)
plot(tracks_all$md_winter, xlim = c(-150000,150000), ylim = c(-70000,90000), #xlim = c(-150000,150000), ylim = c(-50000,80000), # xlim = c(-140000,110000), ylim = c(-50000,70000),
     col = color(KDE_md, by = "individual"),
     R = dem30, col.R = "gray2")
title(main = "Winter 2018/19, 2019/20", line = 0.5)
dev.off()
tictoc::toc()  # 9541.241 sec elapsed, ~159 min, ~2.65 hour


png(file = "figures/bassing_etal_2022/mule_deer/md_tracks_noerror.png",
    width = 4800, height = 6000, res = 600)
par(mfrow = c(2,1), mai = c(1,0.8,0.8,0.3), omi = c(0,0.15,0.15,0.15))  # Compare seasons
plot(tracks_all$md_summer, xlim = c(-150000,150000), ylim = c(-70000,90000), #xlim = c(-150000,150000), ylim = c(-50000,80000), # xlim = c(-140000,110000), ylim = c(-50000,70000), 
     col = color(tracks_species$md, by = "individual"),  # color-match indivs
     R = dem30, col.R = "gray2", error = FALSE, pch = 16, cex = 0.25)
title(main = "Summers 2018, 2019", line = 0.5)
mtext("Mule Deer Telemetry", line = 2, cex = 1.5, font = 2)
plot(tracks_all$md_winter, xlim = c(-150000,150000), ylim = c(-70000,90000), #xlim = c(-150000,150000), ylim = c(-50000,80000), # xlim = c(-140000,110000), ylim = c(-50000,70000),
     col = color(tracks_species$md, by = "individual"),
     R = dem30, col.R = "gray2", error = FALSE, pch = 16, cex = 0.25)
title(main = "Winter 2018/19, 2019/20", line = 0.5)
dev.off()

## Wolf
png(file = "figures/bassing_etal_2022/wolf/wolf_tracks.png",
    width = 4800, height = 6000, res = 600)
par(mfrow = c(2,1), mai = c(1,0.8,0.8,0.3), omi = c(0,0.15,0.15,0.15))  # Compare seasons
plot(tracks_all$wolf_summer, xlim = c(-150000,150000), ylim = c(-70000,70000), #xlim = c(-150000,150000), ylim = c(-50000,80000), # xlim = c(-140000,110000), ylim = c(-50000,70000), 
     col = color(tracks_species$wolf, by = "individual"),  # color-match indivs
     R = dem30, col.R = "gray2")
title(main = "Summers 2018, 2019", line = 0.5)
mtext("Gray Wolf Telemetry", line = 2, cex = 1.5, font = 2)
plot(tracks_all$wolf_winter, xlim = c(-150000,150000), ylim = c(-70000,70000), #xlim = c(-150000,150000), ylim = c(-50000,80000), # xlim = c(-140000,110000), ylim = c(-50000,70000),
     col = color(tracks_species$wolf, by = "individual"),
     R = dem30, col.R = "gray2")
title(main = "Winter 2018/19, 2019/20", line = 0.5)
dev.off()

# For reference when there are fewer indivs (see md above)
tictoc::tic()
plot(tracks_all$wolf_winter, xlim = c(-150000,150000), ylim = c(-70000,70000), #xlim = c(-150000,150000), ylim = c(-50000,80000), # xlim = c(-140000,110000), ylim = c(-50000,70000),
     col = color(tracks_species$wolf, by = "individual"),
     R = dem30, col.R = "gray2")
title(main = "Winter 2018/19, 2019/20", line = 0.5)
tictoc::toc()

png(file = "figures/bassing_etal_2022/wolf/wolf_tracks_noerror.png",
    width = 4800, height = 6000, res = 600)
par(mfrow = c(2,1), mai = c(1,0.8,0.8,0.3), omi = c(0,0.15,0.15,0.15))  # Compare seasons
plot(tracks_all$wolf_summer, xlim = c(-150000,150000), ylim = c(-70000,70000), #xlim = c(-150000,150000), ylim = c(-50000,80000), # xlim = c(-140000,110000), ylim = c(-50000,70000), 
     col = color(tracks_species$wolf, by = "individual"),  # color-match indivs
     R = dem30, col.R = "gray2", error = FALSE, pch = 16, cex = 0.25)
title(main = "Summers 2018, 2019", line = 0.5)
mtext("Gray Wolf Telemetry", line = 2, cex = 1.5, font = 2)
plot(tracks_all$wolf_winter, xlim = c(-150000,150000), ylim = c(-70000,70000), #xlim = c(-150000,150000), ylim = c(-50000,80000), # xlim = c(-140000,110000), ylim = c(-50000,70000),
     col = color(tracks_species$wolf, by = "individual"),
     R = dem30, col.R = "gray2", error = FALSE, pch = 16, cex = 0.25)
title(main = "Winter 2018/19, 2019/20", line = 0.5)
dev.off()

## White-tailed deer
png(file = "figures/bassing_etal_2022/wtd/wtd_tracks.png",
    width = 4800, height = 6000, res = 600)
par(mfrow = c(2,1), mai = c(1,0.8,0.8,0.3), omi = c(0,0.15,0.15,0.15))  # Compare seasons
plot(tracks_all$wtd_summer, xlim = c(-150000,150000), ylim = c(-70000,70000), #xlim = c(-150000,150000), ylim = c(-50000,80000), # xlim = c(-140000,110000), ylim = c(-50000,70000), 
     col = color(tracks_species$wtd, by = "individual"),  # color-match indivs
     R = dem30, col.R = "gray2")
title(main = "Summers 2018, 2019", line = 0.5)
mtext("White-Tailed Deer Telemetry", line = 2, cex = 1.5, font = 2)
plot(tracks_all$wtd_winter, xlim = c(-150000,150000), ylim = c(-70000,70000), #xlim = c(-150000,150000), ylim = c(-50000,80000), # xlim = c(-140000,110000), ylim = c(-50000,70000),
     col = color(tracks_species$wtd, by = "individual"),
     R = dem30, col.R = "gray2")
title(main = "Winter 2018/19, 2019/20", line = 0.5)
dev.off()

png(file = "figures/bassing_etal_2022/wtd/wtd_tracks_noerror.png",
    width = 4800, height = 6000, res = 600)
par(mfrow = c(2,1), mai = c(1,0.8,0.8,0.3), omi = c(0,0.15,0.15,0.15))  # Compare seasons
plot(tracks_all$wtd_summer, xlim = c(-150000,150000), ylim = c(-70000,70000), #xlim = c(-150000,150000), ylim = c(-50000,80000), # xlim = c(-140000,110000), ylim = c(-50000,70000), 
     col = color(tracks_species$wtd, by = "individual"),  # color-match indivs
     R = dem30, col.R = "gray2", error = FALSE, pch = 16, cex = 0.25)
title(main = "Summers 2018, 2019", line = 0.5)
mtext("White-Tailed Deer Telemetry", line = 2, cex = 1.5, font = 2)
plot(tracks_all$wtd_winter, xlim = c(-150000,150000), ylim = c(-70000,70000), #xlim = c(-150000,150000), ylim = c(-50000,80000), # xlim = c(-140000,110000), ylim = c(-50000,70000),
     col = color(tracks_species$wtd, by = "individual"),
     R = dem30, col.R = "gray2", error = FALSE, pch = 16, cex = 0.25)
title(main = "Winter 2018/19, 2019/20", line = 0.5)
dev.off()


# Summer 2018, 2019 ----

## Bobcat ----

summary(tracks_all$bobcat_summer)  # tracked for diff intervals (4/8-hr) and durations

# Check sampling schedule
dt.plot(tracks_all$bobcat_summer)
sapply(tracks_all$bobcat_summer, FUN = dt.plot)  # all indivs have uneven sampling
## Should adjust weights for autocorrelated KDE to account for sampling schedule

# Model selection
## Autocorrelation model
GUESS_sbobcat <- lapply(tracks_all$bobcat_summer, 
                        function(x) {ctmm.guess(x, CTMM = ctmm(error = TRUE, isotropic = TRUE), interactive = FALSE)})

FITS_sbobcat <- list()  # empty list
for (i in 1:length(tracks_all$bobcat_summer)) {
  
  # Select best movement model for all indivs
  FITS_sbobcat[[i]] <- ctmm.select(tracks_all$bobcat_summer[[i]], CTMM = GUESS_sbobcat[[i]], trace = 3)
}
names(FITS_sbobcat) <- names(GUESS_sbobcat)
# save(FITS_sbobcat, file = "data/bassing_etal_2022_data/outputs/bobcat/FITS_bobcat_summer_tel2.rda")
load(file = "data/bassing_etal_2022_data/outputs/bobcat/FITS_bobcat_summer_tel2.rda")

# Estimate indiv home ranges
AKDE_sbobcat <- akde(tracks_all$bobcat_summer, CTMM = FITS_sbobcat, weights = TRUE)  # weighted AKDE
# save(AKDE_sbobcat, file = "data/bassing_etal_2022_data/outputs/bobcat/wAKDE_bobcat_summer_tel.rda")  # maybe not needed
load(file = "data/bassing_etal_2022_data/outputs/bobcat/wAKDE_bobcat_summer_tel.rda")

# Plot indiv home ranges
for (nam in names(tracks_all$bobcat_summer)) {
  plot(tracks_all$bobcat_summer[nam], UD = AKDE_sbobcat[nam], main = nam)
}

COL <- color(tracks_all$bobcat_summer, by = "individual")  # color by indiv

# Combined plot of indiv home ranges
png(file = "figures/bassing_etal_2022/bobcat/bobcat_tracks_akde_summer_all.png", 
    width = 4800, height = 3000, res = 600)
plot(tracks_all$bobcat_summer, UD = AKDE_sbobcat, R = dem30,  # 95% AKDE w/ CIs
     xlim = c(-150000,150000), ylim = c(-70000,70000),
     col = COL, col.UD = COL, col.level = COL, col.grid = NA, col.R = "gray2", labels = NA,
     main = "Bobcat Summer Home Ranges")
dev.off()

# Population range
PKDE_sbobcat <- pkde(tracks_all$bobcat_summer, UD = AKDE_sbobcat, 
                     weights = TRUE, grid = list(dr = c(100,100)))  # weighted pkde
# save(PKDE_sbobcat, file = "data/bassing_etal_2022_data/outputs/bobcat/PKDE_bobcat_summer_tel2.rda")
load(file = "data/bassing_etal_2022_data/outputs/bobcat/PKDE_bobcat_summer_tel2.rda")

# Plot population range estimate
png(file = "figures/bassing_etal_2022/bobcat/bobcat_tracks_pkde_summer.png", 
    width = 4800, height = 4200, res = 600)
plot(tracks_all$bobcat_summer, UD = PKDE_sbobcat, col = COL, R = dem30, col.R = "gray2",
     main = "Bobcat Summer Cross-Site Population Range")
## Very large and uncertain, perhaps better to separate by study site
dev.off()

# Population integrated resource selection function (iRSF)

# Include year indicator from telemetry for landcover covariates
formula <- as.formula("~ DEM + road_density + slope + Year1:percforest2018 + 
                      Year2:percforest2019 + Year1:percshrub2018 + Year2:percshrub2019 + 
                      Year1:percgrass2018 + Year2:percgrass2019")

# Year 1 landcover only
formula_y1 <- as.formula("~ DEM + road_density + slope + percforest2018 + percshrub2018 + percgrass2018")

# iRSF for each individual
RSF_sbobcat <- list()  # empty list
for (nam in names(tracks_all$bobcat_summer)) {
  RSF_sbobcat[[nam]] <- rsf.fit(tracks_all$bobcat_summer[[nam]], UD = AKDE_sbobcat[[nam]], 
                                R = R, formula = formula, 
                                integrator = "Riemann")
}
save(RSF_sbobcat, file = "data/bassing_etal_2022_data/outputs/bobcat/RSF_bobcat_summer_tel.rda")
load(file = "data/bassing_etal_2022_data/outputs/bobcat/RSF_bobcat_summer_tel.rda")

###


# Population iRSF
RSF_sbobcat <- rsf.fit(tracks_all$bobcat_summer, UD = PKDE_sbobcat, R = R, formula = formula)
## ISSUE 1 - reference variable should not be needed (see "pop_rsf_debug.R")
#           (no factors in raster data, % landcover does not appear to have issues with collinearity)
# Error in if (reference == "auto") { : argument is of length zero

# Population iRSF w/ Year 1 landcover only
RSF_sbobcat <- rsf.fit(tracks_all$bobcat_summer, UD = PKDE_sbobcat, R = R, formula = formula_y1)
# Error: crs not found: is it missing?
## GEO <- c('longitude','latitude')
## xy <- ctmm:::get.telemetry(tracks_all$bobcat_summer,GEO)  <-- comes out to empty numeric
### Somehow missing long/lat, unable to extract telemetry data?? But there is a projection??
projection(R$DEM) <- projection(tracks_all$bobcat_summer[[1]])
projection(R$slope) <- projection(tracks_all$bobcat_summer[[1]])

RSF_sbobcat <- rsf.fit(tracks_all$bobcat_summer, UD = PKDE_sbobcat, R = R, formula = formula_y1)
# test <- ctmm:::project(xy, to = raster::projection(R[[1]]))  # this works though?

# TEST with no formula
RSF_sbobcat <- rsf.fit(tracks_all$bobcat_summer, UD = PKDE_sbobcat, R = R[-c(5,7,9)])
# Error: crs not found: is it missing?
## Must be same issue as above, see ctmm:::get.telemetry()


###

# Updated indiv home ranges with iRSF
rAKDE_sbobcat <- akde(tracks_all$bobcat_summer, CTMM = RSF_sbobcat, R = list(dem30 = test), weights = TRUE)
# save(rAKDE_sbobcat, file = "data/bassing_etal_2022_data/outputs/bobcat/rsfAKDE_bobcat_summer_tel.rda")  # maybe not needed
load(file = "data/bassing_etal_2022_data/outputs/bobcat/rsfAKDE_bobcat_summer_tel.rda")

# Plot indiv home ranges
for (nam in names(tracks_all$bobcat_summer)) {
  plot(tracks_all$bobcat_summer[nam], UD = rAKDE_sbobcat[nam], main = nam)
}

# Population range with iRSF
rPKDE_sbobcat <- pkde(tracks_all$bobcat_summer, UD = rAKDE_sbobcat, R = list(dem30 = test), weights = TRUE)
# save(rPKDE_sbobcat, file = "data/bassing_etal_2022_data/outputs/bobcat/rsfPKDE_bobcat_summer_tel.rda")  # maybe not needed
load(file = "data/bassing_etal_2022_data/outputs/bobcat/rsfPKDE_bobcat_summer_tel.rda")

# Plot population range estimate
png(file = "figures/bassing_etal_2022/bobcat/bobcat_tracks_rsf_pkde_summer.png", 
    width = 4800, height = 4200, res = 600)
plot(tracks_all$bobcat_summer, UD = rPKDE_sbobcat, col = COL, R = dem30, col.R = "gray2",
     xlim = c(-200000,180000), ylim = c(-130000,150000),
     main = "Bobcat Summer Cross-Site RSF-PKDE")
## Very large and uncertain, perhaps better to separate by study site
dev.off()


# # Test on DEM data only
# test <- raster::readAll(dem30)
# 
# # iRSF for each individual
# RSF_sbobcat <- list()  # empty list
# for (nam in names(tracks_all$bobcat_summer)) {
#   RSF_sbobcat[[nam]] <- rsf.fit(tracks_all$bobcat_summer[[nam]], UD = AKDE_sbobcat[[nam]], 
#                                 R = list(dem30 = test))  #, integrator = "Riemann")
# }
# # save(RSF_sbobcat, file = "data/bassing_etal_2022_data/outputs/bobcat/RSF_bobcat_summer_tel2.rda")
# load(file = "data/bassing_etal_2022_data/outputs/bobcat/RSF_bobcat_summer_tel2.rda")
# # Warning messages:
# #   1: In rsf.fit(tracks_all$bobcat_summer[[nam]], UD = AKDE_sbobcat[[nam]],  :
# #       Calculation stopped before 1.5 Gb allocation.
# #   2: In rsf.fit(tracks_all$bobcat_summer[[nam]], UD = AKDE_sbobcat[[nam]],  :
# #       Calculation stopped before 1.7 Gb allocation.
# ## Should be fine
# 
# # Updated indiv home ranges with iRSF
# rAKDE_sbobcat <- akde(tracks_all$bobcat_summer, CTMM = RSF_sbobcat, R = list(dem30 = test), weights = TRUE)
# # save(rAKDE_sbobcat, file = "data/bassing_etal_2022_data/outputs/bobcat/rsfAKDE_bobcat_summer_tel.rda")  # maybe not needed
# load(file = "data/bassing_etal_2022_data/outputs/bobcat/rsfAKDE_bobcat_summer_tel.rda")
# 
# # Plot indiv home ranges
# for (nam in names(tracks_all$bobcat_summer)) {
#   plot(tracks_all$bobcat_summer[nam], UD = rAKDE_sbobcat[nam], main = nam)
# }
# 
# # Population range with iRSF
# rPKDE_sbobcat <- pkde(tracks_all$bobcat_summer, UD = rAKDE_sbobcat, R = list(dem30 = test), weights = TRUE)
# # save(rPKDE_sbobcat, file = "data/bassing_etal_2022_data/outputs/bobcat/rsfPKDE_bobcat_summer_tel.rda")  # maybe not needed
# load(file = "data/bassing_etal_2022_data/outputs/bobcat/rsfPKDE_bobcat_summer_tel.rda")
# 
# # Plot population range estimate
# png(file = "figures/bassing_etal_2022/bobcat/bobcat_tracks_rsf_pkde_summer.png", 
#     width = 4800, height = 4200, res = 600)
# plot(tracks_all$bobcat_summer, UD = rPKDE_sbobcat, col = COL, R = dem30, col.R = "gray2",
#      xlim = c(-200000,180000), ylim = c(-130000,150000),
#      main = "Bobcat Summer Cross-Site RSF-PKDE")
# ## Very large and uncertain, perhaps better to separate by study site
# dev.off()


### Okanogan site ----

# Subset indivs from Okanogan site
sites <- sapply(tracks_all$bobcat_summer, function(x) {unique(x$StudyArea)})  # identify sites
bobcat_OKs <- tracks_all$bobcat_summer[which(sites == "OK")]  # subset telemetry
FITS_bobcat_OKs <- FITS_sbobcat[names(bobcat_OKs)]  # subset model fits
AKDE_bobcat_OKs <- AKDE_sbobcat[names(bobcat_OKs)]  # subset wAKDEs

# Combined plot of indiv home ranges
png(file = "figures/bassing_etal_2022/bobcat/bobcat_tracks_akde_summer_OK.png", 
    width = 4800, height = 4200, res = 600)
COL <- color(tracks_all$bobcat_summer, by = "individual")  # color by indiv
plot(bobcat_OKs, UD = AKDE_bobcat_OKs, R = dem30,  # 95% AKDE w/ CIs
     xlim = c(-150000,-45000), ylim = c(-20000,65000),
     col = COL, col.UD = COL, col.level = COL, col.grid = NA, col.R = "gray2", labels = NA,
     main = "Okanogan Bobcat Summer Home Ranges")
dev.off()

# Population range
PKDE_bobcat_OKs <- pkde(bobcat_OKs, UD = AKDE_bobcat_OKs, weights = TRUE)  # weighted pkde
# save(PKDE_bobcat_OKs, file = "data/bassing_etal_2022_data/outputs/bobcat/PKDE_bobcat_summer_OK_tel.rda")
load(file = "data/bassing_etal_2022_data/outputs/bobcat/PKDE_bobcat_summer_OK_tel.rda")

# Plot population range estimate
png(file = "figures/bassing_etal_2022/bobcat/bobcat_tracks_pkde_summer_OK.png", 
    width = 4800, height = 4200, res = 600)
plot(bobcat_OKs, UD = PKDE_bobcat_OKs, col = COL, R = dem30, col.R = "gray2",
     main = "Okanogan Bobcat Summer Population Range")
## Large CIs, very uncertain
dev.off()

# Population integrated resource selection function (iRSF)

#### Year 1 Landcover only ----

# formula <- as.formula("~ dem30 + road_density + slope + percforest2018 + percshrub2018 + percgrass2018")

# # iRSF for each individual
# RSF_bobcat_OKs <- list()  # empty list
# for (nam in names(bobcat_OKs)) {
#   RSF_bobcat_OKs[[nam]] <- rsf.fit(bobcat_OKs[[nam]], UD = AKDE_bobcat_OKs[[nam]],
#                                 R = R_short) #, integrator = "Riemann")
# }
# # save(RSF_bobcat_OKs, file = "data/bassing_etal_2022_data/outputs/bobcat/RSF_bobcat_summer_OK_tel.rda")
load(file = "data/bassing_etal_2022_data/outputs/bobcat/RSF_bobcat_summer_OK_tel.rda")
# Warning messages:
# 1: In rsf.fit(bobcat_OKs[[nam]], UD = AKDE_bobcat_OKs[[nam]], R = R_short) :
#   Calculation stopped before 1.7 Gb allocation.
# 2: In cov.loglike(hess, grad) :
#   MLE is near a boundary or optimizer failed.
# 3: In rsf.fit(bobcat_OKs[[nam]], UD = AKDE_bobcat_OKs[[nam]], R = R_short) :
#   Calculation stopped before 1.3 Gb allocation.
# 4: In rsf.fit(bobcat_OKs[[nam]], UD = AKDE_bobcat_OKs[[nam]], R = R_short) :
#   Calculation stopped before 1.9 Gb allocation.
summary(RSF_bobcat_OKs)
sapply(RSF_bobcat_OKs, function(x) {print(x$VAR.loglike)})  # seems small enough
sapply(RSF_bobcat_OKs, function(x) {print(x$beta)})

# Population iRSF
RSF_bobcat_OKs <- rsf.fit(bobcat_OKs, UD = AKDE_bobcat_OKs, R = R_short) #, integrator = "Riemann")
save(RSF_bobcat_OKs, file = "data/bassing_etal_2022_data/outputs/bobcat/popRSF_bobcat_summer_OK_tel.rda")
load(file = "data/bassing_etal_2022_data/outputs/bobcat/popRSF_bobcat_summer_OK_tel.rda")

# Updated indiv home ranges with iRSF
rAKDE_bobcat_OKs <- akde(bobcat_OKs, CTMM = RSF_bobcat_OKs, R = R_short, weights = TRUE)
# save(rAKDE_bobcat_OKs, file = "data/bassing_etal_2022_data/outputs/bobcat/rsfAKDE_bobcat_summer_OK_tel.rda")  # maybe not needed
load(file = "data/bassing_etal_2022_data/outputs/bobcat/rsfAKDE_bobcat_summer_OK_tel.rda")

# Plot indiv home ranges
for (nam in names(bobcat_OKs)) {
  plot(bobcat_OKs[nam], UD = rAKDE_bobcat_OKs[nam], main = nam)
}

# Population range with iRSF
rPKDE_bobcat_OKs <- pkde(bobcat_OKs, UD = rAKDE_bobcat_OKs, R = R_short, weights = TRUE)
# save(rPKDE_bobcat_OKs, file = "data/bassing_etal_2022_data/outputs/bobcat/rsfPKDE_bobcat_summer_OK_tel.rda")
load(file = "data/bassing_etal_2022_data/outputs/bobcat/rsfPKDE_bobcat_summer_OK_tel.rda")

mean_RSF_bobcat_OKs <- mean(RSF_bobcat_OKs)
# save(mean_RSF_bobcat_OKs, file = "data/bassing_etal_2022_data/outputs/bobcat/mean_RSF_bobcat_summer_OK_tel.rda")
load(file = "data/bassing_etal_2022_data/outputs/bobcat/mean_RSF_bobcat_summer_OK_tel.rda")

# Calculate population suitability raster
suitability_bobcat_OKs <- suitability(CTMM = rPKDE_bobcat_OKs, R = R_short)  # doesn't work with a list of telemetry objects
## Do I need to pass this through mean() first?

# Plot population range estimate
png(file = "figures/bassing_etal_2022/bobcat/bobcat_tracks_rsf_pkde_summer_OK.png",
    width = 4800, height = 4200, res = 600)
plot(bobcat_OKs, UD = rPKDE_bobcat_OKs, col = COL, R = dem30, col.R = "gray2", #R = suitability_bobcat_OKs
     main = "Okanogan Bobcat Summer RSF-PKDE")
## Large CIs, very uncertain
dev.off()



# Test on DEM data only
# test <- raster::readAll(dem30)

# # iRSF for each individual
# RSF_bobcat_OKs <- list()  # empty list
# for (nam in names(bobcat_OKs)) {
#   RSF_bobcat_OKs[[nam]] <- rsf.fit(bobcat_OKs[[nam]], UD = AKDE_bobcat_OKs[[nam]], 
#                                 R = list(dem30 = test))
# }
# # save(RSF_bobcat_OKs, file = "data/bassing_etal_2022_data/outputs/bobcat/RSF_bobcat_summer_OK_tel.rda")
# load(file = "data/bassing_etal_2022_data/outputs/bobcat/RSF_bobcat_summer_OK_tel.rda")
# # Warnings:
# # 1: In rsf.fit(bobcat_OKs[[nam]], UD = AKDE_bobcat_OKs[[nam]], R = list(dem30 = test)) :
# #   Calculation stopped before 1.5 Gb allocation.
# # 2: In rsf.fit(bobcat_OKs[[nam]], UD = AKDE_bobcat_OKs[[nam]], R = list(dem30 = test)) :
# #   Calculation stopped before 1.7 Gb allocation.
# ## Should be fine.
# 
# # Updated indiv home ranges with iRSF
# rAKDE_bobcat_OKs <- akde(bobcat_OKs, CTMM = RSF_bobcat_OKs, R = list(dem30 = test), weights = TRUE)
# # save(rAKDE_bobcat_OKs, file = "data/bassing_etal_2022_data/outputs/bobcat/rsfAKDE_bobcat_summer_OK_tel.rda")  # maybe not needed
# load(file = "data/bassing_etal_2022_data/outputs/bobcat/rsfAKDE_bobcat_summer_OK_tel.rda")
# 
# # Plot indiv home ranges
# for (nam in names(bobcat_OKs)) {
#   plot(bobcat_OKs[nam], UD = rAKDE_bobcat_OKs[nam], main = nam)
# }
# 
# # Population range with iRSF
# rPKDE_bobcat_OKs <- pkde(bobcat_OKs, UD = rAKDE_bobcat_OKs, R = list(dem30 = test), weights = TRUE)
# # save(rPKDE_bobcat_OKs, file = "data/bassing_etal_2022_data/outputs/bobcat/rsfPKDE_bobcat_summer_OK_tel.rda")
# load(file = "data/bassing_etal_2022_data/outputs/bobcat/rsfPKDE_bobcat_summer_OK_tel.rda")
# 
# # Plot population range estimate
# png(file = "figures/bassing_etal_2022/bobcat/bobcat_tracks_rsf_pkde_summer_OK.png", 
#     width = 4800, height = 4200, res = 600)
# plot(bobcat_OKs, UD = rPKDE_bobcat_OKs, col = COL, R = dem30, col.R = "gray2",
#      main = "Okanogan Bobcat Summer RSF-PKDE")
# ## Large CIs, very uncertain
# dev.off()


### Northeast site ----

# Subset indivs from Northeast site
bobcat_NEs <- tracks_all$bobcat_summer[which(sites == "NE")]
FITS_bobcat_NEs <- FITS_sbobcat[names(bobcat_NEs)]  # subset model fits
AKDE_bobcat_NEs <- AKDE_sbobcat[names(bobcat_NEs)]  # subset wAKDEs

# Combined plot of indiv home ranges
png(file = "figures/bassing_etal_2022/bobcat/bobcat_tracks_akde_summer_NE.png", 
    width = 4800, height = 4200, res = 600)
COL <- color(tracks_all$bobcat_summer, by = "individual")  # color by indiv
plot(bobcat_NEs, UD = AKDE_bobcat_NEs, R = dem30,  # 95% AKDE w/ CIs
     col = COL, col.UD = COL, col.level = COL, col.grid = NA, col.R = "gray2", # labels = NA,
     main = "Northeast Bobcat Summer Home Ranges")
dev.off()

# Population range
PKDE_bobcat_NEs <- pkde(bobcat_NEs, UD = AKDE_bobcat_NEs, weights = TRUE)  # weighted pkde
# save(PKDE_bobcat_NEs, file = "data/bassing_etal_2022_data/outputs/bobcat/PKDE_bobcat_summer_NE_tel.rda")
load(file = "data/bassing_etal_2022_data/outputs/bobcat/PKDE_bobcat_summer_NE_tel.rda")

# Plot population range estimate
png(file = "figures/bassing_etal_2022/bobcat/bobcat_tracks_pkde_summer_NE.png", 
    width = 4800, height = 4200, res = 600)
plot(bobcat_NEs, UD = PKDE_bobcat_NEs, col = COL, R = dem30, col.R = "gray2",
     main = "Northeast Bobcat Summer Population Range")
dev.off()

# Population integrated resource selection function (iRSF)

#### Year 1 Landcover only ----

# formula <- as.formula("~ dem30 + road_density + slope + percforest2018 + percshrub2018 + percgrass2018")

# # iRSF for each individual
# RSF_bobcat_NEs <- list()  # empty list
# for (nam in names(bobcat_NEs)) {
#   RSF_bobcat_NEs[[nam]] <- rsf.fit(bobcat_NEs[[nam]], UD = AKDE_bobcat_NEs[[nam]],
#                                    R = R_short, integrator = "Riemann")
# }
# # save(RSF_bobcat_NEs, file = "data/bassing_etal_2022_data/outputs/bobcat/RSF_bobcat_summer_NE_tel.rda")
# load(file = "data/bassing_etal_2022_data/outputs/bobcat/RSF_bobcat_summer_NE_tel.rda")
# # Warning messages:
# # 1: In rsf.fit(bobcat_NEs[[nam]], UD = AKDE_bobcat_NEs[[nam]], R = R_short) :
# #   Calculation stopped before 1.5 Gb allocation.
# # 2: In rsf.fit(bobcat_NEs[[nam]], UD = AKDE_bobcat_NEs[[nam]], R = R_short) :
# #   Calculation stopped before 1.7 Gb allocation.
# # 3: In cov.loglike(hess, grad) :
# #   MLE is near a boundary or optimizer failed.
# # 4: In rsf.fit(bobcat_NEs[[nam]], UD = AKDE_bobcat_NEs[[nam]], R = R_short) :
# #   Calculation stopped before 1.3 Gb allocation.
# # 5: In cov.loglike(hess, grad) :
# #   MLE is near a boundary or optimizer failed.
# 
# summary(RSF_bobcat_OKs)
# sapply(RSF_bobcat_OKs, function(x) {print(x$VAR.loglike)})  # seems small enough
# sapply(RSF_bobcat_OKs, function(x) {print(x$beta)})

# Population iRSF
RSF_bobcat_NEs <- rsf.fit(bobcat_NEs[[nam]], UD = AKDE_bobcat_NEs[[nam]], R = R_short) #, integrator = "Riemann")

# Updated indiv home ranges with iRSF
rAKDE_bobcat_NEs <- akde(bobcat_NEs, CTMM = RSF_bobcat_NEs, R = R_short, weights = TRUE)
# save(rAKDE_bobcat_NEs, file = "data/bassing_etal_2022_data/outputs/bobcat/rsfAKDE_bobcat_summer_NE_tel.rda")  # maybe not needed
load(file = "data/bassing_etal_2022_data/outputs/bobcat/rsfAKDE_bobcat_summer_NE_tel.rda")

# Plot indiv home ranges
for (nam in names(bobcat_NEs)) {
  plot(bobcat_NEs[nam], UD = rAKDE_bobcat_NEs[nam], main = nam)
}

# Population range with iRSF
rPKDE_bobcat_NEs <- pkde(bobcat_NEs, UD = rAKDE_bobcat_NEs, R = R_short, weights = TRUE)
save(rPKDE_bobcat_NEs, file = "data/bassing_etal_2022_data/outputs/bobcat/rsfPKDE_bobcat_summer_NE_tel.rda")
load(file = "data/bassing_etal_2022_data/outputs/bobcat/rsfPKDE_bobcat_summer_NE_tel.rda")

mean_RSF_bobcat_NEs <- mean(RSF_bobcat_NEs)
# save(mean_RSF_bobcat_NEs, file = "data/bassing_etal_2022_data/outputs/bobcat/mean_RSF_bobcat_summer_NE_tel.rda")
load(file = "data/bassing_etal_2022_data/outputs/bobcat/mean_RSF_bobcat_summer_NE_tel.rda")

# Calculate population suitability raster
suitability_bobcat_NEs <- suitability(data = bobcatNEs, CTMM = RSF_bobcat_NEs, R = R_short)
## Do I need to pass this through mean() first?

# Plot population range estimate
png(file = "figures/bassing_etal_2022/bobcat/bobcat_tracks_rsf_pkde_summer_NE.png",
    width = 4800, height = 4200, res = 600)
plot(bobcat_NEs, UD = rPKDE_bobcat_NEs, col = COL, R = dem30, col.R = "gray2", #R = suitability_bobcat_NEs
     main = "Northeast Bobcat Summer RSF-PKDE")
## Large CIs, very uncertain
dev.off()


# # Test on DEM data only
# # test <- raster::readAll(dem30)
# 
# # iRSF for each individual
# RSF_bobcat_NEs <- list()  # empty list
# for (nam in names(bobcat_NEs)) {
#   RSF_bobcat_NEs[[nam]] <- rsf.fit(bobcat_NEs[[nam]], UD = AKDE_bobcat_NEs[[nam]], 
#                                    R = list(dem30 = test))
# }
# # save(RSF_bobcat_NEs, file = "data/bassing_etal_2022_data/outputs/bobcat/RSF_bobcat_summer_NE_tel.rda")
# load(file = "data/bassing_etal_2022_data/outputs/bobcat/RSF_bobcat_summer_NE_tel.rda")
# # No warnings, so the indivs with incomplete calculation are from Okanogan
# 
# # Updated indiv home ranges with iRSF
# rAKDE_bobcat_NEs <- akde(bobcat_NEs, CTMM = RSF_bobcat_NEs, R = list(dem30 = test), weights = TRUE)
# # save(rAKDE_bobcat_NEs, file = "data/bassing_etal_2022_data/outputs/bobcat/rsfAKDE_bobcat_summer_NE_tel.rda")  # maybe not needed
# load(file = "data/bassing_etal_2022_data/outputs/bobcat/rsfAKDE_bobcat_summer_NE_tel.rda")
# 
# # Plot indiv home ranges
# for (nam in names(bobcat_NEs)) {
#   plot(bobcat_NEs[nam], UD = rAKDE_bobcat_NEs[nam], main = nam)
# }
# 
# # Population range with iRSF
# rPKDE_bobcat_NEs <- pkde(bobcat_NEs, UD = rAKDE_bobcat_NEs, R = list(dem30 = test), weights = TRUE)
# # save(rPKDE_bobcat_NEs, file = "data/bassing_etal_2022_data/outputs/bobcat/rsfPKDE_bobcat_summer_NE_tel.rda")
# load(file = "data/bassing_etal_2022_data/outputs/bobcat/rsfPKDE_bobcat_summer_NE_tel.rda")
# 
# # Plot population range estimate
# png(file = "figures/bassing_etal_2022/bobcat/bobcat_tracks_rsf_pkde_summer_NE.png", 
#     width = 4800, height = 4200, res = 600)
# plot(bobcat_NEs, UD = rPKDE_bobcat_NEs, col = COL, R = dem30, col.R = "gray2",
#      main = "Northeast Bobcat Summer RSF-PKDE")
# ## Large CIs, very uncertain
# dev.off()


## Cougar ----


## Coyote ----


## Elk ----


## Mule Deer ----

summary(tracks_all$md_summer)  # tracked for diff durations (82 indivs)

# Check sampling schedule
dt.plot(tracks_all$md_summer)
sapply(tracks_all$md_summer, FUN = dt.plot)  # all indivs have fairly even sampling

# Model selection
## Autocorrelation model
GUESS_smd <- lapply(tracks_all$md_summer, 
                    function(x) {ctmm.guess(x, CTMM = ctmm(isotropic = TRUE),  # error = TRUE
                                            interactive = FALSE)})  ## PROBABLY SHOULDN'T FIT ERROR MODEL ##

FITS_smd <- list()  # empty list
for (i in 1:length(tracks_all$md_summer)) {
  
  # Select best movement model for all indivs
  FITS_smd[[i]] <- ctmm.select(tracks_all$md_summer[[i]], CTMM = GUESS_smd[[i]], trace = 3)
}
names(FITS_smd) <- names(GUESS_smd)
# save(FITS_smd, file = "data/bassing_etal_2022_data/outputs/mule_deer/FITS_md_summer_tel.rda")
load(file = "data/bassing_etal_2022_data/outputs/mule_deer/FITS_md_summer_tel.rda")

# Estimate indiv home ranges
AKDE_smd <- akde(tracks_all$md_summer, CTMM = FITS_smd)  # uniform-weight AKDE
# save(AKDE_smd, file = "data/bassing_etal_2022_data/outputs/mule_deer/AKDE_md_summer_tel.rda")
load(file = "data/bassing_etal_2022_data/outputs/mule_deer/AKDE_md_summer_tel.rda")

# Lower resolution of AKDE
AKDE_smd <- akde(tracks_all$md_summer, CTMM = FITS_smd, grid = list(dr = c(100,100)))  # uniform-weight AKDE
save(AKDE_smd, file = "~/Downloads/data/bassing_etal_2022_data/outputs/mule_deer/AKDE_md_summer_tel_res100.rda")
load(file = "~/Downloads/data/bassing_etal_2022_data/outputs/mule_deer/AKDE_md_summer_tel_res100.rda")

# Plot indiv home ranges
for (nam in names(tracks_all$md_summer)) {
  plot(tracks_all$md_summer[nam], UD = AKDE_smd[nam], main = nam)
}

COL <- color(tracks_all$md_summer, by = "individual")  # color by indiv

# Combined plot of indiv home ranges  ## COULD NOT RUN: OUT OF MEMORY (64 GB) ##
png(file = "figures/bassing_etal_2022/mule_deer/md_tracks_akde_summer_all.png", 
    width = 3000, height = 4800, res = 600)
plot(tracks_all$md_summer, UD = AKDE_smd, R = dem30,  # 95% AKDE w/ CIs
     xlim = c(-200000,0), ylim = c(-60000,120000),
     col = COL, col.UD = COL, col.level = COL, col.grid = NA, col.R = "gray2", labels = NA,
     main = "Mule Deer Summer Home Ranges")
dev.off()

png(file = "~/Downloads/figures/bassing_etal_2022/mule_deer/md_tracks_akde_summer_all.png", 
    width = 4800, height = 4200, res = 600)
plot(tracks_all$md_summer, UD = AKDE_smd, R = dem30,  # 95% AKDE w/ CIs
     xlim = c(-200000,0), ylim = c(-60000,120000),
     col = COL, col.UD = COL, col.level = COL, col.grid = NA, col.R = "gray2", labels = NA,
     main = "Mule Deer Summer Home Ranges")
dev.off()

# Population range
PKDE_smd <- pkde(tracks_all$md_summer, UD = AKDE_smd, 
                 # weights = TRUE, 
                 grid = list(dr = c(100,100)))  # unweighted pkde (should fix weights = vector of weights)
# save(PKDE_smd, file = "data/bassing_etal_2022_data/outputs/mule_deer/PKDE_md_summer_tel2.rda")
load(file = "data/bassing_etal_2022_data/outputs/mule_deer/PKDE_md_summer_tel2.rda")

# Lower resolution
PKDE_smd <- pkde(tracks_all$md_summer, UD = AKDE_smd, 
                 # weights = TRUE, 
                 grid = list(dr = c(200,200)))  # unweighted pkde (should fix weights = vector of weights)
save(PKDE_smd, file = "~/Downloads/data/bassing_etal_2022_data/outputs/mule_deer/PKDE_md_summer_tel2_res200.rda")
load(file = "~/Downloads/data/bassing_etal_2022_data/outputs/mule_deer/PKDE_md_summer_tel2_res200.rda")

# Plot population range estimate
png(file = "figures/bassing_etal_2022/mule_deer/md_tracks_pkde_summer.png", 
    width = 4800, height = 4200, res = 600)
plot(tracks_all$md_summer, UD = PKDE_smd, col = COL, R = dem30, col.R = "gray2",
     main = "Mule Deer Summer Cross-Site Population Range")
dev.off()

# Lower resolution population range estimate
png(file = "~/Downloads/figures/bassing_etal_2022/mule_deer/md_tracks_pkde_summer.png",
    width = 4800, height = 4200, res = 600)
plot(tracks_all$md_summer, UD = PKDE_smd, col = COL, R = dem30, col.R = "gray2",
     main = "Mule Deer Summer Cross-Site Population Range")
dev.off()
# Error in rasterImage(as.raster(zc), min(x), min(y), max(x), max(y), interpolate = FALSE) : 
#   cannot allocate memory block of size 16777216 Tb  -->  PKDE file too large

# HERE ----

# Population integrated resource selection function (iRSF)

# Include year indicator from telemetry for landcover covariates
formula <- as.formula("~ DEM + road_density + slope + Year1:percforest2018 + 
                      Year2:percforest2019 + Year1:percshrub2018 + Year2:percshrub2019 + 
                      Year1:percgrass2018 + Year2:percgrass2019")

# Year 1 landcover only
formula_y1 <- as.formula("~ DEM + road_density + slope + percforest2018 + percshrub2018 + percgrass2018")

# iRSF for each individual
RSF_smd <- list()  # empty list
for (nam in names(tracks_all$md_summer)) {
  RSF_smd[[nam]] <- rsf.fit(tracks_all$md_summer[[nam]], UD = AKDE_smd[[nam]], 
                            R = R, formula = formula, 
                            integrator = "Riemann")
}
save(RSF_smd, file = "data/bassing_etal_2022_data/outputs/mule_deer/RSF_md_summer_tel.rda")
load(file = "data/bassing_etal_2022_data/outputs/mule_deer/RSF_md_summer_tel.rda")

###


# Population iRSF
RSF_smd <- rsf.fit(tracks_all$md_summer, UD = PKDE_smd, R = R, formula = formula)
## ISSUE 1 - reference variable should not be needed (see "pop_rsf_debug.R")
#           (no factors in raster data, % landcover does not appear to have issues with collinearity)
# Error in if (reference == "auto") { : argument is of length zero

# Population iRSF w/ Year 1 landcover only
RSF_smd <- rsf.fit(tracks_all$md_summer, UD = PKDE_smd, R = R, formula = formula_y1)
# Error: crs not found: is it missing?
## GEO <- c('longitude','latitude')
## xy <- ctmm:::get.telemetry(tracks_all$md_summer,GEO)  <-- comes out to empty numeric
### Somehow missing long/lat, unable to extract telemetry data?? But there is a projection??
projection(R$DEM) <- projection(tracks_all$md_summer[[1]])
projection(R$slope) <- projection(tracks_all$md_summer[[1]])

RSF_smd <- rsf.fit(tracks_all$md_summer, UD = PKDE_smd, R = R, formula = formula_y1)
# test <- ctmm:::project(xy, to = raster::projection(R[[1]]))  # this works though?

# TEST with no formula
RSF_smd <- rsf.fit(tracks_all$md_summer, UD = PKDE_smd, R = R[-c(5,7,9)])
# Error: crs not found: is it missing?
## Must be same issue as above, see ctmm:::get.telemetry()


###

# Updated indiv home ranges with iRSF
rAKDE_smd <- akde(tracks_all$md_summer, CTMM = RSF_smd, R = list(dem30 = test), weights = TRUE)
# save(rAKDE_smd, file = "data/bassing_etal_2022_data/outputs/mule_deer/rsfAKDE_md_summer_tel.rda")  # maybe not needed
load(file = "data/bassing_etal_2022_data/outputs/mule_deer/rsfAKDE_md_summer_tel.rda")

# Plot indiv home ranges
for (nam in names(tracks_all$md_summer)) {
  plot(tracks_all$md_summer[nam], UD = rAKDE_smd[nam], main = nam)
}

# Population range with iRSF
rPKDE_smd <- pkde(tracks_all$md_summer, UD = rAKDE_smd, R = list(dem30 = test), weights = TRUE)
# save(rPKDE_smd, file = "data/bassing_etal_2022_data/outputs/mule_deer/rsfPKDE_md_summer_tel.rda")  # maybe not needed
load(file = "data/bassing_etal_2022_data/outputs/mule_deer/rsfPKDE_md_summer_tel.rda")

# Plot population range estimate
png(file = "figures/bassing_etal_2022/mule_deer/md_tracks_rsf_pkde_summer.png", 
    width = 4800, height = 4200, res = 600)
plot(tracks_all$md_summer, UD = rPKDE_smd, col = COL, R = dem30, col.R = "gray2",
     xlim = c(-200000,180000), ylim = c(-130000,150000),
     main = "Mule Deer Summer Cross-Site RSF-PKDE")
## Very large and uncertain, perhaps better to separate by study site
dev.off()

### Okanogan site ----

# Subset indivs from Okanogan site
sites <- sapply(tracks_all$md_summer, function(x) {unique(x$StudyArea)})  # identify sites
md_OKs <- tracks_all$md_summer[which(sites == "OK")]  # subset telemetry
FITS_md_OKs <- FITS_smd[names(md_OKs)]  # subset model fits
AKDE_md_OKs <- AKDE_smd[names(md_OKs)]  # subset wAKDEs

# Combined plot of indiv home ranges
png(file = "figures/bassing_etal_2022/mule_deer/md_tracks_akde_summer_OK.png", 
    width = 4800, height = 4200, res = 600)
COL <- color(tracks_all$md_summer, by = "individual")  # color by indiv
plot(md_OKs, UD = AKDE_md_OKs, R = dem30,  # 95% AKDE w/ CIs
     xlim = c(-150000,-45000), ylim = c(-20000,65000),
     col = COL, col.UD = COL, col.level = COL, col.grid = NA, col.R = "gray2", labels = NA,
     main = "Okanogan Mule Deer Summer Home Ranges")
dev.off()

# Population range
PKDE_md_OKs <- pkde(md_OKs, UD = AKDE_md_OKs, weights = TRUE)  # weighted pkde
# save(PKDE_md_OKs, file = "data/bassing_etal_2022_data/outputs/mule_deer/PKDE_md_summer_OK_tel.rda")
load(file = "data/bassing_etal_2022_data/outputs/mule_deer/PKDE_md_summer_OK_tel.rda")

# Plot population range estimate
png(file = "figures/bassing_etal_2022/mule_deer/md_tracks_pkde_summer_OK.png", 
    width = 4800, height = 4200, res = 600)
plot(md_OKs, UD = PKDE_md_OKs, col = COL, R = dem30, col.R = "gray2",
     main = "Okanogan Mule Deer Summer Population Range")
## Large CIs, very uncertain
dev.off()

# Population integrated resource selection function (iRSF)

# # iRSF for each individual
# RSF_md_OKs <- list()  # empty list
# for (nam in names(md_OKs)) {
#   RSF_md_OKs[[nam]] <- rsf.fit(md_OKs[[nam]], UD = AKDE_md_OKs[[nam]],
#                                 R = R, formula = formula) #, integrator = "Riemann")
# }
# # save(RSF_md_OKs, file = "data/bassing_etal_2022_data/outputs/mule_deer/RSF_md_summer_OK_tel.rda")
load(file = "data/bassing_etal_2022_data/outputs/mule_deer/RSF_md_summer_OK_tel.rda")

# Population iRSF
RSF_md_OKs <- rsf.fit(md_OKs, UD = PKDE_md_OKs, R = R, formula = formula) #, integrator = "Riemann")
save(RSF_md_OKs, file = "data/bassing_etal_2022_data/outputs/mule_deer/popRSF_md_summer_OK_tel.rda")
load(file = "data/bassing_etal_2022_data/outputs/mule_deer/popRSF_md_summer_OK_tel.rda")


#### Year 1 Landcover only ----

# formula <- as.formula("~ dem30 + road_density + slope + percforest2018 + percshrub2018 + percgrass2018")

# # iRSF for each individual
# RSF_md_OKs <- list()  # empty list
# for (nam in names(md_OKs)) {
#   RSF_md_OKs[[nam]] <- rsf.fit(md_OKs[[nam]], UD = AKDE_md_OKs[[nam]],
#                                 R = R_short) #, integrator = "Riemann")
# }
# # save(RSF_md_OKs, file = "data/bassing_etal_2022_data/outputs/mule_deer/RSF_md_summer_OK_tel.rda")
load(file = "data/bassing_etal_2022_data/outputs/mule_deer/RSF_md_summer_OK_tel.rda")
summary(RSF_md_OKs)
sapply(RSF_md_OKs, function(x) {print(x$VAR.loglike)})  # seems small enough
sapply(RSF_md_OKs, function(x) {print(x$beta)})

# Population iRSF
RSF_md_OKs <- rsf.fit(md_OKs, UD = AKDE_md_OKs, R = R_short) #, integrator = "Riemann")
save(RSF_md_OKs, file = "data/bassing_etal_2022_data/outputs/mule_deer/popRSF_md_summer_OK_tel.rda")
load(file = "data/bassing_etal_2022_data/outputs/mule_deer/popRSF_md_summer_OK_tel.rda")

# Updated indiv home ranges with iRSF
rAKDE_md_OKs <- akde(md_OKs, CTMM = RSF_md_OKs, R = R_short, weights = TRUE)
# save(rAKDE_md_OKs, file = "data/bassing_etal_2022_data/outputs/mule_deer/rsfAKDE_md_summer_OK_tel.rda")  # maybe not needed
load(file = "data/bassing_etal_2022_data/outputs/mule_deer/rsfAKDE_md_summer_OK_tel.rda")

# Plot indiv home ranges
for (nam in names(md_OKs)) {
  plot(md_OKs[nam], UD = rAKDE_md_OKs[nam], main = nam)
}

# Population range with iRSF
rPKDE_md_OKs <- pkde(md_OKs, UD = rAKDE_md_OKs, R = R_short, weights = TRUE)
# save(rPKDE_md_OKs, file = "data/bassing_etal_2022_data/outputs/mule_deer/rsfPKDE_md_summer_OK_tel.rda")
load(file = "data/bassing_etal_2022_data/outputs/mule_deer/rsfPKDE_md_summer_OK_tel.rda")

mean_RSF_md_OKs <- mean(RSF_md_OKs)
# save(mean_RSF_md_OKs, file = "data/bassing_etal_2022_data/outputs/mule_deer/mean_RSF_md_summer_OK_tel.rda")
load(file = "data/bassing_etal_2022_data/outputs/mule_deer/mean_RSF_md_summer_OK_tel.rda")

# Calculate population suitability raster
suitability_md_OKs <- suitability(CTMM = rPKDE_md_OKs, R = R_short)  # doesn't work with a list of telemetry objects
## Do I need to pass this through mean() first?

# Plot population range estimate
png(file = "figures/bassing_etal_2022/mule_deer/md_tracks_rsf_pkde_summer_OK.png",
    width = 4800, height = 4200, res = 600)
plot(md_OKs, UD = rPKDE_md_OKs, col = COL, R = dem30, col.R = "gray2", #R = suitability_md_OKs
     main = "Okanogan Mule Deer Summer RSF-PKDE")
## Large CIs, very uncertain
dev.off()


## Wolf ----


## White-Tailed Deer ----


# Winter 2018/19, 2019/20 ----

## Bobcat ----

summary(tracks_all$bobcat_winter)  # tracked for diff intervals (4,8,12,16-hr) and durations (1-3 months)

# Check sampling schedule
dt.plot(tracks_all$bobcat_winter)
sapply(tracks_all$bobcat_winter, FUN = dt.plot)  # all indivs have uneven sampling
## Should adjust weights for autocorrelated KDE to account for sampling schedule

# Model selection
## Autocorrelation model
GUESS_wbobcat <- lapply(tracks_all$bobcat_winter, 
                        function(x) {ctmm.guess(x, CTMM = ctmm(error = TRUE, isotropic = TRUE), interactive = FALSE)})

FITS_wbobcat <- list()  # empty list
for (i in 1:length(tracks_all$bobcat_winter)) {
  
  # Select best movement model for all indivs
  FITS_wbobcat[[i]] <- ctmm.select(tracks_all$bobcat_winter[[i]], CTMM = GUESS_wbobcat[[i]], trace = 3)
}
names(FITS_wbobcat) <- names(GUESS_wbobcat)
# save(FITS_wbobcat, file = "data/bassing_etal_2022_data/outputs/bobcat/FITS_bobcat_winter_tel.rda")
load(file = "data/bassing_etal_2022_data/outputs/bobcat/FITS_bobcat_winter_tel.rda")
# Warning messages:
# 1: In FUN(X[[i]], ...) :
#   Objective function failure at c(major=74419428992575792, error all=4.7892030648864e-19, tau=707392995646747008)
# 2: In FUN(X[[i]], ...) :
#   Objective function failure at c(major=111629143488863680, error all=4.7892030648864e-19, tau=1061089493470120576)
# 3: In FUN(X[[i]], ...) :
#   Objective function failure at c(major=2911582844175972, error all=4.7892030648864e-19, tau=8755818189223207)
# 4: In FUN(X[[i]], ...) :
#   Objective function failure at c(major=4367374266263958, error all=4.7892030648864e-19, tau=13133727283834810)
# 5: In FUN(X[[i]], ...) :
#   Objective function failure at c(major=1455791422087986, error all=4.7892030648864e-19, tau=4377909094611606)
# 6: In FUN(X[[i]], ...) :
#   Objective function failure at c(major=3.21636454955259, error all=5.02036960565911e-18, tau=268902678716114432)
# 7: In FUN(X[[i]], ...) :
#   Objective function failure at c(major=3.21636454955259, error all=5.02036960565911e-18, tau=403354018074171648)

# Check the sampling intervals for each individual
for (i in 1:length(tracks_all$bobcat_winter)) {
  print(range(diff(tracks_all$bobcat_winter[[i]]$timestamp)))
}
## No very small differences in sampling intervals (all 4+ hours)

# Estimate indiv home ranges
AKDE_wbobcat <- akde(tracks_all$bobcat_winter, CTMM = FITS_wbobcat, weights = TRUE)  # weighted AKDE
# save(AKDE_wbobcat, file = "data/bassing_etal_2022_data/outputs/bobcat/wAKDE_bobcat_winter_tel.rda")  # maybe not needed

# Plot indiv home ranges
for (nam in names(tracks_all$bobcat_winter)) {
  plot(tracks_all$bobcat_winter[nam], UD = AKDE_wbobcat[nam], main = nam)
}

COL <- color(tracks_all$bobcat_winter, by = "individual")  # color by indiv

# Combined plot of indiv home ranges
png(file = "figures/bassing_etal_2022/bobcat/bobcat_tracks_akde_winter_all.png", 
    width = 4800, height = 3000, res = 600)
plot(tracks_all$bobcat_winter, UD = AKDE_wbobcat, R = dem30,  # 95% AKDE w/ CIs
     xlim = c(-150000,150000), ylim = c(-70000,70000),
     col = COL, col.UD = COL, col.level = COL, col.grid = NA, col.R = "gray2", labels = NA,
     main = "Bobcat Winter Home Ranges")
dev.off()

# Population range
PKDE_wbobcat <- pkde(tracks_all$bobcat_winter, UD = AKDE_wbobcat, 
                     weights = TRUE, grid = list(dr = c(100,100)))  # weighted pkde
# save(PKDE_wbobcat, file = "data/bassing_etal_2022_data/outputs/bobcat/PKDE_bobcat_winter_tel2.rda")
load(file = "data/bassing_etal_2022_data/outputs/bobcat/PKDE_bobcat_winter_tel2.rda")

# Plot population range estimate
png(file = "figures/bassing_etal_2022/bobcat/bobcat_tracks_pkde_winter.png", 
    width = 4800, height = 4200, res = 600)
plot(tracks_all$bobcat_winter, UD = PKDE_wbobcat, col = COL, R = dem30, col.R = "gray2",
     main = "Bobcat Winter Cross-Site Population Range")
## Very large and uncertain, perhaps better to separate by study site
dev.off()

# Population integrated resource selection function (iRSF)

# Test on DEM data only
test <- raster::readAll(dem30)

# iRSF for each individual
RSF_wbobcat <- list()  # empty list
for (nam in names(tracks_all$bobcat_winter)) {
  RSF_wbobcat[[nam]] <- rsf.fit(tracks_all$bobcat_winter[[nam]], UD = AKDE_wbobcat[[nam]], 
                                R = list(dem30 = test))  #, integrator = "Riemann")
}
# save(RSF_wbobcat, file = "data/bassing_etal_2022_data/outputs/bobcat/RSF_bobcat_winter_tel.rda")
load(file = "data/bassing_etal_2022_data/outputs/bobcat/RSF_bobcat_winter_tel.rda")
# Warning message:
# In rsf.fit(tracks_all$bobcat_winter[[nam]], UD = AKDE_wbobcat[[nam]],  :
#   Calculation stopped before 1.5 Gb allocation.

# Updated indiv home ranges with iRSF
rAKDE_wbobcat <- akde(tracks_all$bobcat_winter, CTMM = RSF_wbobcat, R = list(dem30 = test), weights = TRUE)
# save(rAKDE_wbobcat, file = "data/bassing_etal_2022_data/outputs/bobcat/rsfAKDE_bobcat_winter_tel.rda")  # maybe not needed
load(file = "data/bassing_etal_2022_data/outputs/bobcat/rsfAKDE_bobcat_winter_tel.rda")

# Plot indiv home ranges
for (nam in names(tracks_all$bobcat_winter)) {
  plot(tracks_all$bobcat_winter[nam], UD = rAKDE_wbobcat[nam], main = nam)
}

# Population range with iRSF
rPKDE_wbobcat <- pkde(tracks_all$bobcat_winter, UD = rAKDE_wbobcat, R = list(dem30 = test), 
                      weights = TRUE, grid = list(dr = c(500,500)),)
save(rPKDE_wbobcat, file = "data/bassing_etal_2022_data/outputs/bobcat/rsfPKDE_bobcat_winter_tel.rda")
load(file = "data/bassing_etal_2022_data/outputs/bobcat/rsfPKDE_bobcat_winter_tel.rda")

# Plot population range estimate
png(file = "figures/bassing_etal_2022/bobcat/bobcat_tracks_rsf_pkde_winter.png", 
    width = 4800, height = 4200, res = 600)
plot(tracks_all$bobcat_winter, UD = rPKDE_wbobcat, col = COL, R = dem30, col.R = "gray2",
     xlim = c(-200000,180000), ylim = c(-130000,150000),
     main = "Bobcat Winter Cross-Site RSF-PKDE")
## Much stronger selection??, narrower CIs
dev.off()


### Okanogan site ----

# Subset indivs from Okanogan site
sites <- sapply(tracks_all$bobcat_winter, function(x) {unique(x$StudyArea)})  # identify sites
bobcat_OKw <- tracks_all$bobcat_winter[which(sites == "OK")]  # subset telemetry
FITS_bobcat_OKw <- FITS_wbobcat[names(bobcat_OKw)]  # subset model fits
AKDE_bobcat_OKw <- AKDE_wbobcat[names(bobcat_OKw)]  # subset wAKDEs

# Combined plot of indiv home ranges
png(file = "figures/bassing_etal_2022/bobcat/bobcat_tracks_akde_winter_OK.png", 
    width = 4800, height = 4200, res = 600)
COL <- color(tracks_all$bobcat_winter, by = "individual")  # color by indiv
plot(bobcat_OKw, UD = AKDE_bobcat_OKw, R = dem30,  # 95% AKDE w/ CIs
     xlim = c(-160000,-40000), ylim = c(-70000,65000),
     col = COL, col.UD = COL, col.level = COL, col.grid = NA, col.R = "gray2", # labels = NA,
     main = "Okanogan Bobcat Winter Home Ranges")
dev.off()

# Population range
PKDE_bobcat_OKw <- pkde(bobcat_OKw, UD = AKDE_bobcat_OKw, weights = TRUE)  # weighted pkde
# save(PKDE_bobcat_OKw, file = "data/bassing_etal_2022_data/outputs/bobcat/PKDE_bobcat_winter_OK_tel.rda")
load(file = "data/bassing_etal_2022_data/outputs/bobcat/PKDE_bobcat_winter_OK_tel.rda")

# Plot population range estimate
png(file = "figures/bassing_etal_2022/bobcat/bobcat_tracks_pkde_winter_OK.png", 
    width = 4800, height = 4200, res = 600)
plot(bobcat_OKw, UD = PKDE_bobcat_OKw, col = COL, R = dem30, col.R = "gray2",
     main = "Okanogan Bobcat Winter Population Range")
## Large CIs, very uncertain
dev.off()

# Population integrated resource selection function (iRSF)

# Test on DEM data only
# test <- raster::readAll(dem30)

# iRSF for each individual
RSF_bobcat_OKw <- list()  # empty list
for (nam in names(bobcat_OKw)) {
  RSF_bobcat_OKw[[nam]] <- rsf.fit(bobcat_OKw[[nam]], UD = AKDE_bobcat_OKw[[nam]], 
                                   R = list(dem30 = test), integrator = "Riemann")  # Monte Carlo integration was too memory-intensive
}
# save(RSF_bobcat_OKw, file = "data/bassing_etal_2022_data/outputs/bobcat/RSF_bobcat_winter_OK_tel.rda")
load(file = "data/bassing_etal_2022_data/outputs/bobcat/RSF_bobcat_winter_OK_tel.rda")

# Updated indiv home ranges with iRSF
rAKDE_bobcat_OKw <- akde(bobcat_OKw, CTMM = RSF_bobcat_OKw, R = list(dem30 = test), weights = TRUE)
# save(rAKDE_bobcat_OKw, file = "data/bassing_etal_2022_data/outputs/bobcat/rsfAKDE_bobcat_winter_OK_tel.rda")  # maybe not needed
load(file = "data/bassing_etal_2022_data/outputs/bobcat/rsfAKDE_bobcat_winter_OK_tel.rda")

# Plot indiv home ranges
for (nam in names(bobcat_OKw)) {
  plot(bobcat_OKw[nam], UD = rAKDE_bobcat_OKw[nam], main = nam)
}

# Population range with iRSF
rPKDE_bobcat_OKw <- pkde(bobcat_OKw, UD = rAKDE_bobcat_OKw, R = list(dem30 = test), weights = TRUE)
# save(rPKDE_bobcat_OKw, file = "data/bassing_etal_2022_data/outputs/bobcat/rsfPKDE_bobcat_winter_OK_tel.rda")
load(file = "data/bassing_etal_2022_data/outputs/bobcat/rsfPKDE_bobcat_winter_OK_tel.rda")

# Plot population range estimate
png(file = "figures/bassing_etal_2022/bobcat/bobcat_tracks_rsf_pkde_winter_OK.png", 
    width = 4800, height = 4200, res = 600)
plot(bobcat_OKw, UD = rPKDE_bobcat_OKw, col = COL, R = dem30, col.R = "gray2",
     main = "Okanogan Bobcat Winter RSF-PKDE")
## Large CIs, very uncertain, selects for a part across the mountain where there is no tracking data
dev.off()


### Northeast site ----

# Subset indivs from Northeast site
bobcat_NEw <- tracks_all$bobcat_winter[which(sites == "NE")]
FITS_bobcat_NEw <- FITS_wbobcat[names(bobcat_NEw)]  # subset model fits
AKDE_bobcat_NEw <- AKDE_wbobcat[names(bobcat_NEw)]  # subset wAKDEs

# Combined plot of indiv home ranges
png(file = "figures/bassing_etal_2022/bobcat/bobcat_tracks_akde_winter_NE.png", 
    width = 4800, height = 4200, res = 600)
COL <- color(tracks_all$bobcat_winter, by = "individual")  # color by indiv
plot(bobcat_NEw, UD = AKDE_bobcat_NEw, R = dem30,  # 95% AKDE w/ CIs
     # xlim = c(-150000,-45000), ylim = c(-20000,65000),
     col = COL, col.UD = COL, col.level = COL, col.grid = NA, col.R = "gray2", # labels = NA,
     main = "Northeast Bobcat Winter Home Ranges")
dev.off()

# Population range
PKDE_bobcat_NEw <- pkde(bobcat_NEw, UD = AKDE_bobcat_NEw, weights = TRUE)  # weighted pkde
# save(PKDE_bobcat_NEw, file = "data/bassing_etal_2022_data/outputs/bobcat/PKDE_bobcat_winter_NE_tel.rda")
load(file = "data/bassing_etal_2022_data/outputs/bobcat/PKDE_bobcat_winter_NE_tel.rda")

# Plot population range estimate
png(file = "figures/bassing_etal_2022/bobcat/bobcat_tracks_pkde_winter_NE.png", 
    width = 4800, height = 4200, res = 600)
plot(bobcat_NEw, UD = PKDE_bobcat_NEw, col = COL, R = dem30, col.R = "gray2",
     main = "Northeast Bobcat Winter Population Range")
dev.off()

# Population integrated resource selection function (iRSF)

# Test on DEM data only
# test <- raster::readAll(dem30)

# iRSF for each individual
RSF_bobcat_NEw <- list()  # empty list
for (nam in names(bobcat_NEw)) {
  RSF_bobcat_NEw[[nam]] <- rsf.fit(bobcat_NEw[[nam]], UD = AKDE_bobcat_NEw[[nam]], 
                                   R = list(dem30 = test))
}
# save(RSF_bobcat_NEw, file = "data/bassing_etal_2022_data/outputs/bobcat/RSF_bobcat_winter_NE_tel.rda")
load(file = "data/bassing_etal_2022_data/outputs/bobcat/RSF_bobcat_winter_NE_tel.rda")

# Updated indiv home ranges with iRSF
rAKDE_bobcat_NEw <- akde(bobcat_NEw, CTMM = RSF_bobcat_NEw, R = list(dem30 = test), weights = TRUE)
# save(rAKDE_bobcat_NEw, file = "data/bassing_etal_2022_data/outputs/bobcat/rsfAKDE_bobcat_winter_NE_tel.rda")  # maybe not needed
load(file = "data/bassing_etal_2022_data/outputs/bobcat/rsfAKDE_bobcat_winter_NE_tel.rda")

# Plot indiv home ranges
for (nam in names(bobcat_NEw)) {
  plot(bobcat_NEw[nam], UD = rAKDE_bobcat_NEw[nam], main = nam)
}

# Population range with iRSF
rPKDE_bobcat_NEw <- pkde(bobcat_NEw, UD = rAKDE_bobcat_NEw, R = list(dem30 = test), weights = TRUE)
# save(rPKDE_bobcat_NEw, file = "data/bassing_etal_2022_data/outputs/bobcat/rsfPKDE_bobcat_winter_NE_tel.rda")
load(file = "data/bassing_etal_2022_data/outputs/bobcat/rsfPKDE_bobcat_winter_NE_tel.rda")

# Plot population range estimate
png(file = "figures/bassing_etal_2022/bobcat/bobcat_tracks_rsf_pkde_winter_NE.png", 
    width = 4800, height = 4200, res = 600)
plot(bobcat_NEw, UD = rPKDE_bobcat_NEw, col = COL, R = dem30, col.R = "gray2",
     main = "Northeast Bobcat Winter RSF-PKDE")
## Large CIs, very uncertain
dev.off()


## Cougar ----


## Coyote ----


## Elk ----


## Mule Deer ----


## Wolf ----


## White-Tailed Deer ----



