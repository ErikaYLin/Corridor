# Camera Trap Species Distribution Model Testing

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
forest18 <- raster::raster("data/bassing_etal_2022_data/spatial data/perc_forestmix_2018.tif")
forest19 <- raster::raster("data/bassing_etal_2022_data/spatial data/perc_forestmix_2019.tif")
shrub18 <- raster::raster("data/bassing_etal_2022_data/spatial data/perc_shrub_2018.tif")
shrub19 <- raster::raster("data/bassing_etal_2022_data/spatial data/perc_shrub_2019.tif")
grass18 <- raster::raster("data/bassing_etal_2022_data/spatial data/perc_grass_2018.tif")
grass19 <- raster::raster("data/bassing_etal_2022_data/spatial data/perc_grass_2019.tif")

# Named list of raster layers
R <- list(DEM = dem30, road_density = roads, # slope = slope,  # slope is a large file
          percforest2018 = forest18, percforest2019 = forest19, 
          percshrub2019 = shrub18, percshrub2019 = shrub19, 
          percgrass2018 = grass18, percgrass2019 = grass19)
# R_stack <- raster::stack(dem30, roads, slope, forest18, forest19, shrub18, shrub19, grass18, grass19)
## diff extents and projections (WGS84 vs. GRS80)

# Data wrangling ----

# Make consistent naming scheme across telemetry and camera trap data

names(all_spp_tracks)  # list of tracking data grouped by species and season

# Shorten deer names for consistent naming scheme: "species_season"
names(all_spp_tracks)[1] <- "md_summer"  # "mule_deer_summer" -> "md_summer"
names(all_spp_tracks)[2] <- "md_winter"
names(all_spp_tracks)[5] <- "wtd_summer"  # "whitetailed_deer_summer" -> "wtd_summer"
names(all_spp_tracks)[6] <- "wtd_winter"

# Change colnames to match format needed for `telemetry`
colnames(all_spp_camtrap)
colnames(all_spp_camtrap)[2] <- "Lat"
colnames(all_spp_camtrap)[3] <- "Long"
colnames(all_spp_camtrap)[5] <- "timestamp"
colnames(all_spp_camtrap)[7] <- "Img_Time"
# Change species column to match tracking data naming scheme
all_spp_camtrap$Species <- tolower(all_spp_camtrap$Species)  # lowercase
all_spp_camtrap$Species[all_spp_camtrap$Species == "white-tailed deer"] <- "wtd"
all_spp_camtrap$Species[all_spp_camtrap$Species == "mule deer"] <- "md"

# Create column for study area to match telemetry data
all_spp_camtrap$StudyArea <- ifelse(grepl("NE", all_spp_camtrap$CameraLocation), "NE", "OK")

## Telemetry data ----

all_spp_tracks <- all_spp_tracks[order(names(all_spp_tracks))]  # alphabetical order
species <- sort(unique(all_spp_camtrap$Species))  # 7 species, alphabetical order

# Convert to listed dataframes to `telemetry` objects
tracks_all <- lapply(all_spp_tracks, 
                     function(x) {as.telemetry(x, keep = c("Sex", "Season", "StudyArea"))})
for (i in 1:length(tracks_all)) {
  tracks_all[[i]] <- tracks_all[[i]][order(names(tracks_all[[i]]))]  # alphabetize IDs
}
## Some individuals were tracks across seasons, while others were only tracked for 1 or 2

# Match projections for species across seasons
for (i in 1:length(tracks_all)) {
  projection(tracks_all[[i]]) <- median(tracks_all[[1]])
}

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
                     function(x) {as.telemetry(x, keep = c("Sex", "Season", "StudyArea"))})
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

## TEST:
## bobcat color-matching (STILL HAS ISSUES)
colors_bobcat <- color(tracks_species$bobcat, by = "individual")
colors_bobcat <- sapply(colors_bobcat, '[', 1)

png(file = "figures/bassing_etal_2022/bobcat_tracks.png",
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

png(file = "figures/bassing_etal_2022/bobcat_tracks_noerror.png",  # no error for clarity
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
png(file = "figures/bassing_etal_2022/cougar_tracks.png",
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

png(file = "figures/bassing_etal_2022/cougar_tracks_noerror.png",
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
png(file = "figures/bassing_etal_2022/coyote_tracks.png",
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

png(file = "figures/bassing_etal_2022/coyote_tracks_noerror.png",
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
png(file = "figures/bassing_etal_2022/elk_tracks.png",
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

png(file = "figures/bassing_etal_2022/elk_tracks_noerror.png",
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

png(file = "figures/bassing_etal_2022/md_tracks.png",
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
# save(KDE_md, file = "data/bassing_etal_2022_data/IID_KDE_md.rda")
png(file = "figures/bassing_etal_2022/md_tracks_iid_kde_dem.png",
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


png(file = "figures/bassing_etal_2022/md_tracks_noerror.png",
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
png(file = "figures/bassing_etal_2022/wolf_tracks.png",
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

png(file = "figures/bassing_etal_2022/wolf_tracks_noerror.png",
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
png(file = "figures/bassing_etal_2022/wtd_tracks.png",
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

png(file = "figures/bassing_etal_2022/wtd_tracks_noerror.png",
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


## CT data ----

# TODO: 
## Filter/clean CT data by temporal autocorrelation of detection/non-detection (variogram)

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

# Separate data by species, season, and year
ct_all_seasons <- list()
for (sp in species) {
  for (season in unique(all_spp_camtrap$Season)) {
    ct_all_seasons[[paste(sp, season, sep = "_")]] <- as.telemetry(
      all_spp_camtrap[all_spp_camtrap$Season == season & all_spp_camtrap$Species == sp,],
      keep = c("Count", "Season", "StudyArea", "CameraLocation", "AF", "AM", "AU", "OS", "UNK"))
  }
}
# ct_all_seasons <- ct_all_seasons[order(names(ct_all_seasons))]  # alphabetical order


## TEST
## Must be separated by cam first to calculate the diffs
# diff <- data.frame(diff = c(1000,diff(camtrap_NE$timestamp)), count = camtrap_NE$Count, 
#                    cam = camtrap_NE$CameraLocation, species = camtrap_NE$Species)
# # diff <- diff[diff$diff > 3,]  # remove < 3 seconds (within the same sequence)
# diff$minutes <- diff$diff/60  # convert to mins
# diff <- diff[diff$diff <= 60,]  # less than or equal to 1 hour (in mins)
# par(mfrow = c(2,1))
# for (NEsp in unique(diff$species)) {
#   hist(diff$minutes[diff$species == NEsp], 
#        breaks = seq(0, 60, by = 1), #max(diff$minutes)/2000,  # divide by mins/day
#        main = NEsp, xlab = "mins")
#   plot(acf(diff$minutes[diff$species == NEsp], plot = FALSE), main = NEsp)
# }


## TEST

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

# Okanogon study area
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

# Thin data to independent detections (separated by 2 mins) for each species
ct_all2 <- ct_all
for (i in 1:length(ct_all)) {
  ct_all[[i]] <- ct_all[[i]][order(ct_all[[i]]$timestamp),]  # ensure chronological order
  ct_all[[i]]$diff <- c(1000, diff(ct_all[[i]]$timestamp))
  ct_all[[i]] <- ct_all[[i]][ct_all[[i]]$diff > 120,]  # keep only entries >2 min apart
  
  # Convert to telemetry object
  ct_all[[i]] <- as.telemetry(ct_all[[i]], 
                              keep = c("Count", "Season", "StudyArea", "CameraLocation", 
                                       "AF", "AM", "AU", "OS", "UNK"))
}

# Match projections to tracking data
for (i in 1: length(ct_all)) {
  projection(ct_all[[i]]) <- projection(tracks_all[[i]])
}


### Preliminary visualization of CT data ----
# par(mfrow = c(2,1))  # Compare seasons

## Bobcat
png(file = "figures/bassing_etal_2022/bobcat_ct.png",
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
png(file = "figures/bassing_etal_2022/cougar_ct.png",
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
png(file = "figures/bassing_etal_2022/coyote_ct.png",
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
png(file = "figures/bassing_etal_2022/elk_ct.png",
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
png(file = "figures/bassing_etal_2022/md_ct.png",
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
png(file = "figures/bassing_etal_2022/wolf_ct.png",
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
png(file = "figures/bassing_etal_2022/wtd_ct.png",
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

## DEM layer only
test <- raster::readAll(dem30)  # large object
tictoc::tic()
bobcat_sdm <- sdm.fit(ct_all$bobcat_summer, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 62.834 sec elapsed
## ERROR: in if (sqrt(sum((RESCALE - par)^2)) > DIM * .Machine$double.eps) { : missing value where TRUE/FALSE needed
### only for list of raster layers without `raster::readAll`
## WARNING: In cov.loglike(hess, grad) : MLE is near a boundary or optimizer failed.
summary(bobcat_sdm)
## Weird output (due to warning?)
# $name
# [1] "inactive"
# 
# $DOF
# mean      area diffusion     speed 
# 0         0         0         0 
# 
# $CI
#                              low  est      high
# dem30 (1/dem30)      -0.02968246   0 0.02968246
# area (square meters)  0.00000000 Inf        Inf

any(is.na(raster::values(dem30)))  # not due to any NAs in the raster values
any(raster::values(dem30) == 0)  # there are zeros in the raster values
which(raster::values(dem30) == 0)  # only 5 zeros

## TEST
# Try akde on the CT data
bobcat_ctrange <- akde(data = ct_all$bobcat_summer, bobcat_sdm, R = list(dem30 = test), 
                       grid = list(dr = c(100,100)))
## Can't plot due to DOF[area] == 0

## Road density layer only
test <- raster::readAll(roads)  # large object
tictoc::tic()
bobcat_sdm2 <- sdm.fit(ct_all$bobcat_summer, R = list(roads = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 0.664 sec elapsed
## ERROR: in if (sqrt(sum((RESCALE - par)^2)) > DIM * .Machine$double.eps) { : missing value where TRUE/FALSE needed
## In addition: Warning messages:
# 1: In FUN(X[[i]], ...) : Objective function failure at c(roads=0)
# 2: In FUN(X[[i]], ...) : Objective function failure at c(roads=-0.0001220703125)
# 3: In FUN(X[[i]], ...) : Objective function failure at c(roads=0.0001220703125)
# Error in if (sqrt(sum((RESCALE - par)^2)) > DIM * .Machine$double.eps) { : missing value where TRUE/FALSE needed
# Error in if (sqrt(sum((RESCALE - par)^2)) > DIM * .Machine$double.eps) { : missing value where TRUE/FALSE needed
# Error in if (sqrt(sum((RESCALE - par)^2)) > DIM * .Machine$double.eps) { : missing value where TRUE/FALSE needed
# Error in if (sqrt(sum((RESCALE - par)^2)) > DIM * .Machine$double.eps) { : missing value where TRUE/FALSE needed
## WARNING: In cov.loglike(hess, grad) : MLE is near a boundary or optimizer failed.
summary(bobcat_sdm2)

## % Mixed forest layer only
test <- raster::readAll(forest18)  # large object
tictoc::tic()
bobcat_sdm3 <- sdm.fit(ct_all$bobcat_summer, R = list(forest18 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 1.745 sec elapsed
## WARNING:
# 1: In FUN(X[[i]], ...) : Objective function failure at c(=0, =0.00010714285459384)
# 2: In FUN(X[[i]], ...) : Objective function failure at c(=0, =0.000107131819795927)
# 3: In FUN(X[[i]], ...) : Objective function failure at c(=0, =0.000107159446570641)
summary(bobcat_sdm3)
## Weird output, can't get a finite area estimate? (from objective function failures?)
# $name
# [1] "inactive"
# 
# $DOF
# mean      area diffusion     speed 
# 0         0         0         0 
# 
# $CI
#                             low      est     high
# forest18 (1/forest18) -0.4821686 0.279223 1.040615
# area (square meters)   0.0000000      Inf      Inf

## TEST
# Try akde on the CT data
bobcat_ctrange <- akde(data = ct_all$bobcat_summer, bobcat_sdm3, R = list(forest18 = test), 
                       grid = list(dr = c(100,100)))
## Can't plot due to DOF[area] == 0


# TEST
## Using fake raster data w/ only positive data (and no zeros)
r <- dem30
raster::values(r) <- runif(length(raster::values(dem30)), min = 1, max = 50)
# raster::projection(r) <- ctmm::projection(ct_all$bobcat_summer)
plot(ct_all$bobcat_summer, R = r)

tictoc::tic()
bobcat_sdmfake <- sdm.fit(ct_all$bobcat_summer, R = list(fake = r), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 64.059 sec elapsed
## WARNING:
# In FUN(X[[i]], ...) :
#   Objective function failure at c(=0, =1.43992140007614e-06)
summary(bobcat_sdmfake)
## Even with a fake raster?
# $name
# [1] "inactive"
# 
# $DOF
# mean      area diffusion     speed 
# 0         0         0         0 
# 
# $CI
#                             low           est       high
# fake (1/fake)        -0.01360456 -0.0001886374 0.01322728
# area (square meters)  0.00000000           Inf        Inf


## TEST: Separate study sites

# Okanogan
bobcat_OKs <- ct_all$bobcat_summer[ct_all$bobcat_summer$StudyArea == "OK",]

## DEM layer only
test <- raster::readAll(dem30)  # large object
tictoc::tic()
bobcat_sdm_OKs <- sdm.fit(bobcat_OKs, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 66.461 sec elapsed
# 34 Warnings: In FUN(X[[i]], ...) : Objective function failure at c(=0, =7.13150492285974e-05)
summary(bobcat_sdm_OKs)  # infinite area

# Range distribution
bobcat_range_OKs <- akde(data = bobcat_OKs, CTMM = bobcat_sdm_OKs, R = list(dem30 = test))
## Warning: Fit object returned. DOF[area] = 0


# Northeast
bobcat_NEs <- ct_all$bobcat_summer[ct_all$bobcat_summer$StudyArea == "NE",]
# saveRDS(bobcat_NEs, file = "data/bassing_etal_2022_data/bobcat_ct_NEs.rds")  # save for easy access (debug)

## DEM layer only
test <- raster::readAll(dem30)  # large object
tictoc::tic()
bobcat_sdm_NEs <- sdm.fit(bobcat_NEs, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 64.034 sec elapsed
# 47 Warnings: In FUN(X[[i]], ...) : Objective function failure at c(=0, =3.56658396845821e-05)
summary(bobcat_sdm_NEs)  # infinite area

# Range distribution
bobcat_range_NEs <- akde(data = bobcat_NEs, CTMM = bobcat_sdm_NEs, R = list(dem30 = test))
## Warning: Fit object returned. DOF[area] = 0


### TEST: Telemetry ----

## DEM layer only
# test <- raster::readAll(dem30)  # large object
# bobcat_tel_sdm <- sdm.fit(tracks_all$bobcat_summer, R = list(dem30 = test), 
#                           integrator = "Riemann", trace = TRUE)
## SDM only for single telemetry object

# TODO: guess, select, akdes, pkde, pop rsf







## Cougar ----

# Camera trap SDM

## DEM layer only
test <- raster::readAll(dem30)  # large object
tictoc::tic()
cougar_sdm <- sdm.fit(ct_all$cougar_summer, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 62.834 sec elapsed
# No error, but no area
summary(cougar_sdm)

cougar_range_s <- akde(data = ct_all$cougar_summer, CTMM = cougar_sdm, R = list(dem30 = test))
## Warning: Fit object returned. DOF[area] = 0


## TEST: Separate study sites

# Okanogan
cougar_OKs <- ct_all$cougar_summer[ct_all$cougar_summer$StudyArea == "OK",]

## DEM layer only
# test <- raster::readAll(dem30)  # large object
tictoc::tic()
cougar_sdm_OKs <- sdm.fit(cougar_OKs, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 65.317 sec elapsed
# 50 Warnings: In FUN(X[[i]], ...) : Objective function failure at c(=0, =7.34751244689306e-05)
summary(cougar_sdm_OKs)  # infinite area

# Range distribution
cougar_range_OKs <- akde(data = cougar_OKs, CTMM = cougar_sdm_OKs, R = list(dem30 = test))
## Warning: Fit object returned. DOF[area] = 0


# Northeast
cougar_NEs <- ct_all$cougar_summer[ct_all$cougar_summer$StudyArea == "NE",]

## DEM layer only
# test <- raster::readAll(dem30)  # large object
tictoc::tic()
cougar_sdm_NEs <- sdm.fit(cougar_NEs, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 63.386 sec elapsed
# No error, but no area
summary(cougar_sdm_NEs)  # infinite area

# Range distribution
cougar_range_NEs <- akde(data = cougar_NEs, CTMM = cougar_sdm_NEs, R = list(dem30 = test))
## Warning: Fit object returned. DOF[area] = 0


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

## DEM layer only
# test <- raster::readAll(dem30)  # large object
tictoc::tic()
md_sdm <- sdm.fit(ct_all$md_summer, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 65.006 sec elapsed
# 9 Warnings: In FUN(X[[i]], ...) : Objective function failure at c(=0, =2.07499563349292e-05)
summary(md_sdm)  # infinite area

md_range_s <- akde(data = ct_all$md_summer, CTMM = md_sdm, R = list(dem30 = test))
## Warning: Fit object returned. DOF[area] = 0


## TEST: Separate study sites

# Okanogan
md_OKs <- ct_all$md_summer[ct_all$md_summer$StudyArea == "OK",]

## DEM layer only
# test <- raster::readAll(dem30)  # large object
tictoc::tic()
md_sdm_OKs <- sdm.fit(md_OKs, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 64.673 sec elapsed
# 10 Warnings: In FUN(X[[i]], ...) : Objective function failure at c(=0, =2.61896967401056e-05)
# Warning: In cov.loglike(hess, grad) : MLE is near a boundary or optimizer failed.
summary(md_sdm_OKs)  # infinite area

# Range distribution
md_range_OKs <- akde(data = md_OKs, CTMM = md_sdm_OKs, R = list(dem30 = test))
## Warning: Fit object returned. DOF[area] = 0


# Northeast
md_NEs <- ct_all$md_summer[ct_all$md_summer$StudyArea == "NE",]

## DEM layer only
# test <- raster::readAll(dem30)  # large object
tictoc::tic()
md_sdm_NEs <- sdm.fit(md_NEs, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 65.548 sec elapsed
# 2 Warnings: In FUN(X[[i]], ...) : Objective function failure at c(=0, =5.19261354631473e-05)
summary(md_sdm_NEs)  # infinite area

# Range distribution
md_range_NEs <- akde(data = md_NEs, CTMM = md_sdm_NEs, R = list(dem30 = test))
## Warning: Fit object returned. DOF[area] = 0


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

## DEM layer only
# test <- raster::readAll(dem30)  # large object
tictoc::tic()
bobcat_sdm_w <- sdm.fit(ct_all$bobcat_winter, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 62.41 sec elapsed
# No errors, no area
summary(bobcat_sdm_w)  # infinite area

bobcat_range_w <- akde(data = ct_all$bobcat_winter, CTMM = bobcat_sdm_w, R = list(dem30 = test))
## Warning: Fit object returned. DOF[area] = 0


## TEST: Separate study sites

# Okanogan
bobcat_OKw <- ct_all$bobcat_winter[ct_all$bobcat_winter$StudyArea == "OK",]

## DEM layer only
# test <- raster::readAll(dem30)  # large object
tictoc::tic()
bobcat_sdm_OKw <- sdm.fit(bobcat_OKw, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 63.184 sec elapsed
# No errors, no area
summary(bobcat_sdm_OKw)  # infinite area

# Range distribution
bobcat_range_OKw <- akde(data = bobcat_OKw, CTMM = bobcat_sdm_OKw, R = list(dem30 = test))
## Warning: Fit object returned. DOF[area] = 0


# Northeast
bobcat_NEw <- ct_all$bobcat_winter[ct_all$bobcat_winter$StudyArea == "NE",]

## DEM layer only
# test <- raster::readAll(dem30)  # large object
tictoc::tic()
bobcat_sdm_NEw <- sdm.fit(bobcat_NEw, R = list(dem30 = test), integrator = "Riemann", trace = TRUE)
tictoc::toc()  # 62.574 sec elapsed
# No errors, no area
summary(bobcat_sdm_NEw)  # infinite area

# Range distribution
bobcat_range_NEw <- akde(data = bobcat_NEw, CTMM = bobcat_sdm_NEw, R = list(dem30 = test))
## Warning: Fit object returned. DOF[area] = 0


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


