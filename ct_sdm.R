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
# ADD OTHER DATA LATER...
dem30 <- raster::raster("data/bassing_etal_2022_data/spatial data/DEM_30m.tif")  # SRTM digital elevation model


# Data wrangling ----

## Telemetry data ----

names(all_spp_tracks)  # list of tracking data grouped by species and season

# Shorten deer names for consistent naming scheme: "species_season"
names(all_spp_tracks)[1] <- "md_summer"  # "mule_deer_summer" -> "md_summer"
names(all_spp_tracks)[2] <- "md_winter"
names(all_spp_tracks)[5] <- "wtd_summer"  # "whitetailed_deer_summer" -> "wtd_summer"
names(all_spp_tracks)[6] <- "wtd_winter"

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


#### Preliminary visualization of tracking data ----
# par(mfrow = c(2,1), mai = c(1,0.8,0.8,0.3), omi = c(0,0.15,0.15,0.15))  # Compare seasons

# TODO:
## Fix individual track colors to color-match overlapping species across species

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
IID_md <- lapply(tracks_species$md, ctmm.fit)
KDE_md <- akde(tracks_species$md, IID_md, grid = list(dr = ))  # VERY SLOW
png(file = "figures/bassing_etal_2022/md_tracks_iid_kde.png",
    width = 4800, height = 6000, res = 600)
par(mfrow = c(2,1), mai = c(1,0.8,0.8,0.3), omi = c(0,0.15,0.15,0.15))  # Compare seasons
plot(tracks_all$md_summer, xlim = c(-150000,150000), ylim = c(-70000,90000), #xlim = c(-150000,150000), ylim = c(-50000,80000), # xlim = c(-140000,110000), ylim = c(-50000,70000), 
     col = color(KDE_md, by = "individual"))
title(main = "Summers 2018, 2019", line = 0.5)
mtext("Mule Deer Telemetry", line = 2, cex = 1.5, font = 2)
plot(tracks_all$md_winter, xlim = c(-150000,150000), ylim = c(-70000,90000), #xlim = c(-150000,150000), ylim = c(-50000,80000), # xlim = c(-140000,110000), ylim = c(-50000,70000),
     col = color(KDE_md, by = "individual"))
title(main = "Winter 2018/19, 2019/20", line = 0.5)
dev.off()
tictoc::toc()  # 676.447 sec elapsed


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

# Separate by study site for data cleaning
camtrap_NE <- all_spp_camtrap[all_spp_camtrap$StudyArea == "NE",]
camtrap_OK <- all_spp_camtrap[all_spp_camtrap$StudyArea == "OK",]

# Separate CT data into individual `telemetry` objects by species and season
## Separate summers and winters for each year
ct_seasons <- list(all_spp_camtrap[grepl("Summer", all_spp_camtrap$Season),], 
                   all_spp_camtrap[grepl("Winter", all_spp_camtrap$Season),])
## `telemetry` object for each species
ct_all <- list()
for (sp in species) {
  ct_all[[paste(sp, "summer", sep = "_")]] <- as.telemetry(ct_seasons[[1]][ct_seasons[[1]]$Species == sp,],
                                                           keep = c("Count", "Season", "StudyArea", "CameraLocation", 
                                                                    "AF", "AM", "AU", "OS", "UNK"))
  ct_all[[paste(sp, "winter", sep = "_")]] <- as.telemetry(ct_seasons[[2]][ct_seasons[[2]]$Species == sp,],
                                                           keep = c("Count", "Season", "StudyArea", "CameraLocation", 
                                                                    "AF", "AM", "AU", "OS", "UNK"))
}
ct_all <- ct_all[order(names(ct_all))]  # alphabetical order

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


## Cougar ----


## Coyote ----


## Elk ----


## Mule Deer ----


## Wolf ----


## White-Tailed Deer ----



# Winter 2018/19, 2019/20 ----

## Bobcat ----


## Cougar ----


## Coyote ----


## Elk ----


## Mule Deer ----


## Wolf ----


## White-Tailed Deer ----



