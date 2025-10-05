#########################
# CORRIDOR ESTIMATION
#########################

library(ctmm)

?ctmm:::corridor()
# Function for estimating corridor shape and width
## "range" distribution for transitory movement (i.e. dispersal, migration, repeated passages)
## Arguments similar to `adke()` function for home-range estimation
?ctmm::akde
## Auto-correlated KDE using telemetry data and continuous-time movement model


######################################
# Data handling and visualization ----
######################################

# Mule deer (Odocoileus hemionus) migration data
# Wang, Yi-Shan; Blackwell, Paul G.; Merkle, Jerod A.; Potts, Jonathan R. (2019). 
# Data from: Continuous time resource selection analysis for moving animals [Dataset]. Dryad. 
# https://doi.org/10.5061/dryad.f9p3dq4
data <- as.telemetry("data/mule_deer/MuleDeer_Cody_ForPotts.csv")  
class(data[[1]])  # now a list of telemtry objects
summary(data)

# Center projection on geometric median of data
projection(data) <- median(data)  

par(mfrow = c(2,1), mai = c(0.8, 0.8, 0.45, 0.4) + 0.02)  # plotting parameters

# Plot telemetry object with North as up
plot(data, col = color(data, by = "individual"), 
     main = "Mule Deer Migration Paths") 
compass(loc = c(92000, -46000), cex = 1.5)  
## add compass point for North (uses Azimuthal-equidistant projection)

# Plot with migration paths colored by time
plot(data, col = color(data, by = "time"))
compass(loc = c(92000, -46000), cex = 1.5) 
## clear gradient migration over time (red to blue)

# Subset data to single corridor
DATA <- data[c('36827', '36831', '36840', '36935', '36999', '37009', '37010')]
projection(DATA) <- median(DATA)  # re-center projection 

# Plot telemetry object with North as up
plot(DATA, col = color(DATA, by = "individual"), main = "Mule Deer Migration Paths") 
compass(loc = c(86000, -7000), cex = 1.5)  # add compass point for North
plot(DATA, col = color(DATA, by = "time")) 
compass(loc = c(86000, -7000), cex = 1.5)

# Inspect single individual
deer <- DATA[[4]]

# Variogram of single deer
SVF <- variogram(deer, CI = "Gauss")
par(mfrow = c(1,1))  # reset plot parameters
plot(SVF, main = "Variogram: ID 36935")  # asymptote?

# Plot subsetted individual paths
plot(DATA, col = color(DATA, by = "individual")) +
  abline(v = c(19, 72), lty = 2)  # lines visually mark cut-offs for migration behaviour

# Keep data within a certain range of distance along x-axis
for (i in 1:length(DATA)) {
  DATA[[i]] <- DATA[[i]][DATA[[i]]$x > 19*1000,]
  DATA[[i]] <- DATA[[i]][DATA[[i]]$x < 72*1000,]
}

# Diagnose sampling schedule
dt.plot(DATA)  # mostly 2-hr sampling

# Search for major outliers in sample
par(mfrow = c(3,2))
plot(DATA[[1]]$t, xlab = NULL, ylab = "Time index", main = names(DATA)[[1]])
plot(DATA[[2]]$t, xlab = NULL, ylab = "Time index", main = names(DATA)[[2]])
plot(DATA[[3]]$t, xlab = NULL, ylab = "Time index", main = names(DATA)[[3]])
plot(DATA[[4]]$t, xlab = NULL, ylab = "Time index", main = names(DATA)[[4]])
plot(DATA[[5]]$t, xlab = NULL, ylab = "Time index", main = names(DATA)[[5]])
plot(DATA[[6]]$t, xlab = NULL, ylab = "Time index", main = names(DATA)[[6]])

# Remove outliers
DATA[[5]] <- DATA[[5]][-which(DATA[[5]]$t == max(DATA[[5]]$t)),]
DATA[[7]] <- DATA[[7]][-which(DATA[[7]]$t == min(DATA[[7]]$t)),]
## irregular sampling due to crude segmentation

summary(DATA)  # inspect data
projection(DATA) <- median(DATA)  # re-center projection

# Plot segmented data
par(mfrow = c(1,1))
plot(DATA, col = color(DATA, by = "individual"), 
     main = "Migration Paths without Range Residency",  # no more range residency
     xlim = c(-22000,32000), ylim = c(-20000,20000))
plot(DATA, col = color(DATA, by = "individual"), 
     main = "Migration Paths without Range Residency", error = FALSE, pch = 16, cex = 0.65,
     xlim = c(-22000,32000), ylim = c(-20000,20000))


#######################
# Model Selection ----
#######################

# Model guesstimate and selection (single longest step)
FITS <- list()  # empty list
for (i in 1:length(DATA)) {  # for each individual
  
  # Estimate model parameters
  GUESS <- ctmm.guess(DATA[[i]], interactive = FALSE)  # interactive mode is manual
  # Model selection
  FIT <- ctmm.select(DATA[[i]], GUESS, trace = 3, verbose = TRUE)  # fits multiple models
  FITS[[i]] <- FIT  # store model selection results
}
names(FITS) <- names(DATA)  # match names of list items

load(file = "data/mule_deer/fits_corridor.rda")

# Inspect model selection results for each individual
summary(FITS$`36827`)
summary(FITS$`36831`)
summary(FITS$`36840`)
summary(FITS$`36935`)
summary(FITS$`36999`)
summary(FITS$`37009`)
summary(FITS$`37010`)

## Anisotropic Ornstein-Uhlenbeck foraging (OUF) model selected for all individuals
### Ideally use the non-resident models (range = FALSE)

# Extract and store the best fit for each individual
corfits <- list()
for (i in 1:length(FITS)) {
  corfits[[i]] <- FITS[[i]]$`OUF anisotropic`
}


#############################
# Corridor Distribution ----
#############################

# Generate corridor range distribution (cross-sectional KDE)
COR <- list()  # empty list
COR[[1]] <- ctmm:::corridor(data = DATA, CTMM = corfits, res.time = 4)
## will suggest a better temporal resolution

# Can be slow
load(file = "data/mule_deer/corridor_sensitivity_fix_tres.rda")

# Plot corridor with tracks
plot(x = DATA, col = color(DATA, by = "individual"), 
     main = "Corridor Distribution",
     xlim = c(-22000,32000), ylim = c(-20000,20000))
plot(COR[[1]], level.UD = NA, add = TRUE)

# Add 95% contour
plot(x = DATA, col = color(DATA, by = "individual"), 
     main = "Corridor Distribution",
     xlim = c(-22000,32000), ylim = c(-20000,20000))
plot(COR[[1]], add = TRUE)


# Compare to occurrence distribution
OCC <- list()  # empty list
OCC[[1]] <- occurrence(DATA, corfits)
OCC[[1]] <- mean(OCC[[1]])  # average distributions

load(file = "data/mule_deer/occurrence_sensitivity.rda")

# Plot occurrence distribution
plot(x = DATA, col = color(DATA, by = "individual"),
     main = "Occurrence Distribution",
     xlim = c(-22000,32000), ylim = c(-20000,20000))
plot(OCC[[1]], level.UD = NA, add = TRUE)

# Add 95% contour
plot(x = DATA, col = color(DATA, by = "individual"), 
     main = "Occurrence Distribution",
     xlim = c(-22000,32000), ylim = c(-20000,20000))
plot(OCC[[1]], add = TRUE)


# #####################################
# # Is there sampling dependency? ----
# #####################################
# 
# # Duplicate data for manipulation (coarsening)
# DATA2 <- DATA
# 
# # Calculate number of times to coarsen data
# log2(min(sapply(DATA2, nrow)))  # about 6 times
# 
# # Corridor
# # Generate range distribution for each subset of data
# for (m in 2:6) {
#   
#   # Remove every other location to double sampling interval
#   for (i in 1:length(DATA2)) { 
#     DATA2[[i]] <- DATA2[[i]][as.logical(1:nrow(DATA2[[i]])%%2),]
#   }
#   
#   # Corridor range distribution
#   COR[[m]] <- ctmm:::corridor(data = DATA2, CTMM = corfits, res.time = 4)
# }
# # save(COR, file = "data/mule_deer/corridor_sensitivity_fix_tres_full.rda")
# load(file = "data/mule_deer/corridor_sensitivity_fix_tres.rda")  # lower resolution version
# 
# 
# # Occurrence
# # Generate occurrence distribution for each subset of data
# for (m in 2:6) {
#   
#   # Remove every other location to double sampling interval
#   for (i in 1:length(DATA2)) { 
#     DATA2[[i]] <- DATA2[[i]][as.logical(1:nrow(DATA2[[i]])%%2),]
#   }
#   
#   # Occurrence distribution
#   OCC[[m]] <- occurrence(DATA2, corfits)
#   OCC[[m]] <- mean(OCC[[m]])
# }
# 
# load(file = "data/mule_deer/occurrence_sensitivity.rda")
# 
# # Compare the resulting distributions
# par(mfrow = c(3,2), mai = c(0.6,0.5,0.45,0.15), omi = c(0,0.25,0.25,0.25), cex.main = 1.2)
# # Plot distributions
# plot(OCC[[1]], main = "2-Hour", xlim = c(-22000,32000), ylim = c(-20000,20000))
# # title("Occurrence Distribution")
# mtext(substitute(paste(bold("Occurrence Distribution"))), line = 3, side = 3)
# plot(COR[[1]], main = "2-Hour", xlim = c(-22000,32000), ylim = c(-20000,20000))
# # title("Corridor Distribution")
# mtext(substitute(paste(bold("Corridor Distribution"))), line = 3, side = 3)
# plot(OCC[[2]], main = "4-Hour", xlim = c(-22000,32000), ylim = c(-20000,20000))
# plot(COR[[2]], main = "4-Hour", xlim = c(-22000,32000), ylim = c(-20000,20000))
# plot(OCC[[3]], main = "8-Hour", xlim = c(-22000,32000), ylim = c(-20000,20000))
# plot(COR[[3]], main = "8-Hour", xlim = c(-22000,32000), ylim = c(-20000,20000))
# 
# 


# # Occurrence distribution
# par(mfrow = c(2,3), mai = c(0.6,0.5,0.45,0.15), omi = c(0,0.25,0,0.25), cex.main = 1.4)
# # Plot all distributions
# plot(OCC[[1]], main = "2-Hour")
# plot(OCC[[2]], main = "4-Hour")
# plot(OCC[[3]], main = "8-Hour")
# plot(OCC[[4]], main = "16-Hour")
# plot(OCC[[5]], main = "32-Hour")
# plot(OCC[[6]], main = "64-Hour")
# 
# # Corridor distribution
# par(mfrow = c(2,3), mai = c(0.6,0.5,0.45,0.15), omi = c(0,0.25,0,0.25), cex.main = 1.4)
# # Plot all distributions
# plot(COR[[1]], main = "2-Hour")
# plot(COR[[2]], main = "4-Hour")
# plot(COR[[3]], main = "8-Hour")
# plot(COR[[4]], main = "16-Hour")
# plot(COR[[5]], main = "32-Hour")
# plot(COR[[6]], main = "64-Hour")


