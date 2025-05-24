# Generating figures for OD vs CRD

# Load `ctmm` package
library(ctmm)

# Mule Deer ----

# Mule deer (Odocoileus hemionus) migration data
# Wang, Yi-Shan; Blackwell, Paul G.; Merkle, Jerod A.; Potts, Jonathan R. (2019). 
# Data from: Continuous time resource selection analysis for moving animals [Dataset]. Dryad. 
# https://doi.org/10.5061/dryad.f9p3dq4
data <- read.csv("data/mule_deer/MuleDeer_Cody_ForPotts.csv")  
colnames(data)[1] <- "ID"  # change AID column to ID for `as.telemetry()` to work
data <- as.telemetry(data)  # convert to telemetry object (drops uneeded columns)
class(data[[1]])  # `data` is now a list of telemetry objects

# Sampling summary
summary(data)
## sampling interval: 2 hours

# Projection (class "telemetry")
median(data)
projection(data) <- median(data)  # center projection on geometric median of data
projection(data)

par(mfrow = c(2,1), mai = c(0.8, 0.8, 0.45, 0.4) + 0.02)
# Plot telemetry object with North as up
plot(data, col = color(data, by = "individual"), 
     main = "Mule Deer Migration Paths") 
compass(loc = c(92000, -46000), cex = 1.7)  
## add compass point for North (uses Azimuthal-equidistant projection)

# Plot with migration paths colored by time
plot(data, col = color(data, by = "time"))
compass(loc = c(92000, -46000), cex = 1.7) 
## clear gradient migration over time

# Subset data
deer <- data[[1]]  # ID: 36823
summary(deer)  # inspect data

# Variogram of data
SVF <- variogram(deer, CI = "Gauss")
plot(SVF, main = "Variogram of Individual 36823") 
## Sill/asymptote = roughly 750 km^2
## Range = about 3 months (autocorrelated until about 3 months)
## Slight initial curvature, slow to asymptote (very shallow slope, almost linear)
## No nugget


## Single Corridor ----

# Subset data to single corridor
DATA <- data[c('36827', '36831', '36840', '36935', '36999', '37009', '37010')]
projection(DATA) <- median(DATA)  # center projection on geometric median of data

# Store subset of data separately
datasub <- data[c('36827', '36831', '36840', '36935', '36999', '37009', '37010')]

# Plot telemetry object with North as up
par(mfrow = c(2,1), mai = c(0.8, 0.8, 0.45, 0.4) + 0.02)
plot(DATA, col = color(DATA, by = "individual"), main = "Mule Deer Migration Paths") 
compass(loc = c(86000, -10000), cex = 1.7)  # add compass point for North
plot(DATA, col = color(DATA, by = "time")) 
compass(loc = c(86000, -10000), cex = 1.7)

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
par(mfrow = c(4,2))
plot(DATA[[1]]$t, xlab = NULL, ylab = "Time index", main = names(DATA)[[1]])
plot(DATA[[2]]$t, xlab = NULL, ylab = "Time index", main = names(DATA)[[2]])
plot(DATA[[3]]$t, xlab = NULL, ylab = "Time index", main = names(DATA)[[3]])
plot(DATA[[4]]$t, xlab = NULL, ylab = "Time index", main = names(DATA)[[4]])
plot(DATA[[5]]$t, xlab = NULL, ylab = "Time index", main = names(DATA)[[5]])
plot(DATA[[6]]$t, xlab = NULL, ylab = "Time index", main = names(DATA)[[6]])
plot(DATA[[7]]$t, xlab = NULL, ylab = "Time index", main = names(DATA)[[7]])

# Remove outliers
DATA[[5]] <- DATA[[5]][-which(DATA[[5]]$t == max(DATA[[5]]$t)),]
DATA[[7]] <- DATA[[7]][-which(DATA[[7]]$t == min(DATA[[7]]$t)),]

summary(DATA)  # inspect data

projection(DATA) <- median(DATA)  # re-center projection

# Plot cleaned data
par(mfrow = c(1,1))
plot(DATA, col = color(DATA, by = "individual"), 
     main = "Migration Paths without Range Residency")  # no more range residency
compass(loc = c(35000, -15000), cex = 1.7) 


# Sampling frequency/interval ----

## Model Fitting ----

# Model estimate and selection
FITS <- list()  # empty list
for (i in 1:length(DATA)) {  # for each individual
  
  # Estimate model parameters
  GUESS <- ctmm.guess(DATA[[i]], interactive = FALSE)  # interactive mode is manual
  # Model selection
  FIT <- ctmm.select(DATA[[i]], GUESS, trace = 3, verbose = TRUE)  # fits multiple models
  FITS[[i]] <- FIT  # store model selection results
}
names(FITS) <- names(DATA)  # match names of list items

# save(FITS, file = "data/mule_deer/fits_corridor.rda")  # single longest step
load(file = "data/mule_deer/fits_corridor.rda")

# Inspect model selection results for each individual
summary(FITS$`36827`)
summary(FITS$`36831`)
summary(FITS$`36840`)
summary(FITS$`36935`)
summary(FITS$`36999`)
summary(FITS$`37009`)
summary(FITS$`37010`)

## Anisotropic Ornstein-Uhlenbeck foraging model selected for all individuals

# Extract and store the best fit for each individual
corfits <- list()
for (i in 1:length(FITS)) {
  corfits[[i]] <- FITS[[i]]$`OUF anisotropic`
}

### Occurrence Distribution ----

# Duplicate data for manipulation (coarsening)
DATA2 <- DATA

# Generate occurrence distributions
OCC <- list()  # empty list
OCC[[1]] <- occurrence(DATA2, corfits)
OCC[[1]] <- mean(OCC[[1]])  # average distributions

# Calculate number of times to coarsen data
log2(min(sapply(DATA2, nrow)))  # about 6 times

# Generate occurrence distribution for each subset of data
for (m in 2:6) {
  
  # Remove every other location to double sampling interval
  for (i in 1:length(DATA2)) { 
    DATA2[[i]] <- DATA2[[i]][as.logical(1:nrow(DATA2[[i]])%%2),]
  }
  
  # Occurrence distribution
  OCC[[m]] <- occurrence(DATA2, corfits)
  OCC[[m]] <- mean(OCC[[m]])
}

# save(OCC, file = "data/mule_deer/occurrence_sensitivity.rda")
load(file = "data/mule_deer/occurrence_sensitivity.rda")

# Combined figure
# png(file = "figures/occurrence_comb_2x2.png", width = 6.5, height = 5.1, units = "in", res = 600)
par(mfrow = c(2,2), mai = c(0.8,0.3,0.7,0), omi = c(0,0.25,0,0.25),
    mar = c(4,4,2,0.4) + 0.1, cex.main = 1.2)
# png(file = "figures/occurrence_comb_H.png", width = 9, height = 5.55, units = "in", res = 600)
# par(mfrow = c(2,3), mai = c(0.6,0.5,0.45,0.15), omi = c(0,0.25,0,0.25), cex.main = 1.4)
# Plot all distributions
plot(OCC[[1]], main = "2-Hour")
# plot(OCC[[2]], main = "4-Hour")
plot(OCC[[3]], main = "8-Hour")
plot(OCC[[4]], main = "16-Hour")
# plot(OCC[[5]], main = "32-Hour")
plot(OCC[[6]], main = "64-Hour")
# dev.off()


### Corridor Distribution ----

# Duplicate data for manipulation (coarsening)
DATA3 <- DATA

# Generate corridor range distribution
COR <- list()  # empty list
COR[[1]] <- ctmm:::corridor(data = DATA, CTMM = corfits)

# Generate range distribution for each subset of data
for (m in 2:6) {
  
  # Remove every other location to double sampling interval
  for (i in 1:length(DATA3)) { 
    DATA3[[i]] <- DATA3[[i]][as.logical(1:nrow(DATA3[[i]])%%2),]
  }
  
  # Corridor range distribution
  COR[[m]] <- ctmm:::corridor(data = DATA3, CTMM = corfits)
}

# save(COR, file = "data/mule_deer/corridor_sensitivity.rda")
load(file = "data/mule_deer/corridor_sensitivity.rda")

# Combined figure
# png(file = "figures/corridor_comb_2x2.png", width = 6.5, height = 5.1, units = "in", res = 600)
par(mfrow = c(2,2), mai = c(0.8,0.3,0.7,0), omi = c(0,0.25,0,0.25),
    mar = c(4,4,2,0.4) + 0.1, cex.main = 1.2)
# png(file = "figures/corridor_comb_H.png", width = 9, height = 5.55, units = "in", res = 600)
# par(mfrow = c(2,3), mai = c(0.6,0.5,0.45,0.15), omi = c(0,0.25,0,0.25), cex.main = 1.4)
# Plot all distributions
plot(COR[[1]], main = "2-Hour")
# plot(COR[[2]], main = "4-Hour")
plot(COR[[3]], main = "8-Hour")
plot(COR[[4]], main = "16-Hour")
# plot(COR[[5]], main = "32-Hour")
plot(COR[[6]], main = "64-Hour")
# dev.off()

# Reorder plots to compare ODs and Corridor distributions
# png(file = "figures/sensitivity_comparison_interval.png", width = 4800, height = 6000, units = "px", res = 600)
# par(mfrow = c(3,2), mai = c(0.8,0.3,0.7,0), omi = c(0,0.25,0,0.25), mar = c(4,4,2,0.4) + 0.1, cex.main = 1.2)
png(file = "figures/sensitivity_comparison_interval.png", width = 4800, height = 6000, units = "px", res = 600)
par(mfrow = c(3,2), mai = c(0.6,0.5,0.45,0.15), omi = c(0,0.25,0.25,0.25), cex.main = 1.2)
# Plot distributions
plot(OCC[[1]], main = "2-Hour")
# title("Occurrence Distribution")
mtext(substitute(paste(bold("Occurrence Distribution"))), line = 3, side = 3)
plot(COR[[1]], main = "2-Hour")
# title("Corridor Distribution")
mtext(substitute(paste(bold("Corridor Distribution"))), line = 3, side = 3)
plot(OCC[[4]], main = "16-Hour")
plot(COR[[4]], main = "16-Hour")
plot(OCC[[6]], main = "64-Hour")
plot(COR[[6]], main = "64-Hour")
dev.off()


# Sampling duration ----

## Model Fitting ----

# Model estimate and selection (same as above)
# save(FITS, file = "data/mule_deer/fits_corridor.rda")  # single longest step
load(file = "data/mule_deer/fits_corridor.rda")

# Inspect model selection results for each individual
summary(FITS$`36827`)
summary(FITS$`36831`)
summary(FITS$`36840`)
summary(FITS$`36935`)
summary(FITS$`36999`)
summary(FITS$`37009`)
summary(FITS$`37010`)

## Anisotropic Ornstein-Uhlenbeck foraging model selected for all individuals

### Occurrence Distribution ----

# Duplicate data for manipulation (shortening)
DATA3 <- DATA

# Generate occurrence distributions
OCC <- list()  # empty list
OCC[[1]] <- occurrence(DATA3, corfits)
OCC[[1]] <- mean(OCC[[1]])  # average distributions

# # Calculate number of times to halve data
log2(min(sapply(DATA3, nrow)))  # about 6 times

# Generate occurrence distribution for each subset of data
for (m in 2:6) {
  
  # Remove second half of data to shorten sampling duration
  for (i in 1:length(DATA3)) { 
    DATA3[[i]] <- DATA3[[i]][1:round(nrow(DATA3[[i]])/2),]
  }
  
  # Occurrence distribution
  OCC[[m]] <- occurrence(DATA3, corfits)
  OCC[[m]] <- mean(OCC[[m]])
}

# save(OCC, file = "data/mule_deer/occurrence_sensitivity_duration.rda")
load(file = "data/mule_deer/occurrence_sensitivity_duration.rda")

# Combined figure
# png(file = "figures/occurrence_comb_2x2.png", width = 6.5, height = 5.1, units = "in", res = 600)
# par(mfrow = c(2,2), mai = c(0.8,0.3,0.7,0), omi = c(0,0.25,0,0.25),
#     mar = c(4,4,2,0.4) + 0.1, cex.main = 1.2)
png(file = "figures/occurrence_comb_H_duration.png", width = 9, height = 5.55, units = "in", res = 600)
par(mfrow = c(2,3), mai = c(0.6,0.5,0.45,0.15), omi = c(0,0.25,0,0.25), cex.main = 1.4)
# Plot all distributions
plot(OCC[[1]], main = "1/2", xlim = c(-25000,35000), ylim = c(-20000,20000))
plot(OCC[[2]], main = "1/4", xlim = c(-25000,35000), ylim = c(-20000,20000))  #
plot(OCC[[3]], main = "1/8", xlim = c(-25000,35000), ylim = c(-20000,20000))
plot(OCC[[4]], main = "1/16", xlim = c(-25000,35000), ylim = c(-20000,20000))
plot(OCC[[5]], main = "1/32", xlim = c(-25000,35000), ylim = c(-20000,20000))  #
plot(OCC[[6]], main = "1/64", xlim = c(-25000,35000), ylim = c(-20000,20000))
dev.off()


### Corridor Distribution ----

# Duplicate data for manipulation (coarsening)
DATA3 <- DATA

# Generate corridor range distribution
COR <- list()  # empty list
COR[[1]] <- ctmm:::corridor(data = DATA3, CTMM = corfits)

# Generate range distribution for each subset of data
for (m in 2:6) {
  
  # Remove every other location to double sampling interval
  for (i in 1:length(DATA3)) { 
    DATA3[[i]] <- DATA3[[i]][1:round(nrow(DATA3[[i]])/2),]
  }
  
  # Corridor range distribution
  COR[[m]] <- ctmm:::corridor(data = DATA3, CTMM = corfits)
}

# save(COR, file = "data/mule_deer/corridor_sensitivity_duration.rda")
load(file = "data/mule_deer/corridor_sensitivity_duration.rda")

# Combined figure
# png(file = "figures/corridor_comb_2x2.png", width = 6.5, height = 5.1, units = "in", res = 600)
# par(mfrow = c(2,2), mai = c(0.8,0.3,0.7,0), omi = c(0,0.25,0,0.25),
#     mar = c(4,4,2,0.4) + 0.1, cex.main = 1.2)
png(file = "figures/corridor_comb_H_duration.png", width = 9, height = 5.55, units = "in", res = 600)
par(mfrow = c(2,3), mai = c(0.6,0.5,0.45,0.15), omi = c(0,0.25,0,0.25), cex.main = 1.4)
# Plot all distributions
plot(COR[[1]], main = "1/2", xlim = c(-25000,35000), ylim = c(-20000,20000))
plot(COR[[2]], main = "1/4", xlim = c(-25000,35000), ylim = c(-20000,20000))
plot(COR[[3]], main = "1/8", xlim = c(-25000,35000), ylim = c(-20000,20000))
plot(COR[[4]], main = "1/16", xlim = c(-25000,35000), ylim = c(-20000,20000))
plot(COR[[5]], main = "1/32", xlim = c(-25000,35000), ylim = c(-20000,20000))
plot(COR[[6]], main = "1/64", xlim = c(-25000,35000), ylim = c(-20000,20000))
dev.off()


# Reorder plots to compare ODs and Corridor distributions
# png(file = "figures/sensitivity_comparison_duration.png", width = 4800, height = 6000, units = "px", res = 600)
# par(mfrow = c(3,2), mai = c(0.8,0.3,0.7,0), omi = c(0,0.25,0,0.25), mar = c(4,4,2,0.4) + 0.1, cex.main = 1.2)
png(file = "figures/sensitivity_comparison_duration.png", width = 4800, height = 6000, units = "px", res = 600)
par(mfrow = c(3,2), mai = c(0.6,0.5,0.45,0.15), omi = c(0,0.25,0.25,0.25), cex.main = 1.2)
# Plot distributions
plot(OCC[[1]], main = "1/2", xlim = c(-25000,35000), ylim = c(-20000,20000))
mtext(substitute(paste(bold("Occurrence Distribution"))), line = 3, side = 3)
plot(COR[[1]], main = "1/2", xlim = c(-25000,35000), ylim = c(-20000,20000))
mtext(substitute(paste(bold("Corridor Distribution"))), line = 3, side = 3)
plot(OCC[[4]], main = "1/16", xlim = c(-25000,35000), ylim = c(-20000,20000))
plot(COR[[4]], main = "1/16", xlim = c(-25000,35000), ylim = c(-20000,20000))
plot(OCC[[6]], main = "1/64", xlim = c(-25000,35000), ylim = c(-20000,20000))
plot(COR[[6]], main = "1/64", xlim = c(-25000,35000), ylim = c(-20000,20000))
dev.off()


# Jaguars ----
# Morato, Ronaldo G. Jaguar Conservation in the Caatinga Biome Citation
# Acknowledgements: Gediendson Araujo, Leanes Silva, Valdomiro Lemos, Antonio Carlos Csemark 
# Grants Used: FAPESP 2013/10029-6, FAPESP 2014/24921-0 License Type: Custom License 
# Terms: No study-specific terms specified Principal Investigator
# Data retrieved from Movebank
jaguar <- read.csv("data/jaguar/Jaguar Conservation in the Caatinga Biome.csv")



