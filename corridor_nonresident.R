# Impact of Resident vs Non-Resident Model on Corridor width

# Load ctmm
library(ctmm)

# Mule deer (Odocoileus hemionus) migration data
# Wang, Yi-Shan; Blackwell, Paul G.; Merkle, Jerod A.; Potts, Jonathan R. (2019). 
# Data from: Continuous time resource selection analysis for moving animals [Dataset]. Dryad. 
# https://doi.org/10.5061/dryad.f9p3dq4
data <- as.telemetry("data/mule_deer/MuleDeer_Cody_ForPotts.csv")  
class(data[[1]])  # `data` is now a list of telemetry objects
summary(data)

projection(data) <- median(data)  # center projection on geometric median of data

# Subset data
deer <- data[[1]]  # ID: 36823
summary(deer)  # inspect data

# Variogram of data
SVF <- variogram(deer, CI = "Gauss")
plot(SVF, main = "Variogram of Individual 36823")
## some ranging behavior, but these are not targeted for the corridor distribution


## Single Corridor ----

## See `corridor_sensitivity.R` for more detailed subset process

# Subset data to single corridor
DATA <- data[c('36827', '36831', '36840', '36935', '36999', '37009', '37010')]
projection(DATA) <- median(DATA)  # center projection on geometric median of data

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

# Remove outliers
DATA[[5]] <- DATA[[5]][-which(DATA[[5]]$t == max(DATA[[5]]$t)),]
DATA[[7]] <- DATA[[7]][-which(DATA[[7]]$t == min(DATA[[7]]$t)),]

summary(DATA)  # inspect data
projection(DATA) <- median(DATA)  # re-center projection

# Plot subset data (single corridor)
plot(DATA, col = color(DATA, by = "individual"), 
     main = "Migration Paths without Range Residency",  # no more range residency
     xlim = c(-22000,32000), ylim = c(-20000,20000))
compass(loc = c(30000, -16000), cex = 1.7)


## Model Fitting ----

# Fit and select among range resident models
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

# Fit and select among non-resident models
FITS_nonres <- list()  # empty list
for (i in 1:length(DATA)) {  # for each individual
  
  # Estimate model parameters (set `range = FALSE`)
  GUESS <- ctmm.guess(DATA[[i]], ctmm(range = FALSE), interactive = FALSE)  # interactive mode is manual
  
  # Model selection
  FIT_nonres <- ctmm.select(DATA[[i]], GUESS, trace = 3, verbose = TRUE)  # fits multiple models
  FITS_nonres[[i]] <- FIT_nonres  # store model selection results
}
names(FITS_nonres) <- names(DATA)  # match names of list items

# save(FITS_nonres, file = "data/mule_deer/fits_nonres_corridor.rda")  # single longest step
load(file = "data/mule_deer/fits_nonres_corridor.rda")

# Inspect model selection results for each individual
summary(FITS_nonres$`36827`)
summary(FITS_nonres$`36831`)
summary(FITS_nonres$`36840`)
summary(FITS_nonres$`36935`)
summary(FITS_nonres$`36999`)
summary(FITS_nonres$`37009`)
summary(FITS_nonres$`37010`)

# Extract and store the best fit for each individual
corfits <- list()
for (i in 1:length(FITS_nonres)) {
  corfits[[i]] <- FITS_nonres[[i]][[1]]
}


### Corridor Distribution ----

# Duplicate data for manipulation (coarsening)
DATA3 <- DATA

# Generate corridor range distribution
COR <- list()  # empty list
COR[[1]] <- ctmm:::corridor(data = DATA, CTMM = corfits, res.time = 4, grid = list(dr = c(100,100)))
# COR[[1]] <- corridor(data = DATA, CTMM = corfits, grid = list(dr = c(100,100)))

# Generate range distribution for each subset of data
for (m in 2:6) {
  
  # Remove every other location to double sampling interval
  for (i in 1:length(DATA3)) { 
    DATA3[[i]] <- DATA3[[i]][as.logical(1:nrow(DATA3[[i]])%%2),]
  }
  
  # Corridor range distribution
  COR[[m]] <- ctmm:::corridor(data = DATA3, CTMM = corfits, res.time = 4, grid = list(dr = c(100,100)))
  # COR[[m]] <- corridor(data = DATA3, CTMM = corfits, grid = list(dr = c(100,100)))
}

save(COR, file = "data/mule_deer/corridor_sensitivity_nonres.rda")






### FIXME:
# Error in seq.default(EXT[1, i] - dr[i], EXT[2, i] + dr[i], length.out = 1 +  : 
#                        'from' must be a finite number





