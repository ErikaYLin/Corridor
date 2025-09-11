# Figures for corridor demos

# Load `ctmm` package
library(ctmm)

# Load packages
library(plot3D)

# 3D corridor distribution ----





# Testing for plots at different resolutions ----

## Jaguar data cleaning ----

# Import jaguar data
jaguar <- read.csv("data/jaguar/Jaguar Conservation in the Caatinga Biome.csv")
jaguar <- as.telemetry(jaguar)

# Subset data
jag1 <- jaguar[[1]]  # ID: Courisco
summary(jag1)  # inspect data
projection(jag1) <- median(jag1)

# Keep data within a certain range of distance along x-axis
data <- jag1[jag1$x > 7*1000,]

# Delineate separate paths based on time index
plot(x = data$x, y = data$t, col = color(data, by = "time"), 
     xlab = "X (m)", ylab = "Time (s)") +
  abline(h = 1.459e9, lty = 2) +
  abline(h = 1.458e9, lty = 2) +
  abline(h = 1.455e9, lty = 2)

# Split data into separate paths (effectively 4 complete passages through corridor)
DATA <- list()
DATA[[1]] <- data[data$t <= 1.455e9,]
DATA[[2]] <- data[data$t > 1.455e9 & data$t <= 1.458e9,]
DATA[[3]] <- data[data$t > 1.458e9 & data$t <= 1.459e9,]
DATA[[4]] <- data[data$t > 1.459e9,]

summary(DATA)  # 1-hr sampling over several (10-26) days each
projection(DATA) <- median(DATA)

## Refit corridor model at lower resolution ----

# Model estimate and selection results
load(file = "data/jaguar/fit_courisco_4paths.rda")

# Inspect model selection results for each individual
summary(FITS[[1]])
summary(FITS[[2]])
summary(FITS[[3]])
summary(FITS[[4]])

# Extract and store the best fit for each individual
corfits <- list()
for (i in 1:length(FITS)) {
  corfits[[i]] <- FITS[[i]]$`OUF anisotropic`
}

# Duplicate data for manipulation (coarsening)
DATA3 <- DATA
corfits2 <- corfits

# Generate corridor range distribution
COR <- list()  # empty list
COR[[1]] <- ctmm:::corridor(data = DATA3, CTMM = corfits2, res.space = 5)

# Generate range distribution for each subset of data

# Remove an individual path each loop
DATA3 <- DATA3[-1]
corfits2 <- corfits2[-1]

# Corridor range distribution
COR[[2]] <- ctmm:::corridor(data = DATA3, CTMM = corfits2, res.space = 5)

# Remove an individual path each loop
DATA3 <- DATA3[-1]
corfits2 <- corfits2[-1]

COR[[3]] <- ctmm:::corridor(data = DATA3, CTMM = corfits2, res.space = 1)

save(COR, file = "data/jaguar/corridor_sensitivity_paths_res5.rda")
load(file = "data/jaguar/corridor_sensitivity_paths_res5.rda")

# Combined figure
png(file = "figures/jaguar/corridor_comb_H_paths.png", width = 9, height = 2.6, units = "in", res = 600)
par(mfrow = c(1,3), mai = c(0.6,0.5,0.45,0.15), omi = c(0,0.25,0,0.25), cex.main = 1.4)
# Plot all distributions
plot(COR[[1]], main = "4 Passages", xlim = c(-25000,10000), ylim = c(-7000,9000))
plot(COR[[2]], main = "3 Passages", xlim = c(-25000,10000), ylim = c(-7000,9000))
plot(COR[[3]], main = "2 Passages", xlim = c(-25000,10000), ylim = c(-7000,9000))
dev.off()

# Occurrence combined figure
load(file = "data/jaguar/occurrence_sensitivity_paths.rda")

png(file = "figures/jaguar/occurrence_comb_H_paths.png", width = 12, height = 2.6, units = "in", res = 600)
par(mfrow = c(1,4), mai = c(0.6,0.5,0.45,0.15), omi = c(0,0.25,0,0.25), cex.main = 1.4)
# Plot all distributions
plot(OCC[[1]], main = "4 Passages", xlim = c(-24000,10000), ylim = c(-5000,7000))
plot(OCC[[2]], main = "3 Passages", xlim = c(-24000,10000), ylim = c(-5000,7000))  #
plot(OCC[[3]], main = "2 Passages", xlim = c(-24000,10000), ylim = c(-5000,7000))
plot(OCC[[4]], main = "1 Passages", xlim = c(-24000,10000), ylim = c(-5000,7000))
# plot(OCC[[5]], main = "3 Passages", xlim = c(-25000,35000), ylim = c(-20000,20000))  #
# plot(OCC[[6]], main = "2 Passages", xlim = c(-25000,35000), ylim = c(-20000,20000))
# plot(OCC[[7]], main = "1 Passage", xlim = c(-25000,35000), ylim = c(-20000,20000))
dev.off()

