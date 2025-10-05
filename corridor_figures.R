# Figures for corridor demos

## TODO:
### USE `plot3D` TO CREATE 3D CORRIDOR DISTRIBUTION FOR PRESENTATION FIGURE

# Load `ctmm` package
library(ctmm)

# Load packages
library(plotly)
library(plot3D)

# 3D corridor distribution ----

# Coarsen the corridor distribution
COR <- list()
COR[[1]] <- ctmm:::corridor(data = DATA, CTMM = corfits, res.time = 4, grid = list(dr = c(150,150)))

# Load single corridor data 
DATA <- readRDS(file = "data/mule_deer/singlecor_full.rds")  # mule deer
data <- do.call(rbind, DATA)  # combine individuals into single dataframe
data$ID <- c(rep(names(DATA)[1], nrow(DATA[[1]])), 
             rep(names(DATA)[2], nrow(DATA[[2]])),
             rep(names(DATA)[3], nrow(DATA[[3]])), 
             rep(names(DATA)[4], nrow(DATA[[4]])),
             rep(names(DATA)[5], nrow(DATA[[5]])), 
             rep(names(DATA)[6], nrow(DATA[[6]])),
             rep(names(DATA)[7], nrow(DATA[[7]])))

# Load corridor distribution results
load(file = "data/mule_deer/corridor_sensitivity_fix_tres.rda")  # mule deer

cor3d <- COR[[1]]  # select corridor for full data

X <- cor3d$r$x
Y <- cor3d$r$y
Z <- cor3d$PDF
dimnames(Z) <- list(X,Y)
Z2 <- round(Z, digits = 12)
pdf <- as.data.frame(Z)

trial <- Z[1:3000, 1:3000]
trial <- round(trial, digits = 12)
# trial2 <- cor3d$CDF[1:200, 1:200]
# M <- mesh(x = cor3d$r$x, y = cor3d$r$y, z = cor3d$CDF)

plotly::plot_ly(type = "surface", color = trial, alpha = 0.7) %>%
  plotly::add_surface(x = dimnames(trial)[[2]], y = dimnames(trial)[[1]], z = trial) #, 
                      # showlegend = FALSE)

plotly::plot_ly(type = "surface", color = Z, alpha = 0.7) %>%
  plotly::add_surface(x = dimnames(Z)[[2]], y = dimnames(Z)[[1]], z = Z)

# CKDE_3D2 <- plotly::plot_ly(data, x = ~x, y = ~y, showlegend = FALSE) %>%
#   plotly::add_surface(z = Z)
# trial <- plotly::plot_ly(pdf, type = "surface")

# persp(x = X, y = Y, z = Z, col = Z)
# 
# panelfirst <- function(pmat) {
#   
#   xmin <- min(data$x)  # min x-value
#   
#   # 2D plot of GPS points (bottom panel)
#   scatter2D(x = data$x, y = data$y, colvar = data$ID, pch = 16, add = TRUE, colkey = FALSE)
#   
# }
# 
# CKDE_3D <- plot3D::surf3D(X, Y, Z, colvar = Z, colkey = FALSE, box = FALSE)
# 
# # 3D plot of corridor distribution
# plot3D::surf3D(X, Y, Z, colvar = Z, colkey = FALSE, box = FALSE)
# # 2D plot of GPS points (bottom panel)
# scatter2D(x = data$x, y = data$y, colvar = data$ID, pch = 16, add = TRUE, colkey = FALSE)
# 
# 
# ## ctmm ver
# EXT <- extent(data)
# ctmm:::plot3d(UD = COR[[1]])


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

## TODO:
# Generate corridor range distribution
COR <- list()  # empty list
COR[[1]] <- ctmm:::corridor(data = DATA3, CTMM = corfits2, grid = list(dr = c(100,100))) # reduce absolute resolution

# Generate range distribution for each subset of data

# Remove an individual path each loop
DATA3 <- DATA3[-1]
corfits2 <- corfits2[-1]

# Corridor range distribution
COR[[2]] <- ctmm:::corridor(data = DATA3, CTMM = corfits2, grid = list(dr = c(100,100)))

# Remove an individual path each loop
DATA3 <- DATA3[-1]
corfits2 <- corfits2[-1]

COR[[3]] <- ctmm:::corridor(data = DATA3, CTMM = corfits2, grid = list(dr = c(100,100)))

save(COR, file = "data/jaguar/corridor_sensitivity_paths_res100.rda")
load(file = "data/jaguar/corridor_sensitivity_paths_res100.rda")

# Combined figure
png(file = "figures/jaguar/corridor_comb_H_paths_fix_tres.png", width = 9, height = 2.6, units = "in", res = 600)
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


# Example range residency ----

# Load single corridor data 
DATA <- readRDS(file = "data/mule_deer/singlecor_full.rds")  # mule deer
indiv <- DATA$`36840`  # extract single individual

# Visualize single individual
plot(indiv, col = color(indiv, by = "time")) +  # blue to red
  abline(v = -3, lty = 2)  # mark approximate ranging behavior

indiv_hr <- indiv[indiv$x < (-3000),]  # subset data
projection(indiv_hr) <- median(indiv_hr)  # center projection
plot(indiv_hr, col = color(indiv_hr, by = "time"))

# Estimate model parameters
GUESS <- ctmm.guess(indiv_hr, interactive = FALSE)
# Model selection
FIT <- ctmm.select(indiv_hr, GUESS, trace = 3, verbose = TRUE)  # fits multiple models
# saveRDS(FIT, file = "data/mule_deer/mule_deer_36840_fit.rds")
FIT <- readRDS("data/mule_deer/mule_deer_36840_fit.rds")

summary(FIT)  # inspect selection results
## OU anisotropic was the best fit (OUF anisotropic is okay too)
summary(FIT[[1]])

# Home range estimate
HR <- akde(indiv_hr, FIT[[1]])

png(file = "figures/mule_deer/mule_deer_36840_HR.png", width = 5, height = 5, units = "in", res = 600)
plot(indiv_hr, UD = HR, col = "maroon", col.grid = NA, # ext = extent(indiv_hr),
     main = "Home-Range Estimate: Mule Deer",
     xlim = c(-2500,3500), ylim = c(-5400,2400))
dev.off()


# Example cKDE ----

## Rerun jaguar occurrence at higher temporal resolution ----

# Import jaguar data
jaguar <- read.csv("data/jaguar/Jaguar Conservation in the Caatinga Biome.csv")
jaguar <- as.telemetry(jaguar)

# Subset data
jag1 <- jaguar[[1]]  # ID: Courisco
summary(jag1)  # inspect data
projection(jag1) <- median(jag1)

# Keep data within a certain range of distance along x-axis
data <- jag1[jag1$x > 7*1000,]

# Split data into separate paths (effectively 4 complete passages through corridor)
DATA <- list()
DATA[[1]] <- data[data$t <= 1.455e9,]
DATA[[2]] <- data[data$t > 1.455e9 & data$t <= 1.458e9,]
DATA[[3]] <- data[data$t > 1.458e9 & data$t <= 1.459e9,]
DATA[[4]] <- data[data$t > 1.459e9,]

summary(DATA)  # 1-hr sampling over several (10-26) days each
projection(DATA) <- median(DATA)

# Load fitted movement models
load(file = "data/jaguar/fit_courisco_4paths.rda")

# Extract and store the best fit for each individual
corfits <- list()
for (i in 1:length(FITS)) {
  corfits[[i]] <- FITS[[i]]$`OUF anisotropic`
}

# Duplicate data for manipulation (coarsening)
DATA2 <- DATA

# Generate occurrence distributions
OCC <- list()  # empty list
OCC[[1]] <- occurrence(DATA2, corfits, res.time = 50)
OCC[[1]] <- mean(OCC[[1]])  # average distributions

# Generate occurrence distribution for each subset of data
for (m in 2:8) {
  
  # Remove every other location to double sampling interval
  for (i in 1:length(DATA2)) {
    DATA2[[i]] <- DATA2[[i]][as.logical(1:nrow(DATA2[[i]])%%2),]
  }
  
  # Occurrence distribution
  OCC[[m]] <- occurrence(DATA2, corfits, res.time = 50)
  OCC[[m]] <- mean(OCC[[m]])
}

save(OCC, file = "data/jaguar/occurrence_sensitivity_tres.rda")
load(file = "data/jaguar/occurrence_sensitivity_tres.rda")

load(file = "data/jaguar/corridor_sensitivity_fix_tres.rda")
# load(file = "data/jaguar/occurrence_sensitivity.rda")

# Make an array of times over the same period, but 5 min apart
SEQ <- list()
for (i in 1:length(DATA)) {
  SEQ[[i]] <- seq(from = DATA[[i]]$t[1], to = DATA[[i]]$t[length(DATA[[i]]$t)], by = 5 %#% 'min')
}

# Predict on data and movement model (kriged paths)
pred_paths <- list()
for (i in 1:length(DATA)) {
  pred_paths[[i]] <- predict(corfits[[i]], data = DATA[[i]], t = SEQ[[i]])
}
save(pred_paths, file = "data/jaguar/jaguar_pred_paths.rda")
load(file = "data/jaguar/jaguar_pred_paths.rda")

# Simulate from movement model, conditional on data
sim_paths <- list()
for (i in 1:length(DATA)) {
  sim_paths[[i]] <- simulate(corfits[[i]], data = DATA[[i]], t = SEQ[[i]])
}


trial <- list(pred_paths[[4]], pred_paths[[1]], pred_paths[[2]], pred_paths[[3]])


png(file = "figures/jaguar/jaguar_pred_paths2.png", width = 6.86, height = 4, units = "in", res = 600)
plot(trial, error = FALSE, type = "l", col = color(trial, by = "individual"))  # includes error ellipses from kriging
# plot(DATA, col = "black", add = TRUE)
dev.off()

png(file = "figures/jaguar/jaguar_sim_paths.png", width = 6.86, height = 4, units = "in", res = 600)
plot(sim_paths, type = "l") #, col = color(sim_paths, by = "individual"))  # simulated conditional on data
plot(DATA, col = "black", add = TRUE)
dev.off()

---


## Zoomed-in plots ----

# Plot close-up of jaguar data
png(file = "figures/jaguar/jaguar_zoom2.png", width = 6.86, height = 4, units = "in", res = 600)
plot(DATA, col = color(DATA, by = "individual"), error = 2, pch = 16, cex = 0.8,
     xlim = c(-10000,-4000), ylim = c(-3000,-1000))
plot(OCC[[1]], level.UD = NA, add = TRUE)
dev.off()

# Plot close-up of jaguar data with corridor
png(file = "figures/jaguar/jaguar_zoom3.png", width = 6.86, height = 4, units = "in", res = 600)
plot(DATA, col = color(DATA, by = "individual"), error = FALSE, pch = 16, cex = 0.5,
     xlim = c(-8000,-6000), ylim = c(-3300,-1000))
plot(COR[[1]], level.UD = 0.95, add = TRUE)
dev.off()

# Plot close-up of predicted (kriged) jaguar paths
png(file = "figures/jaguar/jaguar_zoom_pred.png", width = 6.86, height = 4, units = "in", res = 600)
plot(pred_paths, col = color(pred_paths, by = "individual"),
     xlim = c(-7500,-6000), ylim = c(-2700,-1200))
plot(DATA, col = "black", add = TRUE)
dev.off()





# Plot close-up of predicted (kriged) jaguar paths w/ MLP
png(file = "figures/jaguar/jaguar_zoom_pred_MLP2.png", width = 6.86, height = 4, units = "in", res = 600)
plot(pred_paths, col = color(pred_paths, by = "individual"), error = FALSE, type = "l",
     xlim = c(-7500,-6000), ylim = c(-2700,-1200))
# plot(pred_paths, error = FALSE, type = "l", col = color(pred_paths, by = "individual"), add = TRUE)
# plot(pred_paths[[i]], error = FALSE, col = "black", add = TRUE)
# plot(DATA, col = "black", add = TRUE)
dev.off()

# Plot close-up of simulated jaguar paths
plot(sim_paths, col = color(sim_paths, by = "individual"),
     xlim = c(-7500,-6000), ylim = c(-2700,-1200))

plot(DATA, col = color(DATA, by = "individual"),
     xlim = c(-7500,-6000), ylim = c(-2700,-1200))



# Example cKDE mule deer ----

# Load single corridor data 
DATA <- readRDS(file = "data/mule_deer/singlecor_segmented.rds")  # mule deer
load(file = "data/mule_deer/fits_corridor.rda")

# Extract and store the best fit for each individual
corfits <- list()
for (i in 1:length(FITS)) {
  corfits[[i]] <- FITS[[i]]$`OUF anisotropic`
}

load(file = "data/mule_deer/corridor_sensitivity_fix_tres.rda")

# Make an array of times over the same period, but 5 min apart
SEQ <- list()
for (i in 1:length(DATA)) {
  SEQ[[i]] <- seq(from = DATA[[i]]$t[1], to = DATA[[i]]$t[length(DATA[[i]]$t)], by = 5 %#% 'min')
}

# Predict on data and movement model (kriged paths)
pred_paths <- list()
for (i in 1:length(DATA)) {
  pred_paths[[i]] <- predict(corfits[[i]], data = DATA[[i]], t = SEQ[[i]])
}
save(pred_paths, file = "data/mule_deer/mule_deer_pred_paths.rda")

# # Most likely path
# MLP <- list()
# for (i in 1:length(DATA)) {
#   MLP[[i]] <- predict(DATA[[i]], corfits[[i]], dt = 60)
# }

# Simulate from movement model, conditional on data
sim_paths <- list()
for (i in 1:length(DATA)) {
  sim_paths[[i]] <- simulate(corfits[[i]], data = DATA[[i]], t = SEQ[[i]])
}

# Plot full predicted (kriged) paths
png(file = "figures/mule_deer/mule_deer_pred_paths.png", width = 6.86, height = 6.86, units = "in", res = 600)
plot(pred_paths, col = color(pred_paths, by = "individual"))  # includes error ellipses from kriging
plot(DATA, col = "black", cex = 0.2, add = TRUE)
dev.off()

# Plot full simulated paths
png(file = "figures/mule_deer/mule_deer_sim_paths.png", width = 6.86, height = 4, units = "in", res = 600)
plot(sim_paths, type = "l") #, col = color(sim_paths, by = "individual"))  # simulated conditional on data
plot(DATA, col = "black", add = TRUE)
dev.off()


# Plot close-up of mule deer data with corridor
png(file = "figures/mule_deer/mule_deer_zoom.png", width = 6.86, height = 4, units = "in", res = 600)
plot(DATA, col = color(DATA, by = "individual"), error = FALSE, pch = 16, cex = 0.5,
     xlim = c(-12000,-4000), ylim = c(-4000,-500))
plot(COR[[1]], level.UD = 0.95, add = TRUE)
dev.off()

# Plot close-up of predicted (kriged) mule deer paths
png(file = "figures/mule_deer/mule_deer_zoom_pred.png", width = 6.86, height = 6, units = "in", res = 600)
plot(pred_paths, col = color(pred_paths, by = "individual"),
     xlim = c(-8000,4000), ylim = c(-5500,3000))
plot(DATA, col = "black", add = TRUE)
dev.off()

# Plot close-up of simulated jaguar paths
plot(sim_paths, col = color(sim_paths, by = "individual"),
     xlim = c(-7500,-6000), ylim = c(-2700,-1200))



plot(DATA, col = color(DATA, by = "individual"), error = 2, pch = 16, cex = 0.8) #,
     # xlim = c(-10000,-4000), ylim = c(-3000,-1000))
plot(OCC[[1]], level.UD = NA, add = TRUE)

