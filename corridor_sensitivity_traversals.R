# Randomizing path removal for corridor traversal count sensitivity test

# Load packages
library(ctmm)

# Mule deer ----

# Mule deer (Odocoileus hemionus) migration data
# Wang, Yi-Shan; Blackwell, Paul G.; Merkle, Jerod A.; Potts, Jonathan R. (2019). 
# Data from: Continuous time resource selection analysis for moving animals [Dataset]. Dryad. 
# https://doi.org/10.5061/dryad.f9p3dq4
data <- read.csv("data/mule_deer/MuleDeer_Cody_ForPotts.csv")  
data <- as.telemetry(data)  # convert to telemetry object (drops unneeded columns)
projection(data) <- median(data)  # center projection on geometric median of data

# Subset data to single corridor
DATA <- data[c('36827', '36831', '36840', '36935', '36999', '37009', '37010')]
projection(DATA) <- median(DATA)  # center projection on geometric median of data

plot(DATA, col = color(DATA, by = "individual"))

# Keep data within a certain range of distance along x-axis
for (i in 1:length(DATA)) {
  DATA[[i]] <- DATA[[i]][DATA[[i]]$x > 19*1000,]
  DATA[[i]] <- DATA[[i]][DATA[[i]]$x < 72*1000,]
}

# Remove outliers
DATA[[5]] <- DATA[[5]][-which(DATA[[5]]$t == max(DATA[[5]]$t)),]
DATA[[7]] <- DATA[[7]][-which(DATA[[7]]$t == min(DATA[[7]]$t)),]

summary(DATA)  # inspect data
projection(DATA) <- median(DATA)  # re-center projection

plot(DATA, col = color(DATA, by = "individual"), 
     main = "Migration Paths without Range Residency",  # no more range residency
     xlim = c(-22000,32000), ylim = c(-20000,20000))
compass(loc = c(30000, -16000), cex = 1.7)

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

## Anisotropic Ornstein-Uhlenbeck foraging model selected for all individuals
# Extract and store the best fit for each individual
corfits <- list()
for (i in 1:length(FITS)) {
  corfits[[i]] <- FITS[[i]]$`OUF anisotropic`
}


# Leave-One-Out Cross-Validation (LOOCV) for Traversals
## Remove 1: Leave 1 traversal out and test model on remaining ones (n choose k combos)
## Remove 2: Leave 2 traversals out and test model on remaining ones 
## Etc., while averaging the resulting distributions

# Empty lists to store results
OCC <- list()  # averaged distributions
OCC_ALL <- list()  # all distributions
COR <- list()
COR_ALL <- list()

## LOOCV ----
choose(7,1)  # 7 combinations (7 choose 1)

# Occurrence Distribution
OCC_ALL[[1]] <- list()  # empty list for ODs
for (i in 1:length(DATA)) {
  
  # Remove one path each iteration
  DATA2 <- DATA[-i]
  corfits2 <- corfits[-i]
  
  # Occurrence distribution
  OCC_ALL[[1]][[i]] <- occurrence(data = DATA2, CTMM = corfits2, 
                                  grid = list(dr = c(100,100), align.to.origin = TRUE))
  
}  # LOOCV occurrence

# save(OCC_ALL, file = "data/mule_deer/occurrence_sensitivity_LOOCV_all.rda")
load(file = "data/mule_deer/occurrence_sensitivity_LOOCV_all.rda")

OCC[[1]] <- c(OCC_ALL[[1]][[1]], OCC_ALL[[1]][[2]], OCC_ALL[[1]][[3]],
              OCC_ALL[[1]][[4]], OCC_ALL[[1]][[5]], OCC_ALL[[1]][[6]],
              OCC_ALL[[1]][[7]])
OCC[[1]] <- mean(OCC[[1]])  # average resulting distributions

# save(OCC, file = "data/mule_deer/occurrence_sensitivity_LOOCV.rda")
load(file = "data/mule_deer/occurrence_sensitivity_LOOCV.rda")

par(mfrow = c(1,1))
plot(OCC[[1]])


# Corridor distribution
COR_ALL[[1]] <- list()  # empty list for CDs
for (i in 1:length(DATA)) {
  
  # Remove one path each iteration
  DATA2 <- DATA[-i]
  corfits2 <- corfits[-i]
  
  # Corridor distribution
  COR_ALL[[1]][[i]] <- ctmm:::corridor(data = DATA2, CTMM = corfits2, 
                                       grid = list(dr = c(100,100), align.to.origin = TRUE))
  
}  # LOOCV corridor

# save(COR_ALL, file = "data/mule_deer/corridor_sensitivity_LOOCV_all.rda")
load(file = "data/mule_deer/corridor_sensitivity_LOOCV_all.rda")

# mean.UD()/mean.ctmm() debug --> x (corridor output) is missing parts of CTMM, like COV.mu 
## uncertainties not yet propagated for corridor()
# Assign same ctmm model to COR UDs to allow mean
for (i in 1:length(COR_ALL[[1]])) {
    COR_ALL[[1]][[i]]@CTMM <- corfits[[1]]
  }

COR[[1]] <- mean(COR_ALL[[1]])  # average resulting distributions

# save(COR, file = "data/mule_deer/corridor_sensitivity_LOOCV_fix.rda")
load(file = "data/mule_deer/corridor_sensitivity_LOOCV_fix.rda")

## SEE `corridor_debug.R` SCRIPT FOR MEAN UD CALCULATIONS

# COR[[1]] <- c(COR_ALL[[1]][[1]], COR_ALL[[1]][[2]], COR_ALL[[1]][[3]],
#               COR_ALL[[1]][[4]], COR_ALL[[1]][[5]], COR_ALL[[1]][[6]],
#               COR_ALL[[1]][[7]])
# COR[[1]] <- mean(COR[[1]])  # average resulting distributions

# save(COR, file = "data/mule_deer/corridor_sensitivity_LOOCV.rda")
load(file = "data/mule_deer/corridor_sensitivity_LOOCV.rda")  # OLD MANUAL FIX

par(mfrow = c(1,1))
plot(COR[[1]], level = NA)
plot(COR[[1]])


## L2OCV ----
choose(7,2)  # 21 combinations

# Occurrence Distribution
OCC_ALL[[2]] <- list()  # empty list for ODs
for (i in 1:length(DATA[-1])) {
  
  # Remove one path each iteration
  DATA2 <- DATA[-i]
  corfits2 <- corfits[-i]
  ## length(DATA2) == 6
  
  for (j in i:length(DATA2)) {
    
    # Remove 2nd path each iteration
    DATA3 <- DATA2[-j]
    corfits3 <- corfits2[-j]
    ## length(DATA2) == 5
    
    # Occurrence distribution
    OCC_ALL[[2]][[paste(i,j+1,sep = ",")]] <- occurrence(data = DATA3, CTMM = corfits3,
                                                         grid = list(dr = c(100,100), 
                                                                     align.to.origin = TRUE))
    # OCC_ALL[[2]][[paste(i,j+1,sep = ",")]] <- paste(names(DATA3))  # CHECK
  }
}  # L2OCV occurrence

# save(OCC_ALL, file = "data/mule_deer/occurrence_sensitivity_LOOCV_all.rda")
load(file = "data/mule_deer/occurrence_sensitivity_LOOCV_all.rda")

# Unlist individual distributions
OCC[[2]] <- list()
for (i in 1:length(OCC_ALL[[2]])) {
  OCC[[2]] <- append(OCC[[2]], OCC_ALL[[2]][[i]])
}
OCC[[2]] <- mean(OCC[[2]])  # average resulting distributions

# save(OCC, file = "data/mule_deer/occurrence_sensitivity_LOOCV.rda")
load(file = "data/mule_deer/occurrence_sensitivity_LOOCV.rda")

par(mfrow = c(1,1))
plot(OCC[[2]])


# Corridor distribution
COR_ALL[[2]] <- list()  # empty list for CDs
for (i in 1:length(DATA[-1])) {
  
  # Remove one path each iteration
  DATA2 <- DATA[-i]
  corfits2 <- corfits[-i]
  ## length(DATA2) == 6
  
  for (j in i:length(DATA2)) {
    
    # Remove 2nd path each iteration
    DATA3 <- DATA2[-j]
    corfits3 <- corfits2[-j]
    ## length(DATA2) == 5
    
    # Corridor distribution
    COR_ALL[[2]][[paste(i,j+1,sep = ",")]] <- ctmm:::corridor(data = DATA3, CTMM = corfits3,
                                                              grid = list(dr = c(100,100), 
                                                                          align.to.origin = TRUE))
  }
}  # L2OCV corridor

save(COR_ALL, file = "data/mule_deer/corridor_sensitivity_LOOCV_all.rda")
load(file = "data/mule_deer/corridor_sensitivity_LOOCV_all.rda")

# mean.UD()/mean.ctmm() debug --> x (corridor output) is missing parts of CTMM, like COV.mu 
## uncertainties not yet propagated for corridor()
# Assign same ctmm model to COR UDs to allow mean
for (i in 1:length(COR_ALL[[2]])) {
  COR_ALL[[2]][[i]]@CTMM <- corfits[[1]]
}

COR[[2]] <- mean(COR_ALL[[2]])  # average resulting distributions

# save(COR, file = "data/mule_deer/corridor_sensitivity_LOOCV_fix.rda")
load(file = "data/mule_deer/corridor_sensitivity_LOOCV_fix.rda")

## SEE `corridor_debug.R` SCRIPT FOR MEAN UD CALCULATIONS

# save(COR, file = "data/mule_deer/corridor_sensitivity_LOOCV.rda")
load(file = "data/mule_deer/corridor_sensitivity_LOOCV.rda")

par(mfrow = c(1,1))
plot(COR[[2]])
plot(COR[[2]], level = NA)


## L3OCV ----
choose(7,3)  # 35 combinations

# Occurrence Distribution
OCC_ALL[[3]] <- list()  # empty list for ODs
for (i in 1:length(DATA[-c(1,2)])) {
  
  # Remove one path each iteration
  DATA2 <- DATA[-i]
  corfits2 <- corfits[-i]
  ## length(DATA2) == 6
  
  for (j in i:length(DATA2[-1])) {
    
    # Remove 2nd path each iteration
    DATA3 <- DATA2[-j]
    corfits3 <- corfits2[-j]
    ## length(DATA2) == 5
    
    for (m in j:length(DATA3)) {
      
      # Remove 3rd path each iteration
      DATA4 <- DATA3[-m]
      corfits4 <- corfits3[-m]
      ## length(DATA2) == 4
     
      # Occurrence distribution
      OCC_ALL[[3]][[paste(i,j+1,m+2,sep = ",")]] <- occurrence(data = DATA4, CTMM = corfits4,
                                                               grid = list(dr = c(100,100), 
                                                                           align.to.origin = TRUE))
      # OCC_ALL[[3]][[paste(i,j+1,m+2,sep = ",")]] <- paste(names(DATA4))  # CHECK
    }
  }
}  # L3OCV occurrence

# save(OCC_ALL, file = "data/mule_deer/occurrence_sensitivity_LOOCV_all.rda")
load(file = "data/mule_deer/occurrence_sensitivity_LOOCV_all.rda")

# Unlist individual distributions
OCC[[3]] <- list()
for (i in 1:length(OCC_ALL[[3]])) {
  OCC[[3]] <- append(OCC[[3]], OCC_ALL[[3]][[i]])
}
OCC[[3]] <- mean(OCC[[3]])  # average resulting distributions

# save(OCC, file = "data/mule_deer/occurrence_sensitivity_LOOCV.rda")
load(file = "data/mule_deer/occurrence_sensitivity_LOOCV.rda")

par(mfrow = c(1,1))
plot(OCC[[3]])


# Corridor distribution
COR_ALL[[3]] <- list()  # empty list for CDs
for (i in 1:length(DATA[-c(1,2)])) {
  
  # Remove one path each iteration
  DATA2 <- DATA[-i]
  corfits2 <- corfits[-i]
  ## length(DATA2) == 6
  
  for (j in i:length(DATA2[-1])) {
    
    # Remove 2nd path each iteration
    DATA3 <- DATA2[-j]
    corfits3 <- corfits2[-j]
    ## length(DATA2) == 5
    
    for (m in j:length(DATA3)) {
      
      # Remove 3rd path each iteration
      DATA4 <- DATA3[-m]
      corfits4 <- corfits3[-m]
      ## length(DATA2) == 4
      
      # Corridor distribution
      COR_ALL[[3]][[paste(i,j+1,m+2,sep = ",")]] <- ctmm:::corridor(data = DATA4, CTMM = corfits4,
                                                                    grid = list(dr = c(100,100), 
                                                                                align.to.origin = TRUE))
    }  # WARNING: In ctmm:::corridor: Suggest res.time » 2
  }
}  # L3OCV corridor

# save(COR_ALL, file = "data/mule_deer/corridor_sensitivity_LOOCV_all.rda")
load(file = "data/mule_deer/corridor_sensitivity_LOOCV_all.rda")

# mean.UD()/mean.ctmm() debug --> x (corridor output) is missing parts of CTMM, like COV.mu 
## uncertainties not yet propagated for corridor()
# Assign same ctmm model to COR UDs to allow mean
for (i in 1:length(COR_ALL[[3]])) {
  COR_ALL[[3]][[i]]@CTMM <- corfits[[1]]
}

COR[[3]] <- mean(COR_ALL[[3]])  # average resulting distributions

# save(COR, file = "data/mule_deer/corridor_sensitivity_LOOCV_fix.rda")
load(file = "data/mule_deer/corridor_sensitivity_LOOCV_fix.rda")

## SEE `corridor_debug.R` SCRIPT FOR MEAN UD CALCULATIONS

# save(COR, file = "data/mule_deer/corridor_sensitivity_LOOCV.rda")
load(file = "data/mule_deer/corridor_sensitivity_LOOCV.rda")

par(mfrow = c(1,1))
plot(COR[[3]])
plot(COR[[3]], level = NA)


## L4OCV ----
choose(7,4)  # 35 combinations

# Occurrence Distribution
OCC_ALL[[4]] <- list()  # empty list for ODs
for (i in 1:length(DATA[-c(1:3)])) {
  
  # Remove one path each iteration
  DATA2 <- DATA[-i]
  corfits2 <- corfits[-i]
  ## length(DATA2) == 6
  
  for (j in i:length(DATA2[-c(1,2)])) {
    
    # Remove 2nd path each iteration
    DATA3 <- DATA2[-j]
    corfits3 <- corfits2[-j]
    ## length(DATA2) == 5
    
    for (m in j:length(DATA3[-1])) {
      
      # Remove 3rd path each iteration
      DATA4 <- DATA3[-m]
      corfits4 <- corfits3[-m]
      ## length(DATA2) == 4
      
      for (n in m:length(DATA4)) {
       
        # Remove 4th path each iteration
        DATA5 <- DATA4[-n]
        corfits5 <- corfits4[-n]
        ## length(DATA2) == 4
        
        # Occurrence distribution
        OCC_ALL[[4]][[paste(i,j+1,m+2,n+3,sep = ",")]] <- occurrence(data = DATA5, CTMM = corfits5,
                                                                     grid = list(dr = c(100,100), 
                                                                                 align.to.origin = TRUE))
        # OCC_ALL[[4]][[paste(i,j+1,m+2,n+3,sep = ",")]] <- paste(names(DATA5))  # CHECK
      }
    }
  }
}  # L4OCV occurrence

# save(OCC_ALL, file = "data/mule_deer/occurrence_sensitivity_LOOCV_all.rda")
load(file = "data/mule_deer/occurrence_sensitivity_LOOCV_all.rda")

# Unlist individual distributions
OCC[[4]] <- list()
for (i in 1:length(OCC_ALL[[4]])) {
  OCC[[4]] <- append(OCC[[4]], OCC_ALL[[4]][[i]])
}
OCC[[4]] <- mean(OCC[[4]])  # average resulting distributions

# save(OCC, file = "data/mule_deer/occurrence_sensitivity_LOOCV.rda")
load(file = "data/mule_deer/occurrence_sensitivity_LOOCV.rda")

par(mfrow = c(1,1))
plot(OCC[[4]])


# Corridor distribution
COR_ALL[[4]] <- list()  # empty list for CDs
for (i in 1:length(DATA[-c(1:3)])) {
  
  # Remove one path each iteration
  DATA2 <- DATA[-i]
  corfits2 <- corfits[-i]
  ## length(DATA2) == 6
  
  for (j in i:length(DATA2[-c(1,2)])) {
    
    # Remove 2nd path each iteration
    DATA3 <- DATA2[-j]
    corfits3 <- corfits2[-j]
    ## length(DATA2) == 5
    
    for (m in j:length(DATA3[-1])) {
      
      # Remove 3rd path each iteration
      DATA4 <- DATA3[-m]
      corfits4 <- corfits3[-m]
      ## length(DATA2) == 4
      
      for (n in m:length(DATA4)) {
        
        # Remove 4th path each iteration
        DATA5 <- DATA4[-n]
        corfits5 <- corfits4[-n]
        ## length(DATA2) == 4
        
        # Corridor distribution
        COR_ALL[[4]][[paste(i,j+1,m+2,n+3,sep = ",")]] <- ctmm:::corridor(data = DATA5, CTMM = corfits5,
                                                                          grid = list(dr = c(100,100), 
                                                                                      align.to.origin = TRUE))
      }
      # WARNINGS:
        # 1: In ctmm:::corridor: Suggest res.time » 3
        # 2: In ctmm:::corridor: Suggest res.time » 2                                                                                        
        # 3: In ctmm:::corridor: Suggest res.time » 2
    }
  }
}  # L4OCV corridor

# save(COR_ALL, file = "data/mule_deer/corridor_sensitivity_LOOCV_all.rda")
load(file = "data/mule_deer/corridor_sensitivity_LOOCV_all.rda")

# mean.UD()/mean.ctmm() debug --> x (corridor output) is missing parts of CTMM, like COV.mu 
## uncertainties not yet propagated for corridor()
# Assign same ctmm model to COR UDs to allow mean
for (i in 1:length(COR_ALL[[4]])) {
  COR_ALL[[4]][[i]]@CTMM <- corfits[[1]]
}

COR[[4]] <- mean(COR_ALL[[4]])  # average resulting distributions

save(COR, file = "data/mule_deer/corridor_sensitivity_LOOCV_fix.rda")
load(file = "data/mule_deer/corridor_sensitivity_LOOCV_fix.rda")

## SEE `corridor_debug.R` SCRIPT FOR MEAN UD CALCULATIONS

# save(COR, file = "data/mule_deer/corridor_sensitivity_LOOCV.rda")
load(file = "data/mule_deer/corridor_sensitivity_LOOCV.rda")

par(mfrow = c(1,1))
plot(COR[[4]])
plot(COR[[4]], level = NA)


## L5OCV ----
choose(7,5)  # 21 combinations

# Occurrence Distribution
OCC_ALL[[5]] <- list()  # empty list for ODs
for (i in 1:length(DATA[-1])) {
  for (j in i:length(DATA[-i])) {
    
    # Keep 2 different paths each iteration
    DATA6 <- DATA[c(i,j+1)]
    corfits6 <- corfits[c(i,j+1)]
    ## length(DATA6) == 2
    
    # Occurrence distribution
    OCC_ALL[[5]][[paste(i,j+1,sep = ",")]] <- occurrence(data = DATA6, CTMM = corfits6,
                                                         grid = list(dr = c(100,100), 
                                                                     align.to.origin = TRUE))
    # OCC_ALL[[5]][[paste(i,j+1,sep = ",")]] <- paste(names(DATA6))  # CHECK
  }
}  # L5OCV occurrence

# save(OCC_ALL, file = "data/mule_deer/occurrence_sensitivity_LOOCV_all.rda")
load(file = "data/mule_deer/occurrence_sensitivity_LOOCV_all.rda")

# Unlist individual distributions
OCC[[5]] <- list()
for (i in 1:length(OCC_ALL[[5]])) {
  OCC[[5]] <- append(OCC[[5]], OCC_ALL[[5]][[i]])
}
OCC[[5]] <- mean(OCC[[5]])  # average resulting distributions

# save(OCC, file = "data/mule_deer/occurrence_sensitivity_LOOCV.rda")
load(file = "data/mule_deer/occurrence_sensitivity_LOOCV.rda")

par(mfrow = c(1,1))
plot(OCC[[5]])


# Corridor distribution
COR_ALL[[5]] <- list()  # empty list for CDs
for (i in 1:length(DATA[-1])) {
  for (j in i:length(DATA[-i])) {
    
    # Keep 2 different paths each iteration
    DATA6 <- DATA[c(i,j+1)]
    corfits6 <- corfits[c(i,j+1)]
    ## length(DATA6) == 2
    
    # Corridor distribution
    COR_ALL[[5]][[paste(i,j+1,sep = ",")]] <- ctmm:::corridor(data = DATA6, CTMM = corfits6,
                                                              grid = list(dr = c(100,100), 
                                                                          align.to.origin = TRUE))
  }
  # WARNINGS:
  #   1: In ctmm:::corridor: Suggest res.time » 2
  #   2: In ctmm:::corridor: Suggest res.time » 3
  #   3: In ctmm:::corridor: Suggest res.time » 2
  #   4: In ctmm:::corridor: Suggest res.time » 3
  #   5: In ctmm:::corridor: Suggest res.time » 2
}  # L5OCV corridor

# save(COR_ALL, file = "data/mule_deer/corridor_sensitivity_LOOCV_all.rda")
load(file = "data/mule_deer/corridor_sensitivity_LOOCV_all.rda")

# mean.UD()/mean.ctmm() debug --> x (corridor output) is missing parts of CTMM, like COV.mu 
## uncertainties not yet propagated for corridor()
# Assign same ctmm model to COR UDs to allow mean
for (i in 1:length(COR_ALL[[5]])) {
  COR_ALL[[5]][[i]]@CTMM <- corfits[[1]]
}

COR[[5]] <- mean(COR_ALL[[5]])  # average resulting distributions

# save(COR, file = "data/mule_deer/corridor_sensitivity_LOOCV_fix.rda")
load(file = "data/mule_deer/corridor_sensitivity_LOOCV_fix.rda")

## SEE `corridor_debug.R` SCRIPT FOR MEAN UD CALCULATIONS

# save(COR, file = "data/mule_deer/corridor_sensitivity_LOOCV.rda")
load(file = "data/mule_deer/corridor_sensitivity_LOOCV.rda")

par(mfrow = c(1,1))
plot(COR[[5]])
plot(COR[[5]], level = NA)


## L6OCV ----
choose(7,6)  # 7 combinations

# Occurrence Distribution
OCC_ALL[[6]] <- list()  # empty list for ODs
for (i in 1:length(DATA)) {
  
  # Keep one path each iteration
  DATA7 <- DATA[i]
  corfits7 <- corfits[i]
  ## length(DATA7) == 1

  # Occurrence distribution
  OCC_ALL[[6]][[i]] <- occurrence(data = DATA7, CTMM = corfits7, grid = list(dr = c(100,100), 
                                                                             align.to.origin = TRUE))
  # OCC_ALL[[6]][[i]] <- paste(names(DATA7))  # CHECK
}  # L6OCV occurrence

OCC_ALL[[6]] <- list()
OCC_ALL[[6]] <- occurrence(data = DATA, CTMM = corfits, grid = list(dr = c(100,100)))

# save(OCC_ALL, file = "data/mule_deer/occurrence_sensitivity_LOOCV_all.rda")
load(file = "data/mule_deer/occurrence_sensitivity_LOOCV_all.rda")

OCC[[6]] <- mean(OCC_ALL[[6]])  # average resulting distributions

# save(OCC, file = "data/mule_deer/occurrence_sensitivity_LOOCV.rda")
load(file = "data/mule_deer/occurrence_sensitivity_LOOCV.rda")

par(mfrow = c(1,1))
plot(OCC[[6]])

# png(file = "figures/mule_deer/occurrence_paths_LOOCV_full.png", width = 4800, height = 6000, units = "px", res = 600)
# par(mfrow = c(3,2), mai = c(0.8,0.3,0.7,0), omi = c(0,0.25,0,0.25), mar = c(4,4,2,0.4) + 0.1, cex.main = 1.2)
png(file = "figures/mule_deer/occurrence_paths_LOOCV_full_fix.png", width = 4800, height = 6000, units = "px", res = 600)
par(mfrow = c(3,2), mai = c(0.6,0.5,0.45,0.15), omi = c(0,0.25,0.25,0.25), cex.main = 1.2)
# Plot distributions
plot(OCC[[6]], main = "Leave 0 Traversals Out", xlim = c(-25000,35000), ylim = c(-20000,20000))
plot(OCC[[1]], main = "Leave 1 Traversal Out", xlim = c(-25000,35000), ylim = c(-20000,20000))
plot(OCC[[2]], main = "Leave 2 Traversals Out", xlim = c(-25000,35000), ylim = c(-20000,20000))
plot(OCC[[3]], main = "Leave 3 Traversals Out", xlim = c(-25000,35000), ylim = c(-20000,20000))
plot(OCC[[4]], main = "Leave 4 Traversals Out", xlim = c(-25000,35000), ylim = c(-20000,20000))
plot(OCC[[5]], main = "Leave 5 Traversals Out", xlim = c(-25000,35000), ylim = c(-20000,20000))
dev.off()


# # Corridor distribution
# COR_ALL[[6]] <- list()  # empty list for CDs
# for (i in 1:length(DATA)) {
#   
#   # Keep one path each iteration
#   DATA7 <- DATA[i]
#   corfits7 <- corfits[i]
#   ## length(DATA7) == 1
#   
#   # Corridor distribution
#   COR_ALL[[6]][[i]] <- ctmm:::corridor(data = DATA7, CTMM = corfits7, grid = list(dr = c(100,100)))
# }  # L6OCV corridor

## CANNOT EVALUATE CORRIDOR WITH ONLY 1 TRAVERSAL
# Corridor distribution based on all traversals
COR_ALL[[6]] <- list()
COR_ALL[[6]] <- ctmm:::corridor(data = DATA, CTMM = corfits, grid = list(dr = c(100,100), 
                                                                         align.to.origin = TRUE))

# save(COR_ALL, file = "data/mule_deer/corridor_sensitivity_LOOCV_all.rda")
load(file = "data/mule_deer/corridor_sensitivity_LOOCV_all.rda")

COR[[6]] <- COR_ALL[[6]]

# save(COR, file = "data/mule_deer/corridor_sensitivity_LOOCV_fix.rda")
load(file = "data/mule_deer/corridor_sensitivity_LOOCV_fix.rda")

# save(COR, file = "data/mule_deer/corridor_sensitivity_LOOCV.rda")
load(file = "data/mule_deer/corridor_sensitivity_LOOCV.rda")

par(mfrow = c(1,1))
plot(COR[[6]])

# png(file = "figures/mule_deer/corridor_paths_LOOCV_full.png", width = 4800, height = 6000, units = "px", res = 600)
# par(mfrow = c(3,2), mai = c(0.8,0.3,0.7,0), omi = c(0,0.25,0,0.25), mar = c(4,4,2,0.4) + 0.1, cex.main = 1.2)
png(file = "figures/mule_deer/corridor_paths_LOOCV_full_fix.png", width = 4800, height = 6000, units = "px", res = 600)
par(mfrow = c(3,2), mai = c(0.6,0.5,0.45,0.15), omi = c(0,0.25,0.25,0.25), cex.main = 1.2)
# Plot distributions
plot(COR[[6]], level = NA, main = "Leave 0 Traversals Out", xlim = c(-25000,35000), ylim = c(-20000,20000))
plot(COR[[1]], level = NA, main = "Leave 1 Traversal Out", xlim = c(-25000,35000), ylim = c(-20000,20000))
plot(COR[[2]], level = NA, main = "Leave 2 Traversals Out", xlim = c(-25000,35000), ylim = c(-20000,20000))
plot(COR[[3]], level = NA, main = "Leave 3 Traversals Out", xlim = c(-25000,35000), ylim = c(-20000,20000))
plot(COR[[4]], level = NA, main = "Leave 4 Traversals Out", xlim = c(-25000,35000), ylim = c(-20000,20000))
plot(COR[[5]], level = NA, main = "Leave 5 Traversals Out", xlim = c(-25000,35000), ylim = c(-20000,20000))
dev.off()

# Reorder plots to compare ODs and Corridor distributions
# png(file = "figures/mule_deer/sensitivity_comparison_paths.png", width = 4800, height = 6000, units = "px", res = 600)
# par(mfrow = c(3,2), mai = c(0.8,0.3,0.7,0), omi = c(0,0.25,0,0.25), mar = c(4,4,2,0.4) + 0.1, cex.main = 1.2)
png(file = "figures/mule_deer/sensitivity_comparison_paths_LOOCV_fix.png", width = 4800, height = 6000, units = "px", res = 600)
par(mfrow = c(3,2), mai = c(0.6,0.5,0.45,0.15), omi = c(0,0.25,0.25,0.25), cex.main = 1.2)
# Plot distributions
plot(OCC[[1]], main = "Leave 1 Traversal Out", xlim = c(-25000,35000), ylim = c(-20000,20000))
mtext(substitute(paste(bold("Occurrence Distribution"))), line = 3, side = 3)
plot(COR[[1]], level = NA, main = "Leave 1 Traversal Out", xlim = c(-25000,35000), ylim = c(-20000,20000))
mtext(substitute(paste(bold("Corridor Distribution"))), line = 3, side = 3)
plot(OCC[[3]], main = "Leave 3 Traversals Out", xlim = c(-25000,35000), ylim = c(-20000,20000))
plot(COR[[3]], level = NA, main = "Leave 3 Traversals Out", xlim = c(-25000,35000), ylim = c(-20000,20000))
plot(OCC[[5]], main = "Leave 5 Traversals Out", xlim = c(-25000,35000), ylim = c(-20000,20000))
plot(COR[[5]], level = NA, main = "Leave 5 Traversals Out", xlim = c(-25000,35000), ylim = c(-20000,20000))
dev.off()



# Jaguars ----

# Morato, Ronaldo G. Jaguar Conservation in the Caatinga Biome Citation
# Acknowledgements: Gediendson Araujo, Leanes Silva, Valdomiro Lemos, Antonio Carlos Csemark 
# Grants Used: FAPESP 2013/10029-6, FAPESP 2014/24921-0 License Type: Custom License 
# Terms: No study-specific terms specified Principal Investigator
# Data retrieved from Movebank
jaguar <- read.csv("data/jaguar/Jaguar Conservation in the Caatinga Biome.csv")
jaguar <- as.telemetry(jaguar)  # convert to telemetry object (drops unneeded columns)

# Projection (class "telemetry")
projection(jaguar) <- median(jaguar)  # center projection on geometric median of data

# Subset data
jag1 <- jaguar[[1]]  # ID: Courisco
projection(jag1) <- median(jag1)  # center projection on geometric median of data

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

projection(DATA) <- median(DATA)


# Model estimate and selection
FITS <- list()  # empty list
for (i in 1:length(DATA)) {  # for each individual
  
  # Estimate model parameters
  GUESS <- ctmm.guess(DATA[[i]], interactive = FALSE)  # interactive mode is manual
  # Model selection
  FIT <- ctmm.select(DATA[[i]], GUESS, trace = 3, verbose = TRUE)  # fits multiple models
  FITS[[i]] <- FIT  # store model selection results
}

# save(FITS, file = "data/jaguar/fit_courisco_4paths_test.rda")  # single longest step (before corridor)
load(file = "data/jaguar/fit_courisco_4paths.rda")
# load(file = "data/jaguar/corridor_sensitivity_paths_random26.rda")
# COR_NEW <- COR


## Anisotropic Ornstein-Uhlenbeck foraging (OUF anisotropic) model selected for all individuals
# Extract and store the best fit for each individual
corfits <- list()
for (i in 1:length(FITS)) {
  corfits[[i]] <- FITS[[i]]$`OUF anisotropic`
}

# Leave-One-Out Cross-Validation (LOOCV) for Traversals
## Remove 1: Leave 1 traversal out and test model on remaining ones (n choose k combos)
## Remove 2: Leave 2 traversals out and test model on remaining ones 
## Etc., while averaging the resulting distributions

# Empty lists to store results
OCC <- list()  # averaged distributions
OCC_ALL <- list()  # all distributions
COR <- list()
COR_ALL <- list()

## LOOCV ----
choose(4,1)  # 4 combinations (7 choose 1)

# Occurrence Distribution
OCC_ALL[[1]] <- list()  # empty list for ODs
for (i in 1:length(DATA)) {
  
  # Remove one path each iteration
  DATA2 <- DATA[-i]
  corfits2 <- corfits[-i]
  
  # Occurrence distribution
  OCC_ALL[[1]][[i]] <- occurrence(data = DATA2, CTMM = corfits2, 
                                  grid = list(dr = c(100,100), 
                                              align.to.origin = TRUE))
  
}  # LOOCV occurrence

# save(OCC_ALL, file = "data/jaguar/occurrence_sensitivity_LOOCV_all.rda")
load(file = "data/jaguar/occurrence_sensitivity_LOOCV_all.rda")

OCC[[1]] <- c(OCC_ALL[[1]][[1]], OCC_ALL[[1]][[2]], OCC_ALL[[1]][[3]], OCC_ALL[[1]][[4]])
OCC[[1]] <- mean(OCC[[1]])  # average resulting distributions

# save(OCC, file = "data/jaguar/occurrence_sensitivity_LOOCV.rda")
load(file = "data/jaguar/occurrence_sensitivity_LOOCV.rda")

par(mfrow = c(1,1))
plot(OCC[[1]])


# Corridor distribution
COR_ALL[[1]] <- list()  # empty list for CDs
for (i in 1:length(DATA)) {
  
  # Remove one path each iteration
  DATA2 <- DATA[-i]
  corfits2 <- corfits[-i]
  
  # Corridor distribution
  COR_ALL[[1]][[i]] <- ctmm:::corridor(data = DATA2, CTMM = corfits2, 
                                       grid = list(dr = c(100,100), 
                                                   align.to.origin = TRUE))
  
}  # LOOCV corridor

# save(COR_ALL, file = "data/jaguar/corridor_sensitivity_LOOCV_all.rda")
load(file = "data/jaguar/corridor_sensitivity_LOOCV_all.rda")

# mean.UD()/mean.ctmm() debug --> x (corridor output) is missing parts of CTMM, like COV.mu 
## uncertainties not yet propagated for corridor()
# Assign same ctmm model to COR UDs to allow mean
for (i in 1:length(COR_ALL[[1]])) {
  COR_ALL[[1]][[i]]@CTMM <- corfits[[1]]
}

COR[[1]] <- mean(COR_ALL[[1]])  # average resulting distributions

# save(COR, file = "data/jaguar/corridor_sensitivity_LOOCV_fix.rda")
load(file = "data/jaguar/corridor_sensitivity_LOOCV_fix.rda")

## SEE `corridor_debug.R` SCRIPT FOR MEAN UD CALCULATIONS

# save(COR, file = "data/jaguar/corridor_sensitivity_LOOCV.rda")
load(file = "data/jaguar/corridor_sensitivity_LOOCV.rda")

par(mfrow = c(1,1))
plot(COR[[1]], level = NA)
plot(COR[[1]])


## L2OCV ----
choose(4,2)  # 6 combinations

# Occurrence Distribution
OCC_ALL[[2]] <- list()  # empty list for ODs
for (i in 1:length(DATA[-1])) {
  
  # Remove one path each iteration
  DATA2 <- DATA[-i]
  corfits2 <- corfits[-i]
  ## length(DATA2) == 3
  
  for (j in i:length(DATA2)) {
    
    # Remove 2nd path each iteration
    DATA3 <- DATA2[-j]
    corfits3 <- corfits2[-j]
    ## length(DATA2) == 2
    
    # Occurrence distribution
    OCC_ALL[[2]][[paste(i,j+1,sep = ",")]] <- occurrence(data = DATA3, CTMM = corfits3,
                                                         grid = list(dr = c(100,100), 
                                                                     align.to.origin = TRUE))
    # OCC_ALL[[2]][[paste(i,j+1,sep = ",")]] <- paste(names(DATA3))  # CHECK
  }
}  # L2OCV occurrence

# save(OCC_ALL, file = "data/jaguar/occurrence_sensitivity_LOOCV_all.rda")
load(file = "data/jaguar/occurrence_sensitivity_LOOCV_all.rda")

# Unlist individual distributions
OCC[[2]] <- list()
for (i in 1:length(OCC_ALL[[2]])) {
  OCC[[2]] <- append(OCC[[2]], OCC_ALL[[2]][[i]])
}
OCC[[2]] <- mean(OCC[[2]])  # average resulting distributions

# save(OCC, file = "data/jaguar/occurrence_sensitivity_LOOCV.rda")
load(file = "data/jaguar/occurrence_sensitivity_LOOCV.rda")

par(mfrow = c(1,1))
plot(OCC[[2]])


# Corridor distribution
COR_ALL[[2]] <- list()  # empty list for CDs
for (i in 1:length(DATA[-1])) {
  
  # Remove one path each iteration
  DATA2 <- DATA[-i]
  corfits2 <- corfits[-i]
  ## length(DATA2) == 3
  
  for (j in i:length(DATA2)) {
    
    # Remove 2nd path each iteration
    DATA3 <- DATA2[-j]
    corfits3 <- corfits2[-j]
    ## length(DATA2) == 2
    
    # Corridor distribution
    COR_ALL[[2]][[paste(i,j+1,sep = ",")]] <- ctmm:::corridor(data = DATA3, CTMM = corfits3,
                                                              grid = list(dr = c(100,100), 
                                                                          align.to.origin = TRUE))
  } # WARNINGS:
  # 1: In ctmm:::corridor: Suggest res.time » 3
  # 2: In ctmm:::corridor: Suggest res.time » 8   
}  # L2OCV corridor

# save(COR_ALL, file = "data/jaguar/corridor_sensitivity_LOOCV_all.rda")
load(file = "data/jaguar/corridor_sensitivity_LOOCV_all.rda")

# mean.UD()/mean.ctmm() debug --> x (corridor output) is missing parts of CTMM, like COV.mu 
## uncertainties not yet propagated for corridor()
# Assign same ctmm model to COR UDs to allow mean
for (i in 1:length(COR_ALL[[2]])) {
  COR_ALL[[2]][[i]]@CTMM <- corfits[[1]]
}

COR[[2]] <- mean(COR_ALL[[2]])  # average resulting distributions

# save(COR, file = "data/jaguar/corridor_sensitivity_LOOCV_fix.rda")
load(file = "data/jaguar/corridor_sensitivity_LOOCV_fix.rda")

## SEE `corridor_debug.R` SCRIPT FOR MEAN UD CALCULATIONS

# save(COR, file = "data/jaguar/corridor_sensitivity_LOOCV.rda")
load(file = "data/jaguar/corridor_sensitivity_LOOCV.rda")

par(mfrow = c(1,1))
plot(COR[[2]])
plot(COR[[2]], level = NA)


## L3OCV ----
choose(4,3)  # 4 combinations

# # Occurrence Distribution
# OCC_ALL[[3]] <- list()  # empty list for ODs
# for (i in 1:length(DATA)) {
#   
#   # Keep one path each iteration
#   DATA4 <- DATA[i]
#   corfits4 <- corfits[i]
#   ## length(DATA7) == 1
#   
#   # Occurrence distribution
#   OCC_ALL[[3]][[i]] <- occurrence(data = DATA4, CTMM = corfits4, grid = list(dr = c(100,100)))
#   # OCC_ALL[[3]][[i]] <- paste(names(DATA4))  # CHECK
# }  # L3OCV occurrence

OCC_ALL[[3]] <- list()
OCC_ALL[[3]] <- occurrence(data = DATA, CTMM = corfits, grid = list(dr = c(100,100), 
                                                                    align.to.origin = TRUE))

# save(OCC_ALL, file = "data/jaguar/occurrence_sensitivity_LOOCV_all.rda")
load(file = "data/jaguar/occurrence_sensitivity_LOOCV_all.rda")

# OCC[[3]] <- c(OCC_ALL[[3]][[1]], OCC_ALL[[3]][[2]], OCC_ALL[[3]][[3]], OCC_ALL[[3]][[4]])

OCC[[3]] <- mean(OCC_ALL[[3]])  # average resulting distributions

# save(OCC, file = "data/jaguar/occurrence_sensitivity_LOOCV.rda")
load(file = "data/jaguar/occurrence_sensitivity_LOOCV.rda")

par(mfrow = c(1,1))
plot(OCC[[3]])

png(file = "figures/jaguar/occurrence_paths_LOOCV_full.png", width = 3200, height = 6000, units = "px", res = 600)
par(mfrow = c(3,1), mai = c(0.8,0.3,0.7,0), omi = c(0,0.1,0,0.25), mar = c(4,4,3,0.4) + 0.1, cex.main = 1.4)
# png(file = "figures/jaguar/occurrence_paths_LOOCV_full.png", width = 4, height = 6.5, units = "in", res = 600)
# par(mfrow = c(3,1), mai = c(0.8,0.3,0.7,0), omi = c(0,0.1,0,0.25), mar = c(4,4,3,0.4) + 0.1, cex.main = 1.2)
# Plot distributions
plot(OCC[[3]], main = "Leave 0 Traversals Out", xlim = c(-23500,9000), ylim = c(-7000,10000))
plot(OCC[[1]], main = "Leave 1 Traversal Out", xlim = c(-23500,9000), ylim = c(-7000,10000))
plot(OCC[[2]], main = "Leave 2 Traversals Out", xlim = c(-23500,9000), ylim = c(-7000,10000))
dev.off()


# # Corridor distribution
# COR_ALL[[3]] <- list()  # empty list for CDs
# for (i in 1:length(DATA)) {
#   
#   # Keep one path each iteration
#   DATA4 <- DATA[i]
#   corfits4 <- corfits[i]
#   ## length(DATA4) == 1
#   
#   # Corridor distribution
#   COR_ALL[[3]][[i]] <- ctmm:::corridor(data = DATA4, CTMM = corfits4, grid = list(dr = c(100,100)))
# }  # L3OCV corridor

## CANNOT EVALUATE CORRIDOR WITH ONLY 1 TRAVERSAL
# Corridor distribution based on all traversals
COR_ALL[[3]] <- list()
COR_ALL[[3]] <- ctmm:::corridor(data = DATA, CTMM = corfits, grid = list(dr = c(100,100), 
                                                                         align.to.origin = TRUE))

# save(COR_ALL, file = "data/jaguar/corridor_sensitivity_LOOCV_all.rda")
load(file = "data/jaguar/corridor_sensitivity_LOOCV_all.rda")

COR[[3]] <- COR_ALL[[3]]

# save(COR, file = "data/jaguar/corridor_sensitivity_LOOCV_fix.rda")
load(file = "data/jaguar/corridor_sensitivity_LOOCV_fix.rda")

# save(COR, file = "data/jaguar/corridor_sensitivity_LOOCV.rda")
load(file = "data/jaguar/corridor_sensitivity_LOOCV.rda")

par(mfrow = c(1,1))
plot(COR[[3]])

png(file = "figures/jaguar/corridor_paths_LOOCV_full.png", width = 2600, height = 6000, units = "px", res = 600)
par(mfrow = c(3,1), mai = c(0.8,0.3,0.7,0), omi = c(0,0.1,0,0.25), mar = c(4,4,3,0.4) + 0.1, cex.main = 1.4)
# png(file = "figures/jaguar/corridor_paths_LOOCV_full.png", width = 4, height = 6.5, units = "in", res = 600)
# par(mfrow = c(3,1), mai = c(0.8,0.3,0.7,0), omi = c(0,0.1,0,0.25), mar = c(4,4,3,0.4) + 0.1, cex.main = 1.2)
# Plot distributions
plot(COR[[3]], main = "Leave 0 Traversals Out", xlim = c(-23500,9000), ylim = c(-7000,10000))
plot(COR[[1]], level = NA, main = "Leave 1 Traversal Out", xlim = c(-23500,9000), ylim = c(-7000,10000))
plot(COR[[2]], level = NA, main = "Leave 2 Traversals Out", xlim = c(-23500,9000), ylim = c(-7000,10000))
dev.off()

# Reorder plots to compare ODs and Corridor distributions
# png(file = "figures/jaguar/sensitivity_comparison_paths.png", width = 4800, height = 6000, units = "px", res = 600)
# par(mfrow = c(3,2), mai = c(0.8,0.3,0.7,0), omi = c(0,0.25,0,0.25), mar = c(4,4,2,0.4) + 0.1, cex.main = 1.2)
png(file = "figures/jaguar/sensitivity_comparison_paths_LOOCV.png", width = 4800, height = 6000, units = "px", res = 600)
par(mfrow = c(3,2), mai = c(0.6,0.5,0.45,0.15), omi = c(0,0.25,0.25,0.25), cex.main = 1.2)
# Plot distributions
plot(OCC[[3]], main = "Leave 0 Traversals Out", xlim = c(-23500,9000), ylim = c(-7000,10000))
mtext(substitute(paste(bold("Occurrence Distribution"))), line = 3, side = 3)
plot(COR[[3]], level = NA, main = "Leave 0 Traversals Out", xlim = c(-23500,9000), ylim = c(-7000,10000))
mtext(substitute(paste(bold("Corridor Distribution"))), line = 3, side = 3)
plot(OCC[[1]], main = "Leave 1 Traversal Out", xlim = c(-23500,9000), ylim = c(-7000,10000))
plot(COR[[1]], level = NA, main = "Leave 1 Traversal Out", xlim = c(-23500,9000), ylim = c(-7000,10000))
plot(OCC[[2]], main = "Leave 2 Traversals Out", xlim = c(-23500,9000), ylim = c(-7000,10000))
plot(COR[[2]], level = NA, main = "Leave 2 Traversals Out", xlim = c(-23500,9000), ylim = c(-7000,10000))
dev.off()















####
# ## TEST WITH CTMM INTERNAL JAGUAR DATA
# library(ctmm)
# 
# data(jaguar)
# dat <- jaguar[[1]]
# projection(dat) <- median(dat)
# plot(dat, col = color(dat, by = "time"))
# 
# # Subset data (separate corridor traversals)
# plot(x = dat$y, y = dat$t, col = color(jaguar[[1]], by = "time"),
#      xlab = "y (m)", ylab = "Time (s)") +
#   abline(h = 1.38265e09) +
#   abline(h = 1.385e09) +
#   abline(h = 1.38552e09) +
#   abline(h = 1.3865e09) +
#   abline(h = 1.38813e09) +
#   abline(h = 1.38825e09)
# 
# # Split data into separate paths (effectively 7 corridor traversals)
# DATA <- list()
# DATA[[1]] <- dat[dat$t <= 1.38265e09,]
# DATA[[2]] <- dat[dat$t > 1.38265e09 & dat$t <= 1.385e09,]
# DATA[[3]] <- dat[dat$t > 1.385e09 & dat$t <= 1.38552e09,]
# DATA[[4]] <- dat[dat$t > 1.38552e09 & dat$t <= 1.3865e09,]
# DATA[[5]] <- dat[dat$t > 1.3865e09 & dat$t <= 1.38813e09,]
# DATA[[6]] <- dat[dat$t > 1.38813e09 & dat$t <= 1.38825e09,]
# DATA[[7]] <- dat[dat$t > 1.38825e09,]
# 
# projection(DATA) <- median(DATA)
# 
# plot(DATA, col = color(DATA, by = "individual"), error = FALSE)
# 
# # Model estimate and selection
# FITS <- list()  # empty list
# for (i in 1:length(DATA)) {  # for each individual
#   
#   # Estimate model parameters
#   GUESS <- ctmm.guess(DATA[[i]], interactive = FALSE)  # interactive mode is manual
#   # Model selection
#   FIT <- ctmm.select(DATA[[i]], GUESS, trace = 3, verbose = TRUE)  # fits multiple models
#   FITS[[i]] <- FIT  # store model selection results
# }
# 
# # save(FITS, file = "data/jaguar/fit_ctmmjaguar.rda")  # single longest step (before corridor)
# load(file = "data/jaguar/fit_ctmmjaguar.rda")
# 
# summary(FITS[[1]])
# summary(FITS[[2]])
# summary(FITS[[3]])
# summary(FITS[[4]])
# summary(FITS[[5]])
# summary(FITS[[6]])
# summary(FITS[[7]])
# 
# ## Anisotropic Ornstein-Uhlenbeck foraging (OUF anisotropic) model selected for all individuals
# # Extract and store the best fit for each individual
# corfits <- list()
# for (i in 1:length(FITS)) {
#   corfits[[i]] <- FITS[[i]][[1]]
# }
# 
# # Generate corridor range distribution
# COR <- list()  # empty list
# COR[[1]] <- ctmm:::corridor(data = DATA, CTMM = corfits)
# 
# plot(DATA, COR)
# 
# 
# # Compare to home range estimate
# GUESS <- ctmm.guess(dat, interactive = FALSE)  # interactive mode is manual
# # Model selection
# FIT <- ctmm.select(dat, GUESS, trace = 3, verbose = TRUE)  # fits multiple models
# summary(FIT)
# 
# HR <- akde(data = dat, CTMM = FIT$`OUF anisotropic`)
# 
# plot(dat, HR)
# 
# png(file = "figures/jaguar/ctmmjaguar_corridor_HR.png", width = 6000, height = 4800, units = "px", res = 600)
# par(mfrow = c(1,2))
# plot(DATA, COR, col = color(DATA, by = "individual"), 
#      main = "Corridor distribution",
#      xlim = c(-10000,11000), ylim = c(-20000,13000))
# plot(dat, HR, 
#      main = "AKDE Range distribution",
#      xlim = c(-10000,11000), ylim = c(-20000,13000))
# dev.off()



####
### OLD RANDOM SEED TRAVERSAL SENSITIVITY
# ## Occurrence Distributions 
# 
# # Duplicate data for manipulation (path removal)
# DATA2 <- DATA
# corfits2 <- corfits
# names(corfits2) <- names(DATA2)
# 
# # Generate occurrence distributions
# OCC <- list()  # empty list
# OCC[[1]] <- occurrence(DATA2, corfits2)
# OCC[[1]] <- mean(OCC[[1]])  # average distributions
# 
# set.seed(1)  # set seed for reproducibility
# 
# # Generate occurrence distribution for each subset of data
# for (m in 2:7) {
#   
#   # Randomize individual selected
#   j <- sample(names(DATA2), size = 1)
#   
#   # Remove selected individual path each loop
#   DATA2 <- DATA2[-which(names(DATA2) == j)]
#   corfits2 <- corfits2[-which(names(corfits2) == j)]
#   
#   # Occurrence distribution
#   OCC[[m]] <- occurrence(DATA2, corfits2)
#   OCC[[m]] <- mean(OCC[[m]])
# }
# 
# # save(OCC, file = "data/mule_deer/occurrence_sensitivity_paths_random1.rda")
# load(file = "data/mule_deer/occurrence_sensitivity_paths_random1.rda")
# 
# # Combined figure
# # png(file = "figures/mule_deer/occurrence_comb_2x2.png", width = 6.5, height = 5.1, units = "in", res = 600)
# # par(mfrow = c(2,2), mai = c(0.8,0.3,0.7,0), omi = c(0,0.25,0,0.25),
# #     mar = c(4,4,2,0.4) + 0.1, cex.main = 1.2)
# png(file = "figures/mule_deer/occurrence_comb_H_paths1.png", width = 9, height = 5.55, units = "in", res = 600)
# par(mfrow = c(2,3), mai = c(0.6,0.5,0.45,0.15), omi = c(0,0.25,0,0.25), cex.main = 1.4)
# # Plot all distributions
# plot(OCC[[1]], main = "7 Passages", xlim = c(-25000,35000), ylim = c(-20000,20000))
# plot(OCC[[2]], main = "6 Passages", xlim = c(-25000,35000), ylim = c(-20000,20000))  #
# plot(OCC[[3]], main = "5 Passages", xlim = c(-25000,35000), ylim = c(-20000,20000))
# plot(OCC[[4]], main = "4 Passages", xlim = c(-25000,35000), ylim = c(-20000,20000))
# plot(OCC[[5]], main = "3 Passages", xlim = c(-25000,35000), ylim = c(-20000,20000))  #
# plot(OCC[[6]], main = "2 Passages", xlim = c(-25000,35000), ylim = c(-20000,20000))
# # plot(OCC[[7]], main = "1 Passage", xlim = c(-25000,35000), ylim = c(-20000,20000))
# dev.off()
# 
# 
# ## Corridor Distributions 
# 
# ## SEED = 1
# # Duplicate data for manipulation (path removal)
# DATA2 <- DATA
# corfits2 <- corfits
# names(corfits2) <- names(DATA2)
# 
# # Generate corridor range distribution
# COR <- list()  # empty list
# COR[[1]] <- ctmm:::corridor(data = DATA2, CTMM = corfits2, grid = list(dr = c(100,100)))
# 
# set.seed(1)  # set seed for reproducibility
# 
# # Generate range distribution for each subset of data
# for (m in 2:6) {
#   
#   # Randomize individual selected
#   j <- sample(names(DATA2), size = 1)
#   
#   # Remove selected individual path each loop
#   DATA2 <- DATA2[-which(names(DATA2) == j)]
#   corfits2 <- corfits2[-which(names(corfits2) == j)]
#   
#   # Corridor range distribution
#   COR[[m]] <- ctmm:::corridor(data = DATA2, CTMM = corfits2, grid = list(dr = c(100,100)))
# }
# 
# # save(COR, file = "data/mule_deer/corridor_sensitivity_paths_random1.rda")
# load(file = "data/mule_deer/corridor_sensitivity_paths_random1.rda")
# 
# # Combined figure
# # png(file = "figures/mule_deer/corridor_comb_2x2_fix.png", width = 6.5, height = 5.1, units = "in", res = 600)
# # par(mfrow = c(2,2), mai = c(0.8,0.3,0.7,0), omi = c(0,0.25,0,0.25),
# #     mar = c(4,4,2,0.4) + 0.1, cex.main = 1.2)
# png(file = "figures/mule_deer/corridor_comb_H_paths1.png", width = 9, height = 5.55, units = "in", res = 600)
# par(mfrow = c(2,3), mai = c(0.6,0.5,0.45,0.15), omi = c(0,0.25,0,0.25), cex.main = 1.4)
# # Plot all distributions
# plot(COR[[1]], main = "7 Passages", xlim = c(-25000,35000), ylim = c(-20000,20000))
# plot(COR[[2]], main = "6 Passages", xlim = c(-25000,35000), ylim = c(-20000,20000))
# plot(COR[[3]], main = "5 Passages", xlim = c(-25000,35000), ylim = c(-20000,20000))
# plot(COR[[4]], main = "4 Passages", xlim = c(-25000,35000), ylim = c(-20000,20000))
# plot(COR[[5]], main = "3 Passages", xlim = c(-25000,35000), ylim = c(-20000,20000))
# plot(COR[[6]], main = "2 Passages", xlim = c(-25000,35000), ylim = c(-20000,20000))
# dev.off()
# 
# # Combined figure with tracks
# # png(file = "figures/mule_deer/corridor_comb_2x2_fix.png", width = 6.5, height = 5.1, units = "in", res = 600)
# # par(mfrow = c(2,2), mai = c(0.8,0.3,0.7,0), omi = c(0,0.25,0,0.25),
# #     mar = c(4,4,2,0.4) + 0.1, cex.main = 1.2)
# png(file = "figures/mule_deer/corridor_comb_H_paths1_tracks.png", width = 9, height = 5.55, units = "in", res = 600)
# par(mfrow = c(2,3), mai = c(0.6,0.5,0.45,0.15), omi = c(0,0.25,0,0.25), cex.main = 1.4)
# # Plot all distributions
# plot(DATA, COR[[1]], main = "7 Passages", col = color(DATA, by = "individual"), xlim = c(-25000,35000))
# plot(DATA, COR[[2]], main = "6 Passages", col = color(DATA, by = "individual"), xlim = c(-25000,35000))
# plot(DATA, COR[[3]], main = "5 Passages", col = color(DATA, by = "individual"), xlim = c(-25000,35000))
# plot(DATA, COR[[4]], main = "4 Passages", col = color(DATA, by = "individual"), xlim = c(-25000,35000))
# plot(DATA, COR[[5]], main = "3 Passages", col = color(DATA, by = "individual"), xlim = c(-25000,35000))
# plot(DATA, COR[[6]], main = "2 Passages", col = color(DATA, by = "individual"), xlim = c(-25000,35000))
# dev.off()
# 
# 
# ## SEED = 26
# # Duplicate data for manipulation (path removal)
# DATA2 <- DATA
# corfits2 <- corfits
# names(corfits2) <- names(DATA2)
# 
# # # Generate corridor range distribution
# # COR <- list()  # empty list
# # COR[[1]] <- ctmm:::corridor(data = DATA2, CTMM = corfits2, grid = list(dr = c(100,100)))
# 
# set.seed(26)  # set seed for reproducibility
# 
# # Generate range distribution for each subset of data
# for (m in 2:6) {
#   
#   # Randomize individual selected
#   j <- sample(names(DATA2), size = 1)
#   
#   # Remove selected individual path each loop
#   DATA2 <- DATA2[-which(names(DATA2) == j)]
#   corfits2 <- corfits2[-which(names(corfits2) == j)]
#   
#   # Corridor range distribution
#   COR[[m]] <- ctmm:::corridor(data = DATA2, CTMM = corfits2, grid = list(dr = c(100,100)))
# }
# 
# # save(COR, file = "data/mule_deer/corridor_sensitivity_paths_random26.rda")
# load(file = "data/mule_deer/corridor_sensitivity_paths_random26.rda")
# 
# # Combined figure
# # png(file = "figures/mule_deer/corridor_comb_2x2_fix.png", width = 6.5, height = 5.1, units = "in", res = 600)
# # par(mfrow = c(2,2), mai = c(0.8,0.3,0.7,0), omi = c(0,0.25,0,0.25),
# #     mar = c(4,4,2,0.4) + 0.1, cex.main = 1.2)
# png(file = "figures/mule_deer/corridor_comb_H_paths26.png", width = 9, height = 5.55, units = "in", res = 600)
# par(mfrow = c(2,3), mai = c(0.6,0.5,0.45,0.15), omi = c(0,0.25,0,0.25), cex.main = 1.4)
# # Plot all distributions
# plot(COR[[1]], main = "7 Passages", xlim = c(-25000,35000), ylim = c(-20000,20000))
# plot(COR[[2]], main = "6 Passages", xlim = c(-25000,35000), ylim = c(-20000,20000))
# plot(COR[[3]], main = "5 Passages", xlim = c(-25000,35000), ylim = c(-20000,20000))
# plot(COR[[4]], main = "4 Passages", xlim = c(-25000,35000), ylim = c(-20000,20000))
# plot(COR[[5]], main = "3 Passages", xlim = c(-25000,35000), ylim = c(-20000,20000))
# plot(COR[[6]], main = "2 Passages", xlim = c(-25000,35000), ylim = c(-20000,20000))
# dev.off()
# 
# # Combined figure with tracks
# # png(file = "figures/mule_deer/corridor_comb_2x2_fix.png", width = 6.5, height = 5.1, units = "in", res = 600)
# # par(mfrow = c(2,2), mai = c(0.8,0.3,0.7,0), omi = c(0,0.25,0,0.25),
# #     mar = c(4,4,2,0.4) + 0.1, cex.main = 1.2)
# png(file = "figures/mule_deer/corridor_comb_H_paths26_tracks.png", width = 9, height = 5.55, units = "in", res = 600)
# par(mfrow = c(2,3), mai = c(0.6,0.5,0.45,0.15), omi = c(0,0.25,0,0.25), cex.main = 1.4)
# # Plot all distributions
# plot(DATA, COR[[1]], main = "7 Passages", col = color(DATA, by = "individual"), xlim = c(-25000,35000))
# plot(DATA, COR[[2]], main = "6 Passages", col = color(DATA, by = "individual"), xlim = c(-25000,35000))
# plot(DATA, COR[[3]], main = "5 Passages", col = color(DATA, by = "individual"), xlim = c(-25000,35000))
# plot(DATA, COR[[4]], main = "4 Passages", col = color(DATA, by = "individual"), xlim = c(-25000,35000))
# plot(DATA, COR[[5]], main = "3 Passages", col = color(DATA, by = "individual"), xlim = c(-25000,35000))
# plot(DATA, COR[[6]], main = "2 Passages", col = color(DATA, by = "individual"), xlim = c(-25000,35000))
# dev.off()
# 
# 
# ## SEED = 100
# # Duplicate data for manipulation (path removal)
# DATA2 <- DATA
# corfits2 <- corfits
# names(corfits2) <- names(DATA2)
# 
# # # Generate corridor range distribution
# # COR <- list()  # empty list
# # COR[[1]] <- ctmm:::corridor(data = DATA2, CTMM = corfits2, grid = list(dr = c(100,100)))
# 
# set.seed(100)  # set seed for reproducibility
# 
# # Generate range distribution for each subset of data
# for (m in 2:6) {
#   
#   # Randomize individual selected
#   j <- sample(names(DATA2), size = 1)
#   
#   # Remove selected individual path each loop
#   DATA2 <- DATA2[-which(names(DATA2) == j)]
#   corfits2 <- corfits2[-which(names(corfits2) == j)]
#   
#   # Corridor range distribution
#   COR[[m]] <- ctmm:::corridor(data = DATA2, CTMM = corfits2, grid = list(dr = c(100,100)))
# }
# 
# # save(COR, file = "data/mule_deer/corridor_sensitivity_paths_random100.rda")
# load(file = "data/mule_deer/corridor_sensitivity_paths_random100.rda")
# 
# # Combined figure
# # png(file = "figures/mule_deer/corridor_comb_2x2_fix.png", width = 6.5, height = 5.1, units = "in", res = 600)
# # par(mfrow = c(2,2), mai = c(0.8,0.3,0.7,0), omi = c(0,0.25,0,0.25),
# #     mar = c(4,4,2,0.4) + 0.1, cex.main = 1.2)
# png(file = "figures/mule_deer/corridor_comb_H_paths100.png", width = 9, height = 5.55, units = "in", res = 600)
# par(mfrow = c(2,3), mai = c(0.6,0.5,0.45,0.15), omi = c(0,0.25,0,0.25), cex.main = 1.4)
# # Plot all distributions
# plot(COR[[1]], main = "7 Passages", xlim = c(-25000,35000), ylim = c(-20000,20000))
# plot(COR[[2]], main = "6 Passages", xlim = c(-25000,35000), ylim = c(-20000,20000))
# plot(COR[[3]], main = "5 Passages", xlim = c(-25000,35000), ylim = c(-20000,20000))
# plot(COR[[4]], main = "4 Passages", xlim = c(-25000,35000), ylim = c(-20000,20000))
# plot(COR[[5]], main = "3 Passages", xlim = c(-25000,35000), ylim = c(-20000,20000))
# plot(COR[[6]], main = "2 Passages", xlim = c(-25000,35000), ylim = c(-20000,20000))
# dev.off()
# 
# # Combined figure with tracks
# # png(file = "figures/mule_deer/corridor_comb_2x2_fix.png", width = 6.5, height = 5.1, units = "in", res = 600)
# # par(mfrow = c(2,2), mai = c(0.8,0.3,0.7,0), omi = c(0,0.25,0,0.25),
# #     mar = c(4,4,2,0.4) + 0.1, cex.main = 1.2)
# png(file = "figures/mule_deer/corridor_comb_H_paths100_tracks.png", width = 9, height = 5.55, units = "in", res = 600)
# par(mfrow = c(2,3), mai = c(0.6,0.5,0.45,0.15), omi = c(0,0.25,0,0.25), cex.main = 1.4)
# # Plot all distributions
# plot(DATA, COR[[1]], main = "7 Passages", col = color(DATA, by = "individual"), xlim = c(-25000,35000))
# plot(DATA, COR[[2]], main = "6 Passages", col = color(DATA, by = "individual"), xlim = c(-25000,35000))
# plot(DATA, COR[[3]], main = "5 Passages", col = color(DATA, by = "individual"), xlim = c(-25000,35000))
# plot(DATA, COR[[4]], main = "4 Passages", col = color(DATA, by = "individual"), xlim = c(-25000,35000))
# plot(DATA, COR[[5]], main = "3 Passages", col = color(DATA, by = "individual"), xlim = c(-25000,35000))
# plot(DATA, COR[[6]], main = "2 Passages", col = color(DATA, by = "individual"), xlim = c(-25000,35000))
# dev.off()
# 
# # Reorder plots to compare ODs and Corridor distributions
# # png(file = "figures/mule_deer/sensitivity_comparison_paths.png", width = 4800, height = 6000, units = "px", res = 600)
# # par(mfrow = c(3,2), mai = c(0.8,0.3,0.7,0), omi = c(0,0.25,0,0.25), mar = c(4,4,2,0.4) + 0.1, cex.main = 1.2)
# png(file = "figures/mule_deer/sensitivity_comparison_paths_random1.png", width = 4800, height = 6000, units = "px", res = 600)
# par(mfrow = c(3,2), mai = c(0.6,0.5,0.45,0.15), omi = c(0,0.25,0.25,0.25), cex.main = 1.2)
# # Plot distributions
# plot(OCC[[1]], main = "7 Passages", xlim = c(-25000,35000), ylim = c(-20000,20000))
# mtext(substitute(paste(bold("Occurrence Distribution"))), line = 3, side = 3)
# plot(COR[[1]], main = "7 Passages", xlim = c(-25000,35000), ylim = c(-20000,20000))
# mtext(substitute(paste(bold("Corridor Distribution"))), line = 3, side = 3)
# plot(OCC[[3]], main = "5 Passages", xlim = c(-25000,35000), ylim = c(-20000,20000))
# plot(COR[[3]], main = "5 Passages", xlim = c(-25000,35000), ylim = c(-20000,20000))
# plot(OCC[[6]], main = "2 Passages", xlim = c(-25000,35000), ylim = c(-20000,20000))
# plot(COR[[6]], main = "2 Passages", xlim = c(-25000,35000), ylim = c(-20000,20000))
# dev.off()
# 
# 
# 
# # Duplicate data for manipulation (path removal)
# DATA2 <- DATA
# names(DATA2) <- c("path1", "path2", "path3", "path4")
# corfits2 <- corfits
# names(corfits2) <- names(DATA2)
# 
# # Generate occurrence distributions
# OCC <- list()  # empty list
# OCC[[1]] <- occurrence(DATA2, corfits2)
# OCC[[1]] <- mean(OCC[[1]])  # average distributions
# 
# set.seed(1)  # set seed for reproducibility
# 
# # Generate occurrence distribution for each subset of data
# for (m in 2:4) {
#   
#   # Randomize individual selected
#   j <- sample(names(DATA2), size = 1)
#   
#   # Remove selected individual path each loop
#   DATA2 <- DATA2[-which(names(DATA2) == j)]
#   corfits2 <- corfits2[-which(names(corfits2) == j)]
#   
#   # Occurrence distribution
#   OCC[[m]] <- occurrence(DATA2, corfits2)
#   OCC[[m]] <- mean(OCC[[m]])
# }
# 
# # save(OCC, file = "data/jaguar/occurrence_sensitivity_paths_random1.rda")
# load(file = "data/jaguar/occurrence_sensitivity_paths_random1.rda")
# 
# # Combined figure
# png(file = "figures/jaguar/occurrence_comb_paths1.png", width = 6.5, height = 5.1, units = "in", res = 600)
# par(mfrow = c(2,2), mai = c(0.8,0.3,0.7,0), omi = c(0,0.25,0,0.25), mar = c(4,4,2,0.4) + 0.1, cex.main = 1.2)
# # png(file = "figures/jaguar/occurrence_comb_H_paths.png", width = 9, height = 5.55, units = "in", res = 600)
# # par(mfrow = c(2,3), mai = c(0.6,0.5,0.45,0.15), omi = c(0,0.25,0,0.25), cex.main = 1.4)
# # Plot all distributions
# plot(OCC[[1]], main = "4 Passages", xlim = c(-25000,10000), ylim = c(-7000,9000))
# plot(OCC[[2]], main = "3 Passages", xlim = c(-25000,10000), ylim = c(-7000,9000))
# plot(OCC[[3]], main = "2 Passages", xlim = c(-25000,10000), ylim = c(-7000,9000))
# plot(OCC[[4]], main = "1 Passage", xlim = c(-25000,10000), ylim = c(-7000,9000))
# dev.off()
# 
#
# ## Corridor Distributions
# 
# ## SEED = 1
# # Duplicate data for manipulation (path removal)
# DATA2 <- DATA
# names(DATA2) <- c("path1", "path2", "path3", "path4")
# corfits2 <- corfits
# names(corfits2) <- names(DATA2)
# 
# 
# # FIXME:
# 
# # Generate corridor range distribution
# COR <- list()  # empty list
# COR[[1]] <- ctmm:::corridor(data = DATA2, CTMM = corfits2, grid = list(dr = c(100,100)))
# 
# # # Generate corridor range distribution
# # COR_NEW <- list()  # empty list
# # COR_NEW[[1]] <- ctmm:::corridor(data = DATA2, CTMM = corfits2, res.time = 2, grid = list(dr = c(100,100)))
# # 
# # ## TEST
# # par(mfrow = c(1,2))
# # plot(DATA, COR_NEW[[1]], main = "res.time = 2", col = color(DATA, by = "individual"), xlim = c(-25000,10000), ylim = c(-7000,9000))
# plot(DATA, COR[[1]], main = "res.time = 1", col = color(DATA, by = "individual"), xlim = c(-25000,10000), ylim = c(-7000,9000))
# 
# # plot(DATA, COR_NEW[[4]], main = "res.time = 2, 100x100 grid, new fits", col = color(DATA, by = "individual"), xlim = c(-50000,100000), ylim = c(-7000,9000))
# 
# summary(DATA)
# 
# 
# 
# set.seed(1)  # set seed for reproducibility
# 
# # Generate range distribution for each subset of data
# for (m in 2:3) {
#   
#   # Randomize individual selected
#   j <- sample(names(DATA2), size = 1)
#   
#   # Remove selected individual path each loop
#   DATA2 <- DATA2[-which(names(DATA2) == j)]
#   corfits2 <- corfits2[-which(names(corfits2) == j)]
#   
#   # Corridor range distribution
#   COR[[m]] <- ctmm:::corridor(data = DATA2, CTMM = corfits2, grid = list(dr = c(100,100)))
# }
# 
# # # save(COR, file = "data/jaguar/corridor_sensitivity_paths_random1.rda")
# # load(file = "data/jaguar/corridor_sensitivity_paths_random1.rda")
# 
# # Combined figure
# png(file = "figures/jaguar/corridor_comb_paths_random1.png", width = 6.5, height = 5.1, units = "in", res = 600)
# par(mfrow = c(2,2), mai = c(0.8,0.3,0.7,0), omi = c(0,0.25,0,0.25), mar = c(4,4,2,0.4) + 0.1, cex.main = 1.2)
# # png(file = "figures/jaguar/corridor_comb_H_paths.png", width = 9, height = 5.55, units = "in", res = 600)
# # par(mfrow = c(2,3), mai = c(0.6,0.5,0.45,0.15), omi = c(0,0.25,0,0.25), cex.main = 1.4)
# # Plot all distributions
# plot(COR[[1]], main = "4 Passages", xlim = c(-25000,10000), ylim = c(-7000,9000))
# plot(COR[[2]], main = "3 Passages", xlim = c(-25000,10000), ylim = c(-7000,9000))
# plot(COR[[3]], main = "2 Passages", xlim = c(-25000,10000), ylim = c(-7000,9000))
# dev.off()

# Combined figure with tracks
png(file = "figures/jaguar/corridor_comb_paths1_tracks.png", width = 6.5, height = 5.1, units = "in", res = 600)
par(mfrow = c(2,2), mai = c(0.8,0.3,0.7,0), omi = c(0,0.25,0,0.25), mar = c(4,4,2,0.4) + 0.1, cex.main = 1.2)
# png(file = "figures/jaguar/corridor_comb_H_paths.png", width = 9, height = 5.55, units = "in", res = 600)
# par(mfrow = c(2,3), mai = c(0.6,0.5,0.45,0.15), omi = c(0,0.25,0,0.25), cex.main = 1.4)
# # Plot all distributions
# plot(DATA, COR[[1]], main = "4 Passages", col = color(DATA, by = "individual"), xlim = c(-25000,10000), ylim = c(-7000,9000))
# plot(DATA, COR[[2]], main = "3 Passages", col = color(DATA, by = "individual"), xlim = c(-25000,10000), ylim = c(-7000,9000))
# plot(DATA, COR[[3]], main = "2 Passages", col = color(DATA, by = "individual"), xlim = c(-25000,10000), ylim = c(-7000,9000))
# dev.off()
# 
# 
# ## SEED = 26
# # Duplicate data for manipulation (path removal)
# DATA2 <- DATA
# names(DATA2) <- c("path1", "path2", "path3", "path4")
# corfits2 <- corfits
# names(corfits2) <- names(DATA2)
# 
# # # Generate corridor range distribution
# # COR <- list()  # empty list
# # COR[[1]] <- ctmm:::corridor(data = DATA2, CTMM = corfits2, grid = list(dr = c(100,100)))
# 
# set.seed(26)  # set seed for reproducibility
# 
# # Generate range distribution for each subset of data
# for (m in 2:3) {
#   
#   # Randomize individual selected
#   j <- sample(names(DATA2), size = 1)
#   
#   # Remove selected individual path each loop
#   DATA2 <- DATA2[-which(names(DATA2) == j)]
#   corfits2 <- corfits2[-which(names(corfits2) == j)]
#   
#   # Corridor range distribution
#   COR[[m]] <- ctmm:::corridor(data = DATA2, CTMM = corfits2, grid = list(dr = c(100,100)))
# }
# 
# # save(COR, file = "data/jaguar/corridor_sensitivity_paths_random26.rda")
# load(file = "data/jaguar/corridor_sensitivity_paths_random26.rda")
# 
# # Combined figure
# png(file = "figures/jaguar/corridor_comb_paths_random26.png", width = 6.5, height = 5.1, units = "in", res = 600)
# par(mfrow = c(2,2), mai = c(0.8,0.3,0.7,0), omi = c(0,0.25,0,0.25), mar = c(4,4,2,0.4) + 0.1, cex.main = 1.2)
# # png(file = "figures/jaguar/corridor_comb_H_paths.png", width = 9, height = 5.55, units = "in", res = 600)
# # par(mfrow = c(2,3), mai = c(0.6,0.5,0.45,0.15), omi = c(0,0.25,0,0.25), cex.main = 1.4)
# # Plot all distributions
# plot(COR[[1]], main = "4 Passages", xlim = c(-25000,10000), ylim = c(-7000,9000))
# plot(COR[[2]], main = "3 Passages", xlim = c(-25000,10000), ylim = c(-7000,9000))
# plot(COR[[3]], main = "2 Passages", xlim = c(-25000,10000), ylim = c(-7000,9000))
# dev.off()
# 
# # Combined figure with tracks
# png(file = "figures/jaguar/corridor_comb_paths26_tracks.png", width = 6.5, height = 5.1, units = "in", res = 600)
# par(mfrow = c(2,2), mai = c(0.8,0.3,0.7,0), omi = c(0,0.25,0,0.25), mar = c(4,4,2,0.4) + 0.1, cex.main = 1.2)
# # png(file = "figures/jaguar/corridor_comb_H_paths.png", width = 9, height = 5.55, units = "in", res = 600)
# # par(mfrow = c(2,3), mai = c(0.6,0.5,0.45,0.15), omi = c(0,0.25,0,0.25), cex.main = 1.4)
# # Plot all distributions
# plot(DATA, COR[[1]], main = "4 Passages", col = color(DATA, by = "individual"), xlim = c(-25000,10000), ylim = c(-7000,9000))
# plot(DATA, COR[[2]], main = "3 Passages", col = color(DATA, by = "individual"), xlim = c(-25000,10000), ylim = c(-7000,9000))
# plot(DATA, COR[[3]], main = "2 Passages", col = color(DATA, by = "individual"), xlim = c(-25000,10000), ylim = c(-7000,9000))
# dev.off()
# 
# 
# ## SEED = 100
# # Duplicate data for manipulation (path removal)
# DATA2 <- DATA
# names(DATA2) <- c("path1", "path2", "path3", "path4")
# corfits2 <- corfits
# names(corfits2) <- names(DATA2)
# 
# # # Generate corridor range distribution
# # COR <- list()  # empty list
# # COR[[1]] <- ctmm:::corridor(data = DATA2, CTMM = corfits2, grid = list(dr = c(100,100)))
# 
# set.seed(100)  # set seed for reproducibility
# 
# # Generate range distribution for each subset of data
# for (m in 2:3) {
#   
#   # Randomize individual selected
#   j <- sample(names(DATA2), size = 1)
#   
#   # Remove selected individual path each loop
#   DATA2 <- DATA2[-which(names(DATA2) == j)]
#   corfits2 <- corfits2[-which(names(corfits2) == j)]
#   
#   # Corridor range distribution
#   COR[[m]] <- ctmm:::corridor(data = DATA2, CTMM = corfits2, grid = list(dr = c(100,100)))
# }
# 
# # save(COR, file = "data/jaguar/corridor_sensitivity_paths_random100.rda")
# load(file = "data/jaguar/corridor_sensitivity_paths_random100.rda")
# 
# # Combined figure
# png(file = "figures/jaguar/corridor_comb_paths_random100.png", width = 6.5, height = 5.1, units = "in", res = 600)
# par(mfrow = c(2,2), mai = c(0.8,0.3,0.7,0), omi = c(0,0.25,0,0.25), mar = c(4,4,2,0.4) + 0.1, cex.main = 1.2)
# # png(file = "figures/jaguar/corridor_comb_H_paths.png", width = 9, height = 5.55, units = "in", res = 600)
# # par(mfrow = c(2,3), mai = c(0.6,0.5,0.45,0.15), omi = c(0,0.25,0,0.25), cex.main = 1.4)
# # Plot all distributions
# plot(COR[[1]], main = "4 Passages", xlim = c(-25000,10000), ylim = c(-7000,9000))
# plot(COR[[2]], main = "3 Passages", xlim = c(-25000,10000), ylim = c(-7000,9000))
# plot(COR[[3]], main = "2 Passages", xlim = c(-25000,10000), ylim = c(-7000,9000))
# dev.off()
# 
# # Combined figure with tracks
# png(file = "figures/jaguar/corridor_comb_paths100_tracks.png", width = 6.5, height = 5.1, units = "in", res = 600)
# par(mfrow = c(2,2), mai = c(0.8,0.3,0.7,0), omi = c(0,0.25,0,0.25), mar = c(4,4,2,0.4) + 0.1, cex.main = 1.2)
# # png(file = "figures/jaguar/corridor_comb_H_paths.png", width = 9, height = 5.55, units = "in", res = 600)
# # par(mfrow = c(2,3), mai = c(0.6,0.5,0.45,0.15), omi = c(0,0.25,0,0.25), cex.main = 1.4)
# # Plot all distributions
# plot(DATA, COR[[1]], main = "4 Passages", col = color(DATA, by = "individual"), xlim = c(-25000,10000), ylim = c(-7000,9000))
# plot(DATA, COR[[2]], main = "3 Passages", col = color(DATA, by = "individual"), xlim = c(-25000,10000), ylim = c(-7000,9000))
# plot(DATA, COR[[3]], main = "2 Passages", col = color(DATA, by = "individual"), xlim = c(-25000,10000), ylim = c(-7000,9000))
# dev.off()
# 
# # Reorder plots to compare ODs and Corridor distributions
# # png(file = "figures/jaguar/sensitivity_comparison_paths.png", width = 4800, height = 6000, units = "px", res = 600)
# # par(mfrow = c(3,2), mai = c(0.8,0.3,0.7,0), omi = c(0,0.25,0,0.25), mar = c(4,4,2,0.4) + 0.1, cex.main = 1.2)
# png(file = "figures/jaguar/sensitivity_comparison_paths_random1.png", width = 4800, height = 6000, units = "px", res = 600)
# par(mfrow = c(3,2), mai = c(0.6,0.5,0.45,0.15), omi = c(0,0.25,0.25,0.25), cex.main = 1.2)
# # Plot distributions
# plot(OCC[[1]], main = "4 Passages", xlim = c(-25000,10000), ylim = c(-7000,9000))
# mtext(substitute(paste(bold("Occurrence Distribution"))), line = 3, side = 3)
# plot(COR[[1]], main = "4 Passages", xlim = c(-25000,10000), ylim = c(-7000,9000))
# mtext(substitute(paste(bold("Corridor Distribution"))), line = 3, side = 3)
# plot(OCC[[2]], main = "3 Passages", xlim = c(-25000,10000), ylim = c(-7000,9000))
# plot(COR[[2]], main = "3 Passages", xlim = c(-25000,10000), ylim = c(-7000,9000))
# plot(OCC[[3]], main = "2 Passages", xlim = c(-25000,10000), ylim = c(-7000,9000))
# plot(COR[[3]], main = "2 Passages", xlim = c(-25000,10000), ylim = c(-7000,9000))
# dev.off()





