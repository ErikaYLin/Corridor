# Corridor Debugging Sandbox

library(ctmm)

# Various datasets ----

## Mule deer (Odocoileus hemionus) migration data ----
# Wang, Yi-Shan; Blackwell, Paul G.; Merkle, Jerod A.; Potts, Jonathan R. (2019). 
# Data from: Continuous time resource selection analysis for moving animals [Dataset]. Dryad. 
# https://doi.org/10.5061/dryad.f9p3dq4
data <- read.csv("data/mule_deer/MuleDeer_Cody_ForPotts.csv")  
data <- as.telemetry(data)  # convert to telemetry object (drops un-needed columns)
projection(data) <- median(data)  # center projection on geometric median of data

# Subset data to single corridor
DATA <- data[c('36827', '36831', '36840', '36935', '36999', '37009', '37010')]
projection(DATA) <- median(DATA)  # center projection on geometric median of data

# Keep data within a certain range of distance along x-axis
for (i in 1:length(DATA)) {
  DATA[[i]] <- DATA[[i]][DATA[[i]]$x > 19*1000,]
  DATA[[i]] <- DATA[[i]][DATA[[i]]$x < 72*1000,]
}

# Remove outliers
DATA[[5]] <- DATA[[5]][-which(DATA[[5]]$t == max(DATA[[5]]$t)),]
DATA[[7]] <- DATA[[7]][-which(DATA[[7]]$t == min(DATA[[7]]$t)),]
projection(DATA) <- median(DATA)  # re-center projection

# Model fits
load(file = "data/mule_deer/fits_corridor.rda")

# Extract and store the best fit for each individual
corfits <- list()
for (i in 1:length(FITS)) {
  corfits[[i]] <- FITS[[i]]$`OUF anisotropic`
}


## Jaguars (Panthera onca) ----
# Morato, Ronaldo G. Jaguar Conservation in the Caatinga Biome Citation
# Acknowledgements: Gediendson Araujo, Leanes Silva, Valdomiro Lemos, Antonio Carlos Csemark 
# Grants Used: FAPESP 2013/10029-6, FAPESP 2014/24921-0 License Type: Custom License 
# Terms: No study-specific terms specified Principal Investigator
# Data retrieved from Movebank
jaguar <- read.csv("data/jaguar/Jaguar Conservation in the Caatinga Biome.csv")
jaguar <- as.telemetry(jaguar)  # convert to telemetry object (drops uneeded columns)
projection(jaguar) <- median(jaguar)  # center projection on geometric median of data

# Subset data
jag1 <- jaguar[[1]]  # ID: Courisco
projection(jag1) <- median(jag1)  # center projection on geometric median of data
# Keep data within a certain range of distance along x-axis
data <- jag1[jag1$x > 7*1000,]

# Split data into separate paths (effectively 4 complete passages through corridor)
DATA <- list()
DATA[[1]] <- data[data$t <= 1.455e9,]
DATA[[2]] <- data[data$t > 1.455e9 & data$t <= 1.458e9,]
DATA[[3]] <- data[data$t > 1.458e9 & data$t <= 1.459e9,]
DATA[[4]] <- data[data$t > 1.459e9,]
projection(DATA) <- median(DATA)

# Model fits
load(file = "data/jaguar/fit_courisco_4paths.rda")

## Anisotropic Ornstein-Uhlenbeck foraging (OUF anisotropic) model selected for all individuals
# Extract and store the best fit for each individual
corfits <- list()
for (i in 1:length(FITS)) {
  corfits[[i]] <- FITS[[i]]$`OUF anisotropic`
}


# Averaging corridor UD ----

## Mule Deer

COR <- list()  # empty list to store results

## MD COR[[1]] ----

load(file = "data/mule_deer/corridor_sensitivity_LOOCV_all.rda")  # COR_ALL for LOOCV
COR_TEST <- COR_ALL[[1]]  # save separately

# Assign same CTMM model to all corridor UDs for averaging
for (i in 1:length(COR_TEST)) {
  COR_TEST[[i]]@CTMM <- corfits[[1]]
}

# COR_mean <- mean.UD(COR_TEST)

# Mean function for corridor distributions
# mean.UD <- function(x,weights=NULL,sample=FALSE,...) {

x <- COR_TEST
weights = NULL
sample = FALSE

####
  
  n <- length(x)
  axes <- x[[1]]$axes
  
  if(is.null(weights)) {
    if(x[[1]]@type=="occurrence") # time weighted by default
    { weights <- sapply(x,function(y){y$W}) }
    else
    { weights <- rep(1,length(x)) }}
  
  weights <- weights/max(weights)
  names(weights) <- names(x)
  WEIGHT <- sum(weights)
  
  # list of individual models
  CTMM <- lapply(x,function(y){y@CTMM})
  # population model
  CTMM <- ctmm:::mean.ctmm(CTMM,weights=weights,sample=sample)
  # population stationary distribution
  # if(sample) { CTMM <- ctmm:::mean_pop(CTMM) }
  
  info <- ctmm:::mean_info(x)
  type <- unique(sapply(x,function(y){attr(y,"type")}))
  if(length(type)>1) { stop("Distribution types ",type," differ.") }
  
  # # harmonic mean bandwidth matrix
  # if(all(sapply(x,function(y){"H" %in% names(y)}))) {
  #   H <- 0
  #   for(i in 1:n) { H <- H + weights[i] * ctmm:::pd.solve(x[[i]]$H) }
  #   H <- H/WEIGHT
  #   H <- ctmm:::pd.solve(H) }
  
  dV <- prod(x[[1]]$dr)
  
  GRID <- ctmm:::grid.union(x) # r,dr of grid union
  DIM <- c(length(GRID$r$x),length(GRID$r$y))
  PDF <- matrix(0,DIM[1],DIM[2]) # initialize Joint PDF
  
  SUB <- list()
  TEST <- list()
  TEST2 <- list()
  for(i in 1:n) {
    SUB[[i]] <- ctmm:::grid.intersection(list(GRID,x[[i]]))
    TEST[[i]] <- list(dim(PDF[SUB[[i]][[1]]$x,SUB[[i]][[1]]$y]), 
                      dim(x[[i]]$PDF[SUB[[i]][[2]]$x,SUB[[i]][[2]]$y]))
    
    # Check for mismatched matrix dimensions
    if(TEST[[i]][[1]][1] != TEST[[i]][[2]][1] | TEST[[i]][[1]][2] != TEST[[i]][[2]][2]) {
      TEST2[[paste(i)]] <- TEST[[i]][[2]] - TEST[[i]][[1]]
    }  # calculate the difference
    
    ## Must sum manually due to problem with non-conforming arrays (matrix dims don't match for some)
    # PDF[SUB[[1]]$x,SUB[[1]]$y] <- PDF[SUB[[1]]$x,SUB[[1]]$y] + x[[i]]$PDF[SUB[[2]]$x,SUB[[2]]$y]  # weights[i] * x[[i]]$PDF[SUB[[2]]$x,SUB[[2]]$y]
  }
  
  # Fix non-conforming matrices
  which(SUB[[2]][[1]]$x == FALSE)  # 18
  which(SUB[[2]][[1]]$y == FALSE)  # 18
  SUB[[2]][[1]]$x[[18]] <- TRUE
  SUB[[2]][[1]]$y[[18]] <- TRUE
  
  which(SUB[[5]][[1]]$y == FALSE)  # 154
  SUB[[5]][[1]]$y[[154]] <- TRUE
  
  which(SUB[[6]][[1]]$x == FALSE)  # 17
  SUB[[6]][[1]]$x[[17]] <- TRUE
  
  # Sum adjusted PDFs
  for (i in 1:n) {
    PDF[SUB[[i]][[1]]$x,SUB[[i]][[1]]$y] <- PDF[SUB[[i]][[1]]$x,SUB[[i]][[1]]$y] + x[[i]]$PDF[SUB[[i]][[2]]$x,SUB[[i]][[2]]$y]  # weights[i] * x[[i]]$PDF[SUB[[2]]$x,SUB[[2]]$y]
  }
  PDF <- PDF / WEIGHT  # Divide by number of distributions
  
  x <- GRID
  x$weights <- weights
  x$axes <- axes
  x$PDF <- PDF
  x$CDF <- ctmm:::pmf2cdf(PDF*dV)
  if(type!="occurrence") { x$DOF.area <- ctmm:::DOF.area(CTMM) }
  # x$H <- H
  x$H <- NULL
  
  x <- ctmm:::new.UD(x,info=info,type=type,CTMM=CTMM)
  
  # return(x)
# }

COR[[1]] <- x

save(COR, file = "data/mule_deer/corridor_sensitivity_LOOCV.rda")
## RETURN TO "corridor_sensitivity_traversals.R" FOR RESULTS AND FIGURES


## MD COR[[2]] ----

COR_TEST <- COR_ALL[[2]]  # save separately

# Assign same CTMM model to all corridor UDs for averaging
for (i in 1:length(COR_TEST)) {
  COR_TEST[[i]]@CTMM <- corfits[[1]]
}

# COR_mean <- mean.UD(COR_TEST)

# Mean function for corridor distributions
# mean.UD <- function(x,weights=NULL,sample=FALSE,...) {

x <- COR_TEST
weights = NULL
sample = FALSE

####

n <- length(x)
axes <- x[[1]]$axes

if(is.null(weights)) {
  if(x[[1]]@type=="occurrence") # time weighted by default
  { weights <- sapply(x,function(y){y$W}) }
  else
  { weights <- rep(1,length(x)) }}

weights <- weights/max(weights)
names(weights) <- names(x)
WEIGHT <- sum(weights)

# list of individual models
CTMM <- lapply(x,function(y){y@CTMM})
# population model
CTMM <- ctmm:::mean.ctmm(CTMM,weights=weights,sample=sample)
# population stationary distribution
# if(sample) { CTMM <- ctmm:::mean_pop(CTMM) }

info <- ctmm:::mean_info(x)
type <- unique(sapply(x,function(y){attr(y,"type")}))
if(length(type)>1) { stop("Distribution types ",type," differ.") }

# # harmonic mean bandwidth matrix
# if(all(sapply(x,function(y){"H" %in% names(y)}))) {
#   H <- 0
#   for(i in 1:n) { H <- H + weights[i] * ctmm:::pd.solve(x[[i]]$H) }
#   H <- H/WEIGHT
#   H <- ctmm:::pd.solve(H) }

dV <- prod(x[[1]]$dr)

GRID <- ctmm:::grid.union(x) # r,dr of grid union
DIM <- c(length(GRID$r$x),length(GRID$r$y))
PDF <- matrix(0,DIM[1],DIM[2]) # initialize Joint PDF

SUB <- list()
TEST <- list()
TEST2 <- list()
for(i in 1:n) {
  SUB[[i]] <- ctmm:::grid.intersection(list(GRID,x[[i]]))
  TEST[[i]] <- list(dim(PDF[SUB[[i]][[1]]$x,SUB[[i]][[1]]$y]), 
                    dim(x[[i]]$PDF[SUB[[i]][[2]]$x,SUB[[i]][[2]]$y]))
  
  # Check for mismatched matrix dimensions
  if(TEST[[i]][[1]][1] != TEST[[i]][[2]][1] | TEST[[i]][[1]][2] != TEST[[i]][[2]][2]) {
    TEST2[[paste(i)]] <- TEST[[i]][[2]] - TEST[[i]][[1]]
    }  # calculate the difference
  
  ## Must sum manually due to problem with non-conforming arrays (matrix dims don't match for some)
  # PDF[SUB[[1]]$x,SUB[[1]]$y] <- PDF[SUB[[1]]$x,SUB[[1]]$y] + x[[i]]$PDF[SUB[[2]]$x,SUB[[2]]$y]  # weights[i] * x[[i]]$PDF[SUB[[2]]$x,SUB[[2]]$y]
}
## Many grid.intersection outputs don't match

### PDFs ----
# Fix non-conforming matrices
which(SUB[[1]][[1]]$x == FALSE)  # 77
which(SUB[[1]][[1]]$y == FALSE)  # 78
SUB[[1]][[1]]$x[[77]] <- TRUE
SUB[[1]][[1]]$y[[78]] <- TRUE

which(SUB[[2]][[1]]$x == FALSE)  # 50
which(SUB[[2]][[1]]$y == FALSE)  # 50
SUB[[2]][[1]]$x[[50]] <- TRUE
SUB[[2]][[1]]$y[[50]] <- TRUE

which(SUB[[4]][[1]]$x == FALSE)  # 146
which(SUB[[4]][[1]]$y == FALSE)  # 208
SUB[[4]][[1]]$x[[146]] <- TRUE
SUB[[4]][[1]]$y[[208]] <- TRUE

which(SUB[[5]][[1]]$x == FALSE)  # 75
which(SUB[[5]][[1]]$y == FALSE)  # 75
SUB[[5]][[1]]$x[[75]] <- TRUE
SUB[[5]][[1]]$y[[75]] <- TRUE

which(SUB[[7]][[1]]$x == FALSE)  # 20
SUB[[7]][[1]]$x[[20]] <- TRUE

which(SUB[[8]][[1]]$x == FALSE)  # 42
SUB[[8]][[1]]$x[[42]] <- TRUE

which(SUB[[9]][[1]]$x == FALSE)  # 112
which(SUB[[9]][[1]]$y == FALSE)  # 174
SUB[[9]][[1]]$x[[112]] <- TRUE
SUB[[9]][[1]]$y[[174]] <- TRUE

which(SUB[[10]][[1]]$x == FALSE)  # 41
which(SUB[[10]][[1]]$y == FALSE)  # 41
SUB[[10]][[1]]$x[[41]] <- TRUE
SUB[[10]][[1]]$y[[41]] <- TRUE

which(SUB[[11]][[1]]$x == FALSE)  # 21
which(SUB[[11]][[1]]$y == FALSE)  # 21
SUB[[11]][[1]]$x[[21]] <- TRUE
SUB[[11]][[1]]$y[[21]] <- TRUE

which(SUB[[13]][[1]]$x == FALSE)  # 115
which(SUB[[13]][[1]]$y == FALSE)  # 177
SUB[[13]][[1]]$x[[115]] <- TRUE
SUB[[13]][[1]]$y[[177]] <- TRUE

which(SUB[[14]][[1]]$x == FALSE)  # 19
SUB[[14]][[1]]$x[[19]] <- TRUE

which(SUB[[18]][[1]]$x == FALSE)  # 21
which(SUB[[18]][[1]]$y == FALSE)  # 21
SUB[[18]][[1]]$x[[21]] <- TRUE
SUB[[18]][[1]]$y[[21]] <- TRUE

which(SUB[[19]][[1]]$y == FALSE)  # 173
SUB[[19]][[1]]$y[[173]] <- TRUE

which(SUB[[20]][[1]]$x == FALSE)  # 100
which(SUB[[20]][[1]]$y == FALSE)  # 162
SUB[[20]][[1]]$x[[100]] <- TRUE
SUB[[20]][[1]]$y[[162]] <- TRUE

which(SUB[[21]][[1]]$y == FALSE)  # 20
SUB[[21]][[1]]$y[[20]] <- TRUE

### ... ----

# Sum adjusted PDFs
for (i in 1:n) {
  PDF[SUB[[i]][[1]]$x,SUB[[i]][[1]]$y] <- PDF[SUB[[i]][[1]]$x,SUB[[i]][[1]]$y] + x[[i]]$PDF[SUB[[i]][[2]]$x,SUB[[i]][[2]]$y]  # weights[i] * x[[i]]$PDF[SUB[[2]]$x,SUB[[2]]$y]
}
PDF <- PDF / WEIGHT  # Divide by number of distributions

x <- GRID
x$weights <- weights
x$axes <- axes
x$PDF <- PDF
x$CDF <- ctmm:::pmf2cdf(PDF*dV)
if(type!="occurrence") { x$DOF.area <- ctmm:::DOF.area(CTMM) }
# x$H <- H
x$H <- NULL

x <- ctmm:::new.UD(x,info=info,type=type,CTMM=CTMM)

# return(x)
# }

COR[[2]] <- x

save(COR, file = "data/mule_deer/corridor_sensitivity_LOOCV.rda")
## RETURN TO "corridor_sensitivity_traversals.R" FOR RESULTS AND FIGURES


## MD COR[[3]] ----

COR_TEST <- COR_ALL[[3]]  # save separately

# Assign same CTMM model to all corridor UDs for averaging
for (i in 1:length(COR_TEST)) {
  COR_TEST[[i]]@CTMM <- corfits[[1]]
}

# COR_mean <- mean.UD(COR_TEST)

# Mean function for corridor distributions
# mean.UD <- function(x,weights=NULL,sample=FALSE,...) {

x <- COR_TEST
weights = NULL
sample = FALSE

####

n <- length(x)
axes <- x[[1]]$axes

if(is.null(weights)) {
  if(x[[1]]@type=="occurrence") # time weighted by default
  { weights <- sapply(x,function(y){y$W}) }
  else
  { weights <- rep(1,length(x)) }}

weights <- weights/max(weights)
names(weights) <- names(x)
WEIGHT <- sum(weights)

# list of individual models
CTMM <- lapply(x,function(y){y@CTMM})
# population model
CTMM <- ctmm:::mean.ctmm(CTMM,weights=weights,sample=sample)
# population stationary distribution
# if(sample) { CTMM <- ctmm:::mean_pop(CTMM) }

info <- ctmm:::mean_info(x)
type <- unique(sapply(x,function(y){attr(y,"type")}))
if(length(type)>1) { stop("Distribution types ",type," differ.") }

# # harmonic mean bandwidth matrix
# if(all(sapply(x,function(y){"H" %in% names(y)}))) {
#   H <- 0
#   for(i in 1:n) { H <- H + weights[i] * ctmm:::pd.solve(x[[i]]$H) }
#   H <- H/WEIGHT
#   H <- ctmm:::pd.solve(H) }

dV <- prod(x[[1]]$dr)

GRID <- ctmm:::grid.union(x) # r,dr of grid union
DIM <- c(length(GRID$r$x),length(GRID$r$y))
PDF <- matrix(0,DIM[1],DIM[2]) # initialize Joint PDF

SUB <- list()
TEST <- list()
TEST2 <- list()
for(i in 1:n) {
  SUB[[i]] <- ctmm:::grid.intersection(list(GRID,x[[i]]))
  TEST[[i]] <- list(dim(PDF[SUB[[i]][[1]]$x,SUB[[i]][[1]]$y]), 
                    dim(x[[i]]$PDF[SUB[[i]][[2]]$x,SUB[[i]][[2]]$y]))
  
  # Check for mismatched matrix dimensions
  if(TEST[[i]][[1]][1] != TEST[[i]][[2]][1] | TEST[[i]][[1]][2] != TEST[[i]][[2]][2]) {
    TEST2[[paste(i)]] <- TEST[[i]][[2]] - TEST[[i]][[1]]
  }  # calculate the difference
  
  ## Must sum manually due to problem with non-conforming arrays (matrix dims don't match for some)
  # PDF[SUB[[1]]$x,SUB[[1]]$y] <- PDF[SUB[[1]]$x,SUB[[1]]$y] + x[[i]]$PDF[SUB[[2]]$x,SUB[[2]]$y]  # weights[i] * x[[i]]$PDF[SUB[[2]]$x,SUB[[2]]$y]
}
## Many grid.intersection outputs don't match

### PDFs ----
# Fix non-conforming matrices
which(SUB[[1]][[1]]$x == FALSE)  # 72
which(SUB[[1]][[1]]$y == FALSE)  # 73
SUB[[1]][[1]]$x[[72]] <- TRUE
SUB[[1]][[1]]$y[[73]] <- TRUE

which(SUB[[2]][[1]]$x == FALSE)  # 111
which(SUB[[2]][[1]]$y == FALSE)  # 112
SUB[[2]][[1]]$x[[111]] <- TRUE
SUB[[2]][[1]]$y[[112]] <- TRUE

which(SUB[[3]][[1]]$y == FALSE)  # 231
SUB[[3]][[1]]$y[[231]] <- TRUE

which(SUB[[4]][[1]]$x == FALSE)  # 109
which(SUB[[4]][[1]]$y == FALSE)  # 110
SUB[[4]][[1]]$x[[109]] <- TRUE
SUB[[4]][[1]]$y[[110]] <- TRUE

which(SUB[[5]][[1]]$y == FALSE)  # 74
SUB[[5]][[1]]$y[[74]] <- TRUE

which(SUB[[6]][[1]]$y == FALSE)  # 71
SUB[[6]][[1]]$y[[71]] <- TRUE

which(SUB[[7]][[1]]$y == FALSE)  # 224
SUB[[7]][[1]]$y[[224]] <- TRUE

which(SUB[[8]][[1]]$y == FALSE)  # 70
SUB[[8]][[1]]$y[[70]] <- TRUE

which(SUB[[9]][[1]]$x == FALSE)  # 40
which(SUB[[9]][[1]]$y == FALSE)  # 40
SUB[[9]][[1]]$x[[40]] <- TRUE
SUB[[9]][[1]]$y[[40]] <- TRUE

which(SUB[[11]][[1]]$x == FALSE)  # 108
which(SUB[[11]][[1]]$y == FALSE)  # 108
SUB[[11]][[1]]$x[[108]] <- TRUE
SUB[[11]][[1]]$y[[108]] <- TRUE

which(SUB[[12]][[1]]$x == FALSE)  # 73
which(SUB[[12]][[1]]$y == FALSE)  # 73
SUB[[12]][[1]]$x[[73]] <- TRUE
SUB[[12]][[1]]$y[[73]] <- TRUE

which(SUB[[13]][[1]]$x == FALSE)  # 168
which(SUB[[13]][[1]]$y == FALSE)  # 230
SUB[[13]][[1]]$x[[168]] <- TRUE
SUB[[13]][[1]]$y[[230]] <- TRUE

which(SUB[[14]][[1]]$x == FALSE)  # 146
which(SUB[[14]][[1]]$y == FALSE)  # 208
SUB[[14]][[1]]$x[[146]] <- TRUE
SUB[[14]][[1]]$y[[208]] <- TRUE

which(SUB[[15]][[1]]$x == FALSE)  # 71
which(SUB[[15]][[1]]$y == FALSE)  # 71
SUB[[15]][[1]]$x[[71]] <- TRUE
SUB[[15]][[1]]$y[[71]] <- TRUE

which(SUB[[16]][[1]]$x == FALSE)  # 28
SUB[[16]][[1]]$x[[28]] <- TRUE

which(SUB[[17]][[1]]$x == FALSE)  # 120
which(SUB[[17]][[1]]$y == FALSE)  # 182
SUB[[17]][[1]]$x[[120]] <- TRUE
SUB[[17]][[1]]$y[[182]] <- TRUE

which(SUB[[18]][[1]]$x == FALSE)  # 27
which(SUB[[18]][[1]]$y == FALSE)  # 27
SUB[[18]][[1]]$x[[27]] <- TRUE
SUB[[18]][[1]]$y[[27]] <- TRUE

which(SUB[[19]][[1]]$x == FALSE)  # 2
which(SUB[[19]][[1]]$y == FALSE)  # 2
SUB[[19]][[1]]$x[[2]] <- TRUE
SUB[[19]][[1]]$y[[2]] <- TRUE

which(SUB[[20]][[1]]$x == FALSE)  # 121
which(SUB[[20]][[1]]$y == FALSE)  # 184
SUB[[20]][[1]]$x[[121]] <- TRUE
SUB[[20]][[1]]$y[[184]] <- TRUE

which(SUB[[21]][[1]]$x == FALSE)  # 57
which(SUB[[21]][[1]]$y == FALSE)  # 58
SUB[[21]][[1]]$x[[57]] <- TRUE
SUB[[21]][[1]]$y[[58]] <- TRUE

which(SUB[[22]][[1]]$y == FALSE)  # 30
SUB[[22]][[1]]$y[[30]] <- TRUE

which(SUB[[23]][[1]]$x == FALSE)  # 120
which(SUB[[23]][[1]]$y == FALSE)  # 182
SUB[[23]][[1]]$x[[120]] <- TRUE
SUB[[23]][[1]]$y[[182]] <- TRUE

which(SUB[[24]][[1]]$x == FALSE)  # 105
which(SUB[[24]][[1]]$y == FALSE)  # 167
SUB[[24]][[1]]$x[[105]] <- TRUE
SUB[[24]][[1]]$y[[167]] <- TRUE

which(SUB[[25]][[1]]$x == FALSE)  # 28
which(SUB[[25]][[1]]$y == FALSE)  # 29
SUB[[25]][[1]]$x[[28]] <- TRUE
SUB[[25]][[1]]$y[[29]] <- TRUE

which(SUB[[27]][[1]]$x == FALSE)  # 26
SUB[[27]][[1]]$x[[26]] <- TRUE

which(SUB[[28]][[1]]$x == FALSE)  # 2
which(SUB[[28]][[1]]$y == FALSE)  # 2
SUB[[28]][[1]]$x[[2]] <- TRUE
SUB[[28]][[1]]$y[[2]] <- TRUE

which(SUB[[29]][[1]]$x == FALSE)  # 120
which(SUB[[29]][[1]]$y == FALSE)  # 182
SUB[[29]][[1]]$x[[120]] <- TRUE
SUB[[29]][[1]]$y[[182]] <- TRUE

which(SUB[[30]][[1]]$x == FALSE)  # 210
which(SUB[[30]][[1]]$y == FALSE)  # 350
SUB[[30]][[1]]$x[[210]] <- TRUE
SUB[[30]][[1]]$y[[350]] <- TRUE

which(SUB[[32]][[1]]$x == FALSE)  # 120
which(SUB[[32]][[1]]$y == FALSE)  # 183
SUB[[32]][[1]]$x[[120]] <- TRUE
SUB[[32]][[1]]$y[[183]] <- TRUE

which(SUB[[33]][[1]]$x == FALSE)  # 105
which(SUB[[33]][[1]]$y == FALSE)  # 167
SUB[[33]][[1]]$x[[105]] <- TRUE
SUB[[33]][[1]]$y[[167]] <- TRUE

which(SUB[[34]][[1]]$x == FALSE)  # 28
which(SUB[[34]][[1]]$y == FALSE)  # 28
SUB[[34]][[1]]$x[[28]] <- TRUE
SUB[[34]][[1]]$y[[28]] <- TRUE

which(SUB[[35]][[1]]$x == FALSE)  # 104
which(SUB[[35]][[1]]$y == FALSE)  # 166
SUB[[35]][[1]]$x[[104]] <- TRUE
SUB[[35]][[1]]$y[[166]] <- TRUE

### ... ----

# Sum adjusted PDFs
for (i in 1:n) {
  PDF[SUB[[i]][[1]]$x,SUB[[i]][[1]]$y] <- PDF[SUB[[i]][[1]]$x,SUB[[i]][[1]]$y] + x[[i]]$PDF[SUB[[i]][[2]]$x,SUB[[i]][[2]]$y]  # weights[i] * x[[i]]$PDF[SUB[[2]]$x,SUB[[2]]$y]
}
PDF <- PDF / WEIGHT  # Divide by number of distributions

x <- GRID
x$weights <- weights
x$axes <- axes
x$PDF <- PDF
x$CDF <- ctmm:::pmf2cdf(PDF*dV)
if(type!="occurrence") { x$DOF.area <- ctmm:::DOF.area(CTMM) }
# x$H <- H
x$H <- NULL

x <- ctmm:::new.UD(x,info=info,type=type,CTMM=CTMM)

# return(x)
# }

COR[[3]] <- x

save(COR, file = "data/mule_deer/corridor_sensitivity_LOOCV.rda")
## RETURN TO "corridor_sensitivity_traversals.R" FOR RESULTS AND FIGURES


## MD COR[[4]] ----

COR_TEST <- COR_ALL[[4]]  # save separately

# Assign same CTMM model to all corridor UDs for averaging
for (i in 1:length(COR_TEST)) {
  COR_TEST[[i]]@CTMM <- corfits[[1]]
}

# COR_mean <- mean.UD(COR_TEST)

# Mean function for corridor distributions
# mean.UD <- function(x,weights=NULL,sample=FALSE,...) {

x <- COR_TEST
weights = NULL
sample = FALSE

####

n <- length(x)
axes <- x[[1]]$axes

if(is.null(weights)) {
  if(x[[1]]@type=="occurrence") # time weighted by default
  { weights <- sapply(x,function(y){y$W}) }
  else
  { weights <- rep(1,length(x)) }}

weights <- weights/max(weights)
names(weights) <- names(x)
WEIGHT <- sum(weights)

# list of individual models
CTMM <- lapply(x,function(y){y@CTMM})
# population model
CTMM <- ctmm:::mean.ctmm(CTMM,weights=weights,sample=sample)
# population stationary distribution
# if(sample) { CTMM <- ctmm:::mean_pop(CTMM) }

info <- ctmm:::mean_info(x)
type <- unique(sapply(x,function(y){attr(y,"type")}))
if(length(type)>1) { stop("Distribution types ",type," differ.") }

# # harmonic mean bandwidth matrix
# if(all(sapply(x,function(y){"H" %in% names(y)}))) {
#   H <- 0
#   for(i in 1:n) { H <- H + weights[i] * ctmm:::pd.solve(x[[i]]$H) }
#   H <- H/WEIGHT
#   H <- ctmm:::pd.solve(H) }

dV <- prod(x[[1]]$dr)

GRID <- ctmm:::grid.union(x) # r,dr of grid union
DIM <- c(length(GRID$r$x),length(GRID$r$y))
PDF <- matrix(0,DIM[1],DIM[2]) # initialize Joint PDF

SUB <- list()
TEST <- list()
TEST2 <- list()
for(i in 1:n) {
  SUB[[i]] <- ctmm:::grid.intersection(list(GRID,x[[i]]))
  TEST[[i]] <- list(dim(PDF[SUB[[i]][[1]]$x,SUB[[i]][[1]]$y]), 
                    dim(x[[i]]$PDF[SUB[[i]][[2]]$x,SUB[[i]][[2]]$y]))
  
  # Check for mismatched matrix dimensions
  if(TEST[[i]][[1]][1] != TEST[[i]][[2]][1] | TEST[[i]][[1]][2] != TEST[[i]][[2]][2]) {
    TEST2[[paste(i)]] <- TEST[[i]][[2]] - TEST[[i]][[1]]
  }  # calculate the difference
  
  ## Must sum manually due to problem with non-conforming arrays (matrix dims don't match for some)
  # PDF[SUB[[1]]$x,SUB[[1]]$y] <- PDF[SUB[[1]]$x,SUB[[1]]$y] + x[[i]]$PDF[SUB[[2]]$x,SUB[[2]]$y]  # weights[i] * x[[i]]$PDF[SUB[[2]]$x,SUB[[2]]$y]
}
## Many grid.intersection outputs don't match

### PDFs ----
# Fix non-conforming matrices
which(SUB[[1]][[1]]$x == FALSE)  # 113
which(SUB[[1]][[1]]$y == FALSE)  # 114
SUB[[1]][[1]]$x[[113]] <- TRUE
SUB[[1]][[1]]$y[[114]] <- TRUE

which(SUB[[2]][[1]]$x == FALSE)  # 188,189
which(SUB[[2]][[1]]$y == FALSE)  # 251
SUB[[2]][[1]]$x[c(188,189)] <- TRUE
SUB[[2]][[1]]$y[[251]] <- TRUE

which(SUB[[3]][[1]]$x == FALSE)  # 111
which(SUB[[3]][[1]]$y == FALSE)  # 111
SUB[[3]][[1]]$x[[111]] <- TRUE
SUB[[3]][[1]]$y[[111]] <- TRUE

which(SUB[[4]][[1]]$x == FALSE)  # 62
which(SUB[[4]][[1]]$y == FALSE)  # 63
SUB[[4]][[1]]$x[[62]] <- TRUE
SUB[[4]][[1]]$y[[63]] <- TRUE

which(SUB[[5]][[1]]$x == FALSE)  # 210
which(SUB[[5]][[1]]$y == FALSE)  # 274
SUB[[5]][[1]]$x[[210]] <- TRUE
SUB[[5]][[1]]$y[[274]] <- TRUE

which(SUB[[6]][[1]]$x == FALSE)  # 187
which(SUB[[6]][[1]]$y == FALSE)  # 187,188
SUB[[6]][[1]]$x[[187]] <- TRUE
SUB[[6]][[1]]$y[c(187,188)] <- TRUE

which(SUB[[7]][[1]]$x == FALSE)  # 116,117
which(SUB[[7]][[1]]$y == FALSE)  # 117
SUB[[7]][[1]]$x[c(116,117)] <- TRUE
SUB[[7]][[1]]$y[[117]] <- TRUE

which(SUB[[8]][[1]]$x == FALSE)  # 209
which(SUB[[8]][[1]]$y == FALSE)  # 271
SUB[[8]][[1]]$x[[209]] <- TRUE
SUB[[8]][[1]]$y[[271]] <- TRUE

which(SUB[[9]][[1]]$x == FALSE)  # 173
which(SUB[[9]][[1]]$y == FALSE)  # 235
SUB[[9]][[1]]$x[[173]] <- TRUE
SUB[[9]][[1]]$y[[235]] <- TRUE

which(SUB[[10]][[1]]$x == FALSE)  # 114
which(SUB[[10]][[1]]$y == FALSE)  # 115
SUB[[10]][[1]]$x[[114]] <- TRUE
SUB[[10]][[1]]$y[[115]] <- TRUE

which(SUB[[11]][[1]]$x == FALSE)  # 189,190
which(SUB[[11]][[1]]$y == FALSE)  # 251
SUB[[11]][[1]]$x[c(189,190)] <- TRUE
SUB[[11]][[1]]$y[[251]] <- TRUE

which(SUB[[12]][[1]]$x == FALSE)  # 109,110
which(SUB[[12]][[1]]$y == FALSE)  # 109
SUB[[12]][[1]]$x[c(109,110)] <- TRUE
SUB[[12]][[1]]$y[[109]] <- TRUE

which(SUB[[13]][[1]]$x == FALSE)  # 60,61
which(SUB[[13]][[1]]$y == FALSE)  # 60,61
SUB[[13]][[1]]$x[c(60,61)] <- TRUE
SUB[[13]][[1]]$y[c(60,61)] <- TRUE

which(SUB[[14]][[1]]$x == FALSE)  # 188
which(SUB[[14]][[1]]$y == FALSE)  # 250
SUB[[14]][[1]]$x[[188]] <- TRUE
SUB[[14]][[1]]$y[[250]] <- TRUE

which(SUB[[15]][[1]]$x == FALSE)  # 289
which(SUB[[15]][[1]]$y == FALSE)  # 429
SUB[[15]][[1]]$x[[289]] <- TRUE
SUB[[15]][[1]]$y[[429]] <- TRUE

which(SUB[[16]][[1]]$x == FALSE)  # 59
which(SUB[[16]][[1]]$y == FALSE)  # 59
SUB[[16]][[1]]$x[[59]] <- TRUE
SUB[[16]][[1]]$y[[59]] <- TRUE

which(SUB[[17]][[1]]$x == FALSE)  # 209
which(SUB[[17]][[1]]$y == FALSE)  # 271
SUB[[17]][[1]]$x[[209]] <- TRUE
SUB[[17]][[1]]$y[[271]] <- TRUE

which(SUB[[18]][[1]]$x == FALSE)  # 173
which(SUB[[18]][[1]]$y == FALSE)  # 235
SUB[[18]][[1]]$x[[173]] <- TRUE
SUB[[18]][[1]]$y[[235]] <- TRUE

which(SUB[[19]][[1]]$x == FALSE)  # 113
which(SUB[[19]][[1]]$y == FALSE)  # 112
SUB[[19]][[1]]$x[[113]] <- TRUE
SUB[[19]][[1]]$y[[112]] <- TRUE

which(SUB[[20]][[1]]$x == FALSE)  # 171
which(SUB[[20]][[1]]$y == FALSE)  # 233
SUB[[20]][[1]]$x[[171]] <- TRUE
SUB[[20]][[1]]$y[[233]] <- TRUE

which(SUB[[21]][[1]]$x == FALSE)  # 127
which(SUB[[21]][[1]]$y == FALSE)  # 189
SUB[[21]][[1]]$x[[127]] <- TRUE
SUB[[21]][[1]]$y[[189]] <- TRUE

which(SUB[[22]][[1]]$x == FALSE)  # 40
which(SUB[[22]][[1]]$y == FALSE)  # 40
SUB[[22]][[1]]$x[[40]] <- TRUE
SUB[[22]][[1]]$y[[40]] <- TRUE

which(SUB[[23]][[1]]$x == FALSE)  # 2,3
which(SUB[[23]][[1]]$y == FALSE)  # 3
SUB[[23]][[1]]$x[c(2,3)] <- TRUE
SUB[[23]][[1]]$y[[3]] <- TRUE

which(SUB[[24]][[1]]$x == FALSE)  # 126
which(SUB[[24]][[1]]$y == FALSE)  # 188
SUB[[24]][[1]]$x[[126]] <- TRUE
SUB[[24]][[1]]$y[[188]] <- TRUE

which(SUB[[25]][[1]]$x == FALSE)  # 224,225
which(SUB[[25]][[1]]$y == FALSE)  # 365
SUB[[25]][[1]]$x[c(224,225)] <- TRUE
SUB[[25]][[1]]$y[[365]] <- TRUE

which(SUB[[26]][[1]]$x == FALSE)  # 1
which(SUB[[26]][[1]]$y == FALSE)  # 1,2
SUB[[26]][[1]]$x[[1]] <- TRUE
SUB[[26]][[1]]$y[c(1,2)] <- TRUE

which(SUB[[27]][[1]]$x == FALSE)  # 133,134
which(SUB[[27]][[1]]$y == FALSE)  # 195
SUB[[27]][[1]]$x[c(133,134)] <- TRUE
SUB[[27]][[1]]$y[[195]] <- TRUE

which(SUB[[28]][[1]]$x == FALSE)  # 111
which(SUB[[28]][[1]]$y == FALSE)  # 173
SUB[[28]][[1]]$x[[111]] <- TRUE
SUB[[28]][[1]]$y[[173]] <- TRUE

which(SUB[[29]][[1]]$x == FALSE)  # 42
which(SUB[[29]][[1]]$y == FALSE)  # 42
SUB[[29]][[1]]$x[[42]] <- TRUE
SUB[[29]][[1]]$y[[42]] <- TRUE

which(SUB[[30]][[1]]$x == FALSE)  # 109
which(SUB[[30]][[1]]$y == FALSE)  # 171,172
SUB[[30]][[1]]$x[[109]] <- TRUE
SUB[[30]][[1]]$y[c(171,172)] <- TRUE

which(SUB[[31]][[1]]$x == FALSE)  # 126
which(SUB[[31]][[1]]$y == FALSE)  # 188
SUB[[31]][[1]]$x[[126]] <- TRUE
SUB[[31]][[1]]$y[[188]] <- TRUE

which(SUB[[32]][[1]]$x == FALSE)  # 225,226
which(SUB[[32]][[1]]$y == FALSE)  # 365
SUB[[32]][[1]]$x[c(225,226)] <- TRUE
SUB[[32]][[1]]$y[[365]] <- TRUE

which(SUB[[33]][[1]]$x == FALSE)  # 866
which(SUB[[33]][[1]]$y == FALSE)  # 682
SUB[[33]][[1]]$x[[866]] <- TRUE
SUB[[33]][[1]]$y[[682]] <- TRUE

which(SUB[[34]][[1]]$x == FALSE)  # 242
which(SUB[[34]][[1]]$y == FALSE)  # 372
SUB[[34]][[1]]$x[[242]] <- TRUE
SUB[[34]][[1]]$y[[372]] <- TRUE

which(SUB[[35]][[1]]$x == FALSE)  # 109,110
which(SUB[[35]][[1]]$y == FALSE)  # 171
SUB[[35]][[1]]$x[c(109,110)] <- TRUE
SUB[[35]][[1]]$y[[171]] <- TRUE

### ... ----

# Sum adjusted PDFs
for (i in 1:n) {
  PDF[SUB[[i]][[1]]$x,SUB[[i]][[1]]$y] <- PDF[SUB[[i]][[1]]$x,SUB[[i]][[1]]$y] + x[[i]]$PDF[SUB[[i]][[2]]$x,SUB[[i]][[2]]$y]  # weights[i] * x[[i]]$PDF[SUB[[2]]$x,SUB[[2]]$y]
}
PDF <- PDF / WEIGHT  # Divide by number of distributions

x <- GRID
x$weights <- weights
x$axes <- axes
x$PDF <- PDF
x$CDF <- ctmm:::pmf2cdf(PDF*dV)
if(type!="occurrence") { x$DOF.area <- ctmm:::DOF.area(CTMM) }
# x$H <- H
x$H <- NULL

x <- ctmm:::new.UD(x,info=info,type=type,CTMM=CTMM)

# return(x)
# }

COR[[4]] <- x

save(COR, file = "data/mule_deer/corridor_sensitivity_LOOCV.rda")
## RETURN TO "corridor_sensitivity_traversals.R" FOR RESULTS AND FIGURES


## MD COR[[5]] ----

COR_TEST <- COR_ALL[[5]]  # save separately

# Assign same CTMM model to all corridor UDs for averaging
for (i in 1:length(COR_TEST)) {
  COR_TEST[[i]]@CTMM <- corfits[[1]]
}

# COR_mean <- mean.UD(COR_TEST)

# Mean function for corridor distributions
# mean.UD <- function(x,weights=NULL,sample=FALSE,...) {

x <- COR_TEST
weights = NULL
sample = FALSE

####

n <- length(x)
axes <- x[[1]]$axes

if(is.null(weights)) {
  if(x[[1]]@type=="occurrence") # time weighted by default
  { weights <- sapply(x,function(y){y$W}) }
  else
  { weights <- rep(1,length(x)) }}

weights <- weights/max(weights)
names(weights) <- names(x)
WEIGHT <- sum(weights)

# list of individual models
CTMM <- lapply(x,function(y){y@CTMM})
# population model
CTMM <- ctmm:::mean.ctmm(CTMM,weights=weights,sample=sample)
# population stationary distribution
# if(sample) { CTMM <- ctmm:::mean_pop(CTMM) }

info <- ctmm:::mean_info(x)
type <- unique(sapply(x,function(y){attr(y,"type")}))
if(length(type)>1) { stop("Distribution types ",type," differ.") }

# # harmonic mean bandwidth matrix
# if(all(sapply(x,function(y){"H" %in% names(y)}))) {
#   H <- 0
#   for(i in 1:n) { H <- H + weights[i] * ctmm:::pd.solve(x[[i]]$H) }
#   H <- H/WEIGHT
#   H <- ctmm:::pd.solve(H) }

dV <- prod(x[[1]]$dr)

GRID <- ctmm:::grid.union(x) # r,dr of grid union
DIM <- c(length(GRID$r$x),length(GRID$r$y))
PDF <- matrix(0,DIM[1],DIM[2]) # initialize Joint PDF

SUB <- list()
TEST <- list()
TEST2 <- list()
for(i in 1:n) {
  SUB[[i]] <- ctmm:::grid.intersection(list(GRID,x[[i]]))
  TEST[[i]] <- list(dim(PDF[SUB[[i]][[1]]$x,SUB[[i]][[1]]$y]), 
                    dim(x[[i]]$PDF[SUB[[i]][[2]]$x,SUB[[i]][[2]]$y]))
  
  # Check for mismatched matrix dimensions
  if(TEST[[i]][[1]][1] != TEST[[i]][[2]][1] | TEST[[i]][[1]][2] != TEST[[i]][[2]][2]) {
    TEST2[[paste(i)]] <- TEST[[i]][[2]] - TEST[[i]][[1]]
  }  # calculate the difference
  
  ## Must sum manually due to problem with non-conforming arrays (matrix dims don't match for some)
  # PDF[SUB[[1]]$x,SUB[[1]]$y] <- PDF[SUB[[1]]$x,SUB[[1]]$y] + x[[i]]$PDF[SUB[[2]]$x,SUB[[2]]$y]  # weights[i] * x[[i]]$PDF[SUB[[2]]$x,SUB[[2]]$y]
}
## Many grid.intersection outputs don't match

### PDFs ----
# Fix non-conforming matrices
which(SUB[[1]][[1]]$x == FALSE)  # 280
which(SUB[[1]][[1]]$y == FALSE)  # 411
SUB[[1]][[1]]$x[[280]] <- TRUE
SUB[[1]][[1]]$y[[411]] <- TRUE

which(SUB[[2]][[1]]$x == FALSE)  # 121
which(SUB[[2]][[1]]$y == FALSE)  # 182
SUB[[2]][[1]]$x[[121]] <- TRUE
SUB[[2]][[1]]$y[[182]] <- TRUE

which(SUB[[3]][[1]]$x == FALSE)  # 282
SUB[[3]][[1]]$x[[282]] <- TRUE

which(SUB[[5]][[1]]$y == FALSE)  # 400
SUB[[5]][[1]]$y[[400]] <- TRUE

which(SUB[[6]][[1]]$x == FALSE)  # 139
which(SUB[[6]][[1]]$y == FALSE)  # 200
SUB[[6]][[1]]$x[[139]] <- TRUE
SUB[[6]][[1]]$y[[200]] <- TRUE

which(SUB[[7]][[1]]$x == FALSE)  # 237
SUB[[7]][[1]]$x[[237]] <- TRUE

which(SUB[[8]][[1]]$x == FALSE)  # 387
which(SUB[[8]][[1]]$y == FALSE)  # 519
SUB[[8]][[1]]$x[[387]] <- TRUE
SUB[[8]][[1]]$y[[519]] <- TRUE

which(SUB[[9]][[1]]$x == FALSE)  # 116
which(SUB[[9]][[1]]$y == FALSE)  # 116
SUB[[9]][[1]]$x[[116]] <- TRUE
SUB[[9]][[1]]$y[[116]] <- TRUE

which(SUB[[10]][[1]]$y == FALSE)  # 507
SUB[[10]][[1]]$y[[507]] <- TRUE

which(SUB[[11]][[1]]$x == FALSE)  # 255
which(SUB[[11]][[1]]$y == FALSE)  # 316
SUB[[11]][[1]]$x[[255]] <- TRUE
SUB[[11]][[1]]$y[[316]] <- TRUE

which(SUB[[12]][[1]]$x == FALSE)  # 238
SUB[[12]][[1]]$x[[238]] <- TRUE

which(SUB[[13]][[1]]$x == FALSE)  # 262
SUB[[13]][[1]]$x[[262]] <- TRUE

which(SUB[[15]][[1]]$y == FALSE)  # 423
SUB[[15]][[1]]$y[[423]] <- TRUE

which(SUB[[16]][[1]]$x == FALSE)  # 118
which(SUB[[16]][[1]]$y == FALSE)  # 118
SUB[[16]][[1]]$x[[118]] <- TRUE
SUB[[16]][[1]]$y[[118]] <- TRUE

which(SUB[[17]][[1]]$y == FALSE)  # 502
SUB[[17]][[1]]$y[[502]] <- TRUE

which(SUB[[18]][[1]]$y == FALSE)  # 317
SUB[[18]][[1]]$y[[317]] <- TRUE

which(SUB[[19]][[1]]$x == FALSE)  # 122
which(SUB[[19]][[1]]$y == FALSE)  # 122
SUB[[19]][[1]]$x[[122]] <- TRUE
SUB[[19]][[1]]$y[[122]] <- TRUE

which(SUB[[20]][[1]]$x == FALSE)  # 250
which(SUB[[20]][[1]]$y == FALSE)  # 250
SUB[[20]][[1]]$x[[250]] <- TRUE
SUB[[20]][[1]]$y[[250]] <- TRUE

which(SUB[[21]][[1]]$x == FALSE)  # 257
which(SUB[[21]][[1]]$y == FALSE)  # 319
SUB[[21]][[1]]$x[[257]] <- TRUE
SUB[[21]][[1]]$y[[319]] <- TRUE

### ... ----

# Sum adjusted PDFs
for (i in 1:n) {
  PDF[SUB[[i]][[1]]$x,SUB[[i]][[1]]$y] <- PDF[SUB[[i]][[1]]$x,SUB[[i]][[1]]$y] + x[[i]]$PDF[SUB[[i]][[2]]$x,SUB[[i]][[2]]$y]  # weights[i] * x[[i]]$PDF[SUB[[2]]$x,SUB[[2]]$y]
}
PDF <- PDF / WEIGHT  # Divide by number of distributions

x <- GRID
x$weights <- weights
x$axes <- axes
x$PDF <- PDF
x$CDF <- ctmm:::pmf2cdf(PDF*dV)
if(type!="occurrence") { x$DOF.area <- ctmm:::DOF.area(CTMM) }
# x$H <- H
x$H <- NULL

x <- ctmm:::new.UD(x,info=info,type=type,CTMM=CTMM)

# return(x)
# }

COR[[5]] <- x

save(COR, file = "data/mule_deer/corridor_sensitivity_LOOCV.rda")
## RETURN TO "corridor_sensitivity_traversals.R" FOR RESULTS AND FIGURES


## MD COR[[6]] ----

# Cannot evaluate `ctmm:::corridor()` on single corridor traversals
## COR_ALL[[6]] consists of the corridor calculated with one of every individual
## as one would normally without removing individuals (leave-none-out).
## This produces a single corridor, so there is no need for averaging.
## RETURN TO "corridor_sensitivity_traversals.R" FOR RESULTS AND FIGURES


## Jaguars

COR <- list()  # empty list to store results

## JAG COR[[1]] ----

load(file = "data/jaguar/corridor_sensitivity_LOOCV_all.rda")  # COR_ALL for LOOCV
COR_TEST <- COR_ALL[[1]]  # save separately

# Assign same CTMM model to all corridor UDs for averaging
for (i in 1:length(COR_TEST)) {
  COR_TEST[[i]]@CTMM <- corfits[[1]]
}

# COR_mean <- mean.UD(COR_TEST)

# Mean function for corridor distributions
# mean.UD <- function(x,weights=NULL,sample=FALSE,...) {

x <- COR_TEST
weights = NULL
sample = FALSE

####

n <- length(x)
axes <- x[[1]]$axes

if(is.null(weights)) {
  if(x[[1]]@type=="occurrence") # time weighted by default
  { weights <- sapply(x,function(y){y$W}) }
  else
  { weights <- rep(1,length(x)) }}

weights <- weights/max(weights)
names(weights) <- names(x)
WEIGHT <- sum(weights)

# list of individual models
CTMM <- lapply(x,function(y){y@CTMM})
# population model
CTMM <- ctmm:::mean.ctmm(CTMM,weights=weights,sample=sample)
# population stationary distribution
# if(sample) { CTMM <- ctmm:::mean_pop(CTMM) }

info <- ctmm:::mean_info(x)
type <- unique(sapply(x,function(y){attr(y,"type")}))
if(length(type)>1) { stop("Distribution types ",type," differ.") }

# # harmonic mean bandwidth matrix
# if(all(sapply(x,function(y){"H" %in% names(y)}))) {
#   H <- 0
#   for(i in 1:n) { H <- H + weights[i] * ctmm:::pd.solve(x[[i]]$H) }
#   H <- H/WEIGHT
#   H <- ctmm:::pd.solve(H) }

dV <- prod(x[[1]]$dr)

GRID <- ctmm:::grid.union(x) # r,dr of grid union
DIM <- c(length(GRID$r$x),length(GRID$r$y))
PDF <- matrix(0,DIM[1],DIM[2]) # initialize Joint PDF

SUB <- list()
TEST <- list()
TEST2 <- list()
for(i in 1:n) {
  SUB[[i]] <- ctmm:::grid.intersection(list(GRID,x[[i]]))
  TEST[[i]] <- list(dim(PDF[SUB[[i]][[1]]$x,SUB[[i]][[1]]$y]), 
                    dim(x[[i]]$PDF[SUB[[i]][[2]]$x,SUB[[i]][[2]]$y]))
  
  # Check for mismatched matrix dimensions
  if(TEST[[i]][[1]][1] != TEST[[i]][[2]][1] | TEST[[i]][[1]][2] != TEST[[i]][[2]][2]) {
    TEST2[[paste(i)]] <- TEST[[i]][[2]] - TEST[[i]][[1]]
  }  # calculate the difference
  
  ## Must sum manually due to problem with non-conforming arrays (matrix dims don't match for some)
  # PDF[SUB[[1]]$x,SUB[[1]]$y] <- PDF[SUB[[1]]$x,SUB[[1]]$y] + x[[i]]$PDF[SUB[[2]]$x,SUB[[2]]$y]  # weights[i] * x[[i]]$PDF[SUB[[2]]$x,SUB[[2]]$y]
}

# Fix non-conforming matrices
which(SUB[[1]][[1]]$x == FALSE)  # 8
SUB[[1]][[1]]$x[[8]] <- TRUE

which(SUB[[2]][[1]]$x == FALSE)  # 3
SUB[[2]][[1]]$x[[3]] <- TRUE

which(SUB[[3]][[1]]$x == FALSE)  # 16
SUB[[3]][[1]]$x[[16]] <- TRUE

which(SUB[[4]][[1]]$x == FALSE)  # 417
which(SUB[[4]][[1]]$y == FALSE)  # 43
SUB[[4]][[1]]$x[[417]] <- TRUE
SUB[[4]][[1]]$y[[43]] <- TRUE

# Sum adjusted PDFs
for (i in 1:n) {
  PDF[SUB[[i]][[1]]$x,SUB[[i]][[1]]$y] <- PDF[SUB[[i]][[1]]$x,SUB[[i]][[1]]$y] + x[[i]]$PDF[SUB[[i]][[2]]$x,SUB[[i]][[2]]$y]  # weights[i] * x[[i]]$PDF[SUB[[2]]$x,SUB[[2]]$y]
}
PDF <- PDF / WEIGHT  # Divide by number of distributions

x <- GRID
x$weights <- weights
x$axes <- axes
x$PDF <- PDF
x$CDF <- ctmm:::pmf2cdf(PDF*dV)
if(type!="occurrence") { x$DOF.area <- ctmm:::DOF.area(CTMM) }
# x$H <- H
x$H <- NULL

x <- ctmm:::new.UD(x,info=info,type=type,CTMM=CTMM)

# return(x)
# }

COR[[1]] <- x

save(COR, file = "data/jaguar/corridor_sensitivity_LOOCV.rda")
## RETURN TO "corridor_sensitivity_traversals.R" FOR RESULTS AND FIGURES


## JAG COR[[2]] ----

load(file = "data/jaguar/corridor_sensitivity_LOOCV_all.rda")  # COR_ALL for LOOCV
COR_TEST <- COR_ALL[[2]]  # save separately

# Assign same CTMM model to all corridor UDs for averaging
for (i in 1:length(COR_TEST)) {
  COR_TEST[[i]]@CTMM <- corfits[[1]]
}

# COR_mean <- mean.UD(COR_TEST)

# Mean function for corridor distributions
# mean.UD <- function(x,weights=NULL,sample=FALSE,...) {

x <- COR_TEST
weights = NULL
sample = FALSE

####

n <- length(x)
axes <- x[[1]]$axes

if(is.null(weights)) {
  if(x[[1]]@type=="occurrence") # time weighted by default
  { weights <- sapply(x,function(y){y$W}) }
  else
  { weights <- rep(1,length(x)) }}

weights <- weights/max(weights)
names(weights) <- names(x)
WEIGHT <- sum(weights)

# list of individual models
CTMM <- lapply(x,function(y){y@CTMM})
# population model
CTMM <- ctmm:::mean.ctmm(CTMM,weights=weights,sample=sample)
# population stationary distribution
# if(sample) { CTMM <- ctmm:::mean_pop(CTMM) }

info <- ctmm:::mean_info(x)
type <- unique(sapply(x,function(y){attr(y,"type")}))
if(length(type)>1) { stop("Distribution types ",type," differ.") }

# # harmonic mean bandwidth matrix
# if(all(sapply(x,function(y){"H" %in% names(y)}))) {
#   H <- 0
#   for(i in 1:n) { H <- H + weights[i] * ctmm:::pd.solve(x[[i]]$H) }
#   H <- H/WEIGHT
#   H <- ctmm:::pd.solve(H) }

dV <- prod(x[[1]]$dr)

GRID <- ctmm:::grid.union(x) # r,dr of grid union
DIM <- c(length(GRID$r$x),length(GRID$r$y))
PDF <- matrix(0,DIM[1],DIM[2]) # initialize Joint PDF

SUB <- list()
TEST <- list()
TEST2 <- list()
for(i in 1:n) {
  SUB[[i]] <- ctmm:::grid.intersection(list(GRID,x[[i]]))
  TEST[[i]] <- list(dim(PDF[SUB[[i]][[1]]$x,SUB[[i]][[1]]$y]), 
                    dim(x[[i]]$PDF[SUB[[i]][[2]]$x,SUB[[i]][[2]]$y]))
  
  # Check for mismatched matrix dimensions
  if(TEST[[i]][[1]][1] != TEST[[i]][[2]][1] | TEST[[i]][[1]][2] != TEST[[i]][[2]][2]) {
    TEST2[[paste(i)]] <- TEST[[i]][[2]] - TEST[[i]][[1]]
  }  # calculate the difference
  
  ## Must sum manually due to problem with non-conforming arrays (matrix dims don't match for some)
  # PDF[SUB[[1]]$x,SUB[[1]]$y] <- PDF[SUB[[1]]$x,SUB[[1]]$y] + x[[i]]$PDF[SUB[[2]]$x,SUB[[2]]$y]  # weights[i] * x[[i]]$PDF[SUB[[2]]$x,SUB[[2]]$y]
}

# Fix non-conforming matrices
which(SUB[[1]][[1]]$x == FALSE)  # 3
SUB[[1]][[1]]$x[[3]] <- TRUE

which(SUB[[2]][[1]]$x == FALSE)  # 52
SUB[[2]][[1]]$x[[52]] <- TRUE

which(SUB[[3]][[1]]$x == FALSE)  # 57
SUB[[3]][[1]]$x[[57]] <- TRUE

which(SUB[[4]][[1]]$x == FALSE)  # 14
SUB[[4]][[1]]$x[[14]] <- TRUE

which(SUB[[5]][[1]]$x == FALSE)  # 435
SUB[[5]][[1]]$x[[435]] <- TRUE

which(SUB[[6]][[1]]$x == FALSE)  # 32,33
which(SUB[[6]][[1]]$y == FALSE)  # 39
SUB[[6]][[1]]$x[c(32,33)] <- TRUE
SUB[[6]][[1]]$y[[39]] <- TRUE

# Sum adjusted PDFs
for (i in 1:n) {
  PDF[SUB[[i]][[1]]$x,SUB[[i]][[1]]$y] <- PDF[SUB[[i]][[1]]$x,SUB[[i]][[1]]$y] + x[[i]]$PDF[SUB[[i]][[2]]$x,SUB[[i]][[2]]$y]  # weights[i] * x[[i]]$PDF[SUB[[2]]$x,SUB[[2]]$y]
}
PDF <- PDF / WEIGHT  # Divide by number of distributions

x <- GRID
x$weights <- weights
x$axes <- axes
x$PDF <- PDF
x$CDF <- ctmm:::pmf2cdf(PDF*dV)
if(type!="occurrence") { x$DOF.area <- ctmm:::DOF.area(CTMM) }
# x$H <- H
x$H <- NULL

x <- ctmm:::new.UD(x,info=info,type=type,CTMM=CTMM)

# return(x)
# }

COR[[2]] <- x

save(COR, file = "data/jaguar/corridor_sensitivity_LOOCV.rda")
## RETURN TO "corridor_sensitivity_traversals.R" FOR RESULTS AND FIGURES


## JAG COR[[3]] ----

# Cannot evaluate `ctmm:::corridor()` on single corridor traversals
## COR_ALL[[3]] consists of the corridor calculated with one of every individual
## as one would normally without removing individuals (leave-none-out).
## This produces a single corridor, so there is no need for averaging.
## RETURN TO "corridor_sensitivity_traversals.R" FOR RESULTS AND FIGURES


