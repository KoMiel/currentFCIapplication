
# import packages
library(rjson)

# set side length for block bootstrapping
density <- 35000

# number of models
nModels <- 100

# test threshold
alpha <- 0.05

# set working directory
setwd("..")

# import source file
source("currentFCI.R")

# read settings file
f <- "settings.json"
settings <- fromJSON(file = f)

# generate a random seed and use it
seed <- runif(1, min = 0, max = 1000000)
set.seed(seed)

# save the random seed for reproducibility
randomSeed <- c(seed, '\n')
filename <- "models/seed.txt"
sink(file = filename, append = FALSE)
cat(randomSeed)
sink()

# import variable names and categories from settings file and combine them
namesIn <- settings$namesIn
if (length(namesIn) == 0) { namesIn <- NULL}
namesUp <- settings$namesUp
if (length(namesUp) == 0) { namesUp <- NULL}
namesOut <- settings$namesOut
if (length(namesOut) == 0) { namesOut <- NULL}
namesMisc <- settings$namesMisc
if (length(namesMisc) == 0) { namesMisc <- NULL}
names <- c(namesIn, namesUp, namesOut, namesMisc)
names <- names[order(names)]

# read data
data <- readRDS("data/datasetHydrosheds.rds")
data <- as.data.frame(data)

# log transformation for exponentially distributed variables
data$ICOD <- log(data$ICOD)
data$UCOD <- log(data$UCOD)
data$ISpeCon <- log(data$ISpeCon)
data$USpeCon <- log(data$USpeCon)
data$ICaCO3 <- log(data$ICaCO3)
data$UCaCO3 <- log(data$UCaCO3)
data$IP <- log(data$IP)
data$UP <- log(data$UP)
data$ITKN <- log(data$ITKN)
data$UTKN <- log(data$UTKN)
data$ITDS <- log(data$ITDS)
data$UTDS <- log(data$UTDS)
data$ITSS <- log(data$ITSS)
data$UTSS <- log(data$UTSS)
data$IPAFInd <- log(data$IPAFInd)
data$UPAFInd <- log(data$UPAFInd)
data$IDraAre <- log(data$IDraAre)

# add IBI to variable names
namesIBI <- c(names, "IBI")

# add x and y coordinates for block building
dataIBI <- data[,c(namesIBI, 'IX', 'IY')]

# add ICI to variable names
namesICI <- c(names, "ICI")

# add x and y coordinates for block building
dataICI <- data[,c(namesICI, 'IX', 'IY')]

# remove incomplete cases
dataICI <- dataICI[complete.cases(dataICI),]

# get count of data points
nDataIBI <- nrow(dataIBI)
nDataICI <- nrow(dataICI)


### block building for IBI

# copy the data
dataIBI_ <- dataIBI

# generate bins along x and y axes
dataIBI$xBin <- round(dataIBI$IX/density)
dataIBI$yBin <- round(dataIBI$IY/density)

# add a block variable
dataIBI$block <- 0

# add a variable for blocking
counter <- 1

# divide data points into unique blocks
for (x in unique(dataIBI$xBin)) {
  for (y in unique(dataIBI$yBin)) {
    dataIBI$block[dataIBI$xBin == x & dataIBI$yBin == y] <- counter
    counter <- counter + 1    
  }
}

# repeat for overlapping blocks
dataIBI_$xBin <- round((dataIBI$IX+density/2)/density)
dataIBI_$yBin <- round((dataIBI$IY+density/2)/density)
dataIBI_$block <- 0

for (x in unique(dataIBI_$xBin)) {
  for (y in unique(dataIBI_$yBin)) {
    dataIBI_$block[dataIBI_$xBin == x & dataIBI_$yBin == y] <- counter
    counter <- counter + 1    
  }
}

# combine the two block structures
dataIBI <- rbind(dataIBI, dataIBI_)



### block building for ICI

# copy the data
dataICI_ <- dataICI

# generate bins along x and y axes
dataICI$xBin <- round(dataICI$IX/density)
dataICI$yBin <- round(dataICI$IY/density)

# add a block variable
dataICI$block <- 0

# add a variable for blocking
counter <- 1

# divide data points into unique blocks
for (x in unique(dataICI$xBin)) {
  for (y in unique(dataICI$yBin)) {
    dataICI$block[dataICI$xBin == x & dataICI$yBin == y] <- counter
    counter <- counter + 1    
  }
}

# repeat for overlapping blocks
dataICI_$xBin <- round((dataICI_$IX+density/2)/density)
dataICI_$yBin <- round((dataICI_$IY+density/2)/density)
dataICI_$block <- 0

for (x in unique(dataICI_$xBin)) {
  for (y in unique(dataICI_$yBin)) {
    dataICI_$block[dataICI_$xBin == x & dataICI_$yBin == y] <- counter
    counter <- counter + 1    
  }
}

# combine the two block structures
dataICI <- rbind(dataICI, dataICI_)



### final preparations for IBI dataset

# remove unnecessary variables
dataIBI <- dataIBI[,c(namesIBI, 'block')]

# get positions of out of stream, miscellaneous and biotic variables
posOut <- which(colnames(dataIBI) %in% namesOut)
posMisc <- which(colnames(dataIBI) %in% c(namesMisc, "IBI"))
posBio <- which(colnames(dataIBI) %in% "IBI")

# get counterpart pairs of upstream and instream variables (e.g. 'UP' and 'IP')
partners <- matrix(nrow = length(namesIn), ncol = 2)
counter <- 1
for (name in namesIn) {
  partner <- paste0("U", substr(name, start = 2, stop = nchar(name)))
  partners[counter,1] <- which(colnames(dataIBI) %in% partner)
  partners[counter,2] <- which(colnames(dataIBI) %in% name)
  counter <- counter + 1
}

# filter out data points with infinite values after log transformation
dataIBI <- dataIBI[complete.cases(do.call(data.frame,lapply(dataIBI, function(x) replace(x, is.infinite(x),NA)))),]



### final preparations for ICI dataset

# remove unnecessary variables
dataICI <- dataICI[,c(namesICI, 'block')]

# get positions of out of stream, miscellaneous and biotic variables
posOut <- which(colnames(dataICI) %in% namesOut)
posMisc <- which(colnames(dataICI) %in% c(namesMisc, "ICI"))
posBio <- which(colnames(dataICI) %in% "ICI")

# get counterpart pairs of upstream and instream variables
partners <- matrix(nrow = length(namesIn), ncol = 2)
counter <- 1
for (name in namesIn) {
  partner <- paste0("U", substr(name, start = 2, stop = nchar(name)))
  partners[counter,1] <- which(colnames(dataICI) %in% partner)
  partners[counter,2] <- which(colnames(dataICI) %in% name)
  counter <- counter + 1
}

# filter out data points with infinite values after log transformation
dataICI <- dataICI[complete.cases(do.call(data.frame,lapply(dataICI, function(x) replace(x, is.infinite(x),NA)))),]



### modelling

# perform bootstrapping
fishModel <- bootstrap(data = dataIBI, nBoot = nModels, pBoot = nDataICI/nDataIBI, alpha = alpha, test = 'KCIT', filename = 'models/modelsIBI/',
                 partners = partners, posOut = posOut, posMisc = posMisc,
                 doPdsep = TRUE, conservative = TRUE, m.max = 3, pdsep.max = 3, posBio = posBio)

# save the model
filename <- paste0('models/IBI.rdata')
save(fishModel, file = filename)

# perform bootstrapping
invModel <- bootstrap(data = dataICI, nBoot = nModels, pBoot = 1, alpha = alpha, test = 'KCIT', filename = 'models/modelsICI/',
                       partners = partners, posOut = posOut, posMisc = posMisc,
                       doPdsep = TRUE, conservative = TRUE, m.max = 3, pdsep.max = 3, posBio = posBio)

# save the model
filename <- paste0('models/invNormFin.rdata')
save(invModel, file = filename)
