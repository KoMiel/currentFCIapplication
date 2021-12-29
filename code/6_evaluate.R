
# import libraries
library(pcalg)

# lower threshold for edges
lim <- 65

# number of models
nModels <- 100



### edges

# load IBI models
load('../models/IBI.rdata')

# aggregate all edge marks
edges <- aggregate(fishModel$Count, by = list(fishModel$Node1, fishModel$Node2), FUN = sum)

# remove upstream variables
edges <- edges[edges[,"Group.1"] %in% c("ILatitu", "ILongit", "IGraMet", "IDraAre", "ICOD", "ISpeCon", "ICaCO3", "IP", "ITKN", "ITDS", "ITSS", "IPAFInd", "IpH", "IBOD5", "IVol","IRiffle", "IPool", "IChanne", "ISubstr", "ICover", "IRipari", "IBI") & edges[,"Group.2"] %in% c("ILatitu", "ILongit", "IGraMet", "IDraAre", "ICOD", "ISpeCon", "ICaCO3", "IP", "ITKN", "ITDS", "ITSS", "IPAFInd", "IpH", "IBOD5", "IVol","IRiffle", "IPool", "IChanne", "ISubstr", "ICover", "IRipari", "IBI"),]

# filter at lower threshold
edges <- edges[edges$x >= lim,]

# calculate fractions
edges$x <- edges$x/nModels

# print statements
print('Edges in IBI models:')
print(edges)


# load ICI models
load('../models/ICI.rdata')

# aggregate all edge marks
edges <- aggregate(invModel$Count, by = list(invModel$Node1, invModel$Node2), FUN = sum)

# remove upstream variables
edges <- edges[edges[,"Group.1"] %in% c("ILatitu", "ILongit", "IGraMet", "IDraAre", "ICOD", "ISpeCon", "ICaCO3", "IP", "ITKN", "ITDS", "ITSS", "IPAFInd", "IpH", "IBOD5", "IVol","IRiffle", "IPool", "IChanne", "ISubstr", "ICover", "IRipari", "ICI") & edges[,"Group.2"] %in% c("ILatitu", "ILongit", "IGraMet", "IDraAre", "ICOD", "ISpeCon", "ICaCO3", "IP", "ITKN", "ITDS", "ITSS", "IPAFInd", "IpH", "IBOD5", "IVol","IRiffle", "IPool", "IChanne", "ISubstr", "ICover", "IRipari", "ICI"),]

# filter at lower threshold
edges <- edges[edges$x >= lim,]

# calculate fractions
edges$x <- edges$x/nModels

# print statements
print('Edges in ICI models:')
print(edges)



### edge marks

# load IBI models
load('../models/IBI.rdata')

# remove upstream variables
edgeMarks <- fishModel[fishModel[,"Node1"] %in% c("ILatitu", "ILongit", "IGraMet", "IDraAre", "ICOD", "ISpeCon", "ICaCO3", "IP", "ITKN", "ITDS", "ITSS", "IPAFInd", "IpH", "IBOD5", "IVol","IRiffle", "IPool", "IChanne", "ISubstr", "ICover", "IRipari", "IBI") & fishModel[,"Node2"] %in% c("ILatitu", "ILongit", "IGraMet", "IDraAre", "ICOD", "ISpeCon", "ICaCO3", "IP", "ITKN", "ITDS", "ITSS", "IPAFInd", "IpH", "IBOD5", "IVol","IRiffle", "IPool", "IChanne", "ISubstr", "ICover", "IRipari", "IBI"),]

# filter at lower threshold
edgeMarks <- edgeMarks[edgeMarks$Count >= lim,]

# calculate fractions
edgeMarks$Count <- edgeMarks$Count/nModels

# select arrowheads
arrowheads <- edgeMarks[edgeMarks$Edgemark == 2,c(1,2,4)]

# select tails
tails <- edgeMarks[edgeMarks$Edgemark == 3,c(1,2,4)]

# print statements
print('Arrowheads in IBI models:')
print(arrowheads)
print('Tails in IBI models:')
print(tails)


# load ICI models
load('../models/ICI.rdata')

# remove upstream variables
edgeMarks <- invModel[invModel[,"Node1"] %in% c("ILatitu", "ILongit", "IGraMet", "IDraAre", "ICOD", "ISpeCon", "ICaCO3", "IP", "ITKN", "ITDS", "ITSS", "IPAFInd", "IpH", "IBOD5", "IVol","IRiffle", "IPool", "IChanne", "ISubstr", "ICover", "IRipari", "ICI") & invModel[,"Node2"] %in% c("ILatitu", "ILongit", "IGraMet", "IDraAre", "ICOD", "ISpeCon", "ICaCO3", "IP", "ITKN", "ITDS", "ITSS", "IPAFInd", "IpH", "IBOD5", "IVol","IRiffle", "IPool", "IChanne", "ISubstr", "ICover", "IRipari", "ICI"),]

# filter at lower threshold
edgeMarks <- edgeMarks[edgeMarks$Count >= lim,]

# calculate fractions
edgeMarks$Count <- edgeMarks$Count/nModels

# select arrowheads
arrowheads <- edgeMarks[edgeMarks$Edgemark == 2,c(1,2,4)]

# select tails
tails <- edgeMarks[edgeMarks$Edgemark == 3,c(1,2,4)]

# print statements
print('Arrowheads in ICI models:')
print(arrowheads)
print('Tails in ICI models:')
print(tails)



### partial correlations

# read data
dataset <- readRDS('../data/datasetHydrosheds.rds')
dataset <- dataset[,2:ncol(dataset)]

# entire dataset for IBI calculations
datasetIBI <- dataset

# correlation matrix
corIBI <- cor(datasetIBI)

# load IBI models
load('../models/IBI.rdata')

# aggregate all edge marks
models <- aggregate(fishModel$Count, by = list(fishModel$Node1, fishModel$Node2), FUN = sum)

# filter at lower threshold
models <- models[models$x > lim,]

# find neighbors of ICI
neighbors <- models[models$Group.1 == 'IBI', 2]

# loop over neighbors
for (var in neighbors) {
    # find all neighbors 
    otherNeighbors <- neighbors <- models[models$Group.1 == var, 2]
    
    # combine the two sets of neighbors
    condSet <- c(neighbors, otherNeighbors)
    
    # keep unique elements
    condSet <- unique(condSet)
    
    # eliminate the two variables that are to be tested
    condSet[condSet != 'IBI' & condSet != var]
    
    # compute partial correlation
    pCor <- pcorOrder(match('IBI', colnames(corIBI)), match(var, colnames(corIBI)), match(condSet, colnames(corIBI)), corIBI)

    # print results
    print(paste0('Partial correlation between IBI and ', var, ' : ', pCor))
}


# get part of dataset for which ICI is measured
datasetICI <- dataset[!is.na(dataset$ICI), ]

# correlation matrix
corICI <- cor(datasetICI)

# load ICI models
load('../models/ICI.rdata')

# aggregate all edge marks
models <- aggregate(invModel$Count, by = list(invModel$Node1, invModel$Node2), FUN = sum)

# filter at lower threshold
models <- models[models$x > lim,]

# find neighbors of ICI
neighbors <- models[models$Group.1 == 'ICI', 2]

# loop over neighbors
for (var in neighbors) {
    # find all neighbors 
    otherNeighbors <- neighbors <- models[models$Group.1 == var, 2]
    
    # combine the two sets of neighbors
    condSet <- c(neighbors, otherNeighbors)
    
    # keep unique elements
    condSet <- unique(condSet)
    
    # eliminate the two variables that are to be tested
    condSet[condSet != 'ICI' & condSet != var]
    
    # compute partial correlation
    pCor <- pcorOrder(match('ICI', colnames(corICI)), match(var, colnames(corICI)), match(condSet, colnames(corICI)), corICI)

    # print results
    print(paste0('Partial correlation between ICI and ', var, ' : ', pCor))
}
