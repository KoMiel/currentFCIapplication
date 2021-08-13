
# this file creates a plot of correlations in relation to distance between points (spatial correlation)

#set working directory
setwd('../data/')

#read data
data <- readRDS('datasetHydrosheds.rds')

#select variables
vars <- c("IX", "IY", "IBI", "ICOD", "ISpeCon", "ICaCO3", "IP", "ITKN", "ITDS", "ITSS", "IPAFInd", "IGraMet", "IDraAre", "IRiffle", "IPool", "IChanne", "ISubstr", "ICover", "IRipari")
data <- data[,vars]

#normalize variables (except X/Y)
for (col in 1:ncol(data)) {
  if(col %in% c(1,2)) {
    next
  }
  data[,col] <- (mean(data[,col]) - data[,col])/(sqrt(var(data[,col])))
}

#generate matrix for data pairs (pairwise location matching)
pairMat <- matrix(ncol = 1+2*length(vars), nrow = nrow(data) * (nrow(data) - 1))

#counter
counter <- 1

#fill matrix with location pairs
for (i in 1:nrow(data)) {
  for (j in 1:nrow(data)) {
    if (i != j) {
      
      # calculate distance
      distance <- sqrt((data[i,1] - data[j,1])^2 + (data[i,2] - data[j,2])^2)
      pairMat[counter, 1] <- distance
      
      #fill in variable values of both locations
      varCounter <- 1
      for (var in vars) {
        pairMat[counter, varCounter*2] <- data[i,var][[1]]
        pairMat[counter, varCounter*2 + 1] <- data[j, var][[1]]
        varCounter <- varCounter + 1
      }
      counter <- counter + 1
    }
  }
}

# round distance for binning
pairMat[,1] <- round(pairMat[,1]/5000)

#matrix for correlations (for each bin)
corMat <- matrix(ncol = length(vars), nrow = length(unique(pairMat[, 1])))

#loop over distance bins
binCounter <- 1
for (bin in unique(pairMat[,1])[order(unique(pairMat[,1]))]) {
  
  #select all pairs of distance bin
  innerMat <- pairMat[pairMat[,1] == bin,]
  
  #calculate correlation for all variables an store in matrix
  varCounter <- 1
  for (var in vars) {
    values1 <- innerMat[, varCounter*2]
    values1 <- values1[!is.na(values1)]
    values2 <- innerMat[, varCounter*2 +1]
    values2 <- values2[!is.na(values2)]
    correlation <- cor(values1, values2)
    corMat[binCounter, varCounter] <- correlation
    varCounter <- varCounter + 1
  }
  binCounter <- binCounter + 1
}

# distances
x <- 5000*(sequence(86)-1)

# calculate mean correlations
means <- rowMeans(corMat[,3:19])

#set working directory
setwd('../plots/')

# plot correlation in relation to distance
pdf('dist_cor_plot.pdf')
plot(x, means, xlab = 'Distance[m]', ylab = 'Correlation', pch = 19)
abline(a = 0, b = 0, lty = 2)
abline()
dev.off()
