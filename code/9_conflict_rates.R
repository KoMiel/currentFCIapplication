
#set working directory
setwd('../models/')

nModels <- 100
nVariables <- 27

##### invertebrates #####

# set up variables to count conflicts
edge_types <- matrix(0, nrow = nVariables, ncol = nVariables)
conflicts <- matrix(0, nrow = nVariables, ncol = nVariables)

# loop over all models
for (run in 1:nModels){

    # load model
    load(file = paste0('ICI/', run, '.rdata'))

    # get arrowheads and tails
    arrowheads <- toSave[[2]]
    tails <- toSave[[3]]

    # create matrices
    arrowheads <- matrix(arrowheads, nrow = nVariables, ncol = nVariables)
    tails <- matrix(tails, nrow = nVariables, ncol = nVariables)

    # search for causal connections and count them
    for (i in 1:nVariables){
        for (j in 1:nVariables){
            if (tails[i, j] == 1){
                if (arrowheads[j, i] == 1){
                    edge_types[i, j] <- edge_types[i, j] + 1
                }
            }
        }    
    }
}


# loop over all variables
for (i in 1:nVariables){
    for (j in 1:nVariables){
        
        # search for conflicts in causal direction
        if (edge_types[i, j] < edge_types[j,i]){
            conflicts[i, j] <- edge_types[i, j]
        } else if (edge_types[i, j] == edge_types[j ,i]){
            conflicts[i, j] <- edge_types[i, j]
            conflicts[j, i] <- 0
        }
    }
}

# calculate conflict rate
conflict_rate <- sum(conflicts)/sum(edge_types)

# print conflict rate
print(conflict_rate)

# second block similar

##### fish #####

edge_types <- matrix(0, nrow = nVariables, ncol = nVariables)
conflicts <- matrix(0, nrow = nVariables, ncol = nVariables)

for (run in 1:num_boot){

    load(file = paste0('IBI/', run, '.rdata'))

    arrowheads <- toSave[[2]]
    tails <- toSave[[3]]

    arrowheads <- matrix(arrowheads, nrow = nVariables, ncol = nVariables)
    tails <- matrix(tails, nrow = nVariables, ncol = nVariables)

    for (i in 1:nVariables){
        for (j in 1:nVariables){
            if (tails[i, j] == 1){
                if (arrowheads[j, i] == 1){
                    edge_types[i, j] <- edge_types[i, j] + 1
                }
            }
        }    
    }
}


for (i in 1:nVariables){
    for (j in 1:nVariables){
    
        if (edge_types[i, j] < edge_types[j,i]){
            conflicts[i, j] <- edge_types[i, j]
        } else if (edge_types[i, j] == edge_types[j ,i]){
            conflicts[i, j] <- edge_types[i, j]
            conflicts[j, i] <- 0
        }
    }
}

conflict_rate <- sum(conflicts)/sum(edge_types)

print(conflict_rate)
