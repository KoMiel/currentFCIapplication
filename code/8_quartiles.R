# import packages
library(formattable)
library(htmltools)
library(webshot)
library(dplyr)

# a function which creates an .png file containing a table
export_formattable <- function(f, file, width = "100%", height = NULL, 
                               background = "white", delay = 0.2) {
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}



#set working directory
setwd('../models/')

# import models
load('IBI.rdata')

# variable names
meaningfulNames <- c("IBI", "CaCO3", "Channel", "COD", "Cover", "DrArea", "Gradient", "Lat", "Long", "P", "PAF", "Pool", "Riffle", "Riparian", "SpeCon", "Substrate", "TDS", "N", "TSS", "CaCO3_U", "COD_U", "P_U", "PAF_U", "SpeCon_U", "TDS_U", "N_U", "TSS_U")

# create matrices
tab <- matrix(nrow = length(meaningfulNames), ncol = length(meaningfulNames))
x <- array(rep(NA, length(meaningfulNames)*length(meaningfulNames)*100), dim=c(length(meaningfulNames), length(meaningfulNames), 100))

# load model and copy results
for (num_model in 1:100) {
    filename <- paste0('modelFishNorm/', num_model, '_new.rdata')
    load(filename) 
    pMax <- toSave[5][[1]]@pMax
    pMax[pMax < 0] <- NA
    x[, , num_model] <- pMax
}


### median (second quartile)

# calculate quantiles for all combinations
for (row in 1:length(meaningfulNames)) {
    for (col in 1:length(meaningfulNames)) {
        tab[row, col] <- quantile(x[row, col, ], probs = 0.5, na.rm = TRUE)[[1]]
    }
}

# insert names
names <- colnames(toSave[4][[1]]$C)
colnames(tab) <- names
rownames(tab) <- names

# change names for better readability
tab <- tab[order(rownames(tab)),order(colnames(tab))]
rownames(tab) <- meaningfulNames
colnames(tab) <- meaningfulNames

# NAs on diagonal
for (i in 1:ncol(tab)) {
    tab[i, i] <- NA
}

# NAs for partner variables
tab['CaCO3_U', 'CaCO3'] <- NA
tab['CaCO3', 'CaCO3_U'] <- NA
tab['COD_U', 'COD'] <- NA
tab['COD', 'COD_U'] <- NA
tab['P_U', 'P'] <- NA
tab['P', 'P_U'] <- NA
tab['PAF_U', 'PAF'] <- NA
tab['PAF', 'PAF_U'] <- NA
tab['SpeCon_U', 'SpeCon'] <- NA
tab['SpeCon', 'SpeCon_U'] <- NA
tab['TDS_U', 'TDS'] <- NA
tab['TDS', 'TDS_U'] <- NA
tab['N_U', 'N'] <- NA
tab['N', 'N_U'] <- NA
tab['TSS_U', 'TSS'] <- NA
tab['TSS', 'TSS_U'] <- NA

# matrix to data frame
df <- as.data.frame(tab)

# NAs for upstream section
df[20:length(names), 20:length(names)] <- NA

# set names for printing of table
df$names <- rownames(df)
rownames(df) <- c()
df <-df[,c(ncol(df),c(1:ncol(df) - 1))]
colnames(df)[1] = " "

# rounding
df <- df %>% 
 mutate_if(is.numeric, round, digits = 2)

# formatting
df_median <- df
df_median <- format(df_median, digits=2, nsmall=2)

# call function
ft <- formattable(df, 
            align = "c",
            list(` ` = formatter("span",style = ~ style(display = "block",
                "border-radius" = "2px",
                "padding" = "5px",
                "font.weight" = "bold",  
                "text-align" = "left")),
            area(col = 2:28) ~ color_tile("transparent", "lightblue"),
            digits = 2) 

            )
            
# export
export_formattable(ft, file = "../plots/fish_p-median.png", width = "2000px")

# other code blocks similar

### first quartile

for (row in 1:length(meaningfulNames)) {
    for (col in 1:length(meaningfulNames)) {
        tab[row, col] <- quantile(x[row, col, ], probs = 0.25, na.rm = TRUE)[[1]]
    }
}

names <- colnames(toSave[4][[1]]$C)
colnames(tab) <- names
rownames(tab) <- names

tab <- tab[order(rownames(tab)),order(colnames(tab))]
rownames(tab) <- meaningfulNames
colnames(tab) <- meaningfulNames

for (i in 1:ncol(tab)) {
    tab[i, i] <- NA
}

tab['CaCO3_U', 'CaCO3'] <- NA
tab['CaCO3', 'CaCO3_U'] <- NA
tab['COD_U', 'COD'] <- NA
tab['COD', 'COD_U'] <- NA
tab['P_U', 'P'] <- NA
tab['P', 'P_U'] <- NA
tab['PAF_U', 'PAF'] <- NA
tab['PAF', 'PAF_U'] <- NA
tab['SpeCon_U', 'SpeCon'] <- NA
tab['SpeCon', 'SpeCon_U'] <- NA
tab['TDS_U', 'TDS'] <- NA
tab['TDS', 'TDS_U'] <- NA
tab['N_U', 'N'] <- NA
tab['N', 'N_U'] <- NA
tab['TSS_U', 'TSS'] <- NA
tab['TSS', 'TSS_U'] <- NA

df <- as.data.frame(tab)

df[20:length(names), 20:length(names)] <- NA

# set names
df$names <- rownames(df)
rownames(df) <- c()
df <-df[,c(ncol(df),c(1:ncol(df) - 1))]
colnames(df)[1] = " "

df <- df %>% 
 mutate_if(is.numeric, round, digits = 2)

df_first <- df
df_first <- format(df_first, digits=2, nsmall=2)

ft <- formattable(df, 
            align = "c",
            list(` ` = formatter("span",style = ~ style(display = "block",
                "border-radius" = "2px",
                "padding" = "5px",
                "font.weight" = "bold",  
                "text-align" = "left")),
            area(col = 2:28) ~ color_tile("transparent", "lightblue"),
            digits = 2) 

            )
            
export_formattable(ft, file = "../plots/fish_p-025.png", width = "2000px")

### third quartile

for (row in 1:length(meaningfulNames)) {
    for (col in 1:length(meaningfulNames)) {
        tab[row, col] <- quantile(x[row, col, ], probs = 0.75, na.rm = TRUE)[[1]]
    }
}

names <- colnames(toSave[4][[1]]$C)
colnames(tab) <- names
rownames(tab) <- names

tab <- tab[order(rownames(tab)),order(colnames(tab))]
rownames(tab) <- meaningfulNames
colnames(tab) <- meaningfulNames

for (i in 1:ncol(tab)) {
    tab[i, i] <- NA
}

tab['CaCO3_U', 'CaCO3'] <- NA
tab['CaCO3', 'CaCO3_U'] <- NA
tab['COD_U', 'COD'] <- NA
tab['COD', 'COD_U'] <- NA
tab['P_U', 'P'] <- NA
tab['P', 'P_U'] <- NA
tab['PAF_U', 'PAF'] <- NA
tab['PAF', 'PAF_U'] <- NA
tab['SpeCon_U', 'SpeCon'] <- NA
tab['SpeCon', 'SpeCon_U'] <- NA
tab['TDS_U', 'TDS'] <- NA
tab['TDS', 'TDS_U'] <- NA
tab['N_U', 'N'] <- NA
tab['N', 'N_U'] <- NA
tab['TSS_U', 'TSS'] <- NA
tab['TSS', 'TSS_U'] <- NA

df <- as.data.frame(tab)

df[20:length(names), 20:length(names)] <- NA

df$names <- rownames(df)
rownames(df) <- c()
df <-df[,c(ncol(df),c(1:ncol(df) - 1))]
colnames(df)[1] = " "

df <- df %>% 
 mutate_if(is.numeric, round, digits = 2)

df_third <- df
df_third <- format(df_third, digits=2, nsmall=2)

df_main <- data.frame(matrix(nrow = 28, ncol = 28))
for (i in 1:28) {
    for (j in 1:28) {
        if (is.na(df_first[i, j])) {
            df_main[i, j] <- '-'
        } else {
            df_main[i, j] <- paste0('[', df_first[i, j], ', ', df_median[i, j], ', ', df_third[i, j], ']')
        }
    }
}

write.csv2(df_main, "fish_pvalues.csv")

ft <- formattable(df, 
            align = "c",
            list(` ` = formatter("span",style = ~ style(display = "block",
                "border-radius" = "2px",
                "padding" = "5px",
                "font.weight" = "bold",  
                "text-align" = "left")),
            area(col = 2:28) ~ color_tile("transparent", "lightblue"),
            digits = 2) 

            )


export_formattable(ft, file = "../plots/fish_p-075.png", width = "2000px")

####### invertebrates

load('ICI.rdata')

meaningfulNames <- c("CaCO3", "Channel", "ICI", "COD", "Cover", "DrArea", "Gradient", "Lat", "Long", "P", "PAF", "Pool", "Riffle", "Riparian", "SpeCon", "Substrate", "TDS", "N", "TSS", "CaCO3_U", "COD_U", "P_U", "PAF_U", "SpeCon_U", "TDS_U", "N_U", "TSS_U")

tab <- matrix(nrow = length(meaningfulNames), ncol = length(meaningfulNames))
x <- array(rep(NA, length(meaningfulNames)*length(meaningfulNames)*100), dim=c(length(meaningfulNames), length(meaningfulNames), 100))

for (num_model in 1:100) {
    filename <- paste0('modelInvNorm/', num_model, '_new.rdata')
    load(filename) 
    pMax <- toSave[5][[1]]@pMax
    pMax[pMax < 0] <- NA
    x[, , num_model] <- pMax
}

### median (second quartile)

for (row in 1:length(meaningfulNames)) {
    for (col in 1:length(meaningfulNames)) {
        tab[row, col] <- quantile(x[row, col, ], probs = 0.5, na.rm = TRUE)[[1]]
    }
}

names <- colnames(toSave[4][[1]]$C)
colnames(tab) <- names
rownames(tab) <- names

tab <- tab[order(rownames(tab)),order(colnames(tab))]
rownames(tab) <- meaningfulNames
colnames(tab) <- meaningfulNames

for (i in 1:ncol(tab)) {
    tab[i, i] <- NA
}

tab['CaCO3_U', 'CaCO3'] <- NA
tab['CaCO3', 'CaCO3_U'] <- NA
tab['COD_U', 'COD'] <- NA
tab['COD', 'COD_U'] <- NA
tab['P_U', 'P'] <- NA
tab['P', 'P_U'] <- NA
tab['PAF_U', 'PAF'] <- NA
tab['PAF', 'PAF_U'] <- NA
tab['SpeCon_U', 'SpeCon'] <- NA
tab['SpeCon', 'SpeCon_U'] <- NA
tab['TDS_U', 'TDS'] <- NA
tab['TDS', 'TDS_U'] <- NA
tab['N_U', 'N'] <- NA
tab['N', 'N_U'] <- NA
tab['TSS_U', 'TSS'] <- NA
tab['TSS', 'TSS_U'] <- NA

df <- as.data.frame(tab)

df[20:length(names), 20:length(names)] <- NA

# set names
df$names <- rownames(df)
rownames(df) <- c()
df <-df[,c(ncol(df),c(1:ncol(df) - 1))]
colnames(df)[1] = " "

df <- df %>% 
 mutate_if(is.numeric, round, digits = 2)
 
df_median <- df
df_median <- format(df_median, digits=2, nsmall=2)

ft <- formattable(df, 
            align = "c",
            list(` ` = formatter("span",style = ~ style(display = "block",
                "border-radius" = "2px",
                "padding" = "5px",
                "font.weight" = "bold",  
                "text-align" = "left")),
            area(col = 2:28) ~ color_tile("transparent", "lightblue"),
            digits = 2) 

            )

export_formattable(ft, file = "../plots/inv_p-median.png", width = "2000px")

### first quartile

for (row in 1:length(meaningfulNames)) {
    for (col in 1:length(meaningfulNames)) {
        tab[row, col] <- quantile(x[row, col, ], probs = 0.25, na.rm = TRUE)[[1]]
    }
}

names <- colnames(toSave[4][[1]]$C)
colnames(tab) <- names
rownames(tab) <- names

tab <- tab[order(rownames(tab)),order(colnames(tab))]
rownames(tab) <- meaningfulNames
colnames(tab) <- meaningfulNames

for (i in 1:ncol(tab)) {
    tab[i, i] <- NA
}

tab['CaCO3_U', 'CaCO3'] <- NA
tab['CaCO3', 'CaCO3_U'] <- NA
tab['COD_U', 'COD'] <- NA
tab['COD', 'COD_U'] <- NA
tab['P_U', 'P'] <- NA
tab['P', 'P_U'] <- NA
tab['PAF_U', 'PAF'] <- NA
tab['PAF', 'PAF_U'] <- NA
tab['SpeCon_U', 'SpeCon'] <- NA
tab['SpeCon', 'SpeCon_U'] <- NA
tab['TDS_U', 'TDS'] <- NA
tab['TDS', 'TDS_U'] <- NA
tab['N_U', 'N'] <- NA
tab['N', 'N_U'] <- NA
tab['TSS_U', 'TSS'] <- NA
tab['TSS', 'TSS_U'] <- NA

df <- as.data.frame(tab)

df[20:length(names), 20:length(names)] <- NA

df$names <- rownames(df)
rownames(df) <- c()
df <-df[,c(ncol(df),c(1:ncol(df) - 1))]
colnames(df)[1] = " "

df <- df %>% 
 mutate_if(is.numeric, round, digits = 2)

df_first <- df
df_first <- format(df_first, digits=2, nsmall=2)

ft <- formattable(df, 
            align = "c",
            list(` ` = formatter("span",style = ~ style(display = "block",
                "border-radius" = "2px",
                "padding" = "5px",
                "font.weight" = "bold",  
                "text-align" = "left")),
            area(col = 2:28) ~ color_tile("transparent", "lightblue"),
            digits = 2) 

            )

export_formattable(ft, file = "../plots/inv_p-025.png", width = "2000px")

### third quartile

for (row in 1:length(meaningfulNames)) {
    for (col in 1:length(meaningfulNames)) {
        tab[row, col] <- quantile(x[row, col, ], probs = 0.75, na.rm = TRUE)[[1]]
    }
}

names <- colnames(toSave[4][[1]]$C)
colnames(tab) <- names
rownames(tab) <- names

tab <- tab[order(rownames(tab)),order(colnames(tab))]
rownames(tab) <- meaningfulNames
colnames(tab) <- meaningfulNames

for (i in 1:ncol(tab)) {
    tab[i, i] <- NA
}

tab['CaCO3_U', 'CaCO3'] <- NA
tab['CaCO3', 'CaCO3_U'] <- NA
tab['COD_U', 'COD'] <- NA
tab['COD', 'COD_U'] <- NA
tab['P_U', 'P'] <- NA
tab['P', 'P_U'] <- NA
tab['PAF_U', 'PAF'] <- NA
tab['PAF', 'PAF_U'] <- NA
tab['SpeCon_U', 'SpeCon'] <- NA
tab['SpeCon', 'SpeCon_U'] <- NA
tab['TDS_U', 'TDS'] <- NA
tab['TDS', 'TDS_U'] <- NA
tab['N_U', 'N'] <- NA
tab['N', 'N_U'] <- NA
tab['TSS_U', 'TSS'] <- NA
tab['TSS', 'TSS_U'] <- NA

df <- as.data.frame(tab)

df[20:length(names), 20:length(names)] <- NA

df$names <- rownames(df)
rownames(df) <- c()
df <-df[,c(ncol(df),c(1:ncol(df) - 1))]
colnames(df)[1] = " "

df <- df %>% 
 mutate_if(is.numeric, round, digits = 2)

df_third <- df
df_third <- format(df_third, digits=2, nsmall=2)

df_main <- data.frame(matrix(nrow = 28, ncol = 28))
for (i in 1:28) {
    for (j in 1:28) {
        if (is.na(df_first[i, j])) {
            df_main[i, j] <- '-'
        } else {
            df_main[i, j] <- paste0('[', df_first[i, j], ', ', df_median[i, j], ', ', df_third[i, j], ']')
        }
    }
}

write.csv2(df_main, "inv_pvalues.csv")

ft <- formattable(df, 
            align = "c",
            list(` ` = formatter("span",style = ~ style(display = "block",
                "border-radius" = "2px",
                "padding" = "5px",
                "font.weight" = "bold",  
                "text-align" = "left")),
            area(col = 2:28) ~ color_tile("transparent", "lightblue"),
            digits = 2) 

            )

export_formattable(ft, file = "../plots/inv_p-075.png", width = "2000px")
