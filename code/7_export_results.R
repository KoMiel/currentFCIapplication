# import packages
library(formattable)
library(htmltools)
library(webshot)


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
load('ICI.rdata')



# each block creates one of the following tables:
# invertebrates: Edges, arrowheads, tails
# fish: Edges, arrowheads, tails

##### invertebrates edges #####

# generate a matrix
names <- unique(invModel$Node1)
tab <- matrix(nrow = length(names), ncol = length(names))
colnames(tab) <- names
rownames(tab) <- names

# aggregate all edge marks
aggI <- aggregate(invModel$Count, by = list(invModel$Node1, invModel$Node2), FUN = sum)

# fill matrix
for (i in 1:nrow(aggI)) {
    row <- aggI[i,]
    tab[row[[1]], row[[2]]] <- row[[3]]
}

# more meaningful column and row names
tab <- tab[order(rownames(tab)),order(colnames(tab))]
rownames(tab) <- c("CaCO3", "Channel", "ICI", "COD", "Cover", "DrArea", "Gradient", "Lat", "Long", "P", "PAF", "Pool", "Riffle", "Riparian", "SpeCon", "Substrate", "TDS", "N", "TSS", "CaCO3_U", "COD_U", "P_U", "PAF_U", "SpeCon_U", "TDS_U", "N_U", "TSS_U")
colnames(tab) <- c("CaCO3", "Channel", "ICI", "COD", "Cover", "DrArea", "Gradient", "Lat", "Long", "P", "PAF", "Pool", "Riffle", "Riparian", "SpeCon", "Substrate", "TDS", "N", "TSS", "CaCO3_U", "COD_U", "P_U", "PAF_U", "SpeCon_U", "TDS_U", "N_U", "TSS_U")

# replace NA with 0
tab[is.na(tab)] <- 0
tab[order(rownames(tab)),order(colnames(tab))]

# matrix to data frame
df <- as.data.frame(tab)

# set names
df$names <- rownames(df)
rownames(df) <- c()
df <-df[,c(ncol(df),c(1:ncol(df) - 1))]
colnames(df)[1] = " "

# call function
ft <- formattable(df, 
            align = "c",
            list(` ` = formatter("span",style = ~ style(display = "block",
                "border-radius" = "2px",
                "padding" = "5px",
                "font.weight" = "bold",  
                "text-align" = "left")),
            area(col = 2:28) ~ color_tile("transparent", "lightblue")) 

            )

export_formattable(ft, file = "../plots/invertebrates_edges.png", width = "2000px")



##### invertebrates arrowheads #####

tab <- matrix(nrow = length(names), ncol = length(names))
colnames(tab) <- names
rownames(tab) <- names

invModel_head <- invModel[invModel$Edgemark == 2,]

for (i in 1:nrow(invModel_head)) {
    row <- invModel_head[i,]
    tab[row[[1]], row[[2]]] <- row[[4]]
}

tab <- tab[order(rownames(tab)),order(colnames(tab))]
rownames(tab) <- c("CaCO3", "Channel", "ICI", "COD", "Cover", "DrArea", "Gradient", "Lat", "Long", "P", "PAF", "Pool", "Riffle", "Riparian", "SpeCon", "Substrate", "TDS", "N", "TSS", "CaCO3_U", "COD_U", "P_U", "PAF_U", "SpeCon_U", "TDS_U", "N_U", "TSS_U")
colnames(tab) <- c("CaCO3", "Channel", "ICI", "COD", "Cover", "DrArea", "Gradient", "Lat", "Long", "P", "PAF", "Pool", "Riffle", "Riparian", "SpeCon", "Substrate", "TDS", "N", "TSS", "CaCO3_U", "COD_U", "P_U", "PAF_U", "SpeCon_U", "TDS_U", "N_U", "TSS_U")

tab[is.na(tab)] <- 0
tab[order(rownames(tab)),order(colnames(tab))]
df <- as.data.frame(tab)

df$names <- rownames(df)
rownames(df) <- c()
df <-df[,c(ncol(df),c(1:ncol(df) - 1))]
colnames(df)[1] = " "

ft <- formattable(df, 
            align = "c",
            list(` ` = formatter("span",style = ~ style(display = "block",
                "border-radius" = "2px",
                "padding" = "5px",
                "font.weight" = "bold",  
                "text-align" = "left")),
            area(col = 2:28) ~ color_tile("transparent", "lightblue"))
            )

export_formattable(ft, file = "../plots/invertebrates_arrowheads.png", width = "2000px")



##### invertebrates tail #####

tab <- matrix(nrow = length(names), ncol = length(names))
colnames(tab) <- names
rownames(tab) <- names

invModel_tail <- invModel[invModel$Edgemark == 1,]

for (i in 1:nrow(invModel_tail)) {
    row <- invModel_tail[i,]
    tab[row[[1]], row[[2]]] <- row[[4]]
}

tab <- tab[order(rownames(tab)),order(colnames(tab))]
rownames(tab) <- c("CaCO3", "Channel", "ICI", "COD", "Cover", "DrArea", "Gradient", "Lat", "Long", "P", "PAF", "Pool", "Riffle", "Riparian", "SpeCon", "Substrate", "TDS", "N", "TSS", "CaCO3_U", "COD_U", "P_U", "PAF_U", "SpeCon_U", "TDS_U", "N_U", "TSS_U")
colnames(tab) <- c("CaCO3", "Channel", "ICI", "COD", "Cover", "DrArea", "Gradient", "Lat", "Long", "P", "PAF", "Pool", "Riffle", "Riparian", "SpeCon", "Substrate", "TDS", "N", "TSS", "CaCO3_U", "COD_U", "P_U", "PAF_U", "SpeCon_U", "TDS_U", "N_U", "TSS_U")

tab[is.na(tab)] <- 0
tab[order(rownames(tab)),order(colnames(tab))]
df <- as.data.frame(tab)

df$names <- rownames(df)
rownames(df) <- c()
df <-df[,c(ncol(df),c(1:ncol(df) - 1))]
colnames(df)[1] = " "

ft <- formattable(df, 
            align = "c",
            list(` ` = formatter("span",style = ~ style(display = "block",
                "border-radius" = "2px",
                "padding" = "5px",
                "font.weight" = "bold",  
                "text-align" = "left")),
            area(col = 2:28) ~ color_tile("transparent", "lightblue"))
            )

export_formattable(ft, file = "../plots/invertebrates_tails.png", width = "2000px")



##### fish edges #####


names <- unique(fishModel$Node1)
tab <- matrix(nrow = length(names), ncol = length(names))
colnames(tab) <- names
rownames(tab) <- names

aggI <- aggregate(fishModel$Count, by = list(fishModel$Node1, fishModel$Node2), FUN = sum)

for (i in 1:nrow(aggI)) {
    row <- aggI[i,]
    tab[row[[1]], row[[2]]] <- row[[3]]
}

tab <- tab[order(rownames(tab)),order(colnames(tab))]
rownames(tab) <- c("IBI", "CaCO3", "Channel", "COD", "Cover", "DrArea", "Gradient", "Lat", "Long", "P", "PAF", "Pool", "Riffle", "Riparian", "SpeCon", "Substrate", "TDS", "N", "TSS", "CaCO3_U", "COD_U", "P_U", "PAF_U", "SpeCon_U", "TDS_U", "N_U", "TSS_U")
colnames(tab) <- c("IBI", "CaCO3", "Channel", "COD", "Cover", "DrArea", "Gradient", "Lat", "Long", "P", "PAF", "Pool", "Riffle", "Riparian", "SpeCon", "Substrate", "TDS", "N", "TSS", "CaCO3_U", "COD_U", "P_U", "PAF_U", "SpeCon_U", "TDS_U", "N_U", "TSS_U")

tab[is.na(tab)] <- 0
tab[order(rownames(tab)),order(colnames(tab))]
df <- as.data.frame(tab)

df$names <- rownames(df)
rownames(df) <- c()
df <-df[,c(ncol(df),c(1:ncol(df) - 1))]
colnames(df)[1] = " "

ft <- formattable(df, 
            align = "c",
            list(` ` = formatter("span",style = ~ style(display = "block",
                "border-radius" = "2px",
                "padding" = "5px",
                "font.weight" = "bold",  
                "text-align" = "left")),
            area(col = 2:28) ~ color_tile("transparent", "lightblue"))
            )

export_formattable(ft, file = "../plots/fish_edges.png", width = "2000px")



##### fish arrowheads #####

tab <- matrix(nrow = length(names), ncol = length(names))
colnames(tab) <- names
rownames(tab) <- names

fishModel_head <- fishModel[fishModel$Edgemark == 2,]

for (i in 1:nrow(fishModel_head)) {
    row <- fishModel_head[i,]
    tab[row[[1]], row[[2]]] <- row[[4]]
}

tab <- tab[order(rownames(tab)),order(colnames(tab))]
rownames(tab) <- c("IBI", "CaCO3", "Channel", "COD", "Cover", "DrArea", "Gradient", "Lat", "Long", "P", "PAF", "Pool", "Riffle", "Riparian", "SpeCon", "Substrate", "TDS", "N", "TSS", "CaCO3_U", "COD_U", "P_U", "PAF_U", "SpeCon_U", "TDS_U", "N_U", "TSS_U")
colnames(tab) <- c("IBI", "CaCO3", "Channel", "COD", "Cover", "DrArea", "Gradient", "Lat", "Long", "P", "PAF", "Pool", "Riffle", "Riparian", "SpeCon", "Substrate", "TDS", "N", "TSS", "CaCO3_U", "COD_U", "P_U", "PAF_U", "SpeCon_U", "TDS_U", "N_U", "TSS_U")

tab[is.na(tab)] <- 0
tab[order(rownames(tab)),order(colnames(tab))]
df <- as.data.frame(tab)

df$names <- rownames(df)
rownames(df) <- c()
df <-df[,c(ncol(df),c(1:ncol(df) - 1))]
colnames(df)[1] = " "

ft <- formattable(df, 
            align = "c",
            list(` ` = formatter("span",style = ~ style(display = "block",
                "border-radius" = "2px",
                "padding" = "5px",
                "font.weight" = "bold",  
                "text-align" = "left")),
            area(col = 2:28) ~ color_tile("transparent", "lightblue"))
            )

export_formattable(ft, file = "../plots/fish_arrowheads.png", width = "2000px")



##### fish tails #####

tab <- matrix(nrow = length(names), ncol = length(names))
colnames(tab) <- names
rownames(tab) <- names

fishModel_tails <- fishModel[fishModel$Edgemark == 1,]

for (i in 1:nrow(fishModel_tails)) {
    row <- fishModel_tails[i,]
    tab[row[[1]], row[[2]]] <- row[[4]]
}

tab <- tab[order(rownames(tab)),order(colnames(tab))]
rownames(tab) <- c("IBI", "CaCO3", "Channel", "COD", "Cover", "DrArea", "Gradient", "Lat", "Long", "P", "PAF", "Pool", "Riffle", "Riparian", "SpeCon", "Substrate", "TDS", "N", "TSS", "CaCO3_U", "COD_U", "P_U", "PAF_U", "SpeCon_U", "TDS_U", "N_U", "TSS_U")
colnames(tab) <- c("IBI", "CaCO3", "Channel", "COD", "Cover", "DrArea", "Gradient", "Lat", "Long", "P", "PAF", "Pool", "Riffle", "Riparian", "SpeCon", "Substrate", "TDS", "N", "TSS", "CaCO3_U", "COD_U", "P_U", "PAF_U", "SpeCon_U", "TDS_U", "N_U", "TSS_U")

tab[is.na(tab)] <- 0
tab[order(rownames(tab)),order(colnames(tab))]
df <- as.data.frame(tab)

df$names <- rownames(df)
rownames(df) <- c()
df <-df[,c(ncol(df),c(1:ncol(df) - 1))]
colnames(df)[1] = " "

ft <- formattable(df, 
            align = "c",
            list(` ` = formatter("span",style = ~ style(display = "block",
                "border-radius" = "2px",
                "padding" = "5px",
                "font.weight" = "bold",  
                "text-align" = "left")),
            area(col = 2:28) ~ color_tile("transparent", "lightblue"))
            )

export_formattable(ft, file = "../plots/fish_tails.png", width = "2000px")
