# Jaccar Index
library(dplyr)
# Your dataset
df <- data.frame(t(data.frame(c1=rnorm(100),
                              c2=rnorm(100),
                              c3=rnorm(100),
                              c4=rnorm(100),
                              c5=rnorm(100),
                              c6=rnorm(100))))

df[df > 0] <- 1
df[df <= 0] <- 0
df

#
# Function Name: jaccard
# Return: Jaccard distance between two vectors
# Parameters:
# 1. df, dataframe of interest
# 2. margin, axis in which the apply function is meant to move along
#
jaccard <- function(df, margin=1) {
  if (margin == 1 | margin == 2) {
    M_00 <- apply(df, margin, sum) == 0
    M_11 <- apply(df, margin, sum) == 2
    if (margin == 1) {
      df <- df[!M_00, ]
      JSim <- sum(M_11) / nrow(df)
    } else {
      df <- df[, !M_00]
      JSim <- sum(M_11) / length(df)
    }
    JDist <- 1 - JSim
    return(c(JSim = JSim, JDist = JDist))
  } else break
}

#jaccard(df[1:2,], margin=2)

#
# Function Name: jaccard_per_row
# Return: list containing Jaccard distance matrix and Jaccard similarity matrix between each observation or column.
# Parameters:
# 1. df, dataframe of interest; 
# 2. margin, axis in which the apply function is meant to move along
#
apply_jaccard <- function(df, margin=1){
   key_pairs <- expand.grid(row.names(df), row.names(df))
   results <- t(apply(key_pairs, 1, function(row) jaccard(df[c(row[1], row[2]),], margin=margin)))
   sim_df <- data.frame(matrix(results[,1], nrow(df), nrow(df)), row.names=row.names(df))
   dist_df <- data.frame(matrix(results[,2], nrow(df), nrow(df)), row.names=row.names(df))
   names(sim_df) <- row.names(df)
   names(dist_df) <- row.names(df)
   list(JSim=sim_df, JDist=dist_df)
}

apply_jaccard(df, margin=2)

df
