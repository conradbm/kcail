# Cosine Similarity || Soft Cosine Similarity

library(dplyr)
# Your dataset
#df <- data.frame(t(data.frame(c1=rnorm(100),
#                              c2=rnorm(100),
#                              c3=rnorm(100),
#                              c4=rnorm(100),
#                              c5=rnorm(100),
#                              c6=rnorm(100))))
#df[df > 0] <- 1
#df[df <= 0] <- 0
#df

#
# Function Name: apply_cosine_similaritiy
# Return: matrix, containing cosine similarities between each vector
# Parameters:
# 1. df, dataframe of interest
# 2. margin, axis in which the apply function is meant to move along
#
apply_cosine_similarity <- function(df){
  cos.sim <- function(df, ix) 
  {
    A = df[ix[1],]
    B = df[ix[2],]
    return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
  }   
  n <- nrow(df) 
  key_pairs <- expand.grid(row.names(df), row.names(df))
  C <- matrix(apply(key_pairs,1,function(cmb){ cos.sim(df, cmb) }),n,n)
  ret_df <- data.frame(C, row.names=row.names(df))
  names(ret_df) <- row.names(df)
  ret_df
}

#apply_cosine_similarity(df)

