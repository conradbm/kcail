# Resampling - Comparing results

#.libPaths('U:\\Documents')
#install.packages('caret')
#install.packages('ggplot2')
#install.packages('mlbench')
#install.packages('doParallel')
#install.packages('earth')
library(ggplot2)
library(caret)
library(tidyr)
library(dplyr)
library(earth)
library(doParallel)

setwd("/Users/bmc/Desktop/KCAIL/KState_Data_Exploration/data/")
df_train <- read.csv("Series3_6.15.17_padel.csv", header=TRUE)
df_test <- read.csv("Selleck_filtered_padel_corrected.csv", header=TRUE)
df_train_imputed <- read.csv("Imputed_Series3_6.15.17_padel.csv", header=TRUE)
df_test_imputed <- read.csv("Imputed_Selleck_filtered_padel_corrected.csv", header=TRUE)

head(df_train)
head(df_test)
head(df_train_imputed)
head(df_test_imputed)

dim(df_train)
dim(df_test)
dim(df_train_imputed)
dim(df_test_imputed)

row.names(df_train) <- df_train$Name
row.names(df_test) <- df_test$Name
row.names(df_train_imputed) <- df_train_imputed$Name
row.names(df_test_imputed) <- df_test_imputed$Name

df_train <- df_train %>% select(-Name)
df_test <- df_test %>% select(-Name)
df_train_imputed <- df_train_imputed %>% select(-Name)
df_test_imputed <- df_test_imputed %>% select(-Name)

str(df_train)

df_train[,2:ncol(df_train)] <- df_train %>% select(-IC50) %>% 
                              apply(2, function(x) as.numeric(x))

df_test[,2:ncol(df_test)] <- df_test %>% select(-IC50) %>% 
                              apply(2, function(x) as.numeric(x))

df_train_imputed[,2:ncol(df_train_imputed)] <- df_train_imputed %>% select(-IC50) %>% 
                                                apply(2, function(x) as.numeric(x))

df_test_imputed[,2:ncol(df_test_imputed)] <- df_test_imputed %>% 
                                             select(-IC50) %>% apply(2, function(x) as.numeric(x))


  
# control
ctrl = trainControl(method="repeatedcv",
                    repeats=3,
                    number = 3,
                    summaryFunction = defaultSummary)

# gridspace
bagEarthGrid <- expand.grid(nprune=c(1,10,100), degree= c(1:5))

# model 1 
cl <- makeCluster(detectCores())
registerDoParallel(cl)

# model 2
set.seed(123)
marsFit <- train(IC50 ~.,
                 data = df_train,
                 method = c("bagEarth"),
                 preProc = c("center", "scale","BoxCox" ,"nzv"),
                 tuneGrid = bagEarthGrid,
                 trControl = ctrl)
ggplot(marsFit)
marsFit
save(marsFit, file = "regression_marsFit.out")

load("regression_marsFit.out")

stopCluster(cl)
results<-predict(marsFit, newdata=df_test)
results
