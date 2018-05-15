# Resampling - Comparing results

#.libPaths('U:\\Documents')
#install.packages('caret')
#install.packages('ggplot2')
#install.packages('mlbench')
library(ggplot2)
library(caret)
library(mlbench)
library(dplyr)

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
                    repeats=10,
                    number = 3,
                    summaryFunction = defaultSummary)

# gridspace
knnGrid <- data.frame(k = seq(3:25)) #knn
leapSeqGrid <- data.frame(nvmax = seq(10:(ncol(df_train))))
bagEarthGrid <- data.frame(expand.grid(nprune=c(1,10,100),degree= c(1:5)))

# model 1 
set.seed(123)

knnFit <- train(IC50 ~.,
                data = df_train,
                method = c("knn"),
                preProc = c("center", "scale","BoxCox" ,"nzv"),
                tuneGrid = knnGrid,
                trControl = ctrl)
ggplot(knnFit)
knnFit

# model 2
set.seed(123)
marsFit <- train(IC50 ~.,
                 data = df_train,
                 method = c("badEarth"),
                 preProc = c("center", "scale","BoxCox" ,"nzv"),
                 tuneGrid = bagEarthGrid,
                 trControl = ctrl)
ggplot(marsFit)

# model 3
set.seed(123)
lmMixedFit <- train(IC50 ~.,
                data = df_train,
                method = c("leapSeq"),
                preProc = c("center", "scale","BoxCox" ,"nzv"),
                tuneGrid = leapSeqGrid,
                trControl = ctrl)

ggplot(lmMixedFit)

# Model 4
set.seed(123)
svmRadialFit <- train(IC50 ~.,
                    data = df_train,
                    method = c("svmRadial"),
                    preProc = c("center", "scale","BoxCox" ,"nzv"),
                    tuneLength = 10,
                    trControl = ctrl)

ggplot(svmRadialFit)
svmRadialFit

# Resampling
resamps <- resamples(list(knn=knnFit,
                          mars=marsFit,
                          lmMixed=lmMixedFit,
                          svmRadial=svmRadialFit))
summary(resamps)

# Are the models statistically different?
diffs <- diff(resamps)
summary(diffs)