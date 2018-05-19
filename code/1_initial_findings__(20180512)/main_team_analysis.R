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
knnGrid <- data.frame(k = seq(3:25)) #knn
leapSeqGrid <- data.frame(nvmax = seq(10:(ncol(df_train))))
bagEarthGrid <- expand.grid(nprune=c(1,10,100), degree= c(1:5))

# model 1 
cl <- makeCluster(detectCores())
registerDoParallel(cl)
set.seed(123)
knnFit <- train(IC50 ~.,
                data = df_train,
                method = c("knn"),
                preProc = c("center", "scale","BoxCox" ,"nzv"),
                tuneGrid = knnGrid,
                trControl = ctrl)
ggplot(knnFit)
knnFit
save(knnFit, file = "regression_knnFit.out")
load("regression_knnFit.out")

knnFit
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



# model 3
set.seed(123)
lmMixedFit <- train(IC50 ~.,
                data = df_train,
                method = c("leapSeq"),
                preProc = c("center", "scale","BoxCox" ,"nzv"),
                tuneGrid = leapSeqGrid,
                trControl = ctrl)

ggplot(lmMixedFit)
save(kmMixedFit, file = "regression_lmMixedFitFit.out")
load("regression_lmMixedFit.out")

# Model 4
set.seed(123)
svmRadialFit <- train(IC50 ~.,
                    data = df_train,
                    method = c("svmRadial"),
                    preProc = c("center", "scale","BoxCox" ,"nzv"),
                    tuneLength = 10,
                    trControl = ctrl)

ggplot(svmRadialFit)
save(svmRadialFit, file = "regression_svmRadialFit.out")
load("regression_svmRadialFit.out")

stopCluster(cl)


# Resampling
resamps <- resamples(list(knn=knnFit,
                          mars=marsFit,
                          lmMixed=lmMixedFit,
                          svmRadial=svmRadialFit))
summary(resamps)

# Are the models statistically different?
diffs <- diff(resamps)
summary(diffs)