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


# partitian data
inTrain <- createDataPartition(Sonar$Class,
                               p=0.75,
                               list=FALSE)
training = Sonar[inTrain,]
testing = Sonar[-inTrain,]

# control
ctrl = trainControl(method="repeatedcv",
                    repeats=10,
                    classProbs = TRUE,
                    summaryFunction = twoClassSummary)
# gridspace
rdaGrid <- data.frame(expand.grid((0:4)/5, 0.5:2.5)) 
names(rdaGrid) <- c("gamma", "lambda")
rdaGrid

knnGrid <- data.frame(k=(1:25))

# model 1 
set.seed(123)
plsFit <- train(Class ~.,
                data = training,
                method = c("pls"),
                preProc = c("center", "scale"),
                tuneLength = 15,
                trControl = ctrl,
                metric="ROC")
ggplot(plsFit)

# model 2
set.seed(123)
rdaFit <- train(Class ~.,
                data = training,
                method = c("rda"),
                preProc = c("center", "scale"),
                tuneGrid=rdaGrid,
                trControl = ctrl,
                metric="ROC")
ggplot(rdaFit)

# model 3
set.seed(123)
knnFit <- train(Class ~.,
                data=training,
                method=c("knn"),
                preProc=c("center", "scale"),
                tuneGrid=knnGrid,
                trControl = ctrl,
                metric="ROC")
knnFit
ggplot(knnFit)

# Resampling
resamps <- resamples(list(knn=knnFit,
                          pls=plsFit,
                          rda=rdaFit))
summary(resamps)

# Are the models statistically different?
diffs <- diff(resamps)
summary(diffs)