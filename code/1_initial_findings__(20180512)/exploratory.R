.libPaths("U:\\Documents")
library(ggplot2)
library(caret)
library(tidyr)
library(dplyr)
#install.packages('corrplot')
library(corrplot)
library(ggrepel)
setwd("C:\\Users\\1517766115.CIV\\Desktop\\Testbed")

df_train <- read.csv("Series3_6.15.17_padel.csv", header=TRUE)
df_extra <- read.csv("Selleck_filtered_padel_corrected.csv", header=TRUE)

# Clean
row.names(df_train) <- as.character(df_train$Name)
#df_train <- df_train %>% mutate_all(as.numeric)
#df_train <- df_train %>% select(everything()) %>% filter(!is.na(IC50))
#df_train[is.na(df_train)]<-0

row.names(df_extra) <- as.character(df_extra$Name)
#df_extra <- df_extra %>% mutate_all(as.numeric)
#df_extra <- df_extra %>% select(everything()) %>% filter(!is.na(IC50))
#df_extra[is.na(df_extra)]<-0

# Bin
df_train$potent <- "IMPOTENT"
df_train[df_train$IC50 < 40,"potent"] <- "POTENT"
df_extra$potent <- "IMPOTENT"
df_train$potent <- as.factor(df_train$potent)
df_extra$potent <- as.factor(df_extra$potent)
str(df_train)
str(df_extra)

# Merge data
row.names(df_train) <- as.character(df_train$Name)
row.names(df_extra) <- as.character(df_extra$Name)
df <- rbind(df_train, df_extra)


# Cluster
clusterRes <- df_train %>% select(-c(IC50, Name, potent)) %>% kmeans(centers=2)
df_train$cluster <- clusterRes$cluster
df_train <- df_train[with(df_train, order(-cluster)),]

clusterRes <- df %>% select(-c(IC50, Name, potent)) %>% kmeans(centers=2)
df$cluster <- clusterRes$cluster
df <- df[with(df, order(-cluster)),]

# Reorder names
df_train <- df_train %>% select(Name, IC50, potent, cluster, everything())
df_extra <- df_extra %>% select(Name, IC50, potent, everything())
df <- df %>% select(Name, IC50, potent, cluster, everything())


# Preprocess
dim(df_train)
preproc <- df_train %>% 
          select(-c(Name,potent, cluster)) %>% 
          preProcess(method=c("center", "scale", "BoxCox", "nzv" ,"pca"))

new_df_train <- predict(preproc,newdata=select(df_train,-c(Name,potent, cluster))) %>% 
                mutate(Name = df_train$Name,
                       cluster = df_train$cluster,
                       potent = df_train$potent)
new_df_train$IC50 <- df_train$IC50
new_df_train$cluster <- new_df_train$cluster %>% as.numeric()
new_df_train$potent <- new_df_train$potent %>% as.factor()

# Visualize clusters and potency
# What does our potent class look like?
ggplot(new_df_train, aes(x=PC1, y=IC50, color=potent)) +
  geom_point(size=5) + 
  geom_label_repel(aes(label = Name),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  facet_wrap(~cluster) +
  ggtitle("Separability", 
          subtitle = "Center | Scale | BoxCox | NZV | PCA | Kmeans")

ggplot(new_df_train, aes(x=PC1, y=PC2, color=cluster)) +
  geom_point(size=5) + 
  geom_label_repel(aes(label = Name),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  facet_wrap(~potent) +
  ggtitle("Separability", 
          subtitle = "Center | Scale | BoxCox | NZV | PCA | Kmeans") +
  geom_abline() + theme(legend.position="top")

# Visualize; IC50 Spread
# What does our distribution of IC50 look like?
ggplot(data=data.frame(Compound=df_train$Name,
                       IC50=df_train$IC50)) +
  geom_point(aes(x=Compound, y=IC50), size=2 ,alpha=1 )+ 
  theme(axis.text.x = element_text(angle = 90, hjust=0.5)) +
  ggtitle('Compound vs. IC50', subtitle="Series Dataset")

ggplot(data=data.frame(Compound=df$Name,
                       IC50=df$IC50,
                       cluster=as.factor(df$cluster)) )+
  geom_point(aes(x=Compound, y=IC50, color=cluster), size=2, alpha=.5 )+ 
  theme(axis.text.x = element_text(angle = 90, hjust=0.5)) +
  ggtitle('Compound vs. IC50', subtitle = "Series and Selleck Datasets")



# At a high level, do clear correlations exist in our data sets?
df_train[,c(5:ncol(df_train))] <- df_train[,c(5:ncol(df_train))] %>% mutate_all(as.numeric)

tile_df_series <- cor(df_train[,c(5:ncol(df_train))]) %>% 
                      data.frame %>% 
                      mutate(from=names(df_train[,c(5:ncol(df_train))])) %>%
                      gather(key="to", value="cor", -from)

ggplot(data=tile_df_series) + geom_tile(aes(x=from, y=to, fill=cor)) +
  scale_fill_gradientn(colours = terrain.colors(100)) +ggtitle("Chemical Feature Correlation",
                                                               subtitle = "Series Dataset")
tile_df_selleck <- cor(df_extra[,c(4:ncol(df_extra))]) %>% 
                      data.frame %>% 
                      mutate(from=names(df_extra[,c(4:ncol(df_extra))])) %>%
                      gather(key="to", value="cor", -from)

ggplot(data=tile_df_selleck) + geom_tile(aes(x=from, y=to, fill=cor)) +
  scale_fill_gradientn(colours = terrain.colors(100)) +ggtitle("Chemical Feature Correlation",
                                                               subtitle = "Selleck Dataset")
tile_df_joint <- cor(df[,c(5:ncol(df))]) %>% 
                    data.frame %>% 
                    mutate(from=names(df[,c(5:ncol(df))])) %>%
                    gather(key="to", value="cor", -from)

ggplot(data=tile_df_joint) + geom_tile(aes(x=from, y=to, fill=cor)) +
  scale_fill_gradientn(colours = terrain.colors(100)) +ggtitle("Chemical Feature Correlation",
                                                               subtitle = "Series and Selleck Datasets")


# Regression; What are the most important interactions?
#cat(names(df_train[,c(5:ncol(df_train))]), sep="*", file="sepNames.csv")
#formula_string <- read.csv('sepNames.csv', header=FALSE)
#formula_string <- paste("IC50~", formula_string[[1]] %>% as.character)
#formula_string
#fit <- lm(formula_string, data=df_train)
#summary(fit)
#names(df_train)
# Plot top 10% of interactions (chem. feat. vs. coeff.)


library(caret)

inTrain <- createDataPartition(y = df$potent,
                               p = .75,
                               list = FALSE)

df$potent
training <- df[inTrain,] %>% select(-c(Name, cluster, IC50)) 

testing <- df[-inTrain,] %>% select(-c(Name, cluster, IC50))

plsFit <- train(potent ~.,
                data = training,
                method = c("pls"),
                preProc = c("center", "scale", "nzv"),
                tuneLength=10,
                trControl = trainControl(method="repeatedcv",number=3, repeats=3, classProbs = TRUE))
ggplot(plsFit)

plsFit
plsProbs <- predict(plsFit,
                    newdata = testing,
                    type = "prob")
plsProbs
