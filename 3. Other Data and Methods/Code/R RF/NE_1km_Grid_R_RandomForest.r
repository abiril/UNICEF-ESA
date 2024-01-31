####  Randon Forest in R

library(readr)
library(terra)
library(randomForest)
library(datasets)
library(caret)

grid_data <- read.csv("/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_Grid_Counts_1km_wData.csv")
head(grid_data)

names(grid_data)
data <- grid_data[,-1]
data <- na.omit(data)
names(data)
names(data)[c(10:17)] <- c("smod.10", "smod.11", "smod.12", "smod.13", "smod.21", "smod.22", "smod.23", "smod.30")
data$NUMPOINTS <- as.factor(ifelse(data$NUMPOINTS > 0, 1, 0))
head(data)

set.seed(23)

ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

rf <- randomForest(NUMPOINTS ~ x + y + land + pop + built_s + built_v + smod.10 + smod.11 + smod.12 + smod.13 + smod.21 + smod.22 + smod.23 + smod.30, 
            data = train, proximity = TRUE) 

rf

save(rf, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/Outputs/NE_1km_school_count_RF_AllVars.RData")
saveRDS(rf, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/Outputs/NE_1km_school_count_RF_AllVars.rds")

rf <- readRDS(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/Outputs/NE_1km_school_count_RF_AllVars.rds")

rf
plot(rf)

p1 <- predict(rf, train)
head(p1)

trained <- cbind(p1, train)
head(trained)
hist(as.numeric(trained$p1))

confusionMatrix(p1, train$NUMPOINTS)

p2 <- predict(rf, test)
tested <- cbind(p2, test)
head(tested)
hist(as.numeric(tested$p2))

confusionMatrix(p2, test$NUMPOINTS)

p3 <- predict(rf, data)
predicted <- cbind(p3, data)
head(predicted)
predicted$diff <- as.factor(as.numeric(predicted$p3) - as.numeric(predicted$NUMPOINTS))
names(predicted)

predicted.r <- rast(predicted[,c(2,3,1,4:20)], type = 'xyz', crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs", digits = 6, extent = NULL)
plot(predicted.r)

save(predicted.r, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/Outputs/NE_1km_school_count_RF_Pred_Raster.RData")

load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/Outputs/NE_1km_school_count_RF_Pred_Raster.RData")


###
### Clustering
pred.data <- as.data.frame(predicted.r, cell = TRUE)
names(pred.data)
kmncluster <- kmeans(pred.data[,-c(1,3,4,6)], centers=10, iter.max = 500, nstart = 5, algorithm="Lloyd")

knr <- rast(predicted.r, nlyr=1)
knr[pred.data$cell] <- kmncluster$cluster
knr

par(mfrow = c(1,2))
plot(predicted.r$NUMPOINTS)
plot(knr, main = 'Unsupervised classification',  type="classes")
###

#MDSplot(rf, train$NUMPOINTS)

#### probability prediction

p4 <- predict(rf, data, type = "prob")
head(p4)
predicted.probs <- cbind(p4, data)
head(predicted.probs)
#predicted.probs$diff <- as.factor(as.numeric(predicted$p3) - as.numeric(predicted$NUMPOINTS))
#names(predicted)

pred.prob.r <- rast(predicted.probs[,c("x", "y", "0", "1", "NUMPOINTS")], type = 'xyz', crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs", digits = 6, extent = NULL)
plot(pred.prob.r)

pred.prob.data <- as.data.frame(pred.prob.r, cell = TRUE, xy = TRUE)
head(pred.prob.data)

names(pred.prob.data)[c(4,5)] <- c("prob_0", "prob_1")
pred.prob.data$pred.schools <- ifelse(pred.prob.data$NUMPOINTS == 1, NA, pred.prob.data$prob_1)
hist(pred.prob.data$pred.schools)

head(pred.prob.data)
prob.r <- rast(pred.prob.data[,c(2,3,1,4,5,6,7)], type = 'xyz', crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs", digits = 6, extent = NULL)
plot(prob.r$pred.schools)

head(pred.prob.data)

quantile(pred.prob.data[,7], probs = c(0.8, 0.9, 0.95, 0.975, 0.99, 0.999, 1), na.rm = TRUE)

pred.prob.data$sig.prob_0.2 <- ifelse(pred.prob.data$prob_1 < 0.042, NA, pred.prob.data$prob_1)
pred.prob.data$sig.prob_0.1 <- ifelse(pred.prob.data$prob_1 < 0.340, NA, pred.prob.data$prob_1)
pred.prob.data$sig.prob_0.05 <- ifelse(pred.prob.data$prob_1 < 0.4665, NA, pred.prob.data$prob_1)
pred.prob.data$sig.prob_0.025 <- ifelse(pred.prob.data$prob_1 < 0.6960, NA, pred.prob.data$prob_1)
pred.prob.data$sig.prob_0.01 <- ifelse(pred.prob.data$prob_1 < 0.9380, NA, pred.prob.data$prob_1)
pred.prob.data$sig.prob_0.001 <- ifelse(pred.prob.data$prob_1 < 0.9948, NA, pred.prob.data$prob_1)

prob.r <- rast(pred.prob.data[,c(2,3,1,4,5,6,7,8:13)], type = 'xyz', crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs", digits = 6, extent = NULL)

par(mfrow = c(2,3))
plot(prob.r$sig.prob_0.2)
plot(prob.r$sig.prob_0.1)
plot(prob.r$sig.prob_0.05)
plot(prob.r$sig.prob_0.025)
plot(prob.r$sig.prob_0.01)
plot(prob.r$sig.prob_0.001)


### spatial clustering
library(rgeoda)
library(sf)

#https://spatialanalysis.github.io/workshop-notes/spatial-clustering.html

pred.df <- as.data.frame(predicted.r, cell = TRUE, xy = TRUE)
pred.sf <- st_as_sf(pred.df, coords = c("x","y"), crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs")
pred.rgeoda <- sf_to_geoda(pred.sf, with_table = TRUE)

head(pred.df)

queen_w <- queen_weights(pred.sf)

pred_clusters <- skater(4, w = queen_w, df = pred.df)

pred_clusters

library(ggplot2)

