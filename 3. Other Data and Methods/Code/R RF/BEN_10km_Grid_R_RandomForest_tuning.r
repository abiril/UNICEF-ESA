####  Randon Forest in R

setwd("/")

library(readr)
library(terra)
library(randomForest)
library(datasets)
library(caret)
library(ranger)
library(dplyr)

grid_data <- read.csv("/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Africa/School_Counts/Gridded_10km_Data/BEN_Grid_Data_10km.csv")
head(grid_data)

names(grid_data)
data <- grid_data[,-1]
data <- na.omit(data)
names(data)
names(data)[c(9:16)] <- c("smod.10", "smod.11", "smod.12", "smod.13", "smod.21", "smod.22", "smod.23", "smod.30")
data$SCHOOLS <- as.factor(ifelse(data$NUMPOINTS > 0, 1, 0))
head(data)

set.seed(23)

ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

rf <- ranger(SCHOOLS ~ x + y + land + pop + built_s + built_v + smod.10 + smod.11 + smod.12 + smod.13 + smod.21 + smod.22 + smod.23 + smod.30, 
            data = train) 

rf

load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Africa/School_Counts/Outputs/BEN_10km_school_count_RF_AllVars.RData")

#### TUNING
n_features <- 14

tuning_paras <- expand.grid(
  num.trees = c(60, 80, 100, 120, 140, 160, 180), #approx n_feature x 10
  mtry = floor(n_features * c(.01, .025, .05, .15, .10, .25, .333, .4, .5)),
  min.node.size = c(1, 3, 5, 10, 15),
  replace = c(TRUE, FALSE),                               
  sample.fraction = c(.5, .63, .8),                       
  rmse = NA 
)

for(i in seq_len(nrow(tuning_paras))) {
  fit <- ranger(
    SCHOOLS ~ x + y + land + pop + built_s + built_v + smod.10 + smod.11 + smod.12 + smod.13 + smod.21 + smod.22 + smod.23 + smod.30, 
    data = train, 
    num.trees = tuning_paras$num.trees[i],
    mtry = tuning_paras$mtry[i],
    min.node.size = tuning_paras$min.node.size[i],
    replace = tuning_paras$replace[i],
    sample.fraction = tuning_paras$sample.fraction[i],
    verbose = FALSE,
    seed = 23,
    respect.unordered.factors = 'order',
  )
  # export OOB error 
  tuning_paras$rmse[i] <- sqrt(fit$prediction.error)
}

tuning_paras %>%
  arrange(rmse) %>%
  mutate(perc_gain = (default_rmse - rmse) / default_rmse * 100) %>%
  head(10)

plot(rf)

p1 <- predict(rf, train)
head(p1)

trained <- cbind(p1, train)
head(trained)
hist(as.numeric(trained$p1))

confusionMatrix(p1, train$SCHOOLS)

p2 <- predict(rf, test)
tested <- cbind(p2, test)
head(tested)
hist(as.numeric(tested$p2))

confusionMatrix(p2, test$SCHOOLS)

p3 <- predict(rf, data)
confusionMatrix(p3, data$SCHOOLS)

predicted <- cbind(p3, data)
head(predicted)
predicted$diff <- as.factor(as.numeric(predicted$p3) - as.numeric(predicted$SCHOOLS))
names(predicted)

Africa <- st_read(dsn = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Shapefiles/Africa_Shapefile/Africa_Boundaries.shp")
Africa <- st_transform(Africa, crs = "ESRI:54009")
AOI <- subset(Africa, ISO == "BEN")

pred.sf <- st_as_sf(predicted, coords = c("x","y"), crs = "ESRI:54009")
pred.country <- st_intersection(pred.sf, AOI)
plot(pred.country)
pred.data <- cbind(st_drop_geometry(pred.country), st_coordinates(pred.country))

names(pred.data)
predicted.r <- rast(pred.data[,c(25,26,1:19)], type = 'xyz')
plot(predicted.r)


save(predicted.r, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Africa/School_Counts/Outputs/BEN_10km_school_count_RF_Pred_Raster.RData")

#load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Africa/School_Counts/Outputs/BEN_10km_school_count_RF_Pred_Raster.RData")


###
### Clustering
pred.data <- as.data.frame(predicted.r, cell = TRUE)
names(pred.data)
head(pred.data)
kmncluster <- kmeans(pred.data[,-c(1,17,18)], centers=10, iter.max = 500, nstart = 5, algorithm="Lloyd")

knr <- rast(predicted.r, nlyr=1)
knr[pred.data$cell] <- kmncluster$cluster
knr

par(mfrow = c(1,2))
plot(predicted.r$SCHOOLS)
plot(knr, main = 'Unsupervised classification',  type="classes")
###

#MDSplot(rf, train$SCHOOLS)

#### probability prediction

p4 <- predict(rf, data, type = "prob")
head(p4)
predicted.probs <- cbind(p4, data)
head(predicted.probs)
names(predicted.probs)
#predicted.probs$diff <- as.factor(as.numeric(predicted$p3) - as.numeric(predicted$SCHOOLS))
names(predicted.probs)


pred.sf <- st_as_sf(predicted.probs, coords = c("x","y"), crs = "ESRI:54009")
head(pred.sf)
pred.country <- st_intersection(pred.sf, AOI)
head(pred.country)
plot(pred.country)
pred.data <- cbind(st_drop_geometry(pred.country), st_coordinates(pred.country))

head(pred.data)
names(pred.data)
pred.prob.r <- rast(pred.data[,c(25,26,1:19)], type = 'xyz')
plot(pred.prob.r)

pred.prob.data <- as.data.frame(pred.prob.r, cell = TRUE, xy = TRUE)
head(pred.prob.data)
names(pred.prob.data)

names(pred.prob.data)[c(4,5)] <- c("prob_0", "prob_1")
pred.prob.data$pred.schools <- ifelse(pred.prob.data$SCHOOLS == 1 | is.na(pred.prob.data$SCHOOLS), NA, pred.prob.data$prob_1)
hist(pred.prob.data$pred.schools)

head(pred.prob.data)
names(pred.prob.data)
prob.r <- rast(pred.prob.data[,c(2,3,1,4:23)], type = 'xyz', crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs", digits = 6, extent = NULL)
plot(prob.r)

par(mfrow = c(1,1))
plot(prob.r$prob_1)



head(pred.prob.data)

quant <- quantile(pred.prob.data[,"prob_1"], probs = c(0.8, 0.9, 0.95, 0.975, 0.99, 0.999, 1), na.rm = TRUE)
quant[1]

pred.prob.data$sig.prob_0.2 <- ifelse(pred.prob.data$prob_1 < quant[1], NA, pred.prob.data$prob_1)
pred.prob.data$sig.prob_0.1 <- ifelse(pred.prob.data$prob_1 < quant[2], NA, pred.prob.data$prob_1)
pred.prob.data$sig.prob_0.05 <- ifelse(pred.prob.data$prob_1 < quant[3], NA, pred.prob.data$prob_1)
pred.prob.data$sig.prob_0.025 <- ifelse(pred.prob.data$prob_1 < quant[4], NA, pred.prob.data$prob_1)
pred.prob.data$sig.prob_0.01 <- ifelse(pred.prob.data$prob_1 < quant[5], NA, pred.prob.data$prob_1)
pred.prob.data$sig.prob_0.001 <- ifelse(pred.prob.data$prob_1 < quant[6], NA, pred.prob.data$prob_1)

head(pred.prob.data)
prob.r <- rast(pred.prob.data[,c(2,3,1,4:29)], type = 'xyz', crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs", digits = 6, extent = NULL)

par(mfrow = c(2,3))
plot(prob.r$sig.prob_0.2)
plot(prob.r$sig.prob_0.1)
plot(prob.r$sig.prob_0.05)
plot(prob.r$sig.prob_0.025)
plot(prob.r$sig.prob_0.01)
plot(prob.r$sig.prob_0.001)

par(mfrow = c(2,2))
plot(prob.r$NUMPOINTS, main = "Number of Schools")
plot(prob.r$SCHOOLS, main = "School Indicator")
plot(prob.r$prob_1, main = "Probability of School")
plot(prob.r$sig.prob_0.05, main = "95% Probability of School")


plot.pred <- pred.prob.data[,c("x", "y", "NUMPOINTS", "SCHOOLS", "prob_1", "sig.prob_0.05")]
p1 <- ggplot() +
    geom_tile(data = plot.pred, aes(x = x, y = y, fill = NUMPOINTS)) +
     scale_fill_viridis(option = "C") +
     coord_equal() 
p2 <- ggplot() +
    geom_tile(data = plot.pred, aes(x = x, y = y, fill = factor(SCHOOLS))) +
     coord_equal() 
p3 <- ggplot() +
    geom_tile(data = plot.pred, aes(x = x, y = y, fill = prob_1)) +
     scale_fill_viridis() +
     coord_equal() 
p4 <- ggplot() +
    geom_tile(data = plot.pred, aes(x = x, y = y, fill = sig.prob_0.05)) +
     scale_fill_viridis() +
     coord_equal() 

ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
ggsave(filename =  "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Africa/School_Counts/Outputs/Benin_Plots.png")

pred.long <- 




save(pred.prob.data, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Africa/School_Counts/Outputs/BEN_10km_school_count_RF_Pred_Probs.RData")
