library(terra)
library(sf)
library(stars)
library(randomForest)
library(caret)

load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/Outputs/NE_10km_school_count_RF_Pred_Probs.RData")

head(pred.prob.data)

pred.prob.data$pos_0.01 <- ifelse(pred.prob.data$sig.prob_0.01 > 0, 1, 0)

grid_10km <- pred.prob.data[,c("x", "y", "cell", "pos_0.01")]
grid_10km.r <- rast(grid_10km, type = 'xyz', crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs", digits = 6, extent = NULL)

grid_data <- read.csv("/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_Grid_Counts_1km_wData.csv")
head(grid_data)

names(grid_data)
data <- grid_data[,-1]
data <- na.omit(data)
names(data)
names(data)[c(10:17)] <- c("smod.10", "smod.11", "smod.12", "smod.13", "smod.21", "smod.22", "smod.23", "smod.30")
data$SCHOOLS <- as.factor(ifelse(data$NUMPOINTS > 0, 1, 0))
head(data)

data.sf <- st_as_sf(data, coords = c("x", "y"), crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs")
data.sf <- cbind(data.sf, extract(grid_10km.r, data.sf))

data <- cbind(st_drop_geometry(data.sf, xy = TRUE), st_coordinates(data.sf))
data <- subset(data, pos_0.01 == 1)
head(data)

set.seed(23)

ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

rf <- randomForest(SCHOOLS ~ X + Y + land + pop + built_s + built_v + smod.10 + smod.11 + smod.12 + smod.13 + smod.21 + smod.22 + smod.23 + smod.30, 
            data = train, proximity = TRUE) 

rf

save(rf, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/Outputs/NE_10km_to_1km_school_count_RF_AllVars.RData")
saveRDS(rf, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/Outputs/NE_10km_to_1km_school_count_RF_AllVars.rds")

rf <- readRDS(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/Outputs/NE_10km_to_1km_school_count_RF_AllVars.rds")


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

predicted.r <- rast(predicted[,c(20,21,1:19,22)], type = 'xyz', crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs", digits = 6, extent = NULL)
plot(predicted.r)

save(predicted.r, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/Outputs/NE_10km_to_1km_school_count_RF_Pred_Raster.RData")

load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/Outputs/NE_10km_to_1km_school_count_RF_Pred_Raster.RData")


### probability
p4 <- predict(rf, data, type = "prob")
head(p4)
predicted.probs <- cbind(p4, data)
head(predicted.probs)
names(predicted.probs)
#predicted.probs$diff <- as.factor(as.numeric(predicted$p3) - as.numeric(predicted$SCHOOLS))
#names(predicted)

pred.prob.r <- rast(predicted.probs[,c(21,22,1:20)], type = 'xyz', crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs", digits = 6, extent = NULL)
plot(pred.prob.r)

pred.prob.data <- as.data.frame(pred.prob.r, cell = TRUE, xy = TRUE)
head(pred.prob.data)

names(pred.prob.data)[c(4,5)] <- c("prob_0", "prob_1")
pred.prob.data$pred.schools <- ifelse(pred.prob.data$SCHOOLS == 1, NA, pred.prob.data$prob_1)
hist(pred.prob.data$pred.schools)

names(pred.prob.data)
prob.r <- rast(pred.prob.data[,c(2,3,1,4:24)], type = 'xyz', crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs", digits = 6, extent = NULL)
plot(prob.r$pred.schools)

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
prob.r <- rast(pred.prob.data[,c(2,3,1,4:30)], type = 'xyz', crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs", digits = 6, extent = NULL)

par(mfrow = c(2,3))
plot(prob.r$sig.prob_0.2)
plot(prob.r$sig.prob_0.1)
plot(prob.r$sig.prob_0.05)
plot(prob.r$sig.prob_0.025)
plot(prob.r$sig.prob_0.01)
plot(prob.r$sig.prob_0.001)

par(mfrow = c(1,1))
plot(prob.r$sig.prob_0.05)

save(pred.prob.data, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/Outputs/NE_10km_to_1km_school_count_RF_Pred_Probs.RData")
