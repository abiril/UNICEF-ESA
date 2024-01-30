# Random Forest Classification for School Indicator
  
## Set Up
  
### Packages
  
library(readr)
library(terra)
library(randomForest)
library(datasets)
library(caret)
library(sf)
library(dplyr)
library(ranger)  
library(kableExtra)
library(caret)
library(rmarkdown)


### Load in Data

'Benin grids 10km x 10km using GHSL grids and data

Set seed'



setwd("/home/azureuser/cloudfiles/code/Users/ariley/FINAL") 

grid_data <- read.csv("Data/Africa_Grid_Data_10km_ghsl.csv")

set.seed(23)



## Benin Model

### Set-Up Data

'Load in data, remove any missing data, name the variables something slightly more sensible, specify countries, and create factor indicator for schools/no schools.'


data <- grid_data[,-1]
data <- na.omit(data)
head(data)
names(data)[c(9:16)] <- c("smod.10", "smod.11", "smod.12", "smod.13", "smod.21", "smod.22", "smod.23", "smod.30")

data$SCHOOLS <- as.factor(ifelse(data$NUMPOINTS > 0, 1, 0))

data <- subset(data, country == 1)


## Fit Initial Random Forest Classification Models

'1. Sample testing and training set (training fraction = 0.7)
2. Run Random Forest model with formula
$$
  SCHOOLS ~ x + y + land + pop +  built_s + built_v + smod.10 + smod.11 + smod.12 + smod.13 + smod.21 + smod.22 + smod.23 + smod.30
$$
  3. Save to file'


set.seed(23)

ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]


rf <- ranger(SCHOOLS ~ x + y + land + pop + built_s + built_v + smod.10 + smod.11 + smod.12 + smod.13 + smod.21 + smod.22 + smod.23 + smod.30, 
             data = train, 
             seed = 23) 

rf

sqrt(rf$prediction.error)

saveRDS(rf, file = "Outputs/Benin_10km_school_indicator_RF_AllVars_ghsl.rds")
rf <- readRDS(file = "Outputs/Benin_10km_school_indicator_RF_AllVars_ghsl.rds")




## Model Tuning

'For a Random Forest classification model using the RandomForest package, the default hyperparameter variables are:
  - $n_features = number of model covariates$
  - $mtry = floor(sqrt(n_features))$
  - $replace = TRUE$
  - $min.node.size = 1$
  - $sample.fraction = 1$
  - $ntree = 500$
  
  We consider a grid of alternative hyperparamters:
  - $n_features = number of model covariates$
  - $mtry = c(seq(1, n_features, 1))$
  - $replace = c(TRUE, FALSE)$
  - $min.node.size = c(seq(1, n_features, 1))$
  - $sample.fraction = c(0.5, 0.632, 0.7, 0.8, 0.9)$
  - $ntree = c(n_features*10, 100, 200, 500, 1000)$'



n_features <- 14

hyper_grid <- expand.grid(
  mtry = c(seq(1, n_features, 1)), 
  min.node.size = c(seq(1, n_features, 1)),
  replace = c(TRUE, FALSE),
  sample.fraction = c(0.5, 0.632, 0.7, 0.8, 0.9),
  num.trees = c(n_features*10, 50, 100, 200, 500),
  rmse = NA
)




'Using a full grid search for each country, we run the model tuning. 
We aim to find the hyperparameter values that minimise the model RMSE.'

set.seed(23)

for(j in seq_len(nrow(hyper_grid))){
  fit <- ranger(SCHOOLS ~ x + y + land + pop + built_s + built_v + smod.10 + smod.11 + smod.12 + smod.13 + smod.21 + smod.22 + smod.23 + smod.30, 
                data = train,
                num.trees = hyper_grid$num.trees[j],
                mtry = hyper_grid$mtry[j],
                min.node.size = hyper_grid$min.node.size[j],
                replace = hyper_grid$replace[j],
                sample.fraction = hyper_grid$sample.fraction[j],
                verbose = FALSE,
                seed = 23,
                respect.unordered.factors = 'order'
  )
  hyper_grid$rmse[j] <- sqrt(fit$prediction.error)
}

default_rmse <- sqrt(rf$prediction.error)

tuned_hyper <- hyper_grid %>% arrange(rmse) %>%
  head(1)



tuned_hyper$default_rmse <- sqrt(rf$prediction.error)

print(tuned_hyper)

saveRDS(tuned_hyper, file = "Outputs/Benin_10km_school_indicator_RF_Tuned_Hyperparameters.rds")

tuned_hyper <- readRDS(file = "Outputs/Benin_10km_school_indicator_RF_Tuned_Hyperparameters.rds")





'We report the tuned hyperparameters, the related RMSE and the percentage gain in RMSE'



tuned_hyper %>%
  kbl() %>%
  kable_styling()



## Run the model with the tuned hyperparameters

'We fit and save the model, and report the model RMSE.'



rf2 <- ranger(SCHOOLS ~ x + y + land + pop + built_s + built_v + smod.10 + smod.11 + smod.12 + smod.13 + smod.21 + smod.22 + smod.23 + smod.30, 
              data = train,
              mtry = tuned_hyper[1,1], 
              min.node.size = tuned_hyper[1,2], 
              replace = tuned_hyper[1,3], 
              sample.fraction = tuned_hyper[1,4],
              num.trees = tuned_hyper[1,5],
              respect.unordered.factors = "order",
              seed = 23
)

tuned_hyper$rmse.final <- sqrt(rf2$prediction.error)

tuned_hyper$perc_gain <- (tuned_hyper$default_rmse - tuned_hyper$rmse.final) / tuned_hyper$default_rmse * 100
tuned_hyper$perc_gain

rf2

saveRDS(rf2, file = "Outputs/Benin_10km_school_indicator_RF_Tuned.rds")
rf2 <- readRDS(file = "Outputs/Benin_10km_school_indicator_RF_Tuned.rds")


tuned_hyper




## Prediction 

'Using the model prediction function, we can predict 4 different values:
1. Prediction on training set
2. Prediction on testing set
3. Prediction on entire country data

We can then view the confusion matrix and model statistics, including:
  
  - Confusion Matrix
- Accuracy
- 95\% CI
- No info rate
- p-value (acc > nir)
- kappa
- Mcnemars Test P-Value
- sensitivity
- Specificity
- pos pred value
- neg pred value
- prevalence
- detection rate
- detection prevalence
- balanced accuracy
- positive class'



Africa <- st_read(dsn = "Data/Africa_Shapefile/Africa_Boundaries.shp")
Africa_54009 <- st_transform(Africa, crs = "ESRI:54009")
Benin <- subset(Africa_54009, ISO == "BEN")

plot(Benin)


p1 <- predict(rf2, train)
trained <- cbind(p1$predictions, train)
names(trained)[1] <- "p1"
confusionMatrix(trained$p1, train$SCHOOLS)

p2 <- predict(rf2, test)
tested <- cbind(p2$predictions, test)
names(tested)[1] <- "p2"
confusionMatrix(tested$p2, test$SCHOOLS)

p3 <- predict(rf2, data)
predicted <- cbind(p3$predictions, data)
names(predicted)[1] <- "p3"
confusionMatrix(predicted$p3, data$SCHOOLS)
predicted$diff <- as.factor(as.numeric(predicted$p3) - as.numeric(predicted$SCHOOLS))

pred.sf <- st_as_sf(predicted, coords = c("x","y"), crs = "ESRI:54009")
pred.country <- st_intersection(pred.sf, Benin)

pred.data <- cbind(st_drop_geometry(pred.country), st_coordinates(pred.country))


pred.all <- do.call(rbind, pred.data)

saveRDS(pred.all, file = "Outputs/Benin_10km_school_indicator_RF_Pred.RDS")



'From prediction 3., we can create the gridded prediction as a raster'



names(pred.data)
data.pred <- pred.data[,c(25:26,1:(ncol(pred.data)-2))]
predicted.r <- rast(data.pred, type = "xyz", crs = "ESRI:54009")
country.r <- predicted.r
saveRDS(country.r, file = "Outputs/Benin_10km_school_indicator_RF_Pred_Raster.RDS")

plot(predicted.r)

'And:

4. Probability prediction on entire country data

First we fit the Random Forest using `probability = TRUE` in the ranger function.
Then one loop we also create the gridded predictions as a raster and consider different thresholds of the predictive probabilities.
This is to create a priority order for other work. We consider significance at 6 different levels: 20%, 10%, 5%, 2.5%, 1%, 0.1%
'


rf3 <- ranger(SCHOOLS ~ x + y + land + pop + built_s + built_v + smod.10 + smod.11 + smod.12 + smod.13 + smod.21 + smod.22 + smod.23 + smod.30, 
              data = train,
              mtry = tuned_hyper[1,1], 
              min.node.size = tuned_hyper[1,2], 
              replace = tuned_hyper[1,3], 
              sample.fraction = tuned_hyper[1,4],
              num.trees = tuned_hyper[1,5],
              respect.unordered.factors = "order",
              probability = TRUE,
              seed = 23
)

tuned_hyper$rmse.final.prob <- sqrt(rf3$prediction.error)
tuned_hyper

saveRDS(rf3, file = "Outputs/Benin_10km_school_indicator_RF_Pred.rds")

p4 <- predict(rf3, data)
predicted.probs <- cbind(p4$predictions, data)
names(predicted.probs)[c(1,2)] <- c("prob_0", "prob_1")

pred.sf <- st_as_sf(predicted.probs, coords = c("x","y"), crs = "ESRI:54009")
st_crs(pred.sf) <- "ESRI:54009"
pred.country <- st_intersection(pred.sf, Benin)
pred.data <- cbind(st_drop_geometry(pred.country), st_coordinates(pred.country))

names(predicted.probs)    
pred.prob.r <- rast(predicted.probs[,c(4,5,1:3,6:21)], type = 'xyz', crs = "ESRI:54009")
pred.prob.data <- as.data.frame(pred.prob.r, cell = TRUE, xy = TRUE)

pred.prob.data$pred.schools <- ifelse(pred.prob.data$SCHOOLS == 1 | is.na(pred.prob.data$SCHOOLS), NA, pred.prob.data$prob_1)

head(pred.prob.data)
names(pred.prob.data)
prob.r <- rast(pred.prob.data[,c(2,3,1,4:23)], type = 'xyz', crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs", digits = 6, extent = NULL)


quant <- quantile(pred.prob.data[,"prob_1"], probs = c(0.8, 0.9, 0.95, 0.975, 0.99, 0.999, 1), na.rm = TRUE)

pred.prob.data$sig.prob_0.2 <- ifelse(pred.prob.data$prob_1 < quant[1], NA, pred.prob.data$prob_1)
pred.prob.data$sig.prob_0.1 <- ifelse(pred.prob.data$prob_1 < quant[2], NA, pred.prob.data$prob_1)
pred.prob.data$sig.prob_0.05 <- ifelse(pred.prob.data$prob_1 < quant[3], NA, pred.prob.data$prob_1)
pred.prob.data$sig.prob_0.025 <- ifelse(pred.prob.data$prob_1 < quant[4], NA, pred.prob.data$prob_1)
pred.prob.data$sig.prob_0.01 <- ifelse(pred.prob.data$prob_1 < quant[5], NA, pred.prob.data$prob_1)
pred.prob.data$sig.prob_0.001 <- ifelse(pred.prob.data$prob_1 < quant[6], NA, pred.prob.data$prob_1)

prob.pred <- pred.prob.data
save(pred.prob.data, file = "Outputs/Benin_10km_school_indicator_RF_Pred_Prob.RData")

prob.r <- rast(pred.prob.data[,c(2,3,1,4:28)], type = 'xyz', crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs", digits = 6, extent = NULL)

plot(prob.r$sig.prob_0.1)




