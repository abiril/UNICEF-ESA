
# Random Forest Regression for School Counts

'We set up the data the same way as the non-spatial approach and load in required model results for the spatial model.
'

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
library(rmarkdown)
library(ggpubr)


### Load in Data

'Africa grids 10km x 10km using GHSL grids and data

Set seed'



setwd("/home/azureuser/cloudfiles/code/Users/ariley/FINAL")

grid_data <- read.csv("Data/Africa_Grid_Data_10km_ghsl.csv")

set.seed(23)
random.seed <- 23



## Individual African Country Models

### Set-Up Data

'Load in data, remove any missing data, name the variables something slightly more sensible, specify countries, and create factor indicator for schools/no schools.

Define output and predictor variables

We also need all the variables to be numeric, in this case.'



grid_data <- subset(grid_data, country == 1)

data <- grid_data[,-1]
data <- na.omit(data)
head(data)
names(data)[c(9:16)] <- c("smod.10", "smod.11", "smod.12", "smod.13", "smod.21", "smod.22", "smod.23", "smod.30")
data$SCHOOLS <- as.factor(ifelse(data$NUMPOINTS > 0, 1, 0))

dependent.variable.name <- "NUMPOINTS"
predictor.variable.names <- colnames(data)[c(4:6, 8:16)]

data[,dependent.variable.name] <- as.numeric(data[,dependent.variable.name])

for(i in 1:12){
    print(class(data[,predictor.variable.names[i]]))
    data[,predictor.variable.names[[i]]] <- as.numeric(data[,predictor.variable.names[[i]]])

}



### Set-Up Spatial Terms

'We take the coordinates as `x` and `y`

And define the distance matrix as the distances between all points

Note: Here we consider just the centroids of the grid cells

We also create a small vector of distance thresholds, a sequence of distances we evaluate spatial autocorrelation at.
We are working in metres.'



xy <- data[, c("x", "y")]

distance.matrix <- as.matrix(dist(xy), ncol = nrow(xy))
head(distance.matrix)

distance.thresholds <- c(0, 10000, 20000, 40000, 80000, 160000, 320000)



### Check multicollinearity

'We can test for potential multicollinearity between the model variables.

We specify our own preference order, highlighting that `land` and `pop` are likely to be the biggest drives.

We set the correlation threshold as 0.6 and the variance inflation factor threshold as 2.5.

We repeat this for each country individually'



preference.order <- c(
    "land", "pop", "built_s", "built_v",
    "smod.30", "smod.23", "smod.22", "smod.21", "smod.13", "smod.12", "smod.11", "smod.10"
  )

predictor.variable.names <- spatialRF::auto_cor(
    x = data[, predictor.variable.names],
    cor.threshold = 0.6,
    preference.order = preference.order 
) %>% 
    spatialRF::auto_vif(
        vif.threshold = 2.5,
        preference.order = preference.order
  )


predictor.variable.names$selected.variables



'We update our predictors and remove any redundant variables'

### Load in Tuned and Cross Validated Non-Spatial Model



load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Africa/School_Counts/Outputs/BEN_10km_school_count_nonspatialRF_tuned_model_final.RData")



### Load in Data with Prediction s

 

load(file = "Outputs/BEN_10km_school_count_data_with_model_pred.RData")




## Motivating Spatial Model

'We can asses the spatial autocorrelation of the non-spatial models Residuals

We are looking for some spatial relationships not explained by the covariates/

We calculate and plot Morans I statistic at each distance threshold'



spatialRF::plot_moran(
  model.non.spatial, 
  verbose = FALSE
  )



$p-values < 0.05$ indicates potential spatial autocorrelation

## Spatial Model

'Using the non-spatial model and the function `rf_spatial`, we fit the spatial model '



model.spatial <- spatialRF::rf_spatial(
  model = model.non.spatial,
  method = "mem.moran.sequential", 
  verbose = FALSE,
  seed = random.seed
)

save(model.spatial, file = "Grid_Model/Outputs/BEN_10km_school_SpatialRF_model.RData")



'We can see this models performance'



spatialRF::print_performance(model.spatial)



'Now we can recheck the residuals and see that the model has captured most of the additional spatial variation
'

spatialRF::plot_moran(
  model.spatial, 
  verbose = FALSE
  )
  


'We can see similar plots as the non-spatial model, to identify important variables, plot the new spatial predictor variables.
'



p1 <- spatialRF::plot_importance(
  model.non.spatial, 
  verbose = FALSE) + 
  ggplot2::ggtitle("Non-spatial model") 

p2 <- spatialRF::plot_importance(
  model.spatial,
  verbose = FALSE) + 
  ggplot2::ggtitle("Spatial model")

p1 | p2 


kableExtra::kbl(
  head(model.spatial$importance$per.variable, n = 10),
  format = "html"
) %>%
  kableExtra::kable_paper("hover", full_width = F)


spatial.predictors <- spatialRF::get_spatial_predictors(model.spatial)
pr <- data.frame(spatial.predictors, data[, c("x", "y")])


p1 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = Benin, fill = "white") +
  ggplot2::geom_raster(
    data = pr,
    ggplot2::aes(
      x = x,
      y = y,
      fill = spatial_predictor_10000_12	,
    ),
    size = 2.5
  ) +
  ggplot2::scale_fill_viridis_c(option = "F") +
  ggplot2::theme_bw() +
  ggplot2::labs(color = "Eigenvalue") +
  #ggplot2::scale_x_continuous(limits = c(-170, -30)) +
  #ggplot2::scale_y_continuous(limits = c(-58, 80))  +
  ggplot2::ggtitle("Variable: spatial_predictor_10000_12	") + 
  ggplot2::theme(legend.position = "bottom") + 
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("")

p2 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = Benin, fill = "white") +
  ggplot2::geom_raster(
    data = pr,
    ggplot2::aes(
      x = x,
      y = y,
      fill = spatial_predictor_40000_12	,
    ),
    size = 2.5
  ) +
  ggplot2::scale_fill_viridis_c(option = "F") +
  ggplot2::theme_bw() +
  ggplot2::labs(color = "Eigenvalue") +
  #ggplot2::scale_x_continuous(limits = c(-170, -30)) +
  #ggplot2::scale_y_continuous(limits = c(-58, 80))  +
  ggplot2::ggtitle("Variable: spatial_predictor_40000_12	") + 
  ggplot2::theme(legend.position = "bottom") + 
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("")

p1 | p2

ggarrange(p1, p2, ncol = 2)

head(pr)



### Optimisation Plot

'The optimised variable selection is graphical shown as a path between variables it chooses and the Models $R^2$
'


p <- spatialRF::plot_optimization(model.spatial)



## Model Tuning
'
We can perform model tuning simiarily to before, however, it now runs with spatial cross-validtion, to maximise spatial predictive performance.

This is however HUGELY computationally expensive.
'


model.spatial.tuned <- rf_tuning(
  model = model.spatial,
  xy = xy,
  repetitions = 30,
  num.trees = c(500, 1000),
  mtry = seq(
    2, length(model.spatial$ranger.arguments$predictor.variable.names), by = 9),
  min.node.size = c(5, 15),
  seed = random.seed,
  verbose = FALSE,
  n.cores = 4
)



'We can see this models performance and the plots as above'



spatialRF::print_performance(model.spatial.tuned)


p1 <- spatialRF::plot_importance(
  model.non.spatial, 
  verbose = FALSE) + 
  ggplot2::ggtitle("Non-spatial model") 

p2 <- spatialRF::plot_importance(
  model.spatial,
  verbose = FALSE) + 
  ggplot2::ggtitle("Spatial model")

pe <- spatialRF::plot_importance(
  model.spatial.tuned,
  verbose = FALSE) + 
  ggplot2::ggtitle("Spatial model Tuned")

p1 | p2 | p3


kableExtra::kbl(
  head(model.spatial.tuned$importance$per.variable, n = 10),
  format = "html"
) %>%
  kableExtra::kable_paper("hover", full_width = F)


spatial.predictors <- spatialRF::get_spatial_predictors(model.spatial.tuned)
pr <- data.frame(spatial.predictors, data[, c("x", "y")])


p1 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = Benin, fill = "white") +
  ggplot2::geom_raster(
    data = pr,
    ggplot2::aes(
      x = x,
      y = y,
      fill = spatial_predictor_10000_12	,
    ),
    size = 2.5
  ) +
  ggplot2::scale_fill_viridis_c(option = "F") +
  ggplot2::theme_bw() +
  ggplot2::labs(color = "Eigenvalue") +
  #ggplot2::scale_x_continuous(limits = c(-170, -30)) +
  #ggplot2::scale_y_continuous(limits = c(-58, 80))  +
  ggplot2::ggtitle("Variable: spatial_predictor_10000_12	") + 
  ggplot2::theme(legend.position = "bottom") + 
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("")

p2 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = Benin, fill = "white") +
  ggplot2::geom_raster(
    data = pr,
    ggplot2::aes(
      x = x,
      y = y,
      fill = spatial_predictor_40000_12	,
    ),
    size = 2.5
  ) +
  ggplot2::scale_fill_viridis_c(option = "F") +
  ggplot2::theme_bw() +
  ggplot2::labs(color = "Eigenvalue") +
  #ggplot2::scale_x_continuous(limits = c(-170, -30)) +
  #ggplot2::scale_y_continuous(limits = c(-58, 80))  +
  ggplot2::ggtitle("Variable: spatial_predictor_40000_12	") + 
  ggplot2::theme(legend.position = "bottom") + 
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("")

p1 | p2

ggarrange(p1, p2, ncol = 2)

head(pr)



### Optimisation Plot

'The optimised variable selection is graphical shown as a path between variables it chooses and the Models $R^2$
'


p <- spatialRF::plot_optimization(model.spatial)





##Model Prediction

'We finally use our fitted and tuned spatial model to predict across the entire domain.'


 

data$Spatial.pred <- stats::predict(
                            object = model.spatial.tuned,
                            data = data, type = "response"
                        )$predictions

ggplot(Benin) +
    geom_sf() + coord_sf() + 
    geom_raster(data, mapping = aes(x = x, y = y, fill = Spatial.pred)) +
    scale_fill_viridis_c()


save(data, file = "Outputs/BEN_10km_school_count_data_with_model_pred2.RData")



## Model Comparison

'We can simply compare models using the `rf_compare` function

Here we compare the tuned non-spatial and spatial models'



comparison <- spatialRF::rf_compare(
  models = list(
    `Non-spatial` = model.non.spatial.tuned,
    `Spatial` = model.spatial.tuned
  ),
  xy = xy,
  repetitions = 30,
  training.fraction = 0.8,
  metrics = "r.squared",
  seed = random.seed
  )

