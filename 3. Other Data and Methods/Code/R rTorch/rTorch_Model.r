### rTorch
library(rTorch)
library(torch)
library(sf)

###https://ml.azure.com/environments/AzureML-ACPT-pytorch-1.13-py38-cuda11.7-gpu/version/10?wsid=/subscriptions/4d109c50-75d4-43d8-98c2-716cd17bbb97/resourceGroups/RS-UNI-OI-GIGA/providers/Microsoft.MachineLearningServices/workspaces/uni-giga-datascience&tid=77410195-14e1-4fb8-904b-ab1892023667


load("/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/ceara_school_count_data.RData")
head(school_data)

schools <- na.omit(st_drop_geometry(school_data))

head(schools)
schools <- schools[,-1]

samp_train <- sample(1, nrow(schools), ceiling(0.7*nrow(schools)))
schools_train <- schools[samp_train,]
schools_test <- schools[-samp_train,]

head(schools_train)
NUMPOINTS <- torch_tensor(schools_train[,1])

