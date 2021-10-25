libPaths("C:/rLib")
library(rgdal)
library(raster)
library(caret)
library(tidyverse)

# set the temporary folder for raster package operations
rasterOptions(tmpdir = "D:/Shukhrat/rf/cache/temp")

img1 <- raster("/filtered_chm.tif")
img1 <- na.exclude(img1)
crs(img1) <- "+proj=utm +zone=55 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" # Setting  a projection

#Calculating topographic variabes
img_slope <- terrain(img1, opt="slope", unit = "degrees", neighbors = 8)
#img_aspect <- terrain(img1, opt="aspect", unit = "degrees", neighbors = 8)
img_TPI <- terrain(img1, opt="TPI", neighbors = 8) #Topographic position index
img_TRI <- terrain(img1, opt="TRI", neighbors = 8) #Topographic ruggedness index
img_rough <- terrain(img1, opt="roughness", neighbors = 8)
#img_flow <- terrain (img1, opt="flowdir", neighbors = 8)

img <- stack(x=c(img1, img_slope, img_TPI, img_TRI, img_rough)) #stacking the datasets

trainData <- shapefile("/uls_cwd_train1.shp") #importing a training data
trainData$class <- as.factor(trainData$class)
responseCol <- "class"

.rs.unloadPackage("tidyr")
dfAll = data.frame(matrix(vector(), nrow = 0, ncol = length(names(img)) + 1))  
for (i in 1:length(unique(trainData[[responseCol]]))){ #it might throw an error, in that case run .rs.unloadPackage("tidyr")
  category <- unique(trainData[[responseCol]])[i]
  categorymap <- trainData[trainData[[responseCol]] == category,]
  dataSet <- extract(img, categorymap)
  if(is(trainData, "SpatialPointsDataFrame")){
    dataSet <- cbind(dataSet, class = as.numeric(rep(category, nrow(dataSet))))
    dfAll <- rbind(dfAll, dataSet[complete.cases(dataSet),])
  }
  if(is(trainData, "SpatialPolygonsDataFrame")){
    dataSet <- dataSet[!unlist(lapply(dataSet, is.null))]
    dataSet <- lapply(dataSet, function(x){cbind(x, class = as.numeric(rep(category, nrow(x))))})
    df <- do.call("rbind", dataSet)
    dfAll <- rbind(dfAll, df)
  }
}

inBuild <- createDataPartition(y = dfAll$class, p = 0.7, list = FALSE)
training <- dfAll[inBuild,]
testing <- dfAll[-inBuild,]

table(training$class)

table(testing$class)


modFit_rf <- train(as.factor(class) ~ filtered_chm+roughness, method = "rf", data = training, importance=TRUE,
                   tuneGrid = data.frame(mtry=c(1,2)), na.action=na.exclude)

beginCluster()
preds_rf <- clusterR(img, raster::predict, args = list(model = modFit_rf))
endCluster()

writeRaster(preds_rf, "D:/Shukhrat/rf/uls/test2/morf_filter/classified_cwd_uls_chm_rough.tif")
#confusionMatrix(preds_rf$layer, as.factor(testing$class))
#classified <- focal(preds_rf, w=matrix(1,7,7), fun=modal)


testing2 <- testing[complete.cases(testing), ]
pred_test <- predict(modFit_rf, testing2)
confusionMatrix(pred_test, as.factor(testing2$class))$overall[1]
confusionMatrix(pred_test, as.factor(testing2$class))

varImp(modFit_rf)
round(randomForest::importance(modFit_rf$finalModel)/sum(randomForest::importance(modFit_rf$finalModel))*100,2)
saveRDS(modFit_rf, file = "/cwd_uls_model_chm_rough.rds")
