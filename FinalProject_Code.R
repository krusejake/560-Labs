library(sp)
library(rgdal)
library(raster)
library(randomForest)
library(sf)
library(rdataviewer)
library(RStoolbox)
library("randomForest")
library(graphics)
library(graphics)
library(caret)
library(reprtree)

setwd("/Users/kruse/OneDrive/Documents/Spring2019/560/FinalProject/FinalProjectCode/FinalProjectCode")

# Some helpful links:

#https://www.r-bloggers.com/how-to-implement-random-forests-in-r/ ##classification accuracy
#https://gis.stackexchange.com/questions/39021/how-to-perform-random-forest-land-cover-classification/39103
#Basics of working with rastr data
#https://www.neonscience.org/raster-data-r
# http://bleutner.github.io/RStoolbox/rstbx-docu/RStoolbox.html

#Read original raster files
r1 <- raster("cons_raster_2.tif")
r2 <- raster("disdam2clip2.tif")
r3 <- raster("disrivers3.tif")
r4 <- raster("hdi_raster.tif")
r5 <- raster("pop_raster.tif")
r6 <- raster("prrast56.tif")
r7 <- raster("rddisclip.tif")

#Reproject r3 and r6, as the projection for those files didn't match the other files
r3_wgs84 <- projectRaster(r3, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
r6_wgs84 <- projectRaster(r6, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#Resample all to same extent so that they intersect
r.new2 = resample(r2, r1, method="ngb")
r.new3 = resample(r3_wgs84, r1, method="ngb")
r.new4 = resample(r4, r1, method="ngb")
r.new5 = resample(r5, r1, method="ngb")
r.new6 = resample(r6_wgs84, r1, method="ngb")
r.new7 = resample(r7, r1, method="ngb")

#Define resampled files as raster files
raster(r.new1)
raster(r.new2)
raster(r.new3)
raster(r.new4)
raster(r.new5)
raster(r.new6)
raster(r.new7)

#Write new files and use these resampled/reprojected files for the model below
writeRaster(r1, filename="Conservation.tif", options="INTERLEAVE=BAND", overwrite=TRUE)
writeRaster(r.new2, filename="DistoDams.tif", options="INTERLEAVE=BAND", overwrite=TRUE)
writeRaster(r.new3, filename="DisttoRivers.tif", options="INTERLEAVE=BAND", overwrite=TRUE)
writeRaster(r.new4, filename="HDI.tif", options="INTERLEAVE=BAND", overwrite=TRUE)
writeRaster(r.new5, filename="Pop.tif", options="INTERLEAVE=BAND", overwrite=TRUE)
writeRaster(r.new6, filename="LandUse.tif", options="INTERLEAVE=BAND", overwrite=TRUE)
writeRaster(r.new7, filename="DistoRoads.tif", options="INTERLEAVE=BAND", overwrite=TRUE)
writeRaster(xvars, filename="xvars.tif", options="INTERLEAVE=BAND", overwrite=TRUE)


#Now just need to load these since they've been resampled to extent, CRS's match
Conservation <- raster("Conservation.tif")
DistoDams <- raster("DistoDams.tif")
DisttoRivers <- raster("DisttoRivers.tif")
HDI <- raster("HDI.tif")
Pop <- raster("Pop.tif")
LandUse <- raster("LandUse.tif")
DistoRoads <- raster("DistoRoads.tif")

#Make the raster stack
xvars <- stack(Conservation,DistoDams,DisttoRivers,HDI,Pop,LandUse,DistoRoads)

xvars
plot(xvars)
summary(LandUse)
head(LandUse)
head(LandUse, n=100)
print(LandUse)
tail(LandUse)
View(LandUse)

#Read in testing points (shp file), then extract the relevant raster points fromthe stack and write them into a data frame
testing_data <- readOGR(".", "1k_randos")
w <- as.data.frame(extract(xvars,testing_data))
testing_data@data = data.frame(testing_data@data, w[match(rownames(testing_data@data), rownames(w)),])
summary(testing_data)
head(testing_data)


#Same for training data
sdata <- readOGR(".", "500randompoints")
v <- as.data.frame(extract(xvars, sdata)) #sets CRS = to raster CRS
sdata@data = data.frame(sdata@data, v[match(rownames(sdata@data), rownames(v)),])
head(sdata@data)
barplot(sdata@data$LandUse)

#Some ways to make sure training and testing points were read in correctly.  
plot(testing_data, main = "Testing Data")
plot(sdata, main = "Training Data")
head(sdata)
summary(sdata)
barplot(table(sdata@data$LandUse))
  title("Land Type Frequency in Testing Data")  
testing_data@data$LandUse<- as.character(testing_data@data$LandUse)
testing_data@data$LandUse <- as.factor(testing_data@data$LandUse)
sdata@data$LandUse<- as.character(sdata@data$LandUse)
sdata@data$LandUse <- as.factor(sdata@data$LandUse)

# RUN RF MODEL
#na.action = na.omit
rf.mdl <- randomForest(LandUse ~ .-CID, data=sdata@data, ntree=500, importance=TRUE, na.action = na.omit)

#^Peaks around 100 trees

rf.mdl <- randomForest(LandUse ~ .-CID, data=sdata@data, ntree=100, mtry=2,importance=TRUE, na.action = na.omit)
rf.mdl

# Checking classification accuracy
mean(rf_pred == testing_data$LandUse)
d <- table(rf_pred, testing_data@data$LandUse)
sum(diag(d))/sum(d) #overall accuracy
1-sum(diag(d))/sum(d) #incorrect classification 

# Checking classification accuracy
importance(rf.mdl)
par( mfrow = c(1,1) )
plot(rf.mdl)
plot(rf_pred)
varImpPlot(rf.mdl, main = "RF Variable Importance Plot", type=1)
varImpPlot(rf_tree, type=1)
regression_tree_pred
summary(rf_pred)



