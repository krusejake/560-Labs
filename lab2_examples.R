# Anything that appears after the symbol # is a comment and will not be executed.

#We can extend plots and anlyses to consider geographic coordinates 
#You will need to install the GISTools package at the first time
install.packages("GISTools") #it also automatically installs the dependencies ‘maptools’, ‘sp’, ‘rgeos’

setwd("C:\\Users\\student.SHC\\Downloads") # change to your own directory

library("GISTools")
data(georgia) #load a set of data from the installed package GISTools; contain objects of class "SpatialPolygonsDataFrame"
names(georgia) # check the column names
summary(georgia) #summarize some descriptive statistics for each attribute
names(georgia2) # check the column names
summary(georgia2) #summarize some descriptive statistics for each attribute

#The georgia data include outlines of counties in Georgia with a number of attributes relating to 
#the 1990 census including population (TotPop90)
#the percentage of the population that are rural (PctRural)
#that have a college degree (PctBach)
#that are elderly (PctEld)
#that are foreign born (PctFB)
#that are classed as being in poverty (PctPov)
#that are black (PctBlack)
#the median income of the county (MedInc).

## Write and Read a Shapefile output
install.packages("rgdal") 
library(rgdal)
writePolyShape(georgia,"georgia_wgs84.shp")
writePolyShape(georgia2,"georgia_nad83.shp")
#Read a shapefile from local directory
georgia_wgs84 <- readOGR("georgia_wgs84.shp")
plot(georgia_wgs84)
# The use of the earlier function readShapePoly() is deprecated and it is not being maintained. 


#plot geometry and add county name labels
par(mar=c(0,0,2,0))
plot(georgia, col="pink",bg="grey",border="blue") # draw the polygons 
title('Counties in Georgia') #add the title
Lat <- georgia$Latitude
Lon <- georgia$Longitud
Name <- georgia$Name
pl<- pointLabel(x=Lon,y=Lat,labels=Name,offset=0,cex=0.6) #add the county name labels
pts <- points(Lon,Lat,col="purple",pch=20) #add the centroids of counties
#"pch=optional number" see below link for more information
#https://www.statmethods.net/advgraphs/parameters.html

#A choropleth is a thematic map in which areas are shaded in proportion to their attributes.
#help(choropleth)
par(mar=c(0,0,0,0))
choropleth(georgia,georgia$MedInc) #default mapping
#Customize your shading stype with different number of classes and colors
#help(auto.shading)
MedInc.shading <- auto.shading(georgia$MedInc, n=7, cols = brewer.pal(n=7, "Blues")) #set a shading style
choropleth(georgia,georgia$MedInc,shading=MedInc.shading)
choro.legend(-81.8,35.0,MedInc.shading, cex=0.45,title="Household Median Income") #add a map legend for the choropleth map
#using the command "display.brewer.all()" to see all color schema 

## Build a global multiple linear regression model
# Model the determinants of educational attainment in the counties of the State of Georgia.
pairs(~MedInc+PctEld+PctFB+PctPov,data=georgia, main="Scatterplot Matrix") #pairwise scatter plot for multiple variables
multilinearMod <- lm(georgia$PctBach ~ georgia$MedInc+ georgia$PctEld + georgia$PctFB + georgia$PctPov, data=georgia) #build a MLR model
summary(multilinearMod) #summarize the MLR results
coefficents_lm <- coefficients(multilinearMod) #regression coefficients

# Calculate the Standardized Regression Coefficient 
coefficents_lm[1] #intercept
coefficents_lm[2] #MedInc 
std_coef_MedInc <- coefficents_lm[2] * sd(georgia$MedInc) / sd(georgia$PctBach)
print(std_coef_MedInc)

#the R-squared value of 0.5699 indicates that it accounts for about half the variation in the dependent variable. 
#This suggests that perhaps some variables have been omitted from the model, or the form of the model is not quite right

## Geographically Weighted Regression 
#You will need to install the GWmodel package at the first time
install.packages("GWmodel")

library(GWmodel)
help("gwr.basic")

# Compute the distances between the data points and the reference location i
DM <- gw.dist(dp.locat=coordinates(georgia),rp.locat=coordinates(georgia),p=2,longlat=TRUE)
help(gw.dist)

# Run the GWR model
# Automatic bandwidth selection to calibrate a basic GWR model
BW <- bw.gwr(PctBach ~ MedInc+PctEld+PctFB+PctPov, data=georgia, approach="CV",kernel="gaussian", adaptive=FALSE, p=2, theta=0, longlat=TRUE)

# Fixed Distance Kernel
gwr.res <- gwr.basic(PctBach ~ MedInc+PctEld+PctFB+PctPov, data=georgia, regression.points=georgia,kernel='gaussian', adaptive=FALSE, bw=BW,longlat=TRUE, cv=TRUE, dMat=DM)
gwr.res 

# Adaptive Kernel with fixed number of nearest neighbors
gwr.res <- gwr.basic(PctBach ~ MedInc+PctEld+PctFB+PctPov, data=georgia, regression.points=georgia,kernel='gaussian', adaptive=TRUE, bw=10)
gwr.res 
head(gwr.res$SDF)

#Result: a SpatialPointsDataFrame (may be gridded) integrated with fit.points,GWR coefficient estimates, y value,predicted values, coefficient standard errors and t-values in its "data" slot.
gwr.sdf <- gwr.res$SDF 
Shading <- auto.shading(gwr.sdf$Intercept, n=7, cols = brewer.pal(n=7, "Reds")) #set a shading style
par(mar=c(0,0,0,0))
choropleth(gwr.sdf, gwr.sdf$Intercept, shading = Shading) # create a chroploeth map
choro.legend(-81.8,35.0,Shading, cex=0.45,title="Intercept") # add a legend

Shading <- auto.shading(gwr.sdf$MedInc, n=7, cols = brewer.pal(n=7, "Blues")) #set a shading style
choropleth(gwr.sdf, gwr.sdf$MedInc, shading = Shading) # create a chroploeth map
choro.legend(-81.8,35.0,Shading, cex=0.45,title="MedInc") # add a legend

Shading <- auto.shading(gwr.sdf$PctFB, n=7, cols = brewer.pal(n=7, "Greens")) #set a shading style
choropleth(gwr.sdf, gwr.sdf$PctFB, shading = Shading) # create a chroploeth map
choro.legend(-81.8,35.0,Shading, cex=0.45,title="PctFB") # add a legend

## Goodness of Fit
fit_PctBach <- gwr.sdf$Intercept + 
              georgia$MedInc*gwr.sdf$MedInc + 
              georgia$PctEld*gwr.sdf$PctEld +
              georgia$PctFB*gwr.sdf$PctFB +
              georgia$PctPov*gwr.sdf$PctPov 

gwr.residuals <- georgia$PctBach - fit_PctBach # actual obervation - prediction value 
ESS <- sum(gwr.residuals*gwr.residuals) #error sum of squared
PctBach_var <- georgia$PctBach - mean(georgia$PctBach)
TSS <- sum(PctBach_var*PctBach_var)# total sum of squared variation
RSS <- TSS - ESS #get the regression sum of squared variation 
gwr.RSquared <- RSS/TSS
print(gwr.RSquared)
#Global LR: R-Squared 0.5699
#GWR: R-Squared 0.778455

# Root of Mean Squared Error (RMSE) or Regression Standard Error
# RMSE = Squared Root of the Regression Sum of Squared / Degree of Freedom
gwr.RMSE <- sqrt(RSS) / (length(georgia$PctBach) - length(coefficients(multilinearMod)))
print(gwr.RMSE)
#Global LR: Residual standard error: 3.784 on 154 degrees of freedom
#GWR: Residual standard error: 0.4102733 on 154 degrees of freedom

## Test whether local parameter/coefficent estimates show significant spatial variation
library(boot)
#help(set.seed)
set.seed(1000) #random number generation
gwrcoef <- function(hpdf,i) gwr.basic(PctBach ~ MedInc+PctEld+PctFB+PctPov, data=georgia[i,], regression.points=georgia,kernel='gaussian', adaptive=TRUE, bw=10)$SDF$MedInc
bootres <- boot(georgia,gwrcoef,100)
gwr.sdf$MedInc_std <- sqrt(apply(bootres$t,2,var))

par(mar=c(0,0,0,0))
Shading <- auto.shading(gwr.sdf$MedInc_std, n=7, cols = brewer.pal(n=7, "Reds")) #set a shading style
choropleth(gwr.sdf, gwr.sdf$MedInc_std, shading = Shading) # create a chroploeth map
choro.legend(-81.8,35.0,Shading, cex=0.45,title="sigTest") # add a legend

# boxplot for the comment on the spatial variability of each parameter estimate rom MC-simulation 
std_MedInc_coef <- sd(gwr.sdf$MedInc)
boxplot(gwr.sdf$MedInc_std, outline = TRUE)
points(1,std_MedInc_coef,col=4,pch=13)
# or look at the density plot
plot(density(gwr.sdf$MedInc_std), main="Monte Carlo Simulation", ylab="Frequency", xlab="Std of MedInc Coefficient")
polygon(density(gwr.sdf$MedInc_std), col="green")
abline(v=std_MedInc_coef, col="red")

## Test for the spatial variability of each parameter estimate in a similar manner
gwrcoef <- function(hpdf,i) gwr.basic(PctBach ~ MedInc+PctEld+PctFB+PctPov, data=georgia[i,], regression.points=georgia,kernel='gaussian', adaptive=TRUE, bw=10)$SDF$PctFB
bootres <- boot(georgia,gwrcoef,100)
gwr.sdf$PctFB_std <- sqrt(apply(bootres$t,2,var))
std_coef_PctFB <- sd(gwr.sdf$PctFB)
boxplot(gwr.sdf$PctFB_std, outline = TRUE)
points(1,std_coef_PctFB,col=4,pch=13)

## Test whether a local coefficient estimate is significant or not
##Generally, if a coefficient estimate is more than 2 standard errors away from zero, then it is "statistically significant".
##If this is greater than zero (i.e. the estimate is more than two standard errors away from zero), it is very unlikely that the true value is zero, i.e. it is statistically significantly (at nearly the 95% confidence level)
# gwr.sdf$MedInc_se <- gwr.RMSE / sqrt(sum((georgia$MedInc-mean(georgia$MedInc)*(georgia$MedInc-mean(georgia$MedInc)))))
# gwr.sdf$sigTest = abs(gwr.sdf$MedInc) -2 * gwr.sdf$MedInc_se 
# par(mar=c(0,0,0,0))
# Shading <- auto.shading(gwr.sdf$sigTest, n=7, cols = brewer.pal(n=7, "Reds")) #set a shading style
# choropleth(gwr.sdf, gwr.sdf$sigTest, shading = Shading) # create a chroploeth map
# choro.legend(-81.8,35.0,Shading, cex=0.45,title="sigTest") # add a legend

## Another Example using Grid reference points
library(GWmodel)
data(LondonHP) #load London Housing Price data
data(LondonBorough) #load boundaryf data 
head(data.frame(londonhp))
help(londonhp) # understand the attributes in the data
head(data.frame(londonborough)) #explore the first lines of data
par(mar=c(0,0,0,0))
plot(londonborough,col=adjustcolor('navyblue',alpha.f=0.5)) #draw the geographic boundary
plot(londonhp, pch=16,cex=0.6, col='firebrick',add=TRUE) #overlay points
summary(londonborough)
londonborough.bbox <- bbox(londonborough)
xCoordRange <- (londonborough.bbox[3]-londonborough.bbox[1])
xoffset <- londonborough.bbox[1]+xCoordRange/20
yCoordRange <- (londonborough.bbox[4]-londonborough.bbox[2])
yoffset <- londonborough.bbox[2]+yCoordRange/8
map.scale(x=xoffset, y=yoffset, len=10000, units="km",ndivs=4)  
north.arrow(xb=xoffset, yb=yoffset+yCoordRange/10, len=miles2ft(0.2), lab="N")  

## Build a global and local MLR model between 
# PURCHASE:the purchase price of the property (Independent Variable)
# FLOORSZ: floor area of the property in square metres
# UNEMPLOY: the rate of unemployment in the census ward in which the house is located
# PROF: the proportion of the workforce in professional or managerial occupations in the census ward in which the house is located

## Add your global MLR code here
###################################

####################################


## Calibrate the GWR model over a regular grid of observations. 
par(mar=c(0,0,0,0))
plot(londonborough,col=adjustcolor('navyblue',alpha.f=0.5)) #draw the geographic boundary
plot(londonhp, pch=16,cex=0.6, col='firebrick',add=TRUE) #overlay points
# The 1000m-1000m grid is now created:
grd <- SpatialGrid(GridTopology(c(503400,155400),c(1000,1000),c(60,48)))
plot(grd,add=TRUE) # add regular grid on the map

#Next, compute the distances between the points on the grid where parameters ak(u,v) will be estimated, 
#and the points where the house prices are observed.
DM <- gw.dist(dp.locat=coordinates(londonhp),rp.locat=coordinates(grd))

## Add your local GWR code here
###################################


####################################


## Other Online Resources
# Another GWR package https://cran.r-project.org/web/packages/spgwr/spgwr.pdf
# Another Housing Price Example in California http://www.rspatial.org/analysis/rst/7-spregression.html
