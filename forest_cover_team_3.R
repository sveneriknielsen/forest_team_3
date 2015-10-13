# see: https://archive.ics.uci.edu/ml/datasets/Covertype

# Required packages ####
require(lattice)
require(latticeExtra)
require(useful)
require(cluster)
require(dplyr)
require(tree)
require(ggplot2)
require(PerformanceAnalytics)
require(corrplot)
require(caret)
require(randomForest)

# Data load and prep ####

#Import data
forest.cover.data.source <- choose.files()
forest.cover.data <- read.table(forest.cover.data.source, header=TRUE, stringsAsFactors=F, sep=",")

#forest.cover.data <- slice(forest.cover.data,1:100) # subset code for debugging

#assign column names per data dictionary
names(forest.cover.data) <- c("Elevation", "Aspect", "Slope", "Horizontal_Distance_To_Hydrology", "Vertical_Distance_To_Hydrology",
                              "Horizontal_Distance_To_Roadways", "Hillshade_9am", "Hillshade_Noon", "Hillshade_3pm", "Horizontal_Distance_To_Fire_Points",
                              "Rawah_WA", "Neota_WA", "Comanche_Peak_WA", "Cache_la_Poudre_WA", "Soil_Type_1",
                              "Soil_Type_2","Soil_Type_3", "Soil_Type_4", "Soil_Type_5", "Soil_Type_6",
                              "Soil_Type_7", "Soil_Type_8", "Soil_Type_9", "Soil_Type_10", "Soil_Type_11",
                              "Soil_Type_12", "Soil_Type_13", "Soil_Type_14", "Soil_Type_15", "Soil_Type_16",
                              "Soil_Type_17", "Soil_Type_18", "Soil_Type_19", "Soil_Type_20", "Soil_Type_21",
                              "Soil_Type_22", "Soil_Type_23", "Soil_Type_24", "Soil_Type_25", "Soil_Type_26",
                              "Soil_Type_27", "Soil_Type_28", "Soil_Type_29", "Soil_Type_30", "Soil_Type_31",
                              "Soil_Type_32", "Soil_Type_33", "Soil_Type_34", "Soil_Type_35", "Soil_Type_36",
                              "Soil_Type_37", "Soil_Type_38", "Soil_Type_39", "Soil_Type_40", "Cover_Type")

# Convert response variable Cover_Type as a factor
forest.cover.data$Cover_Type_Factor <- factor(forest.cover.data$Cover_Type,levels=c(1,2,3,4,5,6,7),labels=c("Spruce_Fir","Lodgepole_Pine","Ponderosa_Pine","Cottonwood_Willow","Aspen","Douglas-fir","Krummholz"))


# Create dummy variables for Cover_Type
# Note that we may want to just call model.matrix() directly as needed by modeling functions
# Extract cover_type dummy vars from Cover_Type factor
cover.dummy.vars <- model.matrix(~Cover_Type_Factor, data = forest.cover.data)[,-1]
colnames(cover.dummy.vars) <- gsub("Factor","",colnames(cover.dummy.vars))
forest.cover.data <- cbind(forest.cover.data,cover.dummy.vars)
rm(cover.dummy.vars)

# Create single factor for wilderness_area
forest.cover.data$wilderness_area <- factor(as.matrix(forest.cover.data[,11:14])%*%1:4, labels = colnames(forest.cover.data[11:14]))

# Create dummy variables for wilderness_area
WA.dummy.vars <- model.matrix(~wilderness_area, data = forest.cover.data)[,-1]
colnames(WA.dummy.vars) <- gsub("Wilderness_area","",colnames(WA.dummy.vars))
forest.cover.data <- cbind(forest.cover.data,WA.dummy.vars)
rm(WA.dummy.vars)

# remove variables that have been converted into factors (wilderness areas & cover type)
forest.cover.data <- select(forest.cover.data, -c(Rawah_WA,Neota_WA,Comanche_Peak_WA,Cache_la_Poudre_WA,Cover_Type)) 


# Create variables for climate_zone.  See for detail: https://archive.ics.uci.edu/ml/machine-learning-databases/covtype/covtype.info ####
forest.cover.data$climate_zone <- ifelse((forest.cover.data$Soil_Type_1==1),2,
                                         ifelse((forest.cover.data$Soil_Type_2==1),2,
                                                ifelse((forest.cover.data$Soil_Type_3==1),2,
                                                       ifelse((forest.cover.data$Soil_Type_4==1),2,
                                                              ifelse((forest.cover.data$Soil_Type_5==1),2,
                                                                     ifelse((forest.cover.data$Soil_Type_6==1),2,
                                                                            ifelse((forest.cover.data$Soil_Type_7==1),3,
                                                                                   ifelse((forest.cover.data$Soil_Type_8==1),3,
                                                                                          ifelse((forest.cover.data$Soil_Type_9==1),4,
                                                                                                 ifelse((forest.cover.data$Soil_Type_10==1),4,
                                                                                                        ifelse((forest.cover.data$Soil_Type_11==1),4,
                                                                                                               ifelse((forest.cover.data$Soil_Type_12==1),4,
                                                                                                                      ifelse((forest.cover.data$Soil_Type_13==1),4,
                                                                                                                             ifelse((forest.cover.data$Soil_Type_14==1),5,
                                                                                                                                    ifelse((forest.cover.data$Soil_Type_15==1),5,
                                                                                                                                           ifelse((forest.cover.data$Soil_Type_16==1),6,
                                                                                                                                                  ifelse((forest.cover.data$Soil_Type_17==1),6,
                                                                                                                                                         ifelse((forest.cover.data$Soil_Type_18==1),6,
                                                                                                                                                                ifelse((forest.cover.data$Soil_Type_19==1),7,
                                                                                                                                                                       ifelse((forest.cover.data$Soil_Type_20==1),7,
                                                                                                                                                                              ifelse((forest.cover.data$Soil_Type_21==1),7,
                                                                                                                                                                                     ifelse((forest.cover.data$Soil_Type_22==1),7,
                                                                                                                                                                                            ifelse((forest.cover.data$Soil_Type_23==1),7,
                                                                                                                                                                                                   ifelse((forest.cover.data$Soil_Type_24==1),7,
                                                                                                                                                                                                          ifelse((forest.cover.data$Soil_Type_25==1),7,
                                                                                                                                                                                                                 ifelse((forest.cover.data$Soil_Type_26==1),7,
                                                                                                                                                                                                                        ifelse((forest.cover.data$Soil_Type_27==1),7,
                                                                                                                                                                                                                               ifelse((forest.cover.data$Soil_Type_28==1),7,
                                                                                                                                                                                                                                      ifelse((forest.cover.data$Soil_Type_29==1),7,
                                                                                                                                                                                                                                             ifelse((forest.cover.data$Soil_Type_30==1),7,
                                                                                                                                                                                                                                                    ifelse((forest.cover.data$Soil_Type_31==1),7,
                                                                                                                                                                                                                                                           ifelse((forest.cover.data$Soil_Type_32==1),7,
                                                                                                                                                                                                                                                                  ifelse((forest.cover.data$Soil_Type_33==1),7,
                                                                                                                                                                                                                                                                         ifelse((forest.cover.data$Soil_Type_34==1),7,
                                                                                                                                                                                                                                                                                ifelse((forest.cover.data$Soil_Type_35==1),8,
                                                                                                                                                                                                                                                                                       ifelse((forest.cover.data$Soil_Type_36==1),8,
                                                                                                                                                                                                                                                                                              ifelse((forest.cover.data$Soil_Type_37==1),8,
                                                                                                                                                                                                                                                                                                     ifelse((forest.cover.data$Soil_Type_38==1),8,
                                                                                                                                                                                                                                                                                                            ifelse((forest.cover.data$Soil_Type_39==1),8,
                                                                                                                                                                                                                                                                                                                   ifelse((forest.cover.data$Soil_Type_40==1),8,0))))))))))))))))))))))))))))))))))))))))


forest.cover.data$climate_zone <- factor(forest.cover.data$climate_zone,levels=c(2,3,4,5,6,7,8,0),labels=c("lower_mountain", "mountain_dry", "mountain", "montain_dry_and_mountain", "mountain_and_subalpine", "subalpine","alpine","missing"))               

climate.dummy.vars <- model.matrix(~climate_zone, data = forest.cover.data)[,-1]
colnames(climate.dummy.vars) <- gsub("climate_zone","",colnames(climate.dummy.vars))
forest.cover.data <- cbind(forest.cover.data,climate.dummy.vars)
rm(climate.dummy.vars)

# create variables for geologic zone
forest.cover.data$geologic_zone <- ifelse((forest.cover.data$Soil_Type_1==1),2,
                                          ifelse((forest.cover.data$Soil_Type_2==1),7,
                                                 ifelse((forest.cover.data$Soil_Type_3==1),7,
                                                        ifelse((forest.cover.data$Soil_Type_4==1),7,
                                                               ifelse((forest.cover.data$Soil_Type_5==1),7,
                                                                      ifelse((forest.cover.data$Soil_Type_6==1),7,
                                                                             ifelse((forest.cover.data$Soil_Type_7==1),5,
                                                                                    ifelse((forest.cover.data$Soil_Type_8==1),5,
                                                                                           ifelse((forest.cover.data$Soil_Type_9==1),2,
                                                                                                  ifelse((forest.cover.data$Soil_Type_10==1),7,
                                                                                                         ifelse((forest.cover.data$Soil_Type_11==1),7,
                                                                                                                ifelse((forest.cover.data$Soil_Type_12==1),7,
                                                                                                                       ifelse((forest.cover.data$Soil_Type_13==1),7,
                                                                                                                              ifelse((forest.cover.data$Soil_Type_14==1),1,
                                                                                                                                     ifelse((forest.cover.data$Soil_Type_15==1),1,
                                                                                                                                            ifelse((forest.cover.data$Soil_Type_16==1),1,
                                                                                                                                                   ifelse((forest.cover.data$Soil_Type_17==1),1,
                                                                                                                                                          ifelse((forest.cover.data$Soil_Type_18==1),7,
                                                                                                                                                                 ifelse((forest.cover.data$Soil_Type_19==1),1,
                                                                                                                                                                        ifelse((forest.cover.data$Soil_Type_20==1),1,
                                                                                                                                                                               ifelse((forest.cover.data$Soil_Type_21==1),1,
                                                                                                                                                                                      ifelse((forest.cover.data$Soil_Type_22==1),2,
                                                                                                                                                                                             ifelse((forest.cover.data$Soil_Type_23==1),2,
                                                                                                                                                                                                    ifelse((forest.cover.data$Soil_Type_24==1),7,
                                                                                                                                                                                                           ifelse((forest.cover.data$Soil_Type_25==1),7,
                                                                                                                                                                                                                  ifelse((forest.cover.data$Soil_Type_26==1),7,
                                                                                                                                                                                                                         ifelse((forest.cover.data$Soil_Type_27==1),7,
                                                                                                                                                                                                                                ifelse((forest.cover.data$Soil_Type_28==1),7,
                                                                                                                                                                                                                                       ifelse((forest.cover.data$Soil_Type_29==1),7,
                                                                                                                                                                                                                                              ifelse((forest.cover.data$Soil_Type_30==1),7,
                                                                                                                                                                                                                                                     ifelse((forest.cover.data$Soil_Type_31==1),7,
                                                                                                                                                                                                                                                            ifelse((forest.cover.data$Soil_Type_32==1),7,
                                                                                                                                                                                                                                                                   ifelse((forest.cover.data$Soil_Type_33==1),7,
                                                                                                                                                                                                                                                                          ifelse((forest.cover.data$Soil_Type_34==1),7,
                                                                                                                                                                                                                                                                                 ifelse((forest.cover.data$Soil_Type_35==1),7,
                                                                                                                                                                                                                                                                                        ifelse((forest.cover.data$Soil_Type_36==1),7,
                                                                                                                                                                                                                                                                                               ifelse((forest.cover.data$Soil_Type_37==1),7,
                                                                                                                                                                                                                                                                                                      ifelse((forest.cover.data$Soil_Type_38==1),7,
                                                                                                                                                                                                                                                                                                             ifelse((forest.cover.data$Soil_Type_39==1),7,
                                                                                                                                                                                                                                                                                                                    ifelse((forest.cover.data$Soil_Type_40==1),7,0))))))))))))))))))))))))))))))))))))))))
#Create factor for geologic_zone
forest.cover.data$geologic_zone <- factor(forest.cover.data$geologic_zone,levels=c(1,2,5,7,0),labels=c("alluvium", "glacial", "mixed_sedimentary", "igneous_and_metamorphic", "missing")) 

#Create dummy variables for geologic_zone
geologic.dummy.vars <- model.matrix(~geologic_zone, data = forest.cover.data)[,-1]
colnames(geologic.dummy.vars) <- gsub("geologic_zone","",colnames(geologic.dummy.vars))
forest.cover.data <- cbind(forest.cover.data,geologic.dummy.vars)
rm(geologic.dummy.vars)

forest.cover.data$Soil_Type <- ifelse((forest.cover.data$Soil_Type_1==1),1,
                                      ifelse((forest.cover.data$Soil_Type_2==1),2,
                                             ifelse((forest.cover.data$Soil_Type_3==1),3,
                                                    ifelse((forest.cover.data$Soil_Type_4==1),4,
                                                           ifelse((forest.cover.data$Soil_Type_5==1),5,
                                                                  ifelse((forest.cover.data$Soil_Type_6==1),6,
                                                                         ifelse((forest.cover.data$Soil_Type_7==1),7,
                                                                                ifelse((forest.cover.data$Soil_Type_8==1),8,
                                                                                       ifelse((forest.cover.data$Soil_Type_9==1),9,
                                                                                              ifelse((forest.cover.data$Soil_Type_10==1),10,
                                                                                                     ifelse((forest.cover.data$Soil_Type_11==1),11,
                                                                                                            ifelse((forest.cover.data$Soil_Type_12==1),12,
                                                                                                                   ifelse((forest.cover.data$Soil_Type_13==1),13,
                                                                                                                          ifelse((forest.cover.data$Soil_Type_14==1),14,
                                                                                                                                 ifelse((forest.cover.data$Soil_Type_15==1),15,
                                                                                                                                        ifelse((forest.cover.data$Soil_Type_16==1),16,
                                                                                                                                               ifelse((forest.cover.data$Soil_Type_17==1),17,
                                                                                                                                                      ifelse((forest.cover.data$Soil_Type_18==1),18,
                                                                                                                                                             ifelse((forest.cover.data$Soil_Type_19==1),19,
                                                                                                                                                                    ifelse((forest.cover.data$Soil_Type_20==1),20,
                                                                                                                                                                           ifelse((forest.cover.data$Soil_Type_21==1),21,
                                                                                                                                                                                  ifelse((forest.cover.data$Soil_Type_22==1),22,
                                                                                                                                                                                         ifelse((forest.cover.data$Soil_Type_23==1),23,
                                                                                                                                                                                                ifelse((forest.cover.data$Soil_Type_24==1),24,
                                                                                                                                                                                                       ifelse((forest.cover.data$Soil_Type_25==1),25,
                                                                                                                                                                                                              ifelse((forest.cover.data$Soil_Type_26==1),26,
                                                                                                                                                                                                                     ifelse((forest.cover.data$Soil_Type_27==1),27,
                                                                                                                                                                                                                            ifelse((forest.cover.data$Soil_Type_28==1),28,
                                                                                                                                                                                                                                   ifelse((forest.cover.data$Soil_Type_29==1),29,
                                                                                                                                                                                                                                          ifelse((forest.cover.data$Soil_Type_30==1),30,
                                                                                                                                                                                                                                                 ifelse((forest.cover.data$Soil_Type_31==1),31,
                                                                                                                                                                                                                                                        ifelse((forest.cover.data$Soil_Type_32==1),32,
                                                                                                                                                                                                                                                               ifelse((forest.cover.data$Soil_Type_33==1),33,
                                                                                                                                                                                                                                                                      ifelse((forest.cover.data$Soil_Type_34==1),34,
                                                                                                                                                                                                                                                                             ifelse((forest.cover.data$Soil_Type_35==1),35,
                                                                                                                                                                                                                                                                                    ifelse((forest.cover.data$Soil_Type_36==1),36,
                                                                                                                                                                                                                                                                                           ifelse((forest.cover.data$Soil_Type_37==1),37,
                                                                                                                                                                                                                                                                                                  ifelse((forest.cover.data$Soil_Type_38==1),38,
                                                                                                                                                                                                                                                                                                         ifelse((forest.cover.data$Soil_Type_39==1),39,
                                                                                                                                                                                                                                                                                                                ifelse((forest.cover.data$Soil_Type_40==1),40,0))))))))))))))))))))))))))))))))))))))))

forest.cover.data$Soil_Type<-factor(forest.cover.data$Soil_Type,levels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,0),labels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","missing")) # convert to factor

# forest.cover.data <- select(forest.cover.data, -c(Soil_Type_1,Soil_Type_2,Soil_Type_3,Soil_Type_4,Soil_Type_5,Soil_Type_6,
#                                                   Soil_Type_7,Soil_Type_8,Soil_Type_9,Soil_Type_10,Soil_Type_11,Soil_Type_12,
#                                                   Soil_Type_13,Soil_Type_14,Soil_Type_15,Soil_Type_16,Soil_Type_17,Soil_Type_18,
#                                                   Soil_Type_19,Soil_Type_20,Soil_Type_21,Soil_Type_22,Soil_Type_23,Soil_Type_24,
#                                                   Soil_Type_25,Soil_Type_26,Soil_Type_27,Soil_Type_28,Soil_Type_29,Soil_Type_30,
#                                                   Soil_Type_31,Soil_Type_32,Soil_Type_33,Soil_Type_34,Soil_Type_35,Soil_Type_36,
#                                                   Soil_Type_37,Soil_Type_38,Soil_Type_39,Soil_Type_40)) #remove variables that have been converted into factors

# Train/Test Split ####
# if necessary due to computational limitations
# sample the training data set to perform EDA
#  dataset = 581,011 observations. the following script takes several hours to run on the full dataset. 
# 16,127 sample size based on 99% confidence interval, 1% error margin 
# See: http://www.raosoft.com/samplesize.html
#forest.cover.data <- forest.cover.data[sample(nrow(forest.cover.data), 16127, replace = FALSE, prob = NULL),]

# split data into training set and test set
set.seed(465)
train=(sample(1:nrow(forest.cover.data),nrow(forest.cover.data)*0.03))

#remove training set variables where all observations have same value. (note: necessary when indicator variables are used?)
for (i in colnames(forest.cover.data[train,])) {
  ifelse(n_distinct(forest.cover.data[train,i])==1,forest.cover.data <- subset(forest.cover.data, select = -c(get(i))),forest.cover.data[train,] <- (forest.cover.data[train,]))
}

# Diagnostics
class(forest.cover.data[train,])
dim(forest.cover.data[train,])
# View(forest.cover.data[train,])
names(forest.cover.data[train,])
str(forest.cover.data[train,])
colSums(-is.na(forest.cover.data[train,])) #missing observations?
sum(colSums(-is.na(forest.cover.data[train,]))) #total missing observations

## BEGIN UNIVARIATE EDA ####

summary(forest.cover.data[train,]) #invalid observations? (negatives, etc.)

#  EDA for continuous variables
for (i in colnames(forest.cover.data[train,1:10])) {
  par(mfrow=c(1,1))
  #scatter.smooth(forest.cover.data[train,i],col=rgb(0, 0, 1, 0.2),las=1,xlab="Observation",main=i)
  #print(i)
  #print(xtabs(~Cover_Type+get(i), data=forest.cover.data[train,]))
  plot(bwplot(get(i)~Cover_Type_Factor, data = forest.cover.data,
              ylab = i, scales=list(x=list(rot=45)),main=i))
  #boxplot(forest.cover.data[train,i], main="Boxplot",main=i)
  qqnorm(forest.cover.data[train,i], main="QQ Plot")
  qqline(forest.cover.data[train,i])
  plot(densityplot(~get(i), data=forest.cover.data[train,],
                   groups=Cover_Type_Factor,
                   xlab=i,
                   plot.points=TRUE,
                   auto.key=TRUE))
  plot(bwplot(Cover_Type_Factor~get(i), data=forest.cover.data[train,], panel=panel.violin, main=i,xlab=i))
}

#  EDA for factor variables
for (i in colnames(forest.cover.data[sapply(forest.cover.data[train,],(is.factor))])) {
  par(mfrow=c(1,1))
  plot(forest.cover.data[train,i],col=rgb(0, 0, 1, 0.2),las=2,main=i) # scatter.smooth() is available for continuous variables
  print(table(forest.cover.data[train,i],useNA=c('always'))) # include NA to find missing values
}

for (i in colnames(forest.cover.data[sapply(forest.cover.data[train,],(is.factor))])) {
  for (j in colnames(forest.cover.data[train,1:10])) {
    par(mfrow=c(4,3))
    print(ggplot(forest.cover.data[train,], aes(x=get(i),y=get(j))) + geom_point(aes(color=Cover_Type_Factor)) + labs(x=i,y=j))
  }} # this loop is overkill, but it provides a complete picture. 



## BEGIN MULTIVARIATE EDA  ####

# Create data subset with no factor variables. (e.g present qualitative variables 
# as indicator variables instead of factor variables
forest.cover.data1 <- select(forest.cover.data, -c(Cover_Type_Factor, 
                                                   wilderness_area, climate_zone, geologic_zone, 
                                                   Soil_Type))

# correlation matrix plot ####
z <- cor(forest.cover.data1[train,])
corrplot(z)
levelplot(z,col.regions = gray(0:100/100),scales=list(x=list(rot=45)))
chart.Correlation(z)

#bivariate scatterplot dissected by Cover_Type. local regression (LOESS) line included.
#a <- xyplot(Hillshade_Noon ~ Hillshade_3pm | cut(Cover_Type, breaks=c(0,1,2,3,4,5,6)), 
#            data = forest.cover.data[train,], main="Cover_Type", type='smooth')
#b <- xyplot(Hillshade_Noon ~ Hillshade_3pm | cut(Cover_Type, breaks=c(0,1,2,3,4,5,6)), 
#            data = forest.cover.data[train,], main="Cover_Type")
#print(a+as.layer(b))


## PCA analysis ####
# #remove training set variables where all observations have same value. (note: necessary when indicator variables are used?)
for (i in colnames(forest.cover.data1[train,])) {
  ifelse(n_distinct(forest.cover.data1[train,i])==1,forest.cover.data1 <- subset(forest.cover.data1[train,], select = -c(get(i))),forest.cover.data1[train,] <- (forest.cover.data1[train,]))
}

#principal components
princomps<- function(data){
  pr.out=prcomp(data, scale=TRUE)
  names(pr.out)
  pr.out$center
  pr.out$scale
  pr.out$rotation
  dim(pr.out$x)
  biplot(pr.out, scale=0)
  pr.out$rotation=-pr.out$rotation
  pr.out$x=-pr.out$x
  biplot(pr.out, scale=0)
  pr.out$sdev
  pr.var=pr.out$sdev^2
  pr.var
  pve=pr.var/sum(pr.var)
  pve
  plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", main="Scree Plot",ylim=c(0,1),type='b')
  plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained",main="Cumulative Variance", ylim=c(0,1),type='b')
  a=c(1,2,8,-3)
  cumsum(a)
}

par(mfrow=c(1,2))
princomps(forest.cover.data1[train,])
par(mfrow=c(1,1))


## Cluster Analysis ####
# 
# # Cluster analysis - Hartigan's Rule (R for everyone 342)
# cluster_best<- FitKMeans(forest.cover.data1[train,],max.clusters=15,nstart=25,seed=465)
# PlotHartigan(cluster_best)
# 
# # Cluster analysis - Gap statistic (R for everyone 344)
# theGap <- clusGap(forest.cover.data1[train,], FUNcluster=pam, K.max=20)
# gapDF <- as.data.frame(theGap$Tab)
# gapDF
# 
# # logW curves
# ggplot(gapDF,aes(x=1:nrow(gapDF))) +
#   geom_line(aes(y=logW), color="blue") +
#   geom_point(aes(y=logW), color="blue") +
#   geom_line(aes(y=E.logW), color="green") +
#   geom_point(aes(y=E.logW), color="green") +
#   labs(x="Number of Clusters")
# 
# # gap curve
# ggplot(gapDF, aes(x=1:nrow(gapDF)))+
#   geom_line(aes(y=gap), color="red") +
#   geom_point(aes(y=gap), color="red") +
#   geom_errorbar(aes(ymin=gap-SE.sim, ymax=gap+SE.sim), color="red") +
#   labs(x="Number of Clusters", y="Gap")
# 
# # K-Means Clustering
# set.seed(2)
# km.out=kmeans(forest.cover.data1[train,],centers = 5,nstart=20)
# km.out$cluster
# plot(forest.cover.data1[train,], col=(km.out$cluster+1), main="K-Means Clustering Results with K=5", pch=20, cex=2)
# 
# # Hierarchical Clustering
# hc.complete=hclust(dist(forest.cover.data1[train,]), method="complete")
# hc.average=hclust(dist(forest.cover.data1[train,]), method="average")
# hc.single=hclust(dist(forest.cover.data1[train,]), method="single")
# par(mfrow=c(1,1))
# plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex=.9)
# plot(hc.average, main="Average Linkage", xlab="", sub="", cex=.9)
# plot(hc.single, main="Single Linkage", xlab="", sub="", cex=.9)
# cutree(hc.complete, 2)
# cutree(hc.average, 2)
# cutree(hc.single, 2)
# cutree(hc.single, 4)
# xsc=scale(forest.cover.data1[train,])
# plot(hclust(dist(xsc), method="complete"), main="Hierarchical Clustering with Scaled Features")
# x=matrix(rnorm(30*3), ncol=3)
# dd=as.dist(1-cor(t(forest.cover.data1[train,])))
# plot(hclust(dd, method="complete"), main="Complete Linkage with Correlation-Based Distance", xlab="", sub="")
# 

## DECISION TREE 

# Basic decision tree (ISLR p. 326)

#subset data for decision tree fitting
forest.cover.data1 <- select(forest.cover.data, -c(wilderness_area, climate_zone, geologic_zone, 
                                                   Soil_Type)) #keep Cover_Type_Factor this time
# fit decision tree on training data
set.seed(465)
par(mfrow=c(1,1))
tree.forest.cover=tree(Cover_Type_Factor~.,forest.cover.data1[train,])
plot(tree.forest.cover, title(main="Basic Decision Tree on Training Data"))
text(tree.forest.cover,pretty=0, cex=0.75)
tree.forest.cover
summary(tree.forest.cover) #evaluate performance on training data

# predict decision tree on test data
tree.pred=predict(tree.forest.cover,forest.cover.data1[-train,],type="class") # evaluate performance on test data. see ISLR p 326.
a <- tree.pred
b <- forest.cover.data1[-train,"Cover_Type_Factor"]
xtab<-table(a,b)
confusionMatrix(xtab) # test data confusion matrix

# decision tree cross validation results on test data
cv.forest.cover=cv.tree(tree.forest.cover, FUN=prune.misclass) # cross-validation test data results
names(cv.forest.cover) #each name can ba be calles with $"name"
cv.forest.cover
par(mfrow=c(1,2))
plot(cv.forest.cover$size, cv.forest.cover$dev, type="b", title(main="Cross Validated Results on Test Data"))
plot(cv.forest.cover$size, cv.forest.cover$k, type="b")

# prune tree
# set best = lowest error rate in cross validation.
prune.cv.forest.cover=prune.misclass(tree.forest.cover, best=4)
par(mfrow=c(1,1))
plot(prune.cv.forest.cover, title=(main="Pruned tree on test data"))
text(prune.cv.forest.cover,pretty=0, cex=0.75)

# fit pruned tree on test data
tree.pred=predict(prune.cv.forest.cover,forest.cover.data1[-train,],type="class") # evaluate performance on test data. see ISLR p 326.
a <- tree.pred
b <- forest.cover.data1[-train,"Cover_Type_Factor"]
xtab<-table(a,b)
confusionMatrix(xtab) # decision tree test data confusion matrix - cross validated & pruned

## RANDOM FOREST

# fit random forest on training data
rf.forest.cover=randomForest(Cover_Type_Factor~.,forest.cover.data1[train,],
                             mtry=13, ntree=25, importance=TRUE)
summary(rf.forest.cover)
importance(rf.forest.cover)

# predict random forest on test data
rf.pred = predict(rf.forest.cover,forest.cover.data1[-train,],type="class") # evaluate performance on test data. see ISLR p 326.
a <- rf.pred
b <- forest.cover.data1[-train,"Cover_Type_Factor"]
xtab<-table(a,b)
confusionMatrix(xtab) # randome forest test data confusion matrix
