# see: https://archive.ics.uci.edu/ml/datasets/Covertype

# Required packages
#####
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

#####
# Data load and ETL
#####

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


# Create variables for climate_zone.  See for detail: https://archive.ics.uci.edu/ml/machine-learning-databases/covtype/covtype.info
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


forest.cover.data$climate_zone <- factor(forest.cover.data$climate_zone,levels=c(2,3,4,5,6,7,8),labels=c("lower_mountain", "mountain_dry", "mountain", "montain_dry_and_mountain", "mountain_and_subalpine", "subalpine","alpine"))               

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
forest.cover.data$geologic_zone <- factor(forest.cover.data$geologic_zone,levels=c(1,2,5,7),labels=c("alluvium", "glacial", "mixed_sedimentary", "igneous_and_metamorphic")) 

#Create dummy variables for geologic_zone
geologic.dummy.vars <- model.matrix(~geologic_zone, data = forest.cover.data)[,-1]
colnames(geologic.dummy.vars) <- gsub("geologic_zone","",colnames(geologic.dummy.vars))
forest.cover.data <- cbind(forest.cover.data,geologic.dummy.vars)
rm(geologic.dummy.vars)

# Collapse Soil_Type to factor
forest.cover.data$Soil_Type <- factor(as.matrix(forest.cover.data[,15:54])%*%1:length(15:54), labels = colnames(forest.cover.data[15:54]))

soil.type.dummy.vars <- model.matrix(~Soil_Type, data = forest.cover.data)[,-1]
colnames(soil.type.dummy.vars) <- gsub("TypeSoil_Type_","Type",colnames(soil.type.dummy.vars))
forest.cover.data <- cbind(forest.cover.data,soil.type.dummy.vars)
rm(soil.type.dummy.vars)

forest.cover.data <- select(forest.cover.data, -c(Soil_Type_1,Soil_Type_2,Soil_Type_3,Soil_Type_4,Soil_Type_5,Soil_Type_6,
                                                  Soil_Type_7,Soil_Type_8,Soil_Type_9,Soil_Type_10,Soil_Type_11,Soil_Type_12,
                                                  Soil_Type_13,Soil_Type_14,Soil_Type_15,Soil_Type_16,Soil_Type_17,Soil_Type_18,
                                                  Soil_Type_19,Soil_Type_20,Soil_Type_21,Soil_Type_22,Soil_Type_23,Soil_Type_24,
                                                  Soil_Type_25,Soil_Type_26,Soil_Type_27,Soil_Type_28,Soil_Type_29,Soil_Type_30,
                                                  Soil_Type_31,Soil_Type_32,Soil_Type_33,Soil_Type_34,Soil_Type_35,Soil_Type_36,
                                                  Soil_Type_37,Soil_Type_38,Soil_Type_39,Soil_Type_40)) #remove variables that have been converted into factors
#####
# Train/Test Split
#####
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

# trim - outlier definition based on training data. Outlier trimming applied to training & test data.
# for (i in colnames(forest.cover.data[sapply(forest.cover.data,(is.numeric))])) { 
#   sd <- (sd(forest.cover.data[train,i])) # standard deviation 
#   mean <- (mean(forest.cover.data[train,i])) # standard deviation
#   outlier_high <- mean + 4*sd
#   outlier_low <- mean - 4*sd
#   forest.cover.data[,i] <- ifelse((forest.cover.data[,i] > outlier_high), outlier_high,
#                          ifelse((forest.cover.data[,i]  < outlier_low), outlier_low, forest.cover.data[,i]))
# }

#####
#  UNIVARIATE EDA 
#####
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

#####
# MULTIVARIATE EDA  
#####
# Create data subset with no factor variables. (e.g present qualitative variables 
# as indicator variables instead of factor variables
forest.cover.data1 <- select(forest.cover.data, -c(Cover_Type_Factor, 
                                                   wilderness_area, climate_zone, geologic_zone, 
                                                   Soil_Type))

# correlation matrix plot
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

#####
# PCA analysis
#####
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

#####
# Cluster Analysis
##### 
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
#####
# VARIABLE ENGINEERING 
#####
# 
# # Center training and test data. 
# # Training and test observations are updated, but only training data is used for calculating mean.
# # Only quantitative variables are centered. Factor variables cannot be centered. Binary variables has mixed opinions.
# for (i in colnames(forest.cover.data[,1:10])){
#   forest.cover.data[,i] <- (mean(forest.cover.data[train,i]) - forest.cover.data[,i])
# }
# 
# # construct interaction variables (x*y) for all variable pairs. 
# # Note: What to do if variable value = 0?  set value to 0.0001 so zero term does not cancel other variable (X*0 = 0)
# var_names <- colnames(select(forest.cover.data,-Cover_Type_Factor,-wilderness_area,-climate_zone,-Soil_Type,-geologic_zone)) # create a list of all variables excluding factor variables
# var_names <- combn(var_names,m=2) # combinations of all variable pairs
# var_names <- t(var_names) # transpose from wide to long data
# #run loop through all variable pair combinations (redundancies removed (x*x) and no repeats (x*y, y*x))
# for (a in row(var_names)) {
#   forest.cover.data[paste0("inter_",var_names[a,1],"_",var_names[a,2])] <- I(forest.cover.data[,paste(var_names[a,1])] * forest.cover.data[,paste(var_names[a,2])])
# } 
# 
# # construct 2nd & 3rd degree polynomial variables from the 10 quantitative variables.
# for (i in colnames(forest.cover.data[train,1:10])){
#   forest.cover.data[paste0("poly2_",i)] <- I(forest.cover.data[,i]^2)
#   forest.cover.data[paste0("poly3_",i)] <- I(forest.cover.data[,i]^3)
# }

#####
## MODEL FITTING - DECISION TREE 
#####
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

#####
# MODEL FITTING - RANDOM FOREST
#####

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

#### Merge the Dummy Columns ####

	## Soil ##
	> forest.cover.data$Soil_Type     <- names(forest.cover.data[15:54])[apply(forest.cover.data[15:54], 1, match, x = 1)]		
	> forest.cover.data$Soil_Type.II  =  factor(forest.cover.data$Soil_Type, levels = c("2702", "2703", "2704", "2705", "2706", "2717", "3501", "3502", "4201", "4703", "4704", "4744", "4758", "5101", "5151", "6101", "6102", "6731", "7101", "7102", "7103", "7201", "7202", "7700", "7701", "7702", "7709", "7710", "7745", "7746", "7755", "7756", "7757", "7790", "8703", "8707", "8708", "8771", "8772", "8776"), labels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40))
	> forest.cover.data$Soil_Type.III =  ifelse(as.numeric(forest.cover.data$Soil_Type.II)==2703 | as.numeric(forest.cover.data$Soil_Type.II)==2717 | as.numeric(forest.cover.data$Soil_Type.II)==2717 | as.numeric(forest.cover.data$Soil_Type.II)==3501 | as.numeric(forest.cover.data$Soil_Type.II)==4703 | as.numeric(forest.cover.data$Soil_Type.II)==5151 | as.numeric(forest.cover.data$Soil_Type.II)==6102 | as.numeric(forest.cover.data$Soil_Type.II)==8707 | as.numeric(forest.cover.data$Soil_Type.II)==8777, 41, forest.cover.data$Soil_Type.II) 

	## Wilderness_Area ##

	> forest.cover.data$Wilderness     <- names(forest.cover.data[11:14])[apply(forest.cover.data[11:14], 1, match, x = 1)]
	> forest.cover.data$Wilderness.II  =  factor(forest.cover.data$Wilderness, levels=c("Rawah", "Comanche_Peak", "Cache_la_Poudre", "Neota"), labels = c(1,2,3,4))


#####
# Train/Test Split
 #####
# 


# split data into training set and test 
> set.seed(465)

> forest.cover.data$gp=runif(dim(forest.cover.data)[1])				## The dim() formula gives you the number of rows.  The runif() formula gives you random numbers up to the number of rows. 
> train=subset(forest.cover.data, forest.cover.data$gp >.30)


> dim(train)
  [1] 406899     81

> test = subset(forest.cover.data, forest.cover.data$gp <=.30)
> dim(test)
  [1] 174336     81
    

##### FITTING REGRESSION TREES ######

> library(ISLR)
> library(tree)
> library(MASS)

> tree.price=tree(Cover_Type ~ Elevation + Aspect + Slope + Horizontal_Distance_Hydrology + Vertical_Distance_Hydrology + Horizontal_Distance_Roadways + Hillshade_9am + Hillshade_Noon + Hillshade_3pm + Horizontal_Distance_Fire + Wilderness.II + Soil_Type.III, train)
> summary(tree.price)

	Regression tree:

	tree(formula = Cover_Type ~ Elevation + Aspect + Slope + Horizontal_Distance_Hydrology + 
    	Vertical_Distance_Hydrology + Horizontal_Distance_Roadways + 
    	Hillshade_9am + Hillshade_Noon + Hillshade_3pm + Horizontal_Distance_Fire + 
    	Wilderness.II + Soil_Type.III, data = train)

	Variables actually used in tree construction:
	[1] "Elevation"     "Wilderness.II" "Soil_Type.III"

	Number of terminal nodes:  7 
	Residual mean deviance:  1.261 = 512800 / 406700 

	Distribution of residuals:
   
	Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
	-4.4870 -0.5419 -0.1663  0.0000  0.4581  5.4580

> tree.price

	node) 			  split 	   n 		deviance 	 yval
       	1) 	root 				406675 		790500 		2.050  
   	2) 	Elevation 	< 2578.5 	40205  		85860 		3.577  
     	4) 	Wilderness.II: 	     1 		3410   		1139 		2.063 *
     	5) 	Wilderness.II:      2,3 	36795  		76190 		3.717 *
   	3) 	Elevation 	> 2578.5 	366470 		600700 		1.883  
     	6) 	Soil_Type.III 	< 34.5 		338243 		280200 		1.749  
      	12) 	Elevation 	< 2920.5 	111927 		108900 		2.166 *
      	13) 	Elevation 	> 2920.5 	226316 		142200 		1.542 *
     	7) 	Soil_Type.III 	> 34.5 		28227 		241500 		3.490  	
      	14) 	Wilderness.II:     1,4 		17587 		113700 		2.475 *
      	15) 	Wilderness.II: 	    2 		10640  		79660 		5.169  
        30) 	Elevation 	< 3227.5 	1153   		7242 		2.548 *
        31) 	Elevation 	> 3227.5 	9487  		63540 		5.487 *
	* denotes terminal node

## With this code, we plot the tree.

> plot(tree.price)
> text(tree.price)


## We now make predictions on the test set:

> yhat=predict(tree.price, newdata=test)
> tree.test=test["Cover_Type"]


> yhat.matrix=as.matrix(yhat)
  [1] 174336      1

> tree.test.matrix=as.matrix(tree.test)


> mean((yhat.matrix-tree.test.matrix)^2)							## This is MSE
  [1] 1.261655
 
> sd((yhat.matrix-tree.test.matrix)^2)/sqrt(dim(test)[1])   					## Standard Error
  [1] 0.009198927

> 1-sum((yhat.matrix-tree.test.matrix)^2)/sum((mean(tree.test.matrix)-tree.test.matrix)^2)		## R^2
  [1] 0.357922

> table(yhat.matrix, tree.test.matrix)
  
  			tree.test.matrix
yhat.matrix               1       2      3      4     5      6      7
  1 			50527	44934 	 0      0    188     0    1197
  2    			 48	1387     0      0     52     0      0
  3  			6226	36006	2169    0    2528   1034   13
  4  			5470	300      0      0      0     0    1744
  5   			329	61	 0      0      0     0     94
  6    			 58	2054	8558   880    74   4193     0
  7   			943	76	 0      0      0     0     3193

_____________________________________________________________________________________________________________________________________

## With this code, we now prune the tree down.........

> names(tree.price)
  [1] "frame"   "where"   "terms"   "call"    "y"       "weights"

> cv.tree=cv.tree(tree.price)
	$size
	[1]     7       6         5       4        3        2         1

	$dev
	[1] 512989.0 530352.6 530352.6 559535.8 607644.0 686632.3 790533.1

	$k
	[1]       -Inf   8534.804   8879.769  29204.125  48115.084  79019.031 103952.224

	$method
	[1] "deviance"

	attr(,"class")
	[1] "prune"         "tree.sequence"



> plot(cv.tree$size, cv.tree$dev, type='b')

## Based on the plot of CV, we prune the tree using the following code:

> prune.tree=prune.tree(tree.price, best=7)
> plot(prune.tree)
> text(prune.tree, pretty=0)

> yhat.pruned=predict(prune.tree, newdata=test)

> mean((yhat.pruned-tree.test.matrix)^2)							## This is MSE
  [1] 1.261655

 
> sd((yhat.pruned-tree.test.matrix)^2)/sqrt(dim(test)[1])   					## Standard Error
  [1] 0.009198927

> 1-sum((yhat.pruned-tree.test.matrix)^2)/sum((mean(tree.test.matrix)-tree.test.matrix)^2)	## R^2
  [1] 0.357922


##### BAGGING ######

##  "Bagging is simply a special case of a random forest with m = p." - Auhtors: Gareth James • Daniela Witten • Trevor Hastie • Robert Tibshirani  ##
> library(randomForest)
> set.seed(1)
> names(train.set)
> bag.tree=randomForest(Cover_Type ~ Elevation + Wilderness.II + Soil_Type.III, train, mtry=3, importance=TRUE)

Elevation + Aspect + Slope + Horizontal_Distance_Hydrology + Vertical_Distance_Hydrology + Horizontal_Distance_Roadways + Hillshade_9am + Hillshade_Noon + Hillshade_3pm + Horizontal_Distance_Fire + Wilderness.II + Soil_Type.III


> bag.tree	## Note that the formula mtry=13 above means that the model will use all 13 predictors.


# We now test this model on the validation set.

> yhat.bag = predict(bag.tree, newdata=test.set)

> yhat.bag.matrix=as.matrix(yhat.bag)
> tree.bag.matrix=as.matrix(tree.test)

> plot(yhat.bag.matrix, tree.bag.matrix)
> abline(0,1)

> mean((yhat.bag.matrix-tree.bag.matrix)^2)	
  [1] 						
  [1] 	

> sd((yhat.bag.matrix-tree.bag.matrix)^2)/sqrt(dim(test.set)[1])   			# Standard Error
  [1] 

> 1-sum((yhat.bag.matrix-tree.bag.matrix)^2)/sum((mean(tree.bag.matrix)-tree.bag.matrix)^2)
  [1] 



##### Boosting ######

> library(gbm)
> set.seed(1)
> boost.tree=gbm(Cover_Type ~ Elevation + Aspect + Slope + Horizontal_Distance_Hydrology + Vertical_Distance_Hydrology + Horizontal_Distance_Roadways + Hillshade_9am + Hillshade_Noon + Hillshade_3pm + Horizontal_Distance_Fire + Wilderness.II + Soil_Type.III, data=train, distribution="gaussian", n.trees=1000, interaction.depth=4)

> summary(boost.tree)

Elevation                                         Elevation 56.1281636
Soil_Type.III                                 Soil_Type.III 27.1895005
Wilderness.II                                 Wilderness.II 15.6039158
Horizontal_Distance_Hydrology Horizontal_Distance_Hydrology  0.4533118
Horizontal_Distance_Fire           Horizontal_Distance_Fire  0.3608064
Horizontal_Distance_Roadways   Horizontal_Distance_Roadways  0.2643019
Aspect                                               Aspect  0.0000000
Slope                                                 Slope  0.0000000
Vertical_Distance_Hydrology     Vertical_Distance_Hydrology  0.0000000
Hillshade_9am                                 Hillshade_9am  0.0000000
Hillshade_Noon                               Hillshade_Noon  0.0000000
Hillshade_3pm                                 Hillshade_3pm  0.0000000

> par(mfrow =c(1,2))
> plot(boost.tree, i="Elevation")	# You choose variable that you want to see plotted
> plot(boost.tree, i="Soil_Type.III")	# You choose variable that you want to see plotted

> boost.matrix=as.matrix(boost.tree)
> yhat.boost = predict(boost.tree, newdata=test, n.trees=1000)

> mean((yhat.boost-tree.test.matrix)^2)								## This is MSE
  [1] 1.366025

 
> sd((yhat.boost-tree.test.matrix)^2)/sqrt(dim(test)[1])   					## Standard Error
  [1] 0.008964941

> 1-sum((yhat.boost-tree.test.matrix)^2)/sum((mean(tree.test.matrix)-tree.test.matrix)^2)	## R^2
  [1] 0.3048063


# We use the formula below to create new models with different shrinkage and see if it improves the results.

> boost.tree.2=gbm(Cover_Type ~ Elevation + Aspect + Slope + Horizontal_Distance_Hydrology + Vertical_Distance_Hydrology + Horizontal_Distance_Roadways + Hillshade_9am + Hillshade_Noon + Hillshade_3pm + Horizontal_Distance_Fire + Wilderness.II + Soil_Type.III, data=train, distribution="gaussian", n.trees=500, interaction.depth=4, shrinkage=0.2, verbose=F)

> yhat.boost.2=predict(boost.tree.2, newdata=test, n.trees=500)

> yhat.boost.matrix.2=as.matrix(yhat.boost.2)								## Mean Squared Error
> mean((yhat.boost.matrix.2-tree.test.matrix)^2)
  [1] 0.7356197

> sd((yhat.boost.matrix.2-tree.test.matrix)^2)/sqrt(dim(test)[1])   					## Standard Error
  [1] 0.005950289

> 1-sum((yhat.boost.2-tree.test.matrix)^2)/sum((mean(tree.test.matrix)-tree.test.matrix)^2)		## R^2
  [1] 0.6256305
_________________________________________________________________________________________________________________________________________
_________________________________________________________________________________________________________________________________________

# Shrinkage of 0.4

> boost.tree.3=gbm(Cover_Type ~ Elevation + Aspect + Slope + Horizontal_Distance_Hydrology + Vertical_Distance_Hydrology + Horizontal_Distance_Roadways + Hillshade_9am + Hillshade_Noon + Hillshade_3pm + Horizontal_Distance_Fire + Wilderness.II + Soil_Type.III, data=train, distribution="gaussian", n.trees=500, interaction.depth=4, shrinkage=0.4, verbose=F)

> yhat.boost.3=predict(boost.tree.3, newdata=test, n.trees=500)

> yhat.boost.matrix.3=as.matrix(yhat.boost.3)								## Mean Squared Error
> mean((yhat.boost.matrix.3-tree.test.matrix)^2)
  [1] 0.6617295

> sd((yhat.boost.matrix.3-tree.test.matrix)^2)/sqrt(dim(test)[1])   					## Standard Error
  [1] 0.005950289

> 1-sum((yhat.boost.3-tree.test.matrix)^2)/sum((mean(tree.test.matrix)-tree.test.matrix)^2)		## R^2
  [1] 0.6632345


> yhat.boost.3.matrix=as.matrix(yhat.boost.3)
> table(yhat.boost.3.matrix, tree.test.matrix)

_________________________________________________________________________________________________________________________________________
_________________________________________________________________________________________________________________________________________

# Shrinkage of 0.3

> boost.tree.4=gbm(Cover_Type ~ Elevation + Aspect + Slope + Horizontal_Distance_Hydrology + Vertical_Distance_Hydrology + Horizontal_Distance_Roadways + Hillshade_9am + Hillshade_Noon + Hillshade_3pm + Horizontal_Distance_Fire + Wilderness.II + Soil_Type.III, data=train, distribution="gaussian", n.trees=500, interaction.depth=4, shrinkage=0.3, verbose=F)

> yhat.boost.4=predict(boost.tree.4, newdata=test, n.trees=500)

> yhat.boost.matrix.4=as.matrix(yhat.boost.4)								## Mean Squared Error
> mean((yhat.boost.matrix.4-tree.test.matrix)^2)
  [1] 0.685798

> sd((yhat.boost.matrix.4-tree.test.matrix)^2)/sqrt(dim(test)[1])   					## Standard Error
  [1] 0.005512805

> 1-sum((yhat.boost.4-tree.test.matrix)^2)/sum((mean(tree.test.matrix)-tree.test.matrix)^2)		## R^2
  [1] 0.6509856

_________________________________________________________________________________________________________________________________________
_________________________________________________________________________________________________________________________________________

# Shrinkage of 0.1

> boost.tree.5=gbm(Cover_Type ~ Elevation + Aspect + Slope + Horizontal_Distance_Hydrology + Vertical_Distance_Hydrology + Horizontal_Distance_Roadways + Hillshade_9am + Hillshade_Noon + Hillshade_3pm + Horizontal_Distance_Fire + Wilderness.II + Soil_Type.III, data=train, distribution="gaussian", n.trees=500, interaction.depth=4, shrinkage=0.1, verbose=F)

> yhat.boost.5=predict(boost.tree.5, newdata=test, n.trees=500)

> yhat.boost.matrix.5=as.matrix(yhat.boost.5)								## Mean Squared Error
> mean((yhat.boost.matrix.5-tree.test.matrix)^2)
  [1] 0.8223398

> sd((yhat.boost.matrix.5-tree.test.matrix)^2)/sqrt(dim(test)[1])   					## Standard Error
  [1] 0.00641892

> 1-sum((yhat.boost.5-tree.test.matrix)^2)/sum((mean(tree.test.matrix)-tree.test.matrix)^2)		## R^2
  [1] 0.5814971

_________________________________________________________________________________________________________________________________________
_________________________________________________________________________________________________________________________________________

# Shrinkage of 0.05

> boost.tree.6=gbm(Cover_Type ~ Elevation + Aspect + Slope + Horizontal_Distance_Hydrology + Vertical_Distance_Hydrology + Horizontal_Distance_Roadways + Hillshade_9am + Hillshade_Noon + Hillshade_3pm + Horizontal_Distance_Fire + Wilderness.II + Soil_Type.III, data=train, distribution="gaussian", n.trees=500, interaction.depth=4, shrinkage=0.05, verbose=F)

> yhat.boost.6=predict(boost.tree.6, newdata=test, n.trees=500)

> yhat.boost.matrix.6=as.matrix(yhat.boost.6)								## Mean Squared Error
> mean((yhat.boost.matrix.6-tree.test.matrix)^2)
  [1] 0.9126393

> sd((yhat.boost.matrix.6-tree.test.matrix)^2)/sqrt(dim(test)[1])   					## Standard Error
  [1] 0.006848817

> 1-sum((yhat.boost.6-tree.test.matrix)^2)/sum((mean(tree.test.matrix)-tree.test.matrix)^2)		## R^2
  [1] 0.5355421

_________________________________________________________________________________________________________________________________________
_________________________________________________________________________________________________________________________________________

# Shrinkage of 0.025

> boost.tree.7=gbm(Cover_Type ~ Elevation + Aspect + Slope + Horizontal_Distance_Hydrology + Vertical_Distance_Hydrology + Horizontal_Distance_Roadways + Hillshade_9am + Hillshade_Noon + Hillshade_3pm + Horizontal_Distance_Fire + Wilderness.II + Soil_Type.III, data=train, distribution="gaussian", n.trees=500, interaction.depth=4, shrinkage=0.025, verbose=F)

> yhat.boost.7=predict(boost.tree.7, newdata=test, n.trees=500)

> yhat.boost.matrix.7=as.matrix(yhat.boost.7)								## Mean Squared Error
> mean((yhat.boost.matrix.7-tree.test.matrix)^2)
  [1] 1.000791

> sd((yhat.boost.matrix.7-tree.test.matrix)^2)/sqrt(dim(test)[1])   					## Standard Error
  [1] 0.007331368

> 1-sum((yhat.boost.7-tree.test.matrix)^2)/sum((mean(tree.test.matrix)-tree.test.matrix)^2)		## R^2
  [1] 0.4906803

_________________________________________________________________________________________________________________________________________
_________________________________________________________________________________________________________________________________________

## Bayesian Trees ##

> library(rpart)
> mytree = rpart(Cover_Type ~ Elevation + Aspect + Slope + Horizontal_Distance_Hydrology + Vertical_Distance_Hydrology + Horizontal_Distance_Roadways + Hillshade_9am + Hillshade_Noon + Hillshade_3pm + Horizontal_Distance_Fire + Wilderness.II + Soil_Type.III, data=train, minbucket = 1, minsplit=1)
> summary(mytree)

        CP 	  nsplit       rel 	  error           xerror             xstd
	1	0.13149721      0 	1.0000000 	1.0000049	0.004140864
	2	0.09995729      1 	0.8685028 	0.8692314 	0.004084104
	3 	0.06086449      2 	0.7685455 	0.7698051 	0.002969443
	4	0.03694256      3 	0.7076810 	0.7089460 	0.003131523
	5	0.01123271      4 	0.6707385 	0.6706482 	0.003092120
	6	0.01079633      5 	0.6595058 	0.6608297 	0.003114809
	7	0.01000000      6 	0.6487094 	0.6517338 	0.003107035


> rpart2 = rpart(Cover_Type ~ Elevation + Aspect + Slope + Horizontal_Distance_Hydrology + Vertical_Distance_Hydrology + Horizontal_Distance_Roadways + Hillshade_9am + Hillshade_Noon + Hillshade_3pm + Horizontal_Distance_Fire + Wilderness.II + Soil_Type.III, data=train, parms=list(prior=c(0.85,0.15)), cp=.01) 
> summary(rapart)
  
  Variable importance

                        Elevation                  Soil_Type.III                  Wilderness.II 
                           38                            31                            28 
     
		Horizontal_Distance_Fire          Hillshade_Noon           Horizontal_Distance_Hydrology 
                            1                             1                             1 



> plot(rpart2)
> text(rpart2)

 

> test.data.frame=data.frame(test)	## For Bayesian, one must use a dataframe.
> rpart2.predict = predict(rpart2, test.data.frame)


> table(rpart2.predict, test.data.frame$Cover_Type)

	rpart2.predict         	   1     2      3     4     5     6     7
  			1 	50527 	44934   0     0   188     0    1197
  			2    	 48   	1387    0     0    52     0     0
  			3    	6226 	36006  2169   0   2528  1034    13
  			4  	5470     300    0     0     0     0    1744
  			5   	329    	 61     0     0     0     0    94
  			6      	58  	2054  8558   880   74   4193    0
  			7      	943      76     0     0     0     0    3193




 								
> mean((rpart2.predict-test.data.frame$Cover_Type)^2)								## Mean Squared Error
  [1] 1.261655

> sd((rpart2.predict-test.data.frame$Cover_Type)^2)/sqrt(dim(test)[1])   					## Standard Error
  [1] 0.009198927

> 1-sum((rpart2.predict-test.data.frame$Cover_Type)^2)/sum((mean(tree.test.matrix)-tree.test.matrix)^2)		## R^2
  [1] 0.357922

_________________________________________________________________________________________________________________________________________________________________
_________________________________________________________________________________________________________________________________________________________________
_________________________________________________________________________________________________________________________________________________________________
_________________________________________________________________________________________________________________________________________________________________
_________________________________________________________________________________________________________________________________________________________________
_________________________________________________________________________________________________________________________________________________________________


#### Cluster Analysis ####


## K-Means

	# K-means clustering is the most popular partitioning method. It requires the analyst to specify the number of clusters to extract. 
	# A plot of the within groups sum of squares by number of clusters extracted can help determine the appropriate number of clusters. 
	
	# In terms of a data.frame, a clustering algorithm finds out which rows are similar to each other.	

	# A robust version of K-means based on mediods can be invoked by using pam( ) instead of kmeans( ). The function pamk( ) in the 
	fpc package is a wrapper for pam that also prints the suggested number of clusters based on optimum average silhouette width.

	> require(cluster)
	> set.seed(17)
	> kmeans.dataframe=as.data.frame(train)
	> kmeans.dataframe.2 = kmeans.dataframe[,c(1:15, 80)] 	

	> kmeans.2=kmeans(kmeans.dataframe.2,    2,   nstart=20)
	> kmeans.3=kmeans(kmeans.dataframe.2,    3,   nstart=20)
	> kmeans.4=kmeans(kmeans.dataframe.2,    4,   nstart=20)
	> kmeans.7=kmeans(kmeans.dataframe.2,    7,   nstart=20)
	> kmeans.10=kmeans(kmeans.dataframe.2,  10,   nstart=10)
	> kmeans.15=kmeans(kmeans.dataframe.2,  15,   nstart=10)	
	> kmeans.20=kmeans(kmeans.dataframe.2,  20,   nstart=10)

	> library(fpc)
	> require(fpc)
	> best.cluster=kmeansruns(kmeans.dataframe.2, krange=1:7, criterion="ch")
	> asw.cluster =kmeansruns(kmeans.dataframe.2, krange=1:7, criterion="asw")

	> plot(kmeans.7$cluster, data=kmeans.dataframe.2,  class="Cover_Type")

	> kmeansAIC = function(fit){
	  m = ncol(fit$centers)
	  n = length(fit$cluster)
	  k = nrow(fit$centers)
	  D = fit$tot.withinss
	  return(D + 2*m*k)
	   }
	> aic=sapply(kmeans.7, kmeansAIC)

	
	> kmeans.2	# Within Sum of Squares equals 46.6%	R This provides the distance between a point and its centroid. R minimizes within cluster sum-of-squares.
	> kmeans.3	# Within Sum of Squares equals 62.0%
	> kmeans.4	# Within Sum of Squares equals 70.6%
	> kmeans.7	# Within Sum of Squares equals 82.2%
	> kmeans.10	# Within Sum of Squares equals 86.5%
	> kmeans.15	# Within Sum of Squares equals 90.2%
	> kmeans.20	# Within Sum of Squares equals 91.9%

	> kmeans.2$tot.withinss		[1] 938057353598
	> kmeans.3$tot.withinss		[1] 666619444909
	> kmeans.4$tot.withinss		[1] 516968233315
	> kmeans.7$tot.withinss		[1] 313227451254
	> kmeans.10$tot.withinss	[1] 237041697753
	> kmeans.15$tot.withinss	[1] 172647604334
	> kmeans.20$tot.withinss	[1] 142509924114

_______________________________________________________________________________________________________
_______________________________________________________________________________________________________


	## Combining ratios $withinss and $betweenss with an appropriate algorithm may help to choose the right number of k clusters
	# First iteration is directly below
	> wss <- sum(kmeans(kmeans.dataframe.2, centers=1)$withinss)
	
	# Iterations 2:15
	> for (i in 2:15) wss[i] <- sum(kmeans(kmeans.dataframe.2, centers=i)$withinss)
	
	# Plot the 15 withinss values. One for each k 
	> plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

_____________________________________________________________________________________________________________________________________________________________

	> require(cluster)
	> theGap=clusGap(kmeans.dataframe.2, FUNcluster=pam, K.max=10)
	> gapDF = as.data.frame(theGap$Tab)
	> gapDF

		logW		E.logW       	gap      	SE.sim
	1	8.572041 	9.341200 	0.7691586	0.001987463
	2	8.526392 	9.309235 	0.7828428 	0.009333276
	3	8.512830 	9.299332 	0.7865024 	0.005622896
	4	8.504951 	9.291546 	0.7865953 	0.004182545
	5	8.498858 	9.286134 	0.7872758 	0.003375069
	6	8.492443 	9.281628 	0.7891848 	0.003043337
	7	8.483840 	9.277796 	0.7939559 	0.002632973
	8	8.479272 	9.274548 	0.7952759 	0.002501184
	9	8.473463 	9.271314 	0.7978515 	0.002356782
	10	8.464936 	9.268428 	0.8034923 	0.002370468
	11	8.461417 	9.265692 	0.8042758 	0.002289062
	12	8.455662 	9.263114 	0.8074519 	0.002184732
	13	8.452694 	9.260841 	0.8081468 	0.002121427
	14	8.449890 	9.258659 	0.8087693 	0.002066710
	15	8.446192 	9.256534 	0.8103415 	0.001989467
	16	8.443606 	9.254447 	0.8108416 	0.002026683
	17	8.440895 	9.252487 	0.8115917 	0.001895343
	18	8.437073 	9.250577 	0.8135040 	0.001864144
	19	8.434761	9.248635 	0.8138735 	0.001748455
	20	8.432359	9.246870 	0.8145108 	0.001797018


	> library(ggplot2)
	> require(ggplot2)
	# logW curves
	> ggplot(gapDF, aes=(x=1:nrow(gapDF))) +
	+ geom_line(aes(y=logW), color="blue") +
	+ geom_point(aes(y=logW), color="blue") +
	+ geom_line(aes(y=E.logW), color="green") +
	+ geom_point(aes(E.logW), color="green") +
	+ labs(x="Number of Clusters")

	# gap curve
	> ggplot(gapDF, aes=(x=1:nrow(gapDF))) +
	+ geom_line(aes(y=gap), color="red") +
	+ geom_point(aes(y=gap), color="red") +
	+ geom_errorbar(aes(ymin=gap-SE.sim, ymax=gap+SE.sim), color="red") + 
	+ labs(x="Number of Clusters", y="Gap")


_______________________________________________________________________________________________________________________________________________
_____________________________________________________________________________________________________________________________________________________________________________________________
_____________________________________________________________________________________________________________________________________________________________________________________________

## McLust: Model Based Clustering

## The key idea for model-based clustering is that observations come from groups
with different statistical distributions (such as different means and variances). The
algorithms try to find the best set of such underlying distributions to explain the
observed data. We use the mclust package [53, 54] to demonstrate this.

> library(mclust)
> seg.mc.2 <- Mclust(kmeans.dataframe.2, k=2)
> summary(seg.mc)

> seg.mc.4 <- Mclust(kmeans.dataframe.2, k=4)
> summary(seg.mc)

> seg.mc.7 <- Mclust(kmeans.dataframe.2, k=7)
> summary(seg.mc)

> seg.mc.10 <- Mclust(kmeans.dataframe.2, k=10)
> summary(seg.mc)

> seg.mc.14 <- Mclust(kmeans.dataframe.2, k=14)
> summary(seg.mc)
_____________________________________________________________________________________________________________________________________________________________________________________________
_____________________________________________________________________________________________________________________________________________________________________________________________


## Hierarchical Clustering


	> kmeans.dataframe=as.data.frame(train)						## This creates the dataframe

	> dist.1=dist(kmeans.dataframe, method="euclidean")				## Creates the model using complete
	> hierarc.comp.1=hclust(dist.1, method="complete")
	> plot.comp.1=plot(hierarc.comp.1)
	> plot(cut(as.dendrogram(hierarc.comp.1), h=15)$lower[[50]])	# Best cut by far....
	> hierarc.segment=cutree(hierarc.comp.1, k=2)					## You have to CUT the TREE prior to runnning the mean function!!
	> table(hierarc.segment)
	> seg.summ <- function(cluster.data, groups){aggregate(cluster.data, list(groups), function(x) mean(as.numeric(x)))}
	> seg.summ(cluster.data, hierarc.segment)  ## This gives you the means of the 2 cluster solution!!!



	> hierarc.avg.2=hclust(dist(hier.arch.dataframe), method="average")	## Creates the model using complete
	> hierarc.segment.2=cutree(hierarc.avg.2, k=4)				## You have to CUT the TREE prior to runnning the mean function!!
	> table(hierarc.segment.2)						## Tells you how many observations are in each cluster


	> hc.clusters=cutree(hierarc.comp.1, 4)
	> table(hc.clusters)							## This formula tells you how many observations are in each cluster.

	> cluster.cut.4=rect.hclust(hierarc.comp.1, k=4, border="red")		## This splits it into 4 clusters.
	> rect.hclust(hierarc.comp.2, k=13, border="red")			## This splits it into 13 clusters.

		

	# A very nice tool for displaying more appealing trees is provided by the R package ape. In this case, what we need is to convert the hclust objects into phylo pbjects with the funtions as.phylo	
	# load package ape; remember to install it: install.packages('ape')
	> library(ape)
	# plot basic tree
	> plot(as.phylo(hierarc.comp.2), cex = 0.9, label.offset = 1)


	This is where we decide to cut the tree:
	> seg.hc.avg.segment <- cutree(hierarc.comp.2, k=2)			## You have to CUT the TREE prior to runnning the mean function!!
	> table(seg.hc.avg.segment)
	

	> rect.hclust.3=rect.hclust(hierarc.comp.1, k=3, border="red")		# Split into 3 clusters
	> rect.hclust.13=rect.hclust(hierarc.avg.2, k=13, border="red")		# Split into 13 clusters


_____________________________________________________________________________________________________________________________________________________________


	> cor(cophenetic(data.frame(hierarc.comp.2)), segment.dist)				# This gives us an R^2 of 0.35.
		[1] 0.5947901	

________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________

	> plot(hier.arc.avg) # Need this first
	> rect.hclust(hier.arc.avg, 20)						# This cut the dendrogram into 20 clusters.
	> seg.hc.avg.segment <- cutree(hier.arc.avg, k=20)
	> table(seg.hc.avg.segment)
_________________________________________________________________________________________________________________________________________
_________________________________________________________________________________________________________________________________________
####
# Neural Net Model 
####
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
require(nnet)
require(pROC)
require(klaR)
require(ROCR)
select<-dplyr::select #unmask select from dplyr due to MASS package

#####
#Import data
#####
#forest.cover.data <- read.table(file="C:/Users/RS/Documents/Northwestern - Predictive Analytics/454 - Advanced Modeling Techniques/Group Project/covtype_data.txt", header=TRUE, stringsAsFactors=F, sep=",")

forest.cover.data <- read.csv("~/R/covtype_data.txt", header=FALSE) # SSCC/Cyberduck location
.libPaths( c( .libPaths(), "/sscc/opt/R-3.1.1/lib64/R/library") ) # SSCC/Cyberduck location
backup<-forest.cover.data
forest.cover.data<-backup
#####
# Data ETL
#####


#forest.cover.data <- slice(forest.cover.data,1:100) # subset code for debugging

names(forest.cover.data) <- c("Elevation", "Aspect", "Slope", "Hydrology_Horizontal_Distance", "Hydrology_Vertical_Distance",
                              "Roadways_Horizontal_Distance", "Hillshade_9am", "Hillshade_Noon", "Hillshade_3pm", "Fire_Points_Horizontal_Distance",
                              "WA_Rawah", "WA_Neota", "WA_Comanche_Peak", "WA_Cache_la_Poudre", "Soil_Type_1",
                              "Soil_Type_2","Soil_Type_3", "Soil_Type_4", "Soil_Type_5", "Soil_Type_6",
                              "Soil_Type_7", "Soil_Type_8", "Soil_Type_9", "Soil_Type_10", "Soil_Type_11",
                              "Soil_Type_12", "Soil_Type_13", "Soil_Type_14", "Soil_Type_15", "Soil_Type_16",
                              "Soil_Type_17", "Soil_Type_18", "Soil_Type_19", "Soil_Type_20", "Soil_Type_21",
                              "Soil_Type_22", "Soil_Type_23", "Soil_Type_24", "Soil_Type_25", "Soil_Type_26",
                              "Soil_Type_27", "Soil_Type_28", "Soil_Type_29", "Soil_Type_30", "Soil_Type_31",
                              "Soil_Type_32", "Soil_Type_33", "Soil_Type_34", "Soil_Type_35", "Soil_Type_36",
                              "Soil_Type_37", "Soil_Type_38", "Soil_Type_39", "Soil_Type_40", "Cover_Type")

# Create variables for Cover_TYpe
forest.cover.data$Cover_Type_1 <- ifelse((forest.cover.data$Cover_Type==1),1,-1)
forest.cover.data$Cover_Type_2 <- ifelse((forest.cover.data$Cover_Type==2),1,-1)
forest.cover.data$Cover_Type_3 <- ifelse((forest.cover.data$Cover_Type==3),1,-1)
forest.cover.data$Cover_Type_4 <- ifelse((forest.cover.data$Cover_Type==4),1,-1)
forest.cover.data$Cover_Type_5 <- ifelse((forest.cover.data$Cover_Type==5),1,-1)
forest.cover.data$Cover_Type_6 <- ifelse((forest.cover.data$Cover_Type==6),1,-1)
forest.cover.data$Cover_Type_7 <- ifelse((forest.cover.data$Cover_Type==7),1,-1)

# Create Cover_Type_Factor 
forest.cover.data$Cover_Type_Factor <- factor(forest.cover.data$Cover_Type,levels=c(1,2,3,4,5,6,7),
                                              labels=c("Spruce_Fir","Logdepole_Pine","Ponderosa_Pine","Cottonwood_Willow","Aspen","Douglas-fir","Krummholz"))

# drop Cover_Type from dataset because it is an integer variable and we now have cover type dummy variables & factor variable.
forest.cover.data <- select(forest.cover.data, -c(Cover_Type)) 

# Create variables for wilderness_area
forest.cover.data$wilderness_area <- ifelse((forest.cover.data$WA_Rawah==1),1,
                                            ifelse((forest.cover.data$WA_Neota==1),2,
                                                   ifelse((forest.cover.data$WA_Comanche_Peak==1),3,
                                                          ifelse((forest.cover.data$WA_Cache_la_Poudre==1),4,-1))))

forest.cover.data$wilderness_area <- factor(forest.cover.data$wilderness_area,levels=c(1,2,3,4),labels=c("Rawah","Neota","Comanche_Peak","Cache_la_Poudre"))

forest.cover.data$WA_Rawah <- ifelse((forest.cover.data$WA_Rawah==1),1,-1)
forest.cover.data$WA_Neota <- ifelse((forest.cover.data$WA_Neota==1),1,-1)
forest.cover.data$WA_Comanche_Peak <- ifelse((forest.cover.data$WA_Comanche_Peak==1),1,-1)
forest.cover.data$WA_Cache_la_Poudre <- ifelse((forest.cover.data$WA_Cache_la_Poudre==1),1,-1)

# Create variables for climate_zone
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

forest.cover.data$climate_zone_2 <- ifelse((forest.cover.data$climate_zone==2),1,-1)
forest.cover.data$climate_zone_3 <- ifelse((forest.cover.data$climate_zone==3),1,-1)
forest.cover.data$climate_zone_4 <- ifelse((forest.cover.data$climate_zone==4),1,-1)
forest.cover.data$climate_zone_5 <- ifelse((forest.cover.data$climate_zone==5),1,-1)
forest.cover.data$climate_zone_6 <- ifelse((forest.cover.data$climate_zone==6),1,-1)
forest.cover.data$climate_zone_7 <- ifelse((forest.cover.data$climate_zone==7),1,-1)
forest.cover.data$climate_zone_8 <- ifelse((forest.cover.data$climate_zone==8),1,-1)
forest.cover.data$climate_zone <- factor(forest.cover.data$climate_zone,levels=c(2,3,4,5,6,7,8),labels=c("lower_mountain", "mountain_dry", "mountain", "montain_dry_and_mountain", "mountain_and_subalpine", "subalpine","alpine"))               

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
                                                                                                                                                                                                                                                                                                                    ifelse((forest.cover.data$Soil_Type_40==1),7,-1))))))))))))))))))))))))))))))))))))))))
forest.cover.data$geologic_zone_1 <- ifelse((forest.cover.data$geologic_zone==1),1,-1)
forest.cover.data$geologic_zone_2 <- ifelse((forest.cover.data$geologic_zone==2),1,-1)
forest.cover.data$geologic_zone_5 <- ifelse((forest.cover.data$geologic_zone==5),1,-1)
forest.cover.data$geologic_zone_7 <- ifelse((forest.cover.data$geologic_zone==7),1,-1) 

forest.cover.data$geologic_zone <- factor(forest.cover.data$geologic_zone,levels=c(1,2,5,7,0),labels=c("alluvium", "glacial", "mixed_sedimentary", "igneous_and_metamorphic", "missing")) 

## remove variables that have been converted into factors
#forest.cover.data <- select(forest.cover.data, -c(WA_Rawah,WA_Neota,WA_Comanche_Peak,WA_Cache_la_Poudre)) 

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
                                                                                                                                                                                                                                                                                                                ifelse((forest.cover.data$Soil_Type_40==1),40,-1))))))))))))))))))))))))))))))))))))))))

forest.cover.data$Soil_Type<-factor(forest.cover.data$Soil_Type,levels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,0),labels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","missing")) # convert to factor

# forest.cover.data <- select(forest.cover.data, -c(Soil_Type_1,Soil_Type_2,Soil_Type_3,Soil_Type_4,Soil_Type_5,Soil_Type_6,
#                                                   Soil_Type_7,Soil_Type_8,Soil_Type_9,Soil_Type_10,Soil_Type_11,Soil_Type_12,
#                                                   Soil_Type_13,Soil_Type_14,Soil_Type_15,Soil_Type_16,Soil_Type_17,Soil_Type_18,
#                                                   Soil_Type_19,Soil_Type_20,Soil_Type_21,Soil_Type_22,Soil_Type_23,Soil_Type_24,
#                                                   Soil_Type_25,Soil_Type_26,Soil_Type_27,Soil_Type_28,Soil_Type_29,Soil_Type_30,
#                                                   Soil_Type_31,Soil_Type_32,Soil_Type_33,Soil_Type_34,Soil_Type_35,Soil_Type_36,
#                                                   Soil_Type_37,Soil_Type_38,Soil_Type_39,Soil_Type_40)) #remove variables that have been converted into factors

# change Soil_Type_X binary values from (0,1) to (-1,1) to make variable centering more efficient.
forest.cover.data$Soil_Type_1 <- ifelse((forest.cover.data$Soil_Type_1==0),-1,forest.cover.data$Soil_Type_1)
forest.cover.data$Soil_Type_2 <- ifelse((forest.cover.data$Soil_Type_2==0),-1,forest.cover.data$Soil_Type_2)
forest.cover.data$Soil_Type_3 <- ifelse((forest.cover.data$Soil_Type_3==0),-1,forest.cover.data$Soil_Type_3)
forest.cover.data$Soil_Type_4 <- ifelse((forest.cover.data$Soil_Type_4==0),-1,forest.cover.data$Soil_Type_4)
forest.cover.data$Soil_Type_5 <- ifelse((forest.cover.data$Soil_Type_5==0),-1,forest.cover.data$Soil_Type_5)
forest.cover.data$Soil_Type_6 <- ifelse((forest.cover.data$Soil_Type_6==0),-1,forest.cover.data$Soil_Type_6)
forest.cover.data$Soil_Type_7 <- ifelse((forest.cover.data$Soil_Type_7==0),-1,forest.cover.data$Soil_Type_7)
forest.cover.data$Soil_Type_8 <- ifelse((forest.cover.data$Soil_Type_8==0),-1,forest.cover.data$Soil_Type_8)
forest.cover.data$Soil_Type_9 <- ifelse((forest.cover.data$Soil_Type_9==0),-1,forest.cover.data$Soil_Type_9)
forest.cover.data$Soil_Type_10 <- ifelse((forest.cover.data$Soil_Type_10==0),-1,forest.cover.data$Soil_Type_10)
forest.cover.data$Soil_Type_11 <- ifelse((forest.cover.data$Soil_Type_11==0),-1,forest.cover.data$Soil_Type_11)
forest.cover.data$Soil_Type_12 <- ifelse((forest.cover.data$Soil_Type_12==0),-1,forest.cover.data$Soil_Type_12)
forest.cover.data$Soil_Type_13 <- ifelse((forest.cover.data$Soil_Type_13==0),-1,forest.cover.data$Soil_Type_13)
forest.cover.data$Soil_Type_14 <- ifelse((forest.cover.data$Soil_Type_14==0),-1,forest.cover.data$Soil_Type_14)
forest.cover.data$Soil_Type_15 <- ifelse((forest.cover.data$Soil_Type_15==0),-1,forest.cover.data$Soil_Type_15)
forest.cover.data$Soil_Type_16 <- ifelse((forest.cover.data$Soil_Type_16==0),-1,forest.cover.data$Soil_Type_16)
forest.cover.data$Soil_Type_17 <- ifelse((forest.cover.data$Soil_Type_17==0),-1,forest.cover.data$Soil_Type_17)
forest.cover.data$Soil_Type_18 <- ifelse((forest.cover.data$Soil_Type_18==0),-1,forest.cover.data$Soil_Type_18)
forest.cover.data$Soil_Type_19 <- ifelse((forest.cover.data$Soil_Type_19==0),-1,forest.cover.data$Soil_Type_19)
forest.cover.data$Soil_Type_20 <- ifelse((forest.cover.data$Soil_Type_20==0),-1,forest.cover.data$Soil_Type_20)
forest.cover.data$Soil_Type_21 <- ifelse((forest.cover.data$Soil_Type_21==0),-1,forest.cover.data$Soil_Type_21)
forest.cover.data$Soil_Type_22 <- ifelse((forest.cover.data$Soil_Type_22==0),-1,forest.cover.data$Soil_Type_22)
forest.cover.data$Soil_Type_23 <- ifelse((forest.cover.data$Soil_Type_23==0),-1,forest.cover.data$Soil_Type_23)
forest.cover.data$Soil_Type_24 <- ifelse((forest.cover.data$Soil_Type_24==0),-1,forest.cover.data$Soil_Type_24)
forest.cover.data$Soil_Type_25 <- ifelse((forest.cover.data$Soil_Type_25==0),-1,forest.cover.data$Soil_Type_25)
forest.cover.data$Soil_Type_26 <- ifelse((forest.cover.data$Soil_Type_26==0),-1,forest.cover.data$Soil_Type_26)
forest.cover.data$Soil_Type_27 <- ifelse((forest.cover.data$Soil_Type_27==0),-1,forest.cover.data$Soil_Type_27)
forest.cover.data$Soil_Type_28 <- ifelse((forest.cover.data$Soil_Type_28==0),-1,forest.cover.data$Soil_Type_28)
forest.cover.data$Soil_Type_29 <- ifelse((forest.cover.data$Soil_Type_29==0),-1,forest.cover.data$Soil_Type_29)
forest.cover.data$Soil_Type_30 <- ifelse((forest.cover.data$Soil_Type_30==0),-1,forest.cover.data$Soil_Type_30)
forest.cover.data$Soil_Type_31 <- ifelse((forest.cover.data$Soil_Type_31==0),-1,forest.cover.data$Soil_Type_31)
forest.cover.data$Soil_Type_32 <- ifelse((forest.cover.data$Soil_Type_32==0),-1,forest.cover.data$Soil_Type_32)
forest.cover.data$Soil_Type_33 <- ifelse((forest.cover.data$Soil_Type_33==0),-1,forest.cover.data$Soil_Type_33)
forest.cover.data$Soil_Type_34 <- ifelse((forest.cover.data$Soil_Type_34==0),-1,forest.cover.data$Soil_Type_34)
forest.cover.data$Soil_Type_35 <- ifelse((forest.cover.data$Soil_Type_35==0),-1,forest.cover.data$Soil_Type_35)
forest.cover.data$Soil_Type_36 <- ifelse((forest.cover.data$Soil_Type_36==0),-1,forest.cover.data$Soil_Type_36)
forest.cover.data$Soil_Type_37 <- ifelse((forest.cover.data$Soil_Type_37==0),-1,forest.cover.data$Soil_Type_37)
forest.cover.data$Soil_Type_38 <- ifelse((forest.cover.data$Soil_Type_38==0),-1,forest.cover.data$Soil_Type_38)
forest.cover.data$Soil_Type_39 <- ifelse((forest.cover.data$Soil_Type_39==0),-1,forest.cover.data$Soil_Type_39)
forest.cover.data$Soil_Type_40 <- ifelse((forest.cover.data$Soil_Type_40==0),-1,forest.cover.data$Soil_Type_40)


# if necessary due to computational limitations
# sample the training data set to perform EDA
#  dataset = 581,011 observations. the following script takes several hours to run on the full dataset. 
# 16,127 sample size based on 99% confidence interval, 1% error margin 
# See: http://www.raosoft.com/samplesize.html
#forest.cover.data <- forest.cover.data[sample(nrow(forest.cover.data), 16127, replace = FALSE, prob = NULL),]

#####
# Train/Test Split
#####
# split data into training set and test set
set.seed(465)
train=(sample(1:nrow(forest.cover.data),nrow(forest.cover.data)*0.70)) #original 70% train / 30% test split
train=(sample(train,15938)) # if smaller sample is needed for computational complexity, take sample from training set. See: http://www.raosoft.com/samplesize.html

#remove set variables where all observations have same value in training set. (note: necessary when indicator variables are used?)
for (i in colnames(forest.cover.data[train,])) {
  ifelse(n_distinct(forest.cover.data[train,i])==1,forest.cover.data <- subset(forest.cover.data, select = -c(get(i))),forest.cover.data[train,] <- (forest.cover.data[train,]))
}

# # trim - outlier definition based on training data. Outlier trimming applied to training & test data.
# for (i in colnames(forest.cover.data[sapply(forest.cover.data,(is.numeric))])) { 
#   sd <- (sd(forest.cover.data[train,i])) # standard deviation 
#   mean <- (mean(forest.cover.data[train,i])) # standard deviation
#   outlier_high <- mean + 4*sd
#   outlier_low <- mean - 4*sd
#   forest.cover.data[,i] <- ifelse((forest.cover.data[,i] > outlier_high), outlier_high,
#                          ifelse((forest.cover.data[,i]  < outlier_low), outlier_low, forest.cover.data[,i]))
# }
# Center training and test data. 
# construct interaction variables (x*y) for all variable pairs. 
var_names <- colnames(select(forest.cover.data,-Cover_Type_Factor)) # create a list of all variables excluding factor variables
var_names <- combn(var_names,m=2) # combinations of all variable pairs
var_names <- t(var_names) # transpose from wide to long data
##### 
# MODEL FITTING - NEURAL NET (NN)
#####
#####
## NN data prep
#####
# drop respone variable factor variables (keep until now because factors needed for EDA)
forest.cover.data <- select(forest.cover.data, -c( wilderness_area,climate_zone, geologic_zone, Soil_Type))

# NN Preprocessing
sum(apply(forest.cover.data[train,],2,function(x) sum(is.na(x)))) # verify no missing data. Neural nets rely on all observations.

# Standardize data (mean = 0, sd = 1)
# http://www.inside-r.org/node/86978
for (i in 1:10) { # 10 numerical variables, others are binary.
  sd <- sd(forest.cover.data[train,i]) #calculated std. deviation of training data
  mean <- mean(forest.cover.data[train,i]) # calculate mean based on training data
  ifelse(is.numeric(forest.cover.data[,i]), # center numeric data, do not change factor data.
         forest.cover.data[,i] <- ((forest.cover.data[,i] - mean)/sd) # center train/test data based on training calculation
         ,
         forest.cover.data[,i] <<- forest.cover.data[,i]) 
}

#forest.cover.data <- backup

#####
# Fit NN model
#####
# fit NN
fit  <- avNNet(Cover_Type_Factor~., data=forest.cover.data[train,], 
               repeats=20, 
               size=5, 
               decay=0.25,
               bag=FALSE,
               linout=TRUE)
pred2 <-predict(fit, forest.cover.data[-train,]) # predictions on test data.
pred2 <- as.data.frame(pred2) # pred predicts percentage likelihood for each factor
nn.pred <- colnames(pred2)[max.col(pred2,ties.method="first")] # choose the highest percentage item.

a <- as.factor(nn.pred)
b <- forest.cover.data[-train,"Cover_Type_Factor"]
l <- union(a, b)
Table2 <- table(factor(a, l), factor(b, l))
confusionMatrix(Table2)

#random forest model
library(randomForest);
forest_fit <- randomForest(Cover_Type_Factor ~ Elevation + Aspect + Slope + Horizontal_Distance_To_Hydrology + Vertical_Distance_To_Hydrology + Horizontal_Distance_To_Roadways + Hillshade_9am + Hillshade_Noon + Hillshade_3pm + Horizontal_Distance_To_Fire_Points + wilderness_area + climate_zone + geologic_zone + (Elevation * geologic_zone) + (geologic_zone * climate_zone), data=forest.cover.data[train,], method="class", ntree=151);

summary(forest_fit);

#training data in-sample prediction
table(forest_fit$predict);
#confusion matrix
table(forest_fit$predict, forest.cover.data[train,]$Cover_Type_Factor);
#number of correct predictions
sum(ifelse(forest_fit$predict == forest.cover.data[train,]$Cover_Type_Factor, 1, 0));
#percentage correct
sum(ifelse(forest_fit$predict == forest.cover.data[train,]$Cover_Type_Factor, 1, 0))/dim(forest.cover.data[train,])[1];

fc <- forest.cover.data[-train,]$Cover_Type_Factor;
fc <- as.data.frame(fc);
colnames(fc)[1] <- "Cover_Type_Factor";

#predict 
library(e1071);

fc$Forest_predict <- predict(forest_fit, newdata=forest.cover.data[-train,]);

#test data out-of-sample prediction
table(fc$Forest_predict);
#confusion matrix
table(fc$Forest_predict, fc$Cover_Type_Factor);
#number of correct predictions
sum(ifelse(fc$Forest_predict == fc$Cover_Type_Factor, 1, 0));
#percentage correct
sum(ifelse(fc$Forest_predict == fc$Cover_Type_Factor, 1, 0))/dim(fc)[1];

#produce variable improtance plot
varImpPlot(forest_fit, main="Random Forest Variable Importance");

library(e1071);

#support vector machine model

svm_fit <- svm(Cover_Type_Factor ~ ., data=forest.cover.data[train,], type="C-classification", gamma=0.9, cost=10);

TrainingData_svm <- forest.cover.data[train,];

#training data in-sample prediction accuracy
TrainingData_svm$SVM_predict_in <- predict(forest_fit, newdata=forest.cover.data[train,]);

#confusion matrix
table(TrainingData_svm$SVM_predict_in, forest.cover.data[train,]$Cover_Type_Factor);
#number of correct predictions
sum(ifelse(TrainingData_svm$SVM_predict_in == forest.cover.data[train,]$Cover_Type_Factor, 1, 0));
#percentage correct
sum(ifelse(TrainingData_svm$SVM_predict_in == forest.cover.data[train,]$Cover_Type_Factor, 1, 0))/dim(forest.cover.data[train,])[1];

# predict and calculate out-of-sample accracy
fc$SVM_predict <- predict(svm_fit, newdata=forest.cover.data[-train,]);

#confusion matrix
table(fc$SVM_predict, fc$Cover_Type_Factor);
#number of correct predictions
sum(ifelse(fc$SVM_predict == fc$Cover_Type_Factor, 1, 0));
#percentage correct
sum(ifelse(fc$SVM_predict == fc$Cover_Type_Factor, 1, 0))/dim(fc)[1];


#####
#plot multiclass ROC curve
#####
# aucs = c()
auc_num <- 0
auc_tot <- 0
plot(x=NA, y=NA, xlim=c(0,1), ylim=c(0,1),
     xlab='False Positive Rate',
     ylab='True Positive Rate',
     bty='n')
legend("bottomright",cex=0.75,legend=levels(forest.cover.data$Cover_Type_Factor),text.col=seq_along(levels((forest.cover.data$Cover_Type_Factor)))+7)

for (type.id in 1:7) {
  #print(type.id)
  lvls = levels(forest.cover.data[train,"Cover_Type_Factor"])
  if ((n_distinct(forest.cover.data[train,"Cover_Type_Factor"] == lvls[type.id])) == 1) {  #skip predictions where values all the same. ROC curve will not plot.
    next
  } 
  else
  {
     type = as.factor(forest.cover.data[-train,"Cover_Type_Factor"] == lvls[type.id])    
     score = as.data.frame(pred2[,type.id])
     actual.class = forest.cover.data[-train,"Cover_Type_Factor"] == lvls[type.id]
     pred = prediction(score, actual.class)
     nbperf = performance(pred, "tpr", "fpr")
    
    roc.x = unlist(nbperf@x.values)
    roc.y = unlist(nbperf@y.values)
    lines(roc.y ~ roc.x, col=type.id+7, lwd=2)
    
    nbauc = performance(pred, "auc")
    nbauc = unlist(slot(nbauc, "y.values"))
    aucs[type.id] = nbauc
    print(paste("AUC for",lvls[type.id],"=",round(nbauc,3)))
    
    auc_tot <<- nbauc+auc_tot
    auc_num <<- auc_num+1
  }
}

lines(x=c(0,1), c(0,1))

print(paste("Average AUC for all prediction classes: ", round(auc_tot/auc_num,3)))
# mean(aucs) # AUC. calculate each individually. 
# rm(auc_num)
# rm(auc_tot)
#

#### MULTINOM LOGISTIC REGRESSION ####
forest.cover.data$Cover_Type_1 <- ifelse((forest.cover.data$Cover_Type_Factor=="Spruce_Fir"),1,-1)
forest.cover.data$Cover_Type_2 <- ifelse((forest.cover.data$Cover_Type_Factor=="Lodgepole_Pine"),1,-1)
forest.cover.data$Cover_Type_3 <- ifelse((forest.cover.data$Cover_Type_Factor== "Ponderosa_Pine"),1,-1)
forest.cover.data$Cover_Type_4 <- ifelse((forest.cover.data$Cover_Type_Factor=="Cottonwood_Willow"),1,-1)
forest.cover.data$Cover_Type_5 <- ifelse((forest.cover.data$Cover_Type_Factor=="Aspen"),1,-1)
forest.cover.data$Cover_Type_6 <- ifelse((forest.cover.data$Cover_Type_Factor=="Douglas_fir"),1,-1)
forest.cover.data$Cover_Type_7 <- ifelse((forest.cover.data$Cover_Type_Factor=="Krummholz"),1,-1)
forest.cover.data$ID<-seq.int(nrow(forest.cover.data)) # Add ID column to preserve row numbers

# split data into training set and test set
set.seed(465)
train <- (sample(1:nrow(forest.cover.data),nrow(forest.cover.data)*0.70)) # 70% train / 30% test split
plot(forest.cover.data[train,"Cover_Type_Factor"],main="unbalanced training data") # examine data

train_balanced <- c()
train_balanced <- as.data.frame(train_balanced)
spruce.obs <- 10000
lodgepole.obs <- 12500
ponderosa.obs <- 2500
cottonwood.obs <- 500
aspen.obs <- 1500
douglas.obs <- 2000
krummholz.obs <- 2250
x <- filter(forest.cover.data[train,],Cover_Type_1 ==1)
x1 <- sample(x[,"ID"],spruce.obs) 
x1<-as.data.frame(x1)
train_balanced <- rbind(train_balanced,x1)
x <- filter(forest.cover.data[train,],Cover_Type_2 ==1)
x1 <- sample(x[,"ID"],lodgepole.obs)
x1<-as.data.frame(x1)
train_balanced <- rbind(train_balanced,x1)
x <- filter(forest.cover.data[train,],Cover_Type_3 ==1)
x1 <- sample(x[,"ID"],ponderosa.obs)
x1<-as.data.frame(x1)
train_balanced <- rbind(train_balanced,x1)
x <- filter(forest.cover.data[train,],Cover_Type_4 ==1)
x1 <- sample(x[,"ID"],cottonwood.obs) # replace = T = sample with replacement because less than 2277 samples
x1<-as.data.frame(x1)
train_balanced <- rbind(train_balanced,x1)
x <- filter(forest.cover.data[train,],Cover_Type_5 ==1)
x1 <- sample(x[,"ID"],aspen.obs) 
x1<-as.data.frame(x1)
train_balanced <- rbind(train_balanced,x1)
x <- filter(forest.cover.data[train,],Cover_Type_6 ==1)
x1 <- sample(x[,"ID"],douglas.obs)
x1<-as.data.frame(x1)
train_balanced <- rbind(train_balanced,x1)
x <- filter(forest.cover.data[train,],Cover_Type_7 ==1)
x1 <- sample(x[,"ID"],krummholz.obs)
x1<-as.data.frame(x1)
train_balanced <- rbind(train_balanced,x1)
forest.cover.data <- select(forest.cover.data,-ID)
train_balanced <- as.integer(unlist(train_balanced))
rm(x)
rm(x1)

plot(forest.cover.data[train_balanced,"Cover_Type_Factor"],main="balanced training data") # examine data
table(forest.cover.data[train_balanced,"Cover_Type_Factor"]) # examine data

# Diagnostics
class(forest.cover.data[train_balanced,])
dim(forest.cover.data[train_balanced,])
# View(forest.cover.data[train_balanced,])
names(forest.cover.data[train_balanced,])
str(forest.cover.data[train_balanced,])

continuous.predictors <- c("Elevation", "Aspect", "Slope", "Horizontal_Distance_To_Hydrology", "Vertical_Distance_To_Hydrology", "Horizontal_Distance_To_Roadways", "Hillshade_9am", "Hillshade_Noon", "Hillshade_3pm", "Horizontal_Distance_To_Fire_Points") 
factor.predictors <- c("wilderness_area", "climate_zone", "geologic_zone")
indicator.predictors <- c("wilderness_areaNeota_WA", "wilderness_areaComanche_Peak_WA", "wilderness_areaCache_la_Poudre_WA", "mountain_dry", "mountain", "montain_dry_and_mountain", "mountain_and_subalpine", "subalpine", "alpine", "glacial", "mixed_sedimentary", "igneous_and_metamorphic")
binary.predictors <- c("Soil_Type2", "Soil_Type3", "Soil_Type4", "Soil_Type5", "Soil_Type6", "Soil_Type7", "Soil_Type8", "Soil_Type9", 
                       "Soil_Type10", "Soil_Type11", "Soil_Type12", "Soil_Type13", "Soil_Type14", "Soil_Type15", "Soil_Type16", "Soil_Type17", "Soil_Type18", "Soil_Type19",
                       "Soil_Type20", "Soil_Type21", "Soil_Type22", "Soil_Type23", "Soil_Type24", "Soil_Type25", "Soil_Type26", "Soil_Type27", "Soil_Type28", "Soil_Type29", 
                       "Soil_Type30", "Soil_Type31", "Soil_Type32", "Soil_Type33", "Soil_Type34", "Soil_Type35", "Soil_Type36", "Soil_Type37", "Soil_Type38", "Soil_Type39", 
                       "Soil_Type40")
all.predictors <- c(continuous.predictors, factor.predictors, binary.predictors)


# prep data to put into multinom function
multinom.predictors <- c(continuous.predictors, indicator.predictors)
multinom.predictors.rhs <- paste(multinom.predictors, collapse = "+")
forest.cover.data$multinom.response <- relevel(forest.cover.data$Cover_Type_Factor, ref = "Spruce_Fir")
multinom.model <- multinom(forest.cover.data[train_balanced,]$multinom.response ~ Elevation+Aspect+Slope+Horizontal_Distance_To_Hydrology+Vertical_Distance_To_Hydrology+Horizontal_Distance_To_Roadways+Hillshade_9am+Hillshade_Noon+Hillshade_3pm+Horizontal_Distance_To_Fire_Points+wilderness_area+climate_zone+geologic_zone, data = forest.cover.data[train_balanced,], maxit = 500)

z.test <- summary(multinom.model)$coefficients / summary(multinom.model)$standard.errors
#calculate p-values for each variable and each level of response
p.vals <- (1 - pnorm(abs(z.test), 0, 1)) * 2

#find the relative risk compared to the reference Spruce-Fir
exp(coef(multinom.model))
head(fitted(multinom.model))

train.data <- forest.cover.data[train,]
train.data$multinom.predict <- predict(multinom.model, newdata = train.data, "class")

table(train.data$multinom.predict == "Spruce_Fir", train.data$multinom.response == "Spruce_Fir")
table(train.data$multinom.predict == "Lodgepole_Pine", train.data$multinom.response == "Lodgepole_Pine")
table(train.data$multinom.predict == "Ponderosa_Pine", train.data$multinom.response == "Ponderosa_Pine")
table(train.data$multinom.predict == "Cottonwood_Willow", train.data$multinom.response == "Cottonwood_Willow")
table(train.data$multinom.predict == "Aspen", train.data$multinom.response == "Aspen")
table(train.data$multinom.predict == "Douglas_fir", train.data$multinom.response == "Douglas_fir")
table(train.data$multinom.predict == "Krummholz", train.data$multinom.response == "Krummholz")

# c("Spruce_Fir","Lodgepole_Pine","Ponderosa_Pine","Cottonwood_Willow","Aspen","Douglas_fir","Krummholz")
multinom.cm <- table(train.data$multinom.predict == "Spruce_Fir", train.data$multinom.response == "Spruce_Fir")

true.negative = multinom.cm[1,1]
false.negative = multinom.cm[1,2] 
false.positive = multinom.cm[2,1] 
true.positive = multinom.cm[2,2]
accuracy = (multinom.cm[1,1] + multinom.cm[2,2]) / nrow(train) 
precision = multinom.cm[2,2] / sum(multinom.cm[,2])
recall = multinom.cm[2,2] / sum(multinom.cm[,2])
specificity = multinom.cm[1,1] / sum(multinom.cm[,1])


#### ENSEMBLE - Training set ####
train.data$nn.predict <- predict(fit, forest.cover.data[train,])
train.data$rf.predict <- predict(rf.forest.cover,forest.cover.data1[train,],type="class") # evaluate performance on test data. see ISLR p 326.
columns.to.count <- c("rf.predict", "rf.predict", "multinom.predict")
# Make sure this is ordered by frequency, descending; we may want to have a better default for ties than "most numerous" cover type
response.levels <- c("Lodgepole_Pine", "Spruce_Fir", "Ponderosa_Pine", "Krummholz", "Douglas_fir", "Aspen", "Cottonwood_Willow")

prediction.df <- data.frame(train.data[columns.to.count])
for (i in 1:length(response.levels)) {
  col.name <- response.levels[i]
  col.vals <- apply(train.data, 1, function(x) sum(x[columns.to.count] == response.levels[i]))
  prediction.df[col.name] <- col.vals
}

# set column ensemble.predict value to the column name that has the max number of predictions
prediction.df["ensemble.predict"] <- apply(prediction.df, 1, function(x) names(which.max(x[4:10])))
prediction.df["actual.response"] <- train.data$Cover_Type_Factor

table(prediction.df$ensemble.predict, prediction.df$actual.response)
