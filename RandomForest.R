library(randomForest)
library(leaps)

load("/Users/Ryan_Loi/Dropbox/APPM4580 (Statistical Learning)/SnowCoverData.RData")
n <- 5000

names(dat)
dim(dat)

# Grab uniform index values for subset of the data
set.seed(1)

sv <- runif(n, min = 1, max = 4080535)
sv

sub.i <- logical(4080535) 
sub.i[sv] <- TRUE  

subset <- data.frame(dat$landsat[sub.i], dat$day.of.year[sub.i], dat$elevation[sub.i], dat$slope[sub.i], dat$aspect[sub.i], dat$land.type[sub.i], dat$modis[sub.i])
colnames(subset) <- c("landsat", "day.of.year", "elevation", "slope", "aspect","land.type", "modis")
#subset <- data.frame(dat$landsat[sub.i], dat$day.of.year[sub.i], dat$elevation[sub.i], dat$slope[sub.i], dat$modis[sub.i])
#colnames(subset) <- c("landsat", "day.of.year", "elevation", "slope", "modis")
#y.test <- dat$landsat[!sub]

middle_indices <- which(subset$landsat != 0 & subset$landsat != 100)
subset$landsat[middle_indices] <- 50
subset$landsat

# Creating training vector
train <- sample(1:n, n/2)

subset$landsat[train]

# Random forests

rf.fit <- randomForest(landsat~.,data=dat,subset=train,mtry=3,importance =TRUE)

varImpPlot(rf.fit)

#####Kevin Dang Cross Validation Code.

set.seed(12)
#set.seed(Sys.time()) # to test with random seeds
#rand <- sample.int(15,4080535, replace = TRUE)
rand <- sample.int(15,4996, replace = TRUE)

#dat$cross.val <- rand
subset$cross.val <- rand

subset$cat <- subset$landsat
subset$cat[subset$landsat != 100 & subset$landsat!= 0] <- 50
#dat$cat <- dat$landsat
#dat$cat[dat$landsat != 100 & dat$landsat!= 0] <- 50


percerror <- rep(NA,15)
for (i in 1:15 ){
  #train.dat <- dat[rand != i,]
  #test.dat <- dat[rand == i,]
  
  train.dat <- subset[rand != i,]
  test.dat <- subset[rand == i,]
  
  test.dat$landsat = as.factor(test.dat$landsat)
  train.dat$landsat = as.factor(train.dat$landsat)
  
  model <- randomForest(y=train.dat$landat, x=train.dat[c(-8,-9)],data=train.dat,mtry=3,importance=TRUE)
  predict <- predict(object=model, newdata=test.dat)
  percerror[i] <- mean( predict != test.dat$cat )
  print(percerror)
}
mean(percerror)

