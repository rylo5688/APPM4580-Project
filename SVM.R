library(e1071)
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

middle_indices <- which(subset$landsat != 0 & subset$landsat != 100)
subset$landsat[middle_indices] <- 50
subset$landsat <- as.factor(subset$landsat)
subset$landsat

# Creating training vector
train <- sample(1:n, n/2)

subset$landsat[train]

# Converting 
for (i in 1:16){
  subset[paste("land.type",i, sep = "")] <- as.integer(subset$land.type == i)
}

subset <- subset[-6]


#### Multiclass SVM
# BIC (NOTE: Had to remove land.type13, land.type14 because SVM gave error)
# Accuracy: 0.8886664
f <- landsat~log(slope + 1)+land.type2+land.type3+land.type4+land.type6+land.type7+land.type8+land.type9+land.type10+land.type11+land.type12+land.type16+modis

# CP (NOTE: Had to remove land.type14 because LDA gave error)
# Accuracy: 0.8878654
f <- landsat~cos(2*pi*day.of.year/365)+elevation+log(slope+1)+modis+land.type2+land.type3+land.type4+land.type5+land.type6+land.type7+land.type8+land.type9+land.type10+land.type11+land.type12+land.type13+land.type16

# Trying everything without land.type
# Accuracy: 0.8862635
f <- landsat~cos(2*pi*day.of.year/365)+elevation+log(slope+1)+modis+aspect

fit.svmr <- svm(f,data=subset[train,],kernel="radial",cost=2.37899, gamma=0.04761905)

# tuning
tune.out <- tune(svm,f,data=subset[train,],kernel="radial", ranges=list(cost=seq(0.01,5,length.out=100)))
fit.svmr <- tune.out$best.model

plot(fit.svmr, subset[train,], day.of.year~log(slope + 1))


svmr.pred <- predict(fit.svmr, subset[-train,])
tab <- table(subset[-train,]$landsat, svmr.pred)
tab

# Accuracy
sum(diag(tab))/nrow(subset[-train,])


#####Kevin Dang Cross Validation Code.

set.seed(12)
#set.seed(Sys.time()) # to test with random seeds
rand <- sample.int(15,4080535, replace = TRUE)
#rand <- sample.int(15,4996, replace = TRUE)

dat$cross.val <- rand
#subset$cross.val <- rand

#subset$cat <- subset$landsat
#subset$cat[subset$landsat != 100 & subset$landsat!= 0] <- 50
dat$cat <- dat$landsat
dat$cat[dat$landsat != 100 & dat$landsat!= 0] <- 50

#f <- landsat~cos(2*pi*day.of.year/365)+elevation+log(slope+1)+modis+aspect
# cost: 4.949596
# gamma: 0.2 
percerror <- rep(NA,15)
for (i in 1:15 ){
  print(i)
  train.dat <- dat[rand != i,]
  test.dat <- dat[rand == i,]
  
  test.dat$landsat = as.factor(test.dat$landsat)
  train.dat$landsat = as.factor(train.dat$landsat)
  
  # tuning
  model <- svm(f,data=subset[train,],kernel="radial",cost=4.949596, gamma=0.2)
  predict <- predict(object=model, newdata=test.dat)
  percerror[i] <- mean( predict != test.dat$cat )
  print(percerror)
}
mean(percerror)

# Results
#f <- landsat~cos(2*pi*day.of.year/365)+elevation+log(slope+1)+modis+aspect
# cost: 4.949596
# gamma: 0.2 
# mean percent error = 0.1126466
