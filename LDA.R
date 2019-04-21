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

# Converting 
for (i in 1:16){
  subset[paste("land.type",i, sep = "")] <- as.integer(subset$land.type == i)
}

# Variable Selection
landsat.sub <- regsubsets(landsat~cos(2*pi*day.of.year/365)+elevation+log(slope+1)+land.type+modis, data=subset, nvmax=18)
landsat.sb <- summary(landsat.sub)
landsat.sb

par(mfrow=c(2,3))
plot(landsat.sb$rss, xlab="Number of variables", ylab="RSS")
plot(landsat.sb$rsq, xlab="Number of variables", ylab="RSq")
plot(landsat.sb$adjr2, xlab="Number of variables", ylab="AdjR2")
points(which.max(landsat.sb$rsq), landsat.sb$adjr2[which.max(landsat.sb$rsq)], col="red", cex=2, pch=20)

plot(landsat.sb$cp, xlab="Number of variables", ylab="CP") # will select same model as AIC for regression
points(which.min(landsat.sb$cp), landsat.sb$cp[which.min(landsat.sb$cp)], col="red", cex=2, pch=20)

plot(landsat.sb$bic, xlab="Number of variables", ylab="BIC")
points(which.min(landsat.sb$bic), landsat.sb$bic[which.min(landsat.sb$bic)], col="red", cex=2, pch=20)

# BIC
num_vars <- which.min(landsat.sb$bic) # 1 variables minimizez bic
num_vars
coef <- coef(landsat.sub, num_vars)
covars <- names(coef[2:length(coef)])
covars

# CP 
num_vars <- which.min(landsat.sb$cp) # 2 variables minimizez cp
num_vars
coef <- coef(landsat.sub, num_vars)
covars <- names(coef[2:length(coef)])
covars



##### LDA 
library(MASS)

## NOTE: Had to take out land.type
#subset$slope[subset$slope == 0] <- 0.0000001 # Make slope a very small number so logging doesn't make it -Inf
#table(log(subset$slope) == -Inf)
#lda.fit <- lda(landsat~modis, data=subset, subset=train)

# BIC (NOTE: Had to remove land.type14 because LDA gave error)
f <- landsat~log(slope + 1)+land.type2+land.type3+land.type4+land.type6+land.type7+land.type8+land.type9+land.type10+land.type11+land.type12+land.type13+land.type16+modis

# CP (NOTE: Had to remove land.type14 because LDA gave error)
f <- landsat~cos(2*pi*day.of.year/365)+elevation+log(slope+1)+modis+land.type2+land.type3+land.type4+land.type5+land.type6+land.type7+land.type8+land.type9+land.type10+land.type11+land.type12+land.type13+land.type16

# Trying everything without land.type
f <- landsat~cos(2*pi*day.of.year/365)+elevation+log(slope+1)+modis+aspect

lda.fit <- lda(f, data=subset, subset=train)
lda.fit

# Plotting the fit
par(mfrow=c(1,1))
plot(lda.fit, dimen=1)

library(ggplot2)
ggplotLDAPrep <- function(x){
  if (!is.null(Terms <- x$terms)) {
    data <- model.frame(x)
    X <- model.matrix(delete.response(Terms), data)
    g <- model.response(data)
    xint <- match("(Intercept)", colnames(X), nomatch = 0L)
    if (xint > 0L) 
      X <- X[, -xint, drop = FALSE]
  }
  means <- colMeans(x$means)
  X <- scale(X, center = means, scale = FALSE) %*% x$scaling
  rtrn <- as.data.frame(cbind(X,labels=as.character(g)))
  rtrn <- data.frame(X,labels=as.character(g))
  return(rtrn)
}

fitGraph <- ggplotLDAPrep(lda.fit)
ggplot(fitGraph, aes(LD1,LD2, color=labels))+geom_point()

# Prediction
lda.pred <- predict(object=lda.fit, newdata=subset[-train,])

# Total counts of each classification
sum(lda.pred$class == 0)
sum(lda.pred$class == 50) 
sum(lda.pred$class == 100) 

head(lda.pred$class, 3)
head(lda.pred$posterior, 3)
head(lda.pred$x, 3)

#Kevin Dang Cross Validation Code.

set.seed(12)
#rand <- sample.int(15,4080535, replace = TRUE)
rand <- sample.int(15,4996, replace = TRUE)

#dat$cross.val <- rand
subset$cross.val <- rand

subset$cat <- subset$landsat
subset$cat[subset$landsat != 100 & subset$landsat!= 0] <- 50
#dat$cat <- dat$landsat
#dat$cat[dat$landsat != 100 & dat$landsat!= 0] <- 50


percerror <- rep(0,15)
for (i in 1:15 ){
  train.dat <- subset[rand != i,]
  test.dat <- subset[rand == i,]
  
  model <- lda(landsat~cos(2*pi*day.of.year/365)+elevation+modis, data=train.dat)
  predict <- predict(object=lda.fit, newdata=test.dat)
  percerror[i] <- mean( predict$class != test.dat$cat )
  
}
mean(percerror)


# Results
# BIC: 0.1159335
# CP: 0.1163914
# Everything without land.type: 0.118213



