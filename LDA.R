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

subset <- data.frame(dat$landsat[sub.i], dat$day.of.year[sub.i], dat$elevation[sub.i], dat$slope[sub.i], dat$land.type[sub.i], dat$modis[sub.i])
colnames(subset) <- c("landsat", "day.of.year", "elevation", "slope", "land.type", "modis")
#y.test <- dat$landsat[!sub]

middle_indices <- which(subset$landsat != 0 & subset$landsat != 100)
subset$landsat[middle_indices] <- 50
subset$landsat

# Creating training vector
train <- sample(1:n, n/2)

subset$landsat[train]

# Using forward stepwise selection on the model without transformations
#wo_trans.fwd.sub <- regsubsets(landsat~.,data=subset,nvmax=5,method="forward")
#summary(wo_trans.fwd.sub)
#wo_trans.sb <- summary(wo_trans.fwd.sub)
#par(mfrow=c(2,3))
#plot(wo_trans.sb$rss)
#plot(wo_trans.sb$rsq)
#plot(wo_trans.sb$adjr2)
#plot(wo_trans.sb$cp) # will select same model as AIC for regression
#plot(wo_trans.sb$bic)

# LDA 
library(MASS)

## NOTE: Had to take out land.type
lda.fit <- lda(landsat~slope, data=subset, subset=train)
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

# Total counts of each classification
sum(lda.pred$class == 0)
sum(lda.pred$class == 50) 
sum(lda.pred$class == 100) 

# Prediction
lda.pred <- predict(object=lda.fit, newdata=subset[-train,])

head(lda.pred$class, 3)
head(lda.pred$posterior, 3)
head(lda.pred$x, 3)

library(pROC)
r.lda <- multiclass.roc(lda.pred$class~lda.pred$posterior[,2])
summary(r.lda)

# Plotting each roc curve (pairs each off 0 to 50, 0 to 100, 50 to 100)
# Order: Black, red, green
r.lda['rocs'][[1]]
plot.roc(r.lda['rocs'][[1]][[1]])
sapply(2:length(r.lda['rocs'][[1]]),function(i) lines.roc(r.lda['rocs'][[1]][[i]],col=i))
