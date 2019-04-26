
# load("C:/Users/kevin/Documents/Uni/2019 S1 (Boulder)/APPM 4580/Final Project/SnowCoverData.RData")
load("C:/Users/keda0185/Downloads/R Assign/SnowCoverData.RData")



library(glmulti)

#summary(dat)
#str(dat)
set.seed(1)
sv<- runif(5000,min = 1, max = 4080535)

dat$cat <- dat$landsat
dat$cat[dat$landsat != 100 & dat$landsat!= 0] <- 50
table(dat$cat)

#hist(dat.of.year~)


dat$day <- cos(2*pi*(dat$day.of.year - 50)/365)
dat$sin.slope <- sin( (dat$slope)*pi/180 )


#glmulti(I(cat == 0 )~., data = dat[,-c(1,2)] , family = binomial , level = 1, maxsize = 7,confsetsize = 1, crit = aic)

fit <- glm( I(cat == 0 )~ cos(2*pi*(day.of.year - 50)/365) + elevation + slope + aspect + land.type + modis  ,data=dat,family=binomial, subset = sv) 
summary(fit)


##############1st step, no snow vs some snow

fullmod <- glm( I(cat == 0 ) ~., data = dat[,-c(1,2,10)] , family = binomial, subset = sv)
bwd <- step(fullmod)
summary(bwd)


nullmod <-  glm( I(cat == 0 ) ~ 1, data = dat[,-c(1,2,10)] , family = binomial, subset = sv)
fwd <-  step(nullmod, scope=list(lower=formula(nullmod),upper=formula(fullmod)),
             direction="forward")

summary(fwd)

formula(fwd)
formula(bwd)

summary(bwd)$aic
summary(fwd)$aic


#Trying again with sin.slope
fullmod <- glm( I(cat == 0 ) ~., data = dat[,-c(1,2,4)] , family = binomial, subset = sv)
bwd2 <- step(fullmod)
summary(bwd2)


nullmod <-  glm( I(cat == 0 ) ~ 1, data = dat[,-c(1,2,4)] , family = binomial, subset = sv)
fwd2 <-  step(nullmod, scope=list(lower=formula(nullmod),upper=formula(fullmod)),
              direction="forward")
summary(fwd2)

formula(fwd2)
formula(bwd2)

summary(bwd2)$aic
summary(fwd2)$aic


########################Cross Validation Time#############################
set.seed(12)
rand <- sample.int(15,4080535, replace = TRUE)

dat$cross.val <- rand

# dat$cat <- dat$landsat
# dat$cat[dat$landsat != 100 & dat$landsat!= 0] <- 50


percerror <- rep(NA,15)
for (i in 1:15 ){
  train.dat <- dat[rand != i,]
  test.dat <- dat[rand == i,]
  predict <- rep(0, nrow(test.dat))
  #trainmodel <- glm( I(cat == 0) ~ modis + land.type + day + elevation + slope, data = train.dat , family = binomial) 
  trainmodel <- glm( I(cat == 0) ~ modis + land.type + day + elevation + sin.slope, data = train.dat , family = binomial) 
  predict <- (predict(object = trainmodel, dat = test.dat,  type = "response" ) > 0.5)
  percerror[i] <- mean( predict != (test.dat$landsat == 0) )
  print(percerror)
  
}
mean(percerror)


### RESULTS:
# slope: 0.5895779
# sin.slope: 0.589606


###Step 2 100 Snow vs <100 Snow

cat.z <- which(dat[,"cat"] != 0,)


fullmod <- glm( I(cat == 100 ) ~., data = dat[cat.z,-c(1,2,10)] , family = binomial, subset = sv) 
bwd3 <- step(fullmod)
summary(bwd3)


nullmod <-  glm( I(cat == 100 ) ~ 1, data = dat[cat.z,-c(1,2,10)] , family = binomial, subset = sv)
fwd3 <-  step(nullmod, scope=list(lower=formula(nullmod),upper=formula(fullmod)),
              direction="forward")

summary(fwd3)

formula(fwd3)
formula(bwd3)

summary(bwd3)$aic
summary(fwd3)$aic



fullmod <- glm( I(cat == 100 ) ~., data = dat[cat.z,-c(1,2,4)] , family = binomial, subset = sv) 
bwd4 <- step(fullmod)
#summary(bwd4)


nullmod <-  glm( I(cat == 100 ) ~ 1, data = dat[cat.z,-c(1,2,4)] , family = binomial, subset = sv)
fwd4 <-  step(nullmod, scope=list(lower=formula(nullmod),upper=formula(fullmod)),
              direction="forward")

summary(fwd4)

formula(fwd4)
formula(bwd4)

summary(bwd4)$aic
summary(fwd4)$aic

#######Cross-Validation Time No.2 #################


set.seed(12)
rand <- sample.int(15,4080535, replace = TRUE)

dat$cross.val <- rand

# dat$cat <- dat$landsat
# dat$cat[dat$landsat != 100 & dat$landsat!= 0] <- 50

dat2 <- dat[cat.z,]

percerror <- rep(NA,15)
for (i in 1:15 ){
  train.dat <- dat2[rand != i,]
  test.dat <- dat2[rand == i,]
  predict <- rep(0, nrow(test.dat))
  
  trainmodel <- glm( I(cat == 100) ~ land.type + modis + day , data = train.dat , family = binomial) 
  predict <- (predict(object = trainmodel, test.dat,  type = "response" ) > 0.5)
  percerror[i] <- table(as.numeric( predict != (test.dat$landsat == 100) )  )[["1"]] / nrow(test.dat)
  print(nrow(test.dat))
  print(length(predict))
  print(percerror)
  
}
mean(percerror)

#results
# > mean(percerror)
# [1] 0.06691615






