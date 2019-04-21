#Kevin Dang Cross Validation Code.


set.seed(12)
rand <- sample.int(15,4080535, replace = TRUE)

dat$cross.val <- rand

dat$cat <- dat$landsat
dat$cat[dat$landsat != 100 & dat$landsat!= 0] <- 50


percerror <- rep(0,15)
for (i in 1:15 ){
  train.dat <- dat[rand != i,]
  test.dat <- dat[rand == i,]
  
  #trainmodel <- ( dat = train.dat )
  #predict <-predict( dat = test.dat, model = trainmodel )
  percerror[i] <- mean( predict != test.dat$cat )

}
mean(percerror)





