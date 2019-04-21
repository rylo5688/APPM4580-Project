

load("/Users/Ryan_Loi/Dropbox/APPM4580 (Statistical Learning)/SnowCoverData.RData")

summary(dat)
str(dat)
set.seed(1)
sv<- runif(1000,min = 1, max = 4080535)


pairs(landsat~., data = dat[sv,])
pairs(landsat~ sin( (slope)*pi/180  ) + log(slope) + slope, data = dat[sv,])

#Overall Frequency
par(mfrow=c(1,2))
hist(dat$landsat)
hist(dat$landsat[dat$landsat != 0 & dat$landsat != 100 ])


#Seasonality Effects

# Finding modes of each category
table(dat$day.of.year[dat$landsat == 0])
mode0 <- as.integer(names(which.max(table(dat$day.of.year[dat$landsat == 0]))))
mode50 <- as.integer(names(which.max(table(dat$day.of.year[dat$landsat != 100 & dat$landsat!= 0]))))
mode100 <- as.integer(names(which.max(table(dat$day.of.year[dat$landsat == 100]))))

# Centering around each mode
x <- rep(0, 4080535)
x[dat$landsat == 0] <- dat$day.of.year[dat$landsat == 0] - mode0
x[dat$landsat == 100] <- dat$day.of.year[dat$landsat == 100] - mode0
x[dat$landsat != 100 & dat$landsat!= 0] <- dat$day.of.year[dat$landsat != 100 & dat$landsat!= 0] - mode0

table(x)
x[x < -182] <- x[x < -182] + 365
x[x > 182] <- x[x > 182] - 365
table(x[dat$landsat == 0])

# Original day of year histograms
par(mfrow=c(1,3))
hist(dat$day.of.year[dat$landsat == 0],ylim = c(0,600000) )
hist(dat$day.of.year[dat$landsat == 100],ylim =  c(0,600000),  breaks = seq(0,365,by = 15) )
hist(dat$day.of.year[dat$landsat != 100 & dat$landsat!= 0],ylim =  c(0,600000)  )


landsa# Centering around the mode histogram
hist(x[dat$landsat == 0] , ylim = c(0,600000) )
hist(x[dat$landsat == 100],ylim =  c(0,600000) )
hist(x[dat$landsat != 100 & dat$landsat!= 0] ,ylim =  c(0,600000) )

# Histogram with cosine transformation
cos(2*pi*dat$day.of.year[dat$landsat == 0]/365)
table(dat$day.of.year)
par(mfrow=c(1,3))
hist(cos(dat$day.of.year[dat$landsat == 0]/365), ylim = c(0,600000) )
hist(cos(dat$day.of.year[dat$landsat == 100]/365), ylim =  c(0,600000) )
hist(cos(dat$day.of.year[dat$landsat != 0 & dat$landsat != 100]/365) ,ylim =  c(0,600000) )

#Elevation Effects
par(mfrow=c(1,3))
hist(dat$elevation[dat$landsat == 0], ylim = c(0,400000))
hist(dat$elevation[dat$landsat == 100],ylim = c(0,400000))
hist(dat$elevation[dat$landsat != 100 & dat$landsat!= 0],ylim =  c(0,600000) )


#Slope Effects
par(mfrow=c(1,3))
hist(dat$slope[dat$landsat == 0], ylim = c(0,800000))
hist(dat$slope[dat$landsat == 100],ylim = c(0,800000))
hist(dat$slope[dat$landsat != 100 & dat$landsat!= 0],ylim =  c(0,600000) )

#Slope Effects log transformation
par(mfrow=c(1,3))
hist(log(dat$slope[dat$landsat == 0]+1), ylim = c(0,800000))
hist(log(dat$slope[dat$landsat == 100]+1),ylim = c(0,800000))
hist(log(dat$slope[dat$landsat != 100 & dat$landsat!= 0]+1),ylim =  c(0,600000) )






#Aspect
par(mfrow=c(1,3))
hist(dat$aspect[dat$landsat == 0], ylim = c(0,200000))
hist(dat$aspect[dat$landsat == 100],ylim = c(0,200000)) 
hist(dat$aspect[dat$landsat != 100 & dat$landsat!= 0],ylim =  c(0,600000) )

#These graphs almost follow a sine graph pattern
hist(dat$aspect)

for(i in (0:23)*15){
  hist(dat$landsat[ dat$aspect <= i & dat$aspect >= i+15 ] ,ylim = c(0,200000),breaks = 15 , main = paste("Aspect between"),as.character(i),"-", as.character(i+15) )
  Sys.sleep(0.5)
}



par(mfrow=c(1,1))
boxplot(landsat~land.type, data=dat[sv,])
boxplot(landsat~land.type, data=dat ,plot = FALSE)


n<- 4
par(mfrow=c(2,n))
for(i in 1:(n*2) ){
  hist(dat$landsat[ (dat$landsat == 0 | dat$landsat == 100) & dat$land.type == i] , nclass = 2, main = paste("Land Type", as.character(i), " ends" ))
}

for(i in 1:(n*2) ){
  hist(dat$landsat[ (dat$landsat == 0 | dat$landsat == 100) & dat$land.type == (i+8)] , nclass = 2, main = paste("Land Type", as.character(i+8), " ends" ))
}


par(mfrow=c(2,n))
for(i in 1:(n*2) ){
  hist(dat$landsat[ dat$landsat != 0 & dat$landsat != 100 & dat$land.type == i], main = paste("Land Type", as.character(i) ) )
}


# Modis histograms
par(mfrow=c(1,3))
hist((dat$modis[dat$landsat == 0]),ylim = c(0,600000) )
hist((dat$modis[dat$landsat == 100]),ylim = c(0,600000) )
hist((dat$modis[dat$landsat != 0 & dat$landsat != 100])^3,ylim = c(0,600000) )


