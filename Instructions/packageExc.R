# In order to load the latest version of the package:
devtools::install_github('vpourfaraj/lobsterCatch',ref='main')
#Once the package is installed, data in extdata folder can be accessed using this code:
system.file("extdata", "LobsterSizeFreqs.csv", package = "lobsterCatch")

library(lobsterCatch)
#initialize a parameter file to pass info into the code and then put all into a function

p = list()
p$nrowgrids = 10
p$ncolgrids = 10
p$ngrids=p$nrowgrids * p$ncolgrids
p$initlambda=.1
p$initD = 3
p$smult = 0.993
p$currentZoIInit = 1
p$trapSaturationStart = T

p$trapEastStart = c(5,3,4)
p$trapNorthStart = c(5,3,4)
p$ntrapsstart = length(p$trapEastStart)

p$saturationThresholdStart = 5
p$how_closeStart = 1
p$dstepstart = 5

p$niter =100 #it's not being used anymore?

p$realizations=5 #number of iterations/simulations
p$tSteps=10  #timesteps per iteration

p$lengthBased= TRUE
#run the model
a = SimulateLobsterMovement(p=p)

plot(1:p$tSteps,a[[1]]$traps[,3],xlab='Time',ylab='N Caught')

#lets change a parameter
p$saturationThresholdStart=10

# rerun
b = SimulateLobsterMovement(p=p)

lines(1:p$niter,b$traps[,1])

#or just run it a bunch of times since the model is stochastic
p$saturationThresholdStart = 5
time.to.max=list()
max.catch = list()
realizations = 50
plot(1:p$niter,xlab='Time',ylab='N Caught',ylim=c(0,15),type='n')

for(i in 1:realizations){
  a = SimulateLobsterMovement(p=p)
  for(j in 1:ncol(a$traps)){
    lines(1:p$niter,a$traps[,j])
  }
  time.to.max[[i]] = apply(a$traps,2, which.max)
  max.catch[[i]] = apply(a$traps,2,max)
}
time.to.max = do.call(rbind,time.to.max)
max.catch = do.call(rbind,max.catch)

#calculating dispersion
disp = apply(max.catch,1,dispersion)
mean(disp)


#next trial changing saturation
p$saturationThresholdStart = 8
time.to.max8=c()
max.catch8 = c()
time.to.max8=list()
max.catch8 = list()
realizations = 50

plot(1:p$niter,xlab='Time',ylab='N Caught',ylim=c(0,15),type='n')

for(i in 1:realizations){
  a = SimulateLobsterMovement(p=p)
  for(j in 1:ncol(a$traps)){
    lines(1:p$niter,a$traps[,j])
  }
  time.to.max8[[i]] = apply(a$traps,2, which.max)
  max.catch8[[i]] = apply(a$traps,2,max)
}
p = list()
p$nrowgrids = 10
p$ncolgrids = 10
p$ngrids=p$nrowgrids * p$ncolgrids
p$initlambda=.1
p$initD = 3
p$smult = 0.993
p$currentZoIInit = 1

p$trapEastStart = c(5,2,7)
p$trapNorthStart = c(5,2,7)
p$ntrapsstart = length(p$trapEastStart)

p$saturationThresholdStart = 5
p$how_closeStart = .01
p$dstepstart = 5
p$trapSaturationStart = T

p$niter =100


realizations = 200
dispersionSaturation = c()
meanCatchWithSat = c()
smult_start = seq(.9,1,by=.01)

for(j in 1:length(smult_start)){
  print(smult_start[j])
  max.catchSat = list()
  max.catchnoSat = list()
  p$smult = smult_start[j]
  for(i in 1:realizations){
    a = SimulateLobsterMovement(p=p)
    if(any(a$traps>0)) max.catchSat[[i]] = apply(a$traps,2,max)
  }
  max.catchSat = do.call(rbind,max.catchSat)

  if(p$ntrapsstart==1) meanSat = apply(max.catchSat,2,mean)
  if(p$ntrapsstart>1) meanSat = apply(max.catchSat,1,mean)
  meanCatchWithSat = c(meanCatchWithSat,mean(meanSat)  )

  if(p$ntrapsstart==1) dispSat = apply(max.catchSat,2,dispersion)
  if(p$ntrapsstart>1) dispSat = apply(max.catchSat,1,dispersion)
  dispersionSaturation = c(dispersionSaturation,mean(na.omit(dispSat))  )
}

plot(smult_start,dispersionSaturation,ylim=c(0,2),type = 'b')

plot(smult_start,meanCatchWithSat,type = 'b')

time.to.max8 = do.call(rbind,time.to.max8)
max.catch8 = do.call(rbind,max.catch8)


#What impact does saturation have on mean catch and dispersion index

#base
p = list()
p$nrowgrids = 10
p$ncolgrids = 10
p$ngrids=p$nrowgrids * p$ncolgrids
p$initlambda=.1
p$initD = 3
p$smult = .97 #varying shrinkage factor
p$currentZoIInit = 1

p$trapEastStart = c(5,4,3)
p$trapNorthStart = c(5,4,3)
p$ntrapsstart = length(p$trapEastStart)

p$saturationThresholdStart = 5
p$how_closeStart = 1
p$dstepstart = 5
p$trapSaturationStart = T
p$niter =100


# Densities
lambda = c(.06,.1,.2,.5,1,1.6)


realizations = 10
dispersionSaturation = c()
dispersionNoSaturation = c()
meanCatchWithSat = c()
meanCatchNoSat = c()

for(j in 1:length(lambda)){
  for (m in 1:length(p$smult)) {
    print(lambda[j])
    print(p$smult[m])
    max.catchSat = list()
    max.catchnoSat = list()
    p$initlambda = lambda[j]
    for(i in 1:realizations){
      print(paste(j,i,sep='-'))
      p$trapSaturationStart = T
      a = SimulateLobsterMovement(p=p)
      if(any(a$traps>0)) max.catchSat[[i]] = apply(a$traps,2,max)
      p$trapSaturationStart = F
      b = SimulateLobsterMovement(p=p)
      if(any(b$lobster>0))max.catchnoSat[[i]] = apply(b$traps,2,max)
    }
    max.catchSat = do.call(rbind,max.catchSat)
    max.catchnoSat = do.call(rbind,max.catchnoSat)

    if(p$ntrapsstart==1) meanSat = apply(max.catchSat,2,mean)
    if(p$ntrapsstart>1) meanSat = apply(max.catchSat,1,mean)
    meanCatchWithSat = c(meanCatchWithSat,mean(meanSat)  )

    if(p$ntrapsstart==1) meanNoSat = apply(max.catchnoSat,2,mean)
    if(p$ntrapsstart>1) meanNoSat = apply(max.catchnoSat,1,mean)
    meanCatchNoSat = c(meanCatchNoSat,mean(meanNoSat)  )

    if(p$ntrapsstart==1) dispSat = apply(max.catchSat,2,dispersion)
    if(p$ntrapsstart>1) dispSat = apply(max.catchSat,1,dispersion)
    dispersionSaturation = c(dispersionSaturation,mean(na.omit(dispSat))  )

    if(p$ntrapsstart==1) dispnoSat = apply(max.catchnoSat,2,dispersion)
    if(p$ntrapsstart>1) dispnoSat = apply(max.catchnoSat,1,dispersion)
    dispersionNoSaturation = c(dispersionNoSaturation, mean(na.omit(dispnoSat))  )
  }
}


#calculating dispersion & mean

plot(lambda,dispersionSaturation,ylim=c(0,26),type = 'b')
lines(lambda,dispersionNoSaturation,ylim=c(0,4),type = 'b',col='red')

#################################just shrinkage factor

p = list()
p$nrowgrids = 10
p$ncolgrids = 10
p$ngrids=p$nrowgrids * p$ncolgrids
p$initlambda=.1
p$initD = 3
p$smult = 0.993
p$currentZoIInit = 1

p$trapEastStart = c(5,2,7)
p$trapNorthStart = c(5,2,7)
p$ntrapsstart = length(p$trapEastStart)

p$saturationThresholdStart = 5
p$how_closeStart = .01
p$dstepstart = 5
p$trapSaturationStart = T

p$niter =100


realizations = 200
dispersionSaturation = c()
meanCatchWithSat = c()
smult_start = seq(.9,1,by=.01)

for(j in 1:length(smult_start)){
  print(smult_start[j])
  max.catchSat = list()
  max.catchnoSat = list()
  p$smult = smult_start[j]
  for(i in 1:realizations){
    a = SimulateLobsterMovement(p=p)
    if(any(a$traps>0)) max.catchSat[[i]] = apply(a$traps,2,max)
  }
  max.catchSat = do.call(rbind,max.catchSat)

  if(p$ntrapsstart==1) meanSat = apply(max.catchSat,2,mean)
  if(p$ntrapsstart>1) meanSat = apply(max.catchSat,1,mean)
  meanCatchWithSat = c(meanCatchWithSat,mean(meanSat)  )

  if(p$ntrapsstart==1) dispSat = apply(max.catchSat,2,dispersion)
  if(p$ntrapsstart>1) dispSat = apply(max.catchSat,1,dispersion)
  dispersionSaturation = c(dispersionSaturation,mean(na.omit(dispSat))  )
}

plot(smult_start,dispersionSaturation,ylim=c(0,2),type = 'b')

plot(smult_start,meanCatchWithSat,type = 'b')

#simulate based on parameters from  paper

arena = matrix(0,200,200)
y=x=seq(5,195,10)
traps = expand.grid(x,y)
plotit=F
if(plot.arena){
  require(plot.matrix)
  plot(arena)
  points(traps)
}


p = list()
p$nrowgrids = 200
p$ncolgrids = 200
p$ngrids=p$nrowgrids * p$ncolgrids
p$initlambda=.1
p$initD = 3
p$smult = 0.993
p$currentZoIInit = 1

p$trapEastStart = traps[,1]
p$trapNorthStart = traps[,2]
p$ntrapsstart = length(p$trapEastStart)

p$saturationThresholdStart = 5
p$how_closeStart = .01
p$dstepstart = 5
p$trapSaturationStart = T

p$niter =100


realizations = 1
dispersionSaturation = c()
meanCatchWithSat = c()
smult_start = seq(.9,1,by=.01)

for(j in 1:length(smult_start)){
  print(smult_start[j])
  max.catchSat = list()
  max.catchnoSat = list()
  p$smult = smult_start[j]
  for(i in 1:realizations){
    a = SimulateLobsterMovement(p=p)
    if(any(a$traps>0)) max.catchSat[[i]] = apply(a$traps,2,max)
  }
  max.catchSat = do.call(rbind,max.catchSat)

  if(p$ntrapsstart==1) meanSat = apply(max.catchSat,2,mean)
  if(p$ntrapsstart>1) meanSat = apply(max.catchSat,1,mean)
  meanCatchWithSat = c(meanCatchWithSat,mean(meanSat)  )

  if(p$ntrapsstart==1) dispSat = apply(max.catchSat,2,dispersion)
  if(p$ntrapsstart>1) dispSat = apply(max.catchSat,1,dispersion)
  dispersionSaturation = c(dispersionSaturation,mean(na.omit(dispSat))  )
}

plot(smult_start,dispersionSaturation,ylim=c(0,2),type = 'b')

plot(smult_start,meanCatchWithSat,type = 'b')
