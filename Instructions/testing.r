#testing

require(lobsterCatch)
require(devtools)
load_all('D:/git/lobsterCatch')
require(geosphere)

arena = matrix(0,12,12)
y=x=seq(1,11,2)
traps = expand.grid(x,y)

p = list()
p$nrowgrids = 12
p$ncolgrids = 12
p$ngrids=p$nrowgrids * p$ncolgrids
p$initlambda=.1
p$initD = 3
p$smult = 0.993
p$currentZoIInit = 1

p$trapEastStart = traps[,1]
p$trapNorthStart = traps[,2]
p$ntrapsstart = length(p$trapEastStart)

p$saturationThresholdStart = 5
p$how_closeStart = .1
p$dstepstart = 5 
p$trapSaturationStart = T

p$tSteps = 50

p$realizations = 1

a = SimulateLobsterMovement(p=p)

#nLobsters
uL = unique(a[[1]]$lobsters$I)
dims = length(uL)
dims = ceiling(sqrt(dims))
par( mfrow=c(dims,dims) ) # create a plot with 1 row and 2 columns to show plots side by side
for(i in uL){
with(subset(a[[1]]$lobsters, I==1),plot(EASTING, NORTHING, type='l',ylim=c(-30,30),xlim=c(-30,30)))
points(traps, pch=16, col='red')
#with(subset(outmove, I==1), text(x=EASTING[c(1,100)], y=NORTHING[c(1,100)], c('Start','End')))
}


smm = seq(0.8,.99,by=0.02)
gg = list()
for(i in 1:length(smm)){
p$smult=smm[i]
a = SimulateLobsterMovement(p=p)
gg[[i]]=c(mean(apply(a[[1]]$traps,2,max)),dispersion(apply(a[[1]]$traps,2,max)))
}
