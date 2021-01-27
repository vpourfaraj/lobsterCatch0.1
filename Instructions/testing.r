#testing

require(lobsterCatch)
require(devtools)
load_all('~/git/lobsterCatch')
require(geosphere)

arena = matrix(0,10,10)
y=x=seq(1,9,3)
traps = expand.grid(x,y)

p = list()
p$nrowgrids = 10
p$ncolgrids = 10
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
p$dstepstart = 15 
p$trapSaturationStart = T

p$tSteps = 100

p$realizations = 1

a = SimulateLobsterMovement(p=p)

#nLobsters
uL = unique(a[[1]]$lobsters$I)
dims = length(uL)
dims = ceiling(sqrt(dims))
par( mfrow=c(dims,dims) ) # create a plot with 1 row and 2 columns to show plots side by side
for(i in uL){
with(subset(a[[1]]$lobsters, I==i),plot(EASTING, NORTHING, type='l',ylim=c(-30,30),xlim=c(-30,30)))
points(traps, pch=16, col='red')
#with(subset(outmove, I==1), text(x=EASTING[c(1,100)], y=NORTHING[c(1,100)], c('Start','End')))
}
