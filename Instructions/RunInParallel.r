require(bio.lobster)
require(parallel)
require(lobsterCatch)
require(devtools)
load_all('D:/git/lobsterCatch')

arena = matrix(0,200,200)
y=x=seq(5,195,10)
traps = expand.grid(x,y)

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

p$tSteps = 1


p$realizations = 1
dispersionSaturation = c()
meanCatchWithSat = c()
smult_start = seq(.9,1,length.out=8)

plist = list()

for(i in 1:length(smult_start)){
	pp = p
	pp$smult = smult_start[[i]]
	plist[[i]]=pp
}
#this works in linux
out = mclapply(X=plist, FUN=SimulateLobsterMovement, mc.cores=length(plist))

#in windows machine

nCores = detectCores()
cl <- makeCluster(nCores)
clusterExport(cl,{require(lobsterCatch)
				  require(bio.lobser)
				  })
