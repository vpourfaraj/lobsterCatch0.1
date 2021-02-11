#' This function moves lobsters based on the provided parameters. A distribution of lengths
#' #has been also provided which enables the user to assign length to individuals and include the
#' #effect of size in catchability
#' @param p can be modified to check the impact of each factor (i.e p$smult = 0.9 sets the shrinkage factor)
#' @param plot is set to false by default
#' @export
SimulateLobsterMovement <- function(p=p,plot=F) {
  with(p,{
    outputs.to.return = list()
    for(k in 1:p$realizations){
      start = Sys.time()
      outputs = list()
      initialGrid = rpoisD(n=ngrids,lambda=initlambda, D = initD)
      LobsterStart = data.frame(EASTING = rep(1:ncolgrids,times=nrowgrids), NORTHING = rep(1:nrowgrids,each=ncolgrids), Lobs = initialGrid)
      LobsterStart <- subset(LobsterStart,Lobs>0)
      if(nrow(LobsterStart)>0) {
        replicateCoordinates <- function(d){ rep(d[1:2], d[3]) } #replicates coordinates for grids with more than 1 lobster
        tt <- unlist( apply(X = LobsterStart, MARGIN = 1, FUN = replicateCoordinates) )
        tt<- matrix(tt, ncol = 2, byrow = TRUE)
        colnames(tt)<- c("EASTING","NORTHING")
        initialxyCoordinate  = as.data.frame(tt)
        initialxyCoordinate$trapped = 0 # this will update as a lobster gets caught and we don't need to update movements
        initialxyCoordinate$CL <- NA # Length of lobsters
        lobsterSizeFreq <- read.csv(file = 'LobsterSizeFreqs.csv', header = TRUE, stringsAsFactors = FALSE)
        lobsterSizeFreq$prob <- lobsterSizeFreq$freq / sum(lobsterSizeFreq$freq )
        labels   <- lobsterSizeFreq$bins
        lobProb <- lobsterSizeFreq$prob
        initialxyCoordinate$CL<-sample(x = labels, size = sum(initialGrid), replace = TRUE, prob = lobProb)
        coordinatesOverTime <- list()
        coordinatesOverTime[[1]] <- initialxyCoordinate
        currentZoI<- currentZoIInit
        s =  smult
        ntraps = ntrapsstart
        trapCoordinates = data.frame(EASTING=trapEastStart,NORTHING=trapNorthStart)
        trapCatch = list()
        lobSize   = list() #added to store the size of caught lobster
        trapCatch[[1]] = rep(0,length=ntraps)
        lobSize[[1]]   = rep('',length=ntraps)
        outputs$traps = rep(0,times=ntrapsstart)
        outputs$lobsters = data.frame(EASTING=0,NORTHING=0,trapped=0,T=0,I=0)

        for(t in 2:tSteps){
          if(t>2) currentZoI<- currentZoI * smult

          ko = updateGridLB( lobsterCoordinates = coordinatesOverTime[[t-1]], trapCoordinates=trapCoordinates, trapCatch=trapCatch[[t-1]], lobSize = lobSize[[t-1]], currentZoI = currentZoI, saturationThreshold=saturationThresholdStart, trapSaturation= trapSaturationStart, how_close=how_closeStart,dstep=dstepstart, lengthBased = lengthBased)
          coordinatesOverTime[[t]] <- ko[[1]]
          trapCatch[[t]] <- ko[[2]]
          lobSize[[t]]   <- ko[[3]]
        }
      }
      outmove = do.call(rbind,coordinatesOverTime)
      outmove$T = rep(0:(tSteps-1), each=nrow(tt))
      outmove$I = rep(1:nrow(tt), times=tSteps)
      outtraps = as.data.frame(do.call(rbind, trapCatch))
      outputs$traps = outtraps
      outputs$lobsters = outmove
      outputs$lobSize = lobSize #Not sure if this is working properly yet
      outputs.to.return[[k]] = outputs
      print(paste('Timing', Sys.time()-start, 'for iteration #',k,sep=" "))
    }
    return(outputs.to.return)

  })}
