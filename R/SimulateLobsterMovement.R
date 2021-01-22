#' This function moves lobsters based on the provided parameters.
#' @param p can be modified to check the impact of each factor (i.e p$smult = 0.9 sets the shrinkage factor)
#' @param plot is set to false by default
#' @return a list
#' @export
SimulateLobsterMovement <- function(p=p,plot=F) {
  with(p,{
    start = Sys.time()
    outputs.to.return = list()
    for(k in 1:p$realizations){
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
      coordinatesOverTime <- list()
      coordinatesOverTime[[1]] <- initialxyCoordinate
      currentZoI<- currentZoIInit
      s =  smult
      ntraps = ntrapsstart
      trapCoordinates = data.frame(EASTING=trapEastStart,NORTHING=trapNorthStart)
      trapCatch = list()
      trapCatch[[1]] = rep(0,length=ntraps)
      outputs$traps = rep(0,times=ntrapsstart)
      outputs$lobsters = data.frame(EASTING=0,NORTHING=0,trapped=0,T=0,I=0)
 
     for(t in 2:tSteps){
        if(t>2) currentZoI<- currentZoI * s
        ko = updateGrid( lobsterCoordinates = coordinatesOverTime[[t-1]], trapCoordinates=trapCoordinates, trapCatch=trapCatch[[t-1]], currentZoI = currentZoI,saturationThreshold=saturationThresholdStart,trapSaturation= trapSaturationStart, how_close=how_closeStart,dstep=dstepstart)
        coordinatesOverTime[[t]] <- ko[[1]]
        trapCatch[[t]] <- ko[[2]]
        plot=FALSE
        if(plot){
          par( mfrow=c(1,2) ) # create a plot with 1 row and 2 columns to show plots side by side
          plot( coordinatesOverTime[[t-1]], xlim = c(0,10), ylim = c(0,10), main = paste0('Time = ', t-1) )
          points(x = 5, y = 5, col = 'red')
          plot( coordinatesOverTime[[t]],   xlim = c(0,10), ylim = c(0,10), main = paste0('Time = ', t) )
          points(x = 5, y = 5, col = 'red')
           }
        }
      }
      outmove = do.call(rbind,coordinatesOverTime)
      outmove$T = rep(0:(tSteps-1), each=nrow(tt))
      outmove$I = rep(1:nrow(tt), times=tSteps)
      outtraps = as.data.frame(do.call(rbind, trapCatch))
      outputs$traps = outtraps
      outputs$lobsters = outmove
    outputs.to.return[[k]] = outputs
       }
       print(Sys.time()-start)
    return(outputs.to.return)
  
  })}