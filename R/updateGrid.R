#' This function updates the coordinate of each lobster at each timestep,
#' The distanceToTrap function calculates the distance to the closest trap
#' @param lobsterCoordinates is the coordinates (x,y) of each lobster at each time step
#' @param trapCoordinates is the coordinate of trap(s)
#' @param trapCatch number of lobsters caught
#' @param lobSize size of caught lobster
#' @param radius_of_influence is set to 15 by default
#' @param dstep is set to 5 by default
#' @param currentZoI is the bait's area of influence at each timestpe
#' @param how_close is set to 0.1 by default
#' @param q0 is set to 0.5 by default
#' @param qmin is set to 0 by default
#' @param saturationThresholdis set to 5 by default
#' @param trapSaturation is set to TRUE by default
#' @param lengthBased is set to FALSE by default
#' @return a list of new coordinates, number of catch and their size
#' @export
updateGrid    = function(lobsterCoordinates,trapCoordinates, trapCatch, lobSize, radius_of_influence=15, dstep = 5, currentZoI, how_close=0.1, q0=.5, qmin=0, saturationThreshold=5, trapSaturation=T, lengthBased = F){

  numberOfLobsters <- nrow(lobsterCoordinates)
  if(numberOfLobsters>0) {
    xNew <- vector(mode = 'numeric', length = numberOfLobsters)
    yNew <- vector(mode = 'numeric', length = numberOfLobsters)
    trappedLobster<- vector(mode = 'numeric', length = numberOfLobsters)

    for( lobsterIndex in 1:numberOfLobsters ){
      xOld <- lobsterCoordinates[lobsterIndex,1]
      yOld <- lobsterCoordinates[lobsterIndex,2]
      trapped <- lobsterCoordinates[lobsterIndex,3]
      if(trapped==1){
        xNew[lobsterIndex] = xOld
        yNew[lobsterIndex] = yOld
        trappedLobster[lobsterIndex] = trapped
        next()
      }

      distanceToTrap = distanceClosestTrap(lob_loc = c(xOld,yOld), trap_loc = trapCoordinates[,c(1,2)] )
      if( distanceToTrap[1] > radius_of_influence){
        temp <- randomMove(xLobster = xOld , yLobster = yOld , dStep = dstep)
        xNew[lobsterIndex] <- temp$EASTING
        yNew[lobsterIndex] <- temp$NORTHING
        trappedLobster[lobsterIndex] = 0
      }else{
        temp <- directionalMove(xLobster = xOld , yLobster = yOld , distanceToTrap = distanceToTrap[1], radius_of_influence = radius_of_influence, dStep = dstep, ZoI = currentZoI)
        xNew[lobsterIndex] <- temp$EASTING
        yNew[lobsterIndex] <- temp$NORTHING
        trappedQ = trapInPath(loc1 = c(xOld,yOld), loc2 = c(xNew[lobsterIndex],yNew[lobsterIndex]), trap_loc = trapCoordinates[distanceToTrap[2],],how_close=how_close)
        if(trappedQ[3]==1) {


          if(trapSaturation){

            if(lengthBased){
              pC = catchability(q0= q0,qmin=qmin, saturationThreshold=saturationThreshold, Ct=trapCatch[distanceToTrap[2]], lobSize = lobSize )
            }else{
              pC = catchability(q0= q0,qmin=qmin, saturationThreshold=saturationThreshold, Ct=trapCatch[distanceToTrap[2]], lobSize = NULL )
            }

          }

          if(!trapSaturation) pC = q0
          caught = rbinom(n=1,size=1,prob=pC)
          if(caught==1){
            trapCatch[distanceToTrap[2]] =trapCatch[distanceToTrap[2]]+1
            xNew[lobsterIndex] <- trapCoordinates[distanceToTrap[2],1]
            yNew[lobsterIndex] <- trapCoordinates[distanceToTrap[2],2]
            trappedLobster[lobsterIndex] = 1
            lobSize[distanceToTrap[2]] <- paste0(lobSize[distanceToTrap[2]], '-', lobsterCoordinates[lobsterIndex,4])
          }
        }
      }
    }

    updatedGrid <- data.frame(EASTING = xNew, NORTHING = yNew, trapped = trappedLobster, CL = lobsterCoordinates$CL)
    return(list(updatedGrid, trapCatch, lobSize))

  }
}
