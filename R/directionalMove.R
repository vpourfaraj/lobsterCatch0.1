#' This function moves the lobster for a fixed length toward the trap (+some randomness)
#' This is performed only when a lobster is within the bait influence
#' @param default location of the trap is at (5,5)
#' @param default value for radius of influence is 15
#' @param dStep is how much a lobster moves in each time step
#' @param ZoI is trap's zone of influence which gets updated in each timestep
#' @return a list of new location for each individual lobster
#' @export
directionalMove<- function(xLobster, yLobster, dStep, distanceToTrap,
                           xtrap=5, ytrap=5,radius_of_influence=15,ZoI,scalar=1){
  #need to standardize the points to make bearing function work
  if(any(c(xLobster,yLobster,xtrap,ytrap)>180)) scalar=max(c(xLobster,yLobster,xtrap,ytrap))
  xLP = (xLobster)/scalar
  yLP = (yLobster)/scalar
  xTp = (xtrap)/scalar
  yTp = (ytrap)/scalar
  thetaT = bearing(c(xLP,yLP),c(xTp,yTp))
  b = 1 + 0.9 * (distanceToTrap - ZoI) / radius_of_influence
  thetaR = -180:180
  P = 1/(180^b) * abs(thetaR) ^ b
  Prtheta_R = (1-P) / sum(1-P)
  theta_r = sample(thetaR,size = 1, prob=Prtheta_R)
  theta      <- thetaT + theta_r
  xNew   <- dStep * sin(theta * pi / 180) + xLobster
  yNew   <- dStep * cos(theta * pi / 180) + yLobster

  return( list(EASTING = xNew, NORTHING = yNew) )

}
