#' This function moves the lobster for a fixed length toward the trap (+some randomness)
#' This is performed only when a lobster is within the bait influence
#' @param default location of the trap is at (5,5)
#' @param default value for radius of influence is 15
#' @param dStep is how much a lobster moves in each time step
#' @param ZoI is trap's zone of influence which gets updated in each timestpe
#' @param thetaT calculates angle to the trap
#' @return a list of new location for each individual lobster
#' @export
directionalMove<- function(xLobster, yLobster, dStep, distanceToTrap,
                           xtrap=5, ytrap=5,radius_of_influence=15,ZoI){

  thetaT = atan2(ytrap-yLobster,xtrap-xLobster)*180/pi
  b = 1 + 0.9 * (distanceToTrap - ZoI) / radius_of_influence
  thetaR = -180:180
  P = 1/(180^b) * abs(thetaR) ^ b
  Prtheta_R = (1-P) / sum(1-P)
  theta_r = sample(thetaR,size = 1, prob=Prtheta_R)
  tetha      <- thetaT + theta_r
  xNew   <- dStep * sin(tetha * pi / 180) + xLobster
  yNew   <- dStep * cos(tetha * pi / 180) + yLobster

  return( list(EASTING = xNew, NORTHING = yNew) )

}
