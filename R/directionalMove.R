#' This function moves the lobster a fixed distance which will be toward the trap (+some randomness) if within a zone of attraction. The closer the lobster is to the trap the smaller the random component of movement.
#' @param xLobster location lobster in x direction
#' @param yLobster location of a lobster in y direction
#' @param dStep is how much a lobster moves in each time step
#' @param distanceToTrap how much influence the trap has on movement
#' @param xtrap location of the closest trap in the x direction
#' @param ytrap location of the trap in y direction
#' @param radius_of_influence how large is the attraction zone for the trap at t0
#' @param ZoI is trap's zone of influence which gets updated in each timestep
#' @export
directionalMove<- function(xLobster, yLobster, dStep, distanceToTrap,
                           xtrap=5, ytrap=5,radius_of_influence=15,ZoI){
  thetaT =  atan2(ytrap-yLobster,xtrap-xLobster)*180/pi
  b = 1 + 0.9 * (distanceToTrap - ZoI) / radius_of_influence
  thetaR = -180:180
  P = 1/(180^b) * abs(thetaR) ^ b
  Prtheta_R = (1-P) / sum(1-P)
  theta_r = sample(thetaR,size = 1, prob=Prtheta_R)
  theta      <- thetaT + theta_r
  xNew   <- dStep * cos(theta * pi / 180) + xLobster
  yNew   <- dStep * sin(theta * pi / 180) + yLobster

  return( list(EASTING = xNew, NORTHING = yNew) )

}
