#' This function calculates the Euclidean distance between trap(s) and each individual lobster
#' @param Lobster is the matrix of lobster coordinates
#' @param trap is the location of the trap
#' @export
distanceToTrapCalculator<- function(Lobster,trap = x(5,5)){
  xLobster = Lobster[1]
  yLobster = Lobster[2]
  xtrap = trap[1]
  ytrap = trap[2]
  distanceToTrap<- sqrt((xLobster - xtrap)^2 + (yLobster -ytrap)^2)
  return(distanceToTrap)
}
