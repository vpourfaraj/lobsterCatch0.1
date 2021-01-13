#' The function randomly selects an angle (0:360) and moves the lobster. This is called when a lobster
#' is outside the area of influence.
#' @param xLobster current x coordinate of the lobster
#' @param yLobster current y coordinate of the lobster
#' @param  dStep   fixed length of movemnet in each timestep 
#' @export a list of new coordinates after moving lobsters randomly
#' @export

randomMove<- function(xLobster, yLobster, dStep){
  
  randomAngle<- runif(n=1, min = 0, max=360) 
  xNew<- dStep * sin(randomAngle * pi / 180) + xLobster 
  yNew<- dStep * cos(randomAngle * pi / 180) + yLobster
  
  return( list(EASTING = xNew, NORTHING = yNew) )
}