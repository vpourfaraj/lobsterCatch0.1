#' This function determines if lobster passes through a trap and determines if it is caught
#' @param loc1 is the location of lobster at the start of each time step
#' @param loc2 is the location of lobster at the end of each time step
#' @param trap_loc is the location of trap
#' @param how_close set the area of trap within which the catch occurs, the default value is 0.1
#' @return a vector of locations and shows if any lobster is being trapped or not
#' @export
trapInPath = function(loc1, loc2, trap_loc,how_close=0.1){
  x = seq(loc1[1],loc2[1],length.out = 10)
  y = seq(loc1[2],loc2[2],length.out = 10)
  path = data.frame(EASTING=x, NORTHING=y)
  ds = unlist(apply(path,1,distanceToTrapCalculator,trap=trap_loc))
  if(any(ds<how_close)) {
    i= min(which(ds<how_close))
    path = c(path[i,1],path[i,2])
    trapped = 1
  } else {
    path = loc2
    trapped = 0
  }
  return(c(path, trapped))
}
