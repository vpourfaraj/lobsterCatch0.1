#' This function returns id of the closest trap and the and calculates its distance
#' @param lob_loc is the location of lobster
#' @param trap_loc is the location of trap
#' @export
distanceClosestTrap <- function(lob_loc, trap_loc){
  ds = unlist(apply(trap_loc,1,distanceToTrapCalculator,lob_loc))
  dmin = which.min(ds)
  return(c(distance=ds[dmin], trapid =dmin))
}
