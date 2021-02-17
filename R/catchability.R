#' This function calculates the probability of entry to trap (aka catchability)
#' @param q0 is the initial probability of entry into an empty trap which is set to 0.5 by default
#' @param qmin is the asymptotic minimum probability of entry which is set to 0
#' @param  saturationThreshold is the number of lobsters in a trap at which the probability of
#' another lobster entering the trap drops to qmin, the default value is 5
#' @param Ct is the number of catch
#' @param r is the instantaneous rate of change in qo with respect to Ct
#' @return Returns the probability of entry to trap
#' @export
catchability<- function(q0= 0.5,qmin=0, saturationThreshold=5, Ct=0, lobSize=NULL){

  if( is.null(lobSize) ){
    r = (log(0.01) - log(q0 - qmin))/(saturationThreshold*-1)
    qo = (q0-qmin) / exp(r*Ct) + qmin
    return(qo)
  }else{
    #When lengthBased=TRUE and caught lobster is larger than 115-> q0=0
    temp <- unlist( strsplit( lobSize, split = '-CL' ) )
    temp <- temp[2:length(temp)]
    if( any(temp, na.rm = TRUE)  > 115 ){
      qo = 0
      return(qo)
    }else{
      r = (log(0.01) - log(q0 - qmin))/(saturationThreshold*-1)
      qo = (q0-qmin) / exp(r*Ct) + qmin
      return(qo)
    }

  }
}

