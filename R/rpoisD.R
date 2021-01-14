#' This function generates either a Poisson or a negative binomial distribution of lobsters on the sea bed
#' @param n is the number of observations
#' @param lambda is the mean
#' @param D is the dispersion index
#' @param sz this puts the overdispersion in terms of lambda(mean) and returns the appropriate size for Binomial distribution
#' @return a vector of integers that is used as initial distribution of lobsters on the seabed.
#' @export

rpoisD<-function (n, lambda,D=1) {
  if (D==1){
    rpois(n, lambda)
  }  else {
    sz = lambda^2/(D*lambda-lambda)
    rnbinom(n, size=sz, mu=lambda)
  }
}
