#' deadbandVD Function
#'
#' This function allows you to compute the Volatility Deadband(VD) algorithm
#' @param x The vector of the samples before the deadband algorithm
#' @param d Deadband percent parameter in range 0..1
#' @param offset How many sample do you want skip at begin? Defaults is n=20
#' @param k multiplier used in Bollinger theory
#' @return A list containing the L2 distance and the Number of filtered samples
#' @keywords VD
#' @export
#' @examples
#' deadbandVD(rnorm(40, mean = 0, sd = 1),0.01,20,2)
deadbandVD <- function(x, d, offset, k){
  bbands <- TTR::BBands( x, n = offset, sd = k)
  pctB <- bbands[,"pctB"]
  x_cache<-x[offset+1]
  NumberOfFiltered = 0
  x_out<-numeric(length(x))
  for(i in (offset+1):(length(x)))
  {
    if( abs(pctB[i])>d )
    {
      x_out[i]=x[i]
      x_cache=x[i]
    }
    else
    {
      NumberOfFiltered=NumberOfFiltered+1
      x_out[i]=x_cache
    }
  }
  # EucDist
  EucDist = dist(rbind(x, x_out), method = "euclidean")
  # %NumberOfFiltered
  NumberOfFiltered = NumberOfFiltered/length(x)
  return(list("NumOfFiltered" = NumberOfFiltered,"EuclideanDist" = EucDist[1]))
}
