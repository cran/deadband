#' deadbandAD Function
#'
#' This function allows you to compute the Absolute Deadband(AD) algorithm
#' @param x The vector of the samples before the deadband algorithm
#' @param EUmax The Engineering Unit higher bound
#' @param EUmin The Engineering Unit lower bound
#' @param d Deadband percent parameter in range 0..1
#' @param offset How many sample do you want skip at begin? Defaults is n=20
#' @return A list containing the L2 distance and the Number of filtered samples
#' @keywords AD
#' @export
#' @examples
#' deadbandAD(rnorm(40, mean = 0, sd = 1),+0.5,-0.5,0.01,20)
deadbandAD <- function(x,EUmax,EUmin,d, offset) {
  Delta = abs(EUmax-EUmin)*d
  x_cache<-x[offset+1]
  NumberOfFiltered = 0;
  x_out<-numeric(length(x))
  for(i in (offset+1):(length(x)))
  {
    if( abs(x[i]-x_cache)>Delta )
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
  # EucDist -> Fidelity
  EucDist = dist(rbind(x, x_out), method = "euclidean")
  # %NumberOfFiltered -> Effectiveness
  NumberOfFiltered = NumberOfFiltered/length(x)
  return(list("NumOfFiltered" = NumberOfFiltered,"EuclideanDist" = EucDist[1]))
}
