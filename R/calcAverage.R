#' calcAverage
#' 
#' Calculates rolling or fixed averaging periods for \code{parse_exceedance()}
#' 
#' @param df data.frame prepped by \code{parse_exceedance()}
#' @param thresh air quality threshold definitions
#' @param rolling_spc names of species for which \code{calcRolling()} should be called. Others are passed to \code{calcAverage()}
#' 
#' @author W. S. Drysdale
#' 
#' @export

calcAverage = function(df,thresh,rolling_spc){
  if(df$name[1] %in% rolling_spc)
    calcRolling(df,thresh)
  else
    calcFixed(df,thresh)
}
