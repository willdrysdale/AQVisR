#' hour_grouping
#' 
#' Helper function which returns groupings for fixed averaging intervals over 24 hours. Only supports periods that are a factor of 24
#' 
#' @param period number of hours in grouping period
#' 
#' @author W. S. Drysdale
#' 
#' @export 

hour_grouping = function(period){
  
  if(24 %% period == 0){
    data.frame(hour = 0:23,
               group = rep(1:(24/period),each = period))
  }else{
    stop("period must be a factor of 24")
  }
}