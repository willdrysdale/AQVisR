#' calcRolling
#' 
#' Calculates rolling means for \code{parse_exceedance()}
#' 
#' @param df data.frame prepped by \code{parse_exceedance()}
#' @param thresh air quality threshold definitions
#' 
#' @author W. S. Drysdale
#' 
#' @export

calcRolling = function(df,thresh){
  
  for(i in 1:nrow(thresh)){
    col_name = paste(thresh$name[i],thresh$period_h[i],thresh$threshold_ugm3[i],sep = "_")
    df = df %>% 
      mutate(!!col_name := RcppRoll::roll_mean(x = value,n = thresh$period_h[i],fill = NA,na.rm = T))
  }
  
  df = df %>% 
    dplyr::rename(!!thresh$name[i] := value) %>% 
    dplyr::select(-name) %>% 
    tidyr::pivot_longer(-c(date,code))
  
  # Return
  df
}