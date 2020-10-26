#' calcFixed
#' 
#' Calculates means over fixed periods for \code{parse_exceedance()}. Periods are calcualted via \code{hour_grouping()}
#' 
#' @param df data.frame prepped by \code{parse_exceedance()}
#' @param thresh air quality threshold definitions
#' 
#' @author W. S. Drysdale
#' 
#' @export

calcFixed = function(df,thresh){
  
  thresh = thresh %>% 
    filter(period_h <= 24)
  
  if(nrow(thresh) == 0)
    return(NULL)
  
  parsedList = list()
  
  for(i in 1:nrow(thresh)){
    
    temp_df = df
    
    temp_df$hour = lubridate::hour(temp_df$date)
    
    group_lookup = hour_grouping(thresh$period_h[i])
    
    temp_df = temp_df %>% 
      left_join(group_lookup, "hour")
    
    lubridate::hour(temp_df$date) = temp_df$group
    
    parsedList[[i]] = temp_df %>% 
      dplyr::select(-hour, -group) %>% 
      group_by(date,code,name) %>% 
      summarise_all(mean,na.rm = T) %>% 
      ungroup() %>% 
      dplyr::mutate(name = paste(thresh$name[i],thresh$period_h[i],thresh$threshold_ugm3[i],sep = "_"))
  }
  # Return
  bind_rows(parsedList)
}