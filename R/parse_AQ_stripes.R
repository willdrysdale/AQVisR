#' parse_AQ_stripes
#' 
#' Parse data to produce air quality stripes
#' 
#' @param df data.frame of one site's data
#' @param type one of c("conc","diff").
#'             \itemize{
#'              \item conc - format concentrations as supplied
#'              \item diff - calcualte the difference between the daily median of teh frist n-1 years supplied vs the nth year
#'              }
#' @param preserve_code Logical. Restore the code column to the output
#' 
#' @author W. S. Drysdale
#' 
#' @export
#' 

parse_AQ_stripes = function(df,type = c("conc","diff")[1], preserve_code = F){
  
  if(preserve_code){
    if("code" %in% names(df)){
      
      code = unique(df$code)
      code = code[!is.na(code)]
      
      if(length(code == 1) & !is.na(code))
        pcode = code
      else
        stop(paste0("Cannot preserve code: code == ",code))
    }
  }
  
  df_conc = df %>% 
    mutate(date = lubridate::date(date)) %>% 
    dplyr::select(date,name,value) %>% 
    group_by(date,name) %>% 
    summarise_all(median,na.rm = T) %>% 
    ungroup() %>% 
    mutate(yday = yday(date),
           y = year(date))
  
  if(type == "conc"){
    if(preserve_code){
      df_conc$code = pcode
    }
    
    return(df_conc)
    
  }
  
  df_ref = df %>% 
    filter(year(date) %in% year(min(date,na.rm = T)):(year(max(date,na.rm = T))-1)) %>% 
    mutate(yday = yday(date)) %>% 
    dplyr::select(date,yday,name,value) %>% 
    group_by(yday,name) %>% 
    summarise_all(median,na.rm = T) %>% 
    ungroup()
  
  df_diff = left_join(df_conc,df_ref,by = c("yday","name"),suffix = c(".conc",".ref")) %>% 
    mutate(diff = value.conc-value.ref) %>% 
    dplyr::select(-starts_with("value"))
  
  if(preserve_code)
    df_diff$code = pcode
  
  return(df_diff)
}