#' summariseSitesByDateRange
#' 
#' Return mean median and standard deviation of value or diff column
#' 
#' @param df data frame containing date, value/diff and optionally code
#' @param date_start POSIXct refering to the beginning of the period to be summarised 
#' @param date_end POSIXct refering to the end of the period to be summarised 
#' @param by_code logical, should the data be grouped by code
#' 
#' @author W. S. Drysdale
#' 
#' @export
#' 

summariseSitesByDateRange = function(df,
                                     date_start = lubridate::ymd_hm("2020-01-01 00:00"),
                                     date_end = lubridate::ymd_hm("2020-01-31 00:00"),
                                     by_code = F){
  
  if(by_code == T & "code" %in% names(df)){
    if("diff" %in% names(df)){
      df = df %>% 
        dplyr::select(code,diff,name,date) %>% 
        dplyr::group_nest(code) %>% 
        mutate(data = purrr::map(data,
                                 ~summariseSitesByDateRange(df = .x,
                                                            date_start,
                                                            date_end,
                                                            by_code = F))) %>% 
        tidyr::unnest(data)
      
      return(df)
      
    }else{
      df = df %>% 
        dplyr::select(code,value,name,date) %>% 
        dplyr::group_nest(code) %>% 
        mutate(data = purrr::map(data,
                                 ~summariseSitesByDateRange(df = .x,
                                                            date_start,
                                                            date_end,
                                                            by_code = F))) %>% 
        unnest(data)
      
      return(df)
    }

  }
  
  if("diff" %in% names(df))
    val_names = c("diff","name")
  else
    val_names = c("value","name")
  
  df %>% 
    ungroup %>%
    filter(between(date,date_start,date_end)) %>% 
    dplyr::select(all_of(!!val_names)) %>% 
    group_by(name) %>% 
    summarise_all(c("mean","median","sd"),na.rm = T) %>% 
    mutate(date_range = paste0(date_start," - ",date_end)) %>% 
    data.frame()
}
