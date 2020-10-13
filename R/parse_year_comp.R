#' parse_year_comp
#' 
#' Compare a year's timeseries to previous years. 
#' 
#' @param df data.frame of site data
#' @param comp_year what year should be compared, note currently only year previous to this will be compared
#' @param species names of speices to perform comparison on
#' @param rolling_width width of the rolling median in days
#' @param grp optional grouping label - it is needed less now the function supports grouping by code
#' @param by_code logical, should the function be applied to each code group individually? default false
#' 
#' @author W. S. Drysdale
#' 
#' @export
#' 

parse_year_comp = function(df,
                           comp_year = 2020,
                           species = c("no2","pm25","o3"),
                           rolling_width = 15,
                           grp = NULL,
                           by_code = F,
                           city = NULL){
  
  if(by_code == T & "code" %in% names(df)){
    df = df %>% 
      tidyr::nest(data = c(date,name,value)) %>% 
      mutate(data = map(data, ~parse_year_comp(df = .x,
                                               comp_year = comp_year,
                                               species = species,
                                               rolling_width = rolling_width,
                                               grp = grp,
                                               by_code = F))) %>% 
      tidyr::unnest(data)
    
    if(length(unique(df$y)) > 2)
      warning(paste0("Greater than 2 options for y grouping: ",paste0(unique(df$y), collapse = ", ")))
    
    return(df)
  }
  
  df = df %>% 
    filter(lubridate::year(date) <= comp_year) # remove data following comp_year to simplify averaging
  
  # Include data from previous year at the begining of the time series to allow rolling median to run into the begining of the comparison year
  df_pre = df %>% 
    dplyr::filter(name %in% species,
                  lubridate::year(date) == comp_year-1,
                  lubridate::month(date) %in% 6:12) %>% 
    dplyr::mutate(y = lubridate::year(date),
                  yd = (lubridate::yday(date)-365),
                  y = as.character(comp_year)) %>% 
    dplyr::select(y,yd,name,value) %>% 
    dplyr::group_by(y,yd,name) %>% 
    dplyr::summarise_all(median,na.rm = T) %>% 
    dplyr::group_by(name,y) %>% 
    dplyr::mutate(value_median = zoo::rollmedian(value,k = rolling_width,fill = NA)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(rolling_width = rolling_width)
  
  df = df %>% 
    dplyr::filter(name %in% species) %>% 
    dplyr::mutate(y = lubridate::year(date),
                  yd = lubridate::yday(date),
                  y = dplyr::case_when(y == comp_year ~ as.character(comp_year),
                                min(y,na.rm = T) == (max(y,na.rm = T)-1) ~ as.character(min(y,na.rm = T)),
                                TRUE ~ paste0(min(y,na.rm = T)," - ",(max(y,na.rm = T)-1)))) %>% 
    dplyr::select(y,yd,name,value) %>% 
    dplyr::group_by(y,yd,name) %>% 
    dplyr::summarise_all(median,na.rm = T) %>% 
    dplyr::group_by(name,y) %>% 
    dplyr::mutate(value_median = zoo::rollmedian(value,k = rolling_width,fill = NA)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(rolling_width = rolling_width) %>% 
    rbind(df_pre) %>% 
    arrange(yd)
  
  
  if(!is.null(city))
    df$grp = grp
  
  # return
  df
}
