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
    filter(lubridate::year(date) <= max(comp_year))  # remove data following comp_year to simplify averaging
  
  ref_year = unique(year(df$date))
  ref_year = ref_year[!ref_year %in% comp_year]
  
  if(length(ref_year) == 0)
    stop("No data outside of comp_year supplied")
  
  # Removed 2020/01/13 to allow for multiple comp years
  # Include data from previous year at the begining of the time series to allow rolling median to run into the begining of the comparison year
  # df_pre = df %>% 
  #   dplyr::filter(name %in% species,
  #                 lubridate::year(date) == comp_year-1,
  #                 lubridate::month(date) %in% 6:12) %>% 
  #   dplyr::mutate(y = lubridate::year(date),
  #                 yd = (lubridate::yday(date)-365),
  #                 y = as.character(comp_year)) %>% 
  #   dplyr::select(y,yd,name,value) %>% 
  #   dplyr::group_by(y,yd,name) %>% 
  #   dplyr::summarise_all(median,na.rm = T) %>% 
  #   dplyr::group_by(name,y) %>% 
  #   dplyr::mutate(value_median = zoo::rollmedian(value,k = rolling_width,fill = NA)) %>% 
  #   dplyr::ungroup() %>% 
  #   dplyr::mutate(rolling_width = rolling_width)
  
  ref_label = NULL
  if(length(ref_year) == 1){
    ref_label = as.character(ref_year)
  }else{
    if(sum(diff(ref_year)) == (length(ref_year)-1)){
      ref_label = paste0(min(ref_year,na.rm = T)," - ",max(ref_year,na.rm = T))
    }else{
      ref_label = paste0(ref_year,collapse = ", ")
    }
  }
  
  df = df %>% 
    dplyr::filter(name %in% species) %>% 
    dplyr::mutate(y = lubridate::year(date),
                  yd = lubridate::yday(date)) %>% 
    mutate(y = ifelse(y %in% comp_year, as.character(y), ref_label)) %>% 
    dplyr::select(y,yd,name,value) %>% 
    dplyr::group_by(y,yd,name) %>% 
    dplyr::summarise_all(median,na.rm = T) %>% 
    dplyr::group_by(name,y) %>% 
    dplyr::mutate(value_median = zoo::rollmedian(value,k = rolling_width,fill = NA)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(rolling_width = rolling_width) %>% 
    # rbind(df_pre) %>% 
    arrange(yd)
  
  
  if(!is.null(city))
    df$grp = grp
  
  # return
  df
}
