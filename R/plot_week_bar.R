#' Plot week bar
#' 
#' Plots the output of \code{parse_year_comp()} as a bar chart of the difference of two periods
#' 
#' @param df output of \code{parse_year_comp()}
#' @param current name of group in column "y" to be subtracted from. default 2020
#' @param previous name of group in column "y" to subtract from current, default "2015 - 2019"
#' @param by_code logical, should the analysis be grouped by the code column
#' 
#' @author W. S. Drysdale
#' 
#' @export
#' 

plot_week_bar = function(df,current = "2020", previous = "2015 - 2019", by_code = T){
  
  if(!"code" %in% names(df))
    by_code = F
  
  if(by_code){
    df_week = df %>% 
      filter(yd >= 0) %>% 
      mutate(w = plyr::round_any(yd,7,floor)/7) %>% 
      dplyr::select(y,w,name,value,code) %>% 
      group_by(y,w,name,code) %>% 
      summarise_all(median,na.rm = T) %>% 
      ungroup() %>% 
      dplyr::group_nest(y)
    
    df_week_diff = left_join(df_week$data[df_week$y == current][[1]],
                             df_week$data[df_week$y == previous][[1]],
                             by = c("w","name","code"), suffix = c(".current",".previous")) %>% 
      mutate(value = value.current - value.previous)
  }else{
    df_week = df %>% 
      filter(yd >= 0) %>% 
      mutate(w = plyr::round_any(yd,7,floor)/7) %>% 
      dplyr::select(y,w,name,value) %>% 
      group_by(y,w,name) %>% 
      summarise_all(median,na.rm = T) %>% 
      ungroup() %>% 
      dplyr::group_nest(y)
    
    df_week_diff = left_join(df_week$data[df_week$y == current][[1]],
                             df_week$data[df_week$y == previous][[1]],
                             by = c("w","name"), suffix = c(".current",".previous")) %>% 
      mutate(value = value.current - value.previous)
    
  }
  
  
  
  
  g = ggplot(df_week_diff)+
    geom_bar(aes(w,value,fill = name),position = "dodge",stat = "identity")+
    scale_fill_brewer(palette = "Set1")+
    AQvis_plotTheme()
  
  if(by_code){
    g = g+facet_wrap(code~name)
  }else{
    g = g+facet_wrap(~name)
  }
  
  #
  g
  
}