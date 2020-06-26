#' Plot week bar
#' 
#' Plots the output of \code{parse_year_comp()} as a bar chart of the difference of two periods
#' 
#' @param df output of \code{parse_year_comp()}
#' @param current name of group in column "y" to be subtracted from. default 2020
#' @param previous name of group in column "y" to subtract from current, default "2015 - 2019"
#' @param by_code logical, should the analysis be grouped by the code column
#' @param vline_pos numeric vector to place vertical lines
#' @param na.rm logical, should the data be filtered by \code{filter(!is.na(value))}
#' @param type one of "difference" or "percent". When difference (default),
#' change is current-previous. When percent, change is ((current-previous)/previous)*100
#' @param combine_spc logical, should species be used in facet wrapping
#' 
#' @author W. S. Drysdale
#' 
#' @export
#' 

plot_week_bar = function(df,
                         current = "2020",
                         previous = "2015 - 2019",
                         by_code = T,
                         vline_pos = NULL,
                         xlim = c(0,52),
                         na.rm = F,
                         type = c("difference","percent")[1],
                         combine_spc = F){
  
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
      mutate(value = value.current - value.previous,
             value_percent = ((value.current-value.previous)/value.previous)*100) %>% 
      parse_spec()
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
      mutate(value = value.current - value.previous,
             value_percent = ((value.current-value.previous)/value.previous)*100) %>% 
      parse_spec()
    
  }
  
  if(na.rm){
    df_week_diff = df_week_diff %>% 
      filter(!is.na(value))
  }
  
  if(type == "difference"){
    df_week_diff$plot_value = df_week_diff$value
  }else{
    df_week_diff$plot_value = df_week_diff$value_percent
  }
    
  
  g = df_week_diff %>% 
    ggplot()+
    geom_bar(aes(w,plot_value,fill = name_parsed),stat = "identity")+
    scale_fill_brewer(palette = "Set1",labels = function(x) parse(text = x), name = "")+
    scale_x_continuous(limits = xlim,name = "Week of Year")+
    ylab(ifelse(type == "difference","Absolute Concentration Difference", "Perecentage Concentration Difference"))+
    AQvis_plotTheme()
  
  if(combine_spc){
    if(by_code){
      g = g+facet_wrap(~code,scales = "free",labeller = "label_parsed")
    }
  }else{
    if(by_code){
      g = g+facet_wrap(code~name_parsed,scales = "free",labeller = "label_parsed")
    }else{
      g = g+facet_wrap(~name_parsed,scales = "free",labeller = "label_parsed")
    }
  }
  

  
  if(!is.null(vline_pos)){
    
    vline_dat = data.frame(xint = vline_pos)
    
    g = g+
      geom_vline(data = vline_dat, aes(xintercept = xint), colour = "grey50", linetype = 2)
  }
  #
  g
  
}
