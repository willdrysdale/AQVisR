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
#' @param type one of "difference", "percent" or "absolute".
#'        \itemize
#'        \item difference - change is current-previous
#'        \item percent - change is ((current-previous)/previous)*100
#'        \item absolute - plot absolute values from current period only
#' @param combine_spc logical, should species be used in facet wrapping
#' @param highlight_range numerical vector length 2. range of values to highlight.
#' bars that fall outside of this range have their alpha reduced. default equal to xlim
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
                         type = c("difference","percent","absolute")[1],
                         combine_spc = F,
                         highlight_range = xlim){
  
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
  
  df_week_diff = df_week_diff %>% 
    mutate(highlight = ifelse(between(w,min(highlight_range),max(highlight_range)),T,F))
  
  if(sum(highlight_range == xlim) == 2){
    a = c(1,1)
  }else{
    a = c(0.5,1)
  }
  
  if(na.rm){
    df_week_diff = df_week_diff %>% 
      filter(!is.na(value))
  }
  
  if(type == "difference")
    df_week_diff$plot_value = df_week_diff$value
  
  if(type == "percent")
    df_week_diff$plot_value = df_week_diff$value_percent
  
  if(type == "absolute")
    df_week_diff$plot_value = df_week_diff$value.current
  
  g = df_week_diff %>% 
    ggplot()+
    geom_bar(aes(w,plot_value,fill = name_parsed,alpha = highlight),stat = "identity")+
    scale_fill_brewer(palette = "Set1",labels = function(x) parse(text = x), name = "")+
    scale_x_continuous(limits = xlim,name = "Week of Year")+
    scale_alpha_manual(values = a,guide = F)+
    ylab(case_when(type == "difference" ~ "Absolute Concentration Difference",
                   type == "percent" ~ "Perecentage Concentration Difference",
                   type == "absolute" ~ "Absolute Concentration"))+
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
