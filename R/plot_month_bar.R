#' Plot month bar
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
#'        \itemize{
#'            \item {difference - change is current-previous}
#'            \item {percent - change is ((current-previous)/previous)*100}
#'            \item {absolute - plot absolute values from current period only}
#'        }
#' @param combine_spc logical, should species be used in facet wrapping
#' @param highlight_range numerical vector length 2. range of values to highlight.
#' bars that fall outside of this range have their alpha reduced. default equal to xlim
#' 
#' @author W. S. Drysdale and J. Davison
#' 
#' @export
#' 

plot_month_bar = function(df,
                          current = "2020",
                          previous = "2015 - 2019",
                          by_code = T,
                          vline_pos = NULL,
                          xlim = c(0,12),
                          na.rm = F,
                          type = c("difference","percent","absolute")[1],
                          combine_spc = F,
                          highlight_range = xlim,
                          show_err = T){
  
  month_number <- function(doy = 1) {
    as.Date(doy, origin = "2016-12-31") %>%
      lubridate::month()
  }
  
  boot_median = function(val){
    broom::tidy(boot::boot(val,statistic = function(x,i) median(x[i],na.rm = T),R = 1000)) %>% 
      dplyr::rename(value = statistic,
                    std_err = std.error)
  }
  
  quadrature = function(x){
    sqrt(sum(x^2,na.rm = T))
  }
  
  if(!"code" %in% names(df))
    by_code = F
  
  if(by_code){
    joiners = c("m","name","code")
  }else{
    joiners = c("m","name")
  }
  
  
  df_month = df %>% 
    filter(between(yd,0,364)) %>% 
    mutate(yd = yd+1) %>% 
    mutate(m = month_number(yd)) %>% 
    dplyr::select(y,m,name,value,code) %>% 
    group_by(y,m,name,code) %>% 
    summarise(boot = list(boot_median(value))) %>% 
    unnest(boot) %>% 
    ungroup() %>% 
    dplyr::group_nest(y)
  
  df_month_diff = left_join(df_month$data[df_month$y %in% current][[1]],
                            df_month$data[df_month$y %in% previous][[1]],
                            by = joiners, suffix = c(".current",".previous")) %>% 
    mutate(value = value.current - value.previous,
           value_percent = ((value.current-value.previous)/value.previous)*100,
           std_err = map2_dbl(std_err.previous,std_err.current,~quadrature(c(.x,.y))),
           std_err_percent = sqrt((std_err.previous/value.previous)^2+
                                    (std_err.previous/value.previous)^2+
                                    (std_err.current/value.current)^2)*100,
           highlight = ifelse(between(m,min(highlight_range),max(highlight_range)),T,F)) %>% 
    parse_spec()
  
  
  if(sum(highlight_range == xlim) == 2){
    a = c(1,1)
  }else{
    a = c(0.5,1)
  }
  
  if(na.rm){
    df_month_diff = df_month_diff %>% 
      filter(!is.na(value))
  }
  
  if(type == "difference"){
    df_month_diff$plot_value = df_month_diff$value
    df_month_diff$plot_err = df_month_diff$std_err
  }
  
  if(type == "percent"){
    df_month_diff$plot_value = df_month_diff$value_percent
    df_month_diff$plot_err = df_month_diff$std_err_percent
  }
  
  if(type == "absolute"){
    df_month_diff$plot_value = df_month_diff$value.current
    df_month_diff$plot_err = df_month_diff$std_err.current
  }
  
  g = df_month_diff %>% 
    ggplot()+
    geom_bar(aes(m,plot_value,fill = name_parsed,alpha = highlight),stat = "identity")+
    scale_fill_brewer(palette = "Set1",labels = function(x) parse(text = x), name = "")+
    scale_x_continuous(limits = xlim,name = "Month of Year")+
    scale_alpha_manual(values = a,guide = F)+
    ylab(case_when(type == "difference" ~ "Absolute Concentration Difference",
                   type == "percent" ~ "Perecentage Concentration Difference",
                   type == "absolute" ~ "Absolute Concentration"))+
    AQvis_plotTheme()
  
  if(show_err & !combine_spc){
    g = g+geom_errorbar(aes(x = m,
                            ymin = plot_value-plot_err,
                            ymax = plot_value+plot_err,
                            group = name))
  }
  
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
