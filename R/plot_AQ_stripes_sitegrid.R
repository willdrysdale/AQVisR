#' plot_AQ_stripes_sitegrid
#' 
#' Plot air quality stripes as a grid by site
#' 
#' @param stripeDat output of \code{parse_AQ_stripes_grid()}
#' @param spc name of species in name column to plot
#' @param yday_thresh option cutoff for x axis. Default day of year of system time, less 1
#' @param normalise logical. normalise each sites data. For conc between 0 and 1, diff -1 and 1
#' 
#' @author W. S. Drysdale
#' 
#' @export
#' 

plot_AQ_stripes_sitegrid = function(stripeDat,spc = "no2",yday_thresh = (yday(Sys.time())-1),normalise = F){
  
  spc_df = data.frame(name = spc,stringsAsFactors = F) %>% 
    parse_spec(TeX = T)
  
  if("diff" %in% names(stripeDat)){
    
    plotDat = stripeDat %>% 
      filter(name == spc,
             yday <= yday_thresh) %>% 
      select(code,yday,diff) %>% 
      group_by(code)
    
    
    if(normalise){
      plotDat = plotDat %>% 
        mutate(diff = scales::rescale(diff,to = c(-1,1)))
    }
    
    g = plotDat %>% 
      ggplot()+
      geom_tile(aes(yday,code,fill = diff),colour = "grey50")+
      scale_fill_gradient2(high = "darkred",mid = "#f7f7ee",low = "darkblue",midpoint = 0,name = parse(text = spc_df$name_parsed))+
      scale_x_continuous(expand = c(0,0), name = "Day of Year")+
      ylab("Site Code")+
      coord_equal()+
      AQvis_plotTheme()
    
  }else{
    
    plotDat = stripeDat %>% 
      filter(name == spc,
             yday <= yday_thresh) %>% 
      select(code,yday,value) %>% 
      group_by(code)
    
    
    if(normalise){
      plotDat = plotDat %>% 
        mutate(value = scales::rescale(value))
    }
    
    g = plotDat %>%
      ggplot()+
      geom_tile(aes(yday,code,fill = value),colour = "grey50")+
      scale_fill_gradientn(colours = viridisLite::viridis(200),name = parse(text = spc_df$name_parsed))+
      scale_x_continuous(expand = c(0,0), name = "Day of Year")+
      ylab("Site Code")+
      coord_equal()+
      AQvis_plotTheme()
  }
  
  #
  g
}