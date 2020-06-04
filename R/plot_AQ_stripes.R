#' plot_AQ_stripes
#' 
#' Plot air quality stripes
#' 
#' @param df output of \code{parse_AQ_stripes}
#' @param spc name of species in name column to plot
#' @param vline_pos numeric vector. add vertical lines at the given day of year to stripes of the nth year 
#' @param title add a title to the plot via \code{ggtitle()}
#' 
#' @author W. S. Drysdale
#' 
#' @export
#' 

plot_AQ_stripes = function(df, spc = "no2",vline_pos = NULL,title = NULL){
  
  spc_df = data.frame(name = spc,stringsAsFactors = F) %>% 
    parse_spec(TeX = T)
  
  if("value" %in% names(df)){
    g = df %>% 
      filter(name == spc) %>% 
      ggplot()+
      geom_raster(aes(yday,y,fill = value))+
      scale_fill_gradientn(colours = viridisLite::viridis(200), name = parse(text = spc_df$name_parsed))
    
    
    if(!is.null(vline_pos)){
      lineDat = data.frame(x = rep(vline_pos,each = 2),
                           y = rep(c(max(df$y,na.rm = T)+0.5,max(df$y,na.rm = T)-0.5),length(vline_pos)),
                           grp = rep(1:length(vline_pos),each = 2))
      
      g = g+geom_path(data = lineDat,aes(x,y, group = grp), colour = "red",size = 1)
    }
  }
  
  if("diff" %in% names(df)){
    g = df %>% 
      filter(name == spc) %>% 
      ggplot()+
      geom_raster(aes(yday,y,fill = diff))+
      scale_fill_gradient2(high = "darkred",mid = "#f7f7ee",low = "darkblue",midpoint = 0, name = parse(text = spc_df$name_parsed))
    
    if(!is.null(vline_pos)){
      lineDat = data.frame(x = rep(vline_pos,each = 2),
                           y = rep(c(max(df$y,na.rm = T)+0.5,max(df$y,na.rm = T)-0.5),length(vline_pos)),
                           grp = rep(1:length(vline_pos),each = 2))
      
      g = g+
        geom_path(data = lineDat,aes(x,y, group = grp), colour = "black",size = 1.5)+
        geom_path(data = lineDat,aes(x,y, group = grp), colour = "white",size = 0.5)
    }
    
  }
  
  if("city" %in% names(df)){
    g = g+facet_wrap(~city)
  }
  
  g = g+
    scale_x_continuous(expand = c(0,0),name = "Day of Year")+
    scale_y_continuous(expand = c(0,0),trans = "reverse",name = "Year")+
    AQvis_plotTheme()+
    theme(aspect.ratio = 1/4)
  
  
  if(!is.null(title))
    g = g+ggtitle(title)
  
  g
}
