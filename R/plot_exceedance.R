#' plot_exceedance
#' 
#' Plots the output of \code{count_exceedance()}. Is wrapped by \code{create_exceedance_plotList}) for more control
#' 
#' @param count_df output of \code{count_exceedance()}
#' 
#' @author W. S. Drysdale
#' 
#' @export

plot_exceedance = function(count_df){
  
  hlineDat = count_df[,c("name","n")] %>% 
    unique()
  
  count_df = count_df %>% 
    group_by(code,name,y) %>% 
    dplyr::select(-m) %>%
    summarise_all(sum,na.rm = T)
  
  g = ggplot(count_df)+
    geom_bar(aes(code,n_exceed,fill = as.factor(y)),
             stat = "identity",
             position = position_dodge2(width = 1.1, preserve = "single"),
             colour = "black")+
    geom_hline(data = hlineDat,aes(yintercept = n))+
    scale_fill_brewer(palette = "Set1", name = "year")+
    scale_y_continuous(expand = c(0,0,0.1,0), name = "Exceedances")+
    xlab("Site")+
    AQvis_plotTheme()+
    facet_wrap(~name,scales = "free_y")
  
  g
  
}