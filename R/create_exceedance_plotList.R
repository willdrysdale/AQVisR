#' create_exceedance_plotList
#' 
#' Wrapper around \code{plot_exceedance()} to allow more control over the resulting plots
#' 
#' @param count_df output of \code{count_exceedance()}
#' @param spc character vector of speices to be plotted in threshold format
#' @param max_sites maximum number of sites to show for each spc. only for summary = "none" of \code{count_exceedance}. Default 9
#' @param year_range integer vector of years to show exceedances for, defaults 2019:2020
#' @param top_year which year should be used to order the sites by number of exceedances. Default 2019
#' 
#' @author W. S. Drysdale
#' 
#' @export
#' 

create_exceedance_plotList = function(count_df,
                                      spc = c("no2_1_200","pm10_24_50","pm25_24_25"),
                                      max_sites = 9,
                                      year_range = 2019:2020,
                                      top_year = 2019){
  
  plotList = list()
  
  for(ss in spc){
    plotDat = count_df %>% 
      filter(name == ss,
             !code %in% c("JA2","JA5"),
             y %in% year_range)
    
    if(nrow(plotDat) == 0)
      next
    
    plotList[[ss]] = plotDat %>% 
      filter(code %in% get_top_sites(plotDat,year = top_year)[1:max_sites]) %>% 
      plot_exceedance()+
      coord_flip()
    
  }
  
  plotList
}