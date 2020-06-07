#' parse_AQ_stripes_sitegrid
#' 
#' Wrapper around \code{parse_AQ_stripes()}. Produces data to plot concentraions on a site by site basis
#' 
#' @param siteList list of individual site data
#' @param type one of c("conc","diff").
#'             \itemize{
#'              \item conc - format concentrations as supplied
#'              \item diff - calcualte the difference between the daily median of teh frist n-1 years supplied vs the nth year
#'              }
#' @param year optional numeric vector. Filter the output data by year (more input data than needs to be plotted may be required for \code{type == "diff")}
#' 
#' @author W. S. Drysdale
#' 
#' @export
#' 

parse_AQ_stripes_sitegrid = function(siteList,type = c("conc","diff")[1],year = 2020){
  yearDat = siteList %>% 
    keep(~nrow(.x) > 0) %>% 
    map_df(~.x %>% 
             mutate_if(is.factor,as.character) %>% 
             mutate(code = ifelse("DERR" %in% code, "DERY",code)) %>% # fix Derry AQ sites
             parse_AQ_stripes(preserve_code = T,type = type)) %>% 
    filter(y == year)
  
  yearDat
}