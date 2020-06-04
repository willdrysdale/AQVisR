#' get_top_sites
#' 
#' helper to organise sites into those with the most exceedances, used in \code{create_exceedance_plotList}
#' 
#' @param df \code{ount_exceedance} output
#' @param year the year to sort exceedances by
#' 
#' @author W. S. Drysdale
#' 
#' @export

get_top_sites = function(df,year = 2019){
  df = df[order(df$n_exceed,decreasing = T),] %>% 
    filter(y == year)
  
  df$code
}