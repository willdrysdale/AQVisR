#' parse_exceedance
#' 
#' Parse data so air quality threshold exceedances can be counted. 
#' 
#' @param dat data.frame of site(s) data
#' @param thresh air quality threshold definitions
#' @param rolling_spc names of species for which \code{calcRolling()} should be called. Others are passed to \code{calcAverage()}
#' 
#' @author W. S. Drysdale
#' 
#' @export

parse_exceedance = function(dat,thresh,rolling_spc = "o3"){
  
  datList = dat %>% 
    dplyr::select(date,name,code,value) %>% 
    dplyr::filter(name %in% thresh$name) %>% 
    dplyr::group_by(code) %>% 
    split(.$name) %>% 
    purrr::keep(~nrow(.x) > 1)
  
  threshList = thresh %>% 
    dplyr::filter(name %in% dat$name) %>% 
    split(.$name) %>% 
    purrr::keep(~nrow(.x) >= 1)
  
  
  df_parsed = purrr::map2_df(datList,threshList,
                             ~calcAverage(df = .x,thresh = .y,rolling_spc = rolling_spc)) %>% 
    dplyr::left_join(thresh[,c("period_h","threshold_ugm3","id")],c("name" = "id")) %>% 
    dplyr::mutate(exceed = ifelse(value > threshold_ugm3,T,F))
  
  
  
  # return
  df_parsed
}
