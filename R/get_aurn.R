#' get_aurn
#' 
#' Wrapper around \code{openair::importAURN()} to download and shape AURN data
#' 
#' @param keyword Optional keyword matched to site name using \code{stringr::str_detect()}
#' @param site_type One, or a vector of: "Urban Background", "Urban Traffic", "Rural Background", "Suburban Background", "Urban Industrial", "Suburban Industrial". NULL imports all site types
#' @param meta Optionally supply the site meta data. When NULL this is automatically downloaded via \code{openair::importMeta(source = "aurn")}
#' @param ... additonal paramerters passed to \code{openair::importMeta()}
#' 
#' @author W. S. Drysdale
#' 
#' @export

get_aurn = function(keyword,site_type = NULL, meta = NULL,...){
  
  if(is.null(meta)){
    meta = openair::importMeta(source = "aurn")
  }
  
  if(!is.null(keyword)){
    meta = meta[stringr::str_detect(meta$site,keyword),]
  }
  
  if(!is.null(site_type)){
    meta_filter = meta %>% 
      filter(site_type %in% site_type)
  }
  
  dat = openair::importAURN(site = meta$code,
                            ...)
  
  names(dat) = names(dat) %>% 
    stringr::str_remove_all("\\.") %>% 
    tolower()
  
  # Return
  dat %>% 
    dplyr::select(-site) %>% 
    tidyr::pivot_longer(-c(date,code))
}
