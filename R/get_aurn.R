#' get_aurn
#' 
#' Wrapper around \code{openair::importAURN()} to download and shape AURN data
#' 
#' @param keyword Optional keyword matched to site name using \code{stringr::str_detect()}
#' @param site.type One, or a vector of: "Urban Background", "Urban Traffic", "Rural Background", "Suburban Background", "Urban Industrial", "Suburban Industrial". NULL imports all site types
#' @param meta Optionally supply the site meta data. When NULL this is automatically downloaded via \code{openair::importMeta(source = "aurn")}
#' @param ... additonal paramerters passed to \code{openair::importMeta()}
#' 
#' @author W. S. Drysdale
#' 
#' @export

get_aurn = function(keyword = NULL,site.type = NULL, meta = NULL,...){
  
  if(is.null(meta)){
    meta = openair::importMeta(source = "aurn")
  }
  
  if(!is.null(keyword)){
    meta = meta[stringr::str_detect(meta$site,paste0("(",keyword,")",collapse = "|")),]
  }
  
  if(!is.null(site.type)){
    meta = meta %>% 
      filter(site_type %in% site.type)
  }
  
  dat = openair::importAURN(site = meta$code,
                            ...)
  
  if(is.null(dat))
    return(NULL)
  
  # Return
  tidy_aurn(dat)

}
