#' Tidy Aurn
#' 
#' Helper function to convert \code{openair::import_aurn()} output into a format \code{AQVisR} functions expect
#' 
#' @param dat output of \code{openair::import_aurn()}
#' 
#' @author W. S. Drysdale
#' 
#' @export

tidy_aurn = function(dat){
  names(dat) = names(dat) %>% 
    stringr::str_remove_all("\\.") %>% 
    tolower()
  
  # Return
  dat %>% 
    dplyr::select(-site) %>% 
    tidyr::pivot_longer(-c(date,code)) %>% 
    mutate_if(is.factor,as.character)
}

