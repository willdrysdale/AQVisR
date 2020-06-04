#' parse_spec
#' 
#' Helper to format common species for plotting. Adds a new column called \code{name_parsed}
#' 
#' @param df data.frame containing a column of species names, named "name" by default
#' @param name name of column containing species names to be parsed. Supplied unquoted
#' @param unit logical, should the unit be included in the parsed output? Default TRUE
#' @param TeX logical, should string be wrapped in \code{paste0(latex2exp::TeX())}? Default TRUE
#' 
#' @author W. S. Drysdale
#' 
#' @export 

parse_spec = function(df,name = name,unit = TRUE,TeX = TRUE){
  
  name = enquo(name)
  
  df = df %>% 
    mutate_if(is.factor,as.character)
  
  if(TeX){
    if(unit){
      df %>% 
        dplyr::mutate(name_parsed = case_when(!!name == "o3" ~ paste0(TeX("O_{3} / $\\mu$g m^{-3}")),
                                              !!name == "no" ~ paste0(TeX("NO / $\\mu$g m^{-3}")),
                                              !!name == "no2" ~ paste0(TeX("NO_{2} / $\\mu$g m^{-3}")),
                                              !!name == "nox" ~ paste0(TeX("NO_{x} / $\\mu$g m^{-3}")),
                                              !!name ==  "pm25" ~ paste0(TeX("PM_{2\\\\PERIOD@5} / $\\mu$g m^{-3}")),
                                              !!name == "pm10" ~ paste0(TeX("PM_{10} / $\\mu$g m^{-3}")),
                                              !!name == "so2" ~ paste0(TeX("SO_{2} / $\\mu$g m^{-3}")),
                                              !!name == "co" ~ paste0(TeX("CO / mg m^{-3}")),
                                              !!name == "ws" ~ paste0(TeX("Wind Speed / m s^{-1}")),
                                              !!name == "wd" ~ paste0(TeX("Wind Direction / \u00B0")),
                                              TRUE ~ !!name))
    }else{
      df %>% 
        dplyr::mutate(name_parsed = case_when(!!name == "o3" ~ paste0(TeX("O_{3}")),
                                              !!name == "no" ~ paste0(TeX("NO")),
                                              !!name == "no2" ~ paste0(TeX("NO_{2}")),
                                              !!name == "nox" ~ paste0(TeX("NO_{x}")),
                                              !!name == "pm25" ~ paste0(TeX("PM_{2\\\\PERIOD@5}")),
                                              !!name == "pm10" ~ paste0(TeX("PM_{10}")),
                                              !!name == "so2" ~ paste0(TeX("SO_{2}")),
                                              !!name == "co" ~ paste0(TeX("CO")),
                                              !!name == "ws" ~ paste0(TeX("Wind Speed")),
                                              !!name == "wd" ~ paste0(TeX("Wind Direction")),
                                              TRUE ~ !!name))
    }
  }else{
    if(unit){
      df %>% 
        dplyr::mutate(name_parsed = case_when(!!name == "o3" ~ "O_{3} / $\\mu$g m^{-3}",
                                              !!name == "no" ~ "NO / $\\mu$g m^{-3}",
                                              !!name == "no2" ~ "NO_{2} / $\\mu$g m^{-3}",
                                              !!name == "nox" ~ "NO_{x} / $\\mu$g m^{-3}",
                                              !!name == "pm25" ~ "PM_{2\\\\PERIOD@5} / $\\mu$g m^{-3}",
                                              !!name == "pm10" ~ "PM_{10} / $\\mu$g m^{-3}",
                                              !!name == "so2" ~ "SO_{2} / $\\mu$g m^{-3}",
                                              !!name == "co" ~ "CO / mg m^{-3}",
                                              !!name == "ws" ~ "Wind Speed / m s^{-1}",
                                              !!name == "ws" ~ "Wind Direction / \u00B0",
                                              TRUE ~ !!name))
    }else{
      df %>% 
        dplyr::mutate(name_parsed = case_when(!!name == "o3" ~ "O_{3}",
                                              !!name == "no" ~ "NO",
                                              !!name == "no2" ~ "NO_{2}",
                                              !!name == "nox" ~ "NO_{x}",
                                              !!name == "pm25" ~ "PM_{2\\\\PERIOD@5}",
                                              !!name == "pm10" ~ "PM_{10}",
                                              !!name == "so2" ~ "SO_{2}",
                                              !!name == "co" ~ "CO",
                                              !!name == "ws" ~ "Wind Speed",
                                              !!name == "ws" ~ "Wind Direction",
                                              TRUE ~ !!name))
    }
  }
  
}
