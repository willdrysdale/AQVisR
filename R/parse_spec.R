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
  
  require(latex2exp)
  
  name = enquo(name)
  
  df = df %>% 
    mutate_if(is.factor,as.character)
  
  if(TeX){
    if(unit){
      df %>% 
        dplyr::mutate(name_parsed = case_when(!!name == "o3" ~ paste0(latex2exp::TeX("O_{3} / $\\mu$g m^{-3}")),
                                              !!name == "no" ~ paste0(latex2exp::TeX("NO / $\\mu$g m^{-3}")),
                                              !!name == "no2" ~ paste0(latex2exp::TeX("NO_{2} / $\\mu$g m^{-3}")),
                                              !!name == "nox" ~ paste0(latex2exp::TeX("NO_{x} / $\\mu$g m^{-3}")),
                                              !!name ==  "pm25" ~ paste0(latex2exp::TeX("PM_{2\\\\PERIOD@5} / $\\mu$g m^{-3}")),
                                              !!name == "pm10" ~ paste0(latex2exp::TeX("PM_{10} / $\\mu$g m^{-3}")),
                                              !!name == "so2" ~ paste0(latex2exp::TeX("SO_{2} / $\\mu$g m^{-3}")),
                                              !!name == "co" ~ paste0(latex2exp::TeX("CO / mg m^{-3}")),
                                              !!name == "ws" ~ paste0(latex2exp::TeX("Wind Speed / m s^{-1}")),
                                              !!name == "wd" ~ paste0(latex2exp::TeX("Wind Direction / \u00B0")),
                                              !!name == "ox" ~ paste0(latex2exp::TeX("O_{x} / $\\mu$g m^{-3}")),
                                              TRUE ~ !!name))
    }else{
      df %>% 
        dplyr::mutate(name_parsed = case_when(!!name == "o3" ~ paste0(latex2exp::TeX("O_{3}")),
                                              !!name == "no" ~ paste0(latex2exp::TeX("NO")),
                                              !!name == "no2" ~ paste0(latex2exp::TeX("NO_{2}")),
                                              !!name == "nox" ~ paste0(latex2exp::TeX("NO_{x}")),
                                              !!name == "pm25" ~ paste0(latex2exp::TeX("PM_{2\\\\PERIOD@5}")),
                                              !!name == "pm10" ~ paste0(latex2exp::TeX("PM_{10}")),
                                              !!name == "so2" ~ paste0(latex2exp::TeX("SO_{2}")),
                                              !!name == "co" ~ paste0(latex2exp::TeX("CO")),
                                              !!name == "ws" ~ paste0(latex2exp::TeX("Wind Speed")),
                                              !!name == "wd" ~ paste0(latex2exp::TeX("Wind Direction")),
                                              !!name == "ox" ~ paste0(latex2exp::TeX("O_{x}")),
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
                                              !!name == "ox" ~ "O_{x} / $\\mu$g m^{-3}",
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
                                              !!name == "ox" ~ "O_{x}",
                                              TRUE ~ !!name))
    }
  }
  
}
