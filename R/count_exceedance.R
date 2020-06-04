#' count_exceedance
#' 
#' Count the exceedances, grouped by code for species defined in name. Operated on the output of \code{parse_exceedance()}
#' 
#' @param df output of \code{parse_exceedance()}
#' @param thresh air quality threshold definitions
#' @param period interger or vector of interger periods to count. default c(1,8,24) i.e, count hourly, 8 hourly and daily exceednces defined in thresh
#' @param exceedance_days_only names of threshold groups that where multiple exceedances in one calenday day only count as one. \cr
#'                             useful to remove overcounting of rolling thresholds
#' @param summary one of c("none", "total", "average"). 
#'                \itemize{
#'                 \item none - count per code
#'                 \item total - sum across all codes
#'                 \item average - average per code
#'                }
#' None returns exceedances per group defined by the code column
#' @param summary_name if using total or average name the new code grouping
#' 
#' @author W. S. Drysdale
#' 
#' @export
#' 

count_exceedance = function(df,
                            thresh,
                            period = 1,
                            exceedance_days_only = "o3_8_100",
                            summary = c("none","total","average")[1],
                            summary_name = NULL,
                            total = FALSE,
                            total_name = NULL){
  if(8760 %in% period)
    stop("Yearly not implemented")
  
  if(total){
    summary = "total"
    if(!is.null(total_name))
      total_name = total_name
    
    warning("total and total_name are depreciated, please use summary = 'Total' and summary_name")
    
  }
  
  if(summary != "none"){
    
    if(summary == "average"){
      # codes per year
      
      codes_per_year = df %>% 
        mutate(y = year(date)) %>% 
        select(y,code) %>% 
        distinct() %>% 
        group_by(y) %>% 
        count() %>% 
        dplyr::rename(n_code = n)
      
      
    }
    
    
    if(!is.null(summary_name))
      df$code = summary_name
    else
      df$code = summary
    
    thresh$n = 0
    
  }
  
  dfCountRegular = df %>% 
    mutate(y = year(date),
           m = month(date)) %>% 
    filter(!is.na(exceed),
           period_h %in% period,
           !name %in% exceedance_days_only,
           exceed) %>% 
    group_by(code,name,y,m) %>% 
    count(name = "n_exceed") %>% 
    left_join(thresh[,c("id","n")],by = c("name" = "id"))
  
  if("o3_8_100" %in% unique(df$name)){
    
    dfCountDaysOnly = df %>% 
      mutate(y = year(date),
             m = month(date)) %>% 
      filter(!is.na(exceed),
             period_h %in% period,
             name %in% exceedance_days_only,
             exceed) %>% 
      group_by(date(date),code,name,y,m) %>% 
      count(name = "daily_n_exceed") %>% 
      ungroup() %>% 
      dplyr::select(-`date(date)`) %>% 
      mutate(daily_n_exceed = ifelse(daily_n_exceed == 0,0,1)) %>% 
      group_by(code,name,y,m) %>% 
      count(name = "n_exceed") %>% 
      left_join(thresh[,c("id","n")],by = c("name" = "id"))
    
    
  }else{
    dfCountDaysOnly = NULL
  }
  
  out = bind_rows(dfCountRegular,dfCountDaysOnly)
  
  if(summary == "average"){
    out = left_join(out,codes_per_year,"y") %>% 
      mutate(n_exceed = n_exceed/n_code)
  }
  
  #
  out
  
}