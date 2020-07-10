#' Add different term col
#'
#' This function adds year, quarter of year, month term
#' to summaries value with various term
#'
#' @param df noce data.frame
#'
#' @return df noce data.frame added varrious term cols
#' @export
noce_add_terms <- function(df){
  ans <- df %>%
    dplyr::mutate(year_term = .to_year_term(.data$date),
           quater_term = .to_quarter_year_term(.data$date),
           month_term = .to_month_term(.data$date))
  
  return(ans)
}

.to_quarter_year_term <- function(date_objs){
  n <- length(date_objs)
  ans <- vector("character",n)
  
  for(i in seq_along(date_objs)){
    ans[i] <- .to_quarter_year_single(date_objs[i])
  }
  
  return(ans)
}

.to_month_term <- function(date_objs){
  sprintf("%d%02d", lubridate::year(date_objs), lubridate::month(date_objs))
}


.to_year_term <- function(date_objs){
  lubridate::year(date_objs) %>% as.character() 
}

.to_quarter_year_single <- function(date_obj){
  year <- lubridate::year(date_obj)
  month <- lubridate::month(date_obj)
  
  ans <- NULL
  
  if(month >= 1 && month <= 3){
    ans <- sprintf("%dQ1", year)
  }else if(month >= 4 && month <= 6){
    ans <- sprintf("%dQ2", year)
  }else if(month >= 7 && month <= 9){
    ans <- sprintf("%dQ3", year)
  }else{
    ans <- sprintf("%dQ4", year)
  }
  
  return(ans)
}