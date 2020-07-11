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
                  quarter_term = .to_quarter_year_term(.data$date),
                  month_term = .data$date)
  
  return(ans)
}

.to_quarter_year_term <- function(date_objs){

  tmpq <- lubridate::quarter(date_objs)
  
  tmpm <- ifelse(tmpq == 1, 1,
                ifelse(tmpq == 2, 4,
                       ifelse(tmpq == 3, 7,10 )))
  ans <- date_objs
  lubridate::month(ans) <- tmpm
  lubridate::day(ans) <- 1
  
  return(ans)
  
}


.to_year_term <- function(date_objs){
  ans <- date_objs
  lubridate::month(ans) <- 1
  lubridate::day(ans) <- 1
  return(ans)
}

