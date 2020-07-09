#' Number of construction for new residence.
#'
#' This dataset contains number of construction for new residence.
#' We rearrange the data from e-state(https://www.e-stat.go.jp/) 
#' and then make this data.frame from it. 
#' 
#' @format A data frame with 495225 rows and 6 variables:
#' \describe{
#'   \item{cat01_code}{code for kind of residence}
#'   \item{area_code}{code for region}
#'   \item{date}{date object}
#'   \item{value}{number of construction}
#'   ...
#' }
#' @source \url{https://www.e-stat.go.jp/}
"jyutaku_kosuu"