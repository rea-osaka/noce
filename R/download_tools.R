#' download files for data of number of construction
#'
#' This function downloads excel files for data of number of construction
#' from e-state site.
#'
#' @param appID e-state application id
#' @param table table number string of which you want data
#' @param dist_path a directory that download files were put in
#' @param all_files whether you need to download all files or not
#'
#' @return nothing
#' @export
noce_download_excel_files <- function(appID, table, dist_path = "./", all_files = FALSE){

  metadata <- .noce_download_metadata(appID, table)

  if(!all_files){
    metadata <- metadata %>% .decide_dlfiles(dist_path)
  }

  if(!stringr::str_detect(dist_path, "/$")){
    dist_path <- paste0(dist_path,"/")
  }

  for (i in seq_along(metadata$URL)){
    utils::download.file(metadata$URL[i],
                         paste0(dist_path, metadata$DATASET_NAME[i], ".xls"),
                         mode = "wb" )
    Sys.sleep(1)
  }

}

.metadataDB <- tibble::tibble(
  TABLE_NO = c("15",
               "7-1"),
  KEYWORD = c("都道府県別、工事別、利用関係別／戸数・件数、床面積 AND 月次",
              "都道府県別、用途別（大分類）／建築物の数、床面積、工事費予定額"),
  CYCLE = c("月次",
            "月次")
)


.decide_dlfiles <- function(df, dir_path){
  file_names <- dir(dir_path) %>% stringr::str_match("(.*)\\.xls$")
  file_names <- file_names[,2]
  ans <- df[!(df$DATASET_NAME %in% file_names), ]

  return(ans)
}

.noce_download_metadata <- function(appID, table){

  match_str <- paste0("^",table,"$")
  res <- .metadataDB$TABLE_NO %>% stringr::str_which(match_str)

  if(length(res) == 0){
    stop("table number is not found")
  }

  metadata <-
    estatapi::estat_getDataCatalog(appID,
                                   searchWord = .metadataDB$KEYWORD[res],
                                   dataType = "XLS") %>%
    dplyr::filter(.data$CYCLE == .metadataDB$CYCLE[res],
                  .data$TABLE_NO == .metadataDB$TABLE_NO[res])
  return(metadata)
}

