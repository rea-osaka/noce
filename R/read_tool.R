# エクセルファイルから読み込んだデータのいらない部分を消す処理
.clean_rowdata <- function(row_obj, del_col = 2, base_col = 2){
  ans <- row_obj %>% dplyr::select(-del_col)
  ans <- dplyr::filter(ans, stringr::str_detect(ans[[base_col]], "\\d+"))

  return(ans)
}


# 列名が数字で始まるもののなめを"value"に置き換える
.rename_value <- function(df){
  ans <- df %>%
    dplyr::select(dplyr::matches("^\\d+"), dplyr::everything())
  names(ans)[1] <- "value"
  return(ans)
}
