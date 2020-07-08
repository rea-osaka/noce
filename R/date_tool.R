# 日付は、エクセルデータの各データに行としてはなく、
# 日付ごとのファイルとして管理されているので、
# 読み込み時に、特定の場所を検索することで、そのファイルの
# 日付を確定し、これを各列に追加する必要がある。
#
# エクセル内での日付は、和暦表記（令和2年3月）なので、
# 文字列として読み込みデータオブジェクト（日を適当に補完）にし、
# これを任意の文字列に再度変換してコード等にして使う
#
# 「time_code」、「時間軸(月次)`」、及び、
# 元にはない「date」（日付オブジェクト）を追加する
.add_date_item <- function(df, date_obj){
  ans <- df %>%
    dplyr::mutate( time_code = .into_date_code(date_obj),
                   `時間軸(月次)` = .into_date_string(date_obj),
                   date = date_obj)
  return(ans)
}

#####################################################################
# sub routine
#####################################################################
# 和暦文字列（令和3年1月）から日付オブジェクトを作成
.into_dateobj <- function(date_string){

  # 引数の一つ目の要素だけを扱うことを強制
  date_string <- date_string[1]

  ans <- NULL

  if(stringr::str_detect(date_string, "令和")){
    .match_result <- stringr::str_match(date_string, "令和(.*)年(\\d+)月")
    .year <- ifelse(.match_result[[2]] == "元",
                    2019,
                    2019 + as.numeric(.match_result[[2]]) -1)
    .month <- as.numeric(.match_result[[3]])
    ans <- as.Date(paste0(.year, "-", .month,"-1"))

  }else if(stringr::str_detect(date_string, "平成")){
    .match_result <- stringr::str_match(date_string, "平成(.*)年(\\d+)月")
    .year <- ifelse(.match_result[[2]] == "元",
                    1989,
                    1989 + as.numeric(.match_result[[2]]) -1)
    .month <- as.numeric(.match_result[[3]])
    ans <- as.Date(paste0(.year, "-", .month,"-1"))

  }else{
    # フォーマットに合わない文字列の場合NAを返す
    ans <- NA
  }

  return(ans)
}

#「time_code」列の書式にする
.into_date_code <- function(date_obj){
  if(is.na(date_obj)) return(NA)

  sprintf("%d00%02d%02d",
          lubridate::year(date_obj),
          lubridate::month(date_obj),
          lubridate::month(date_obj)
  )
}

#「時間軸(月次)`」列の書式にする
.into_date_string <- function(date_obj){
  if(is.na(date_obj)) return(NA)

  sprintf("%d年%d月",
          lubridate::year(date_obj),
          lubridate::month(date_obj)
  )
}

# エクセルファイルの中から、
# 日付が表示されている部分を探して読み込む
# 行頭に修正注意事項が書かれているものがあり、注意が必要
# 全てをくまなく検索せず、大体、
# そのあたり(1から5行の間、１列目)で、
# 今のところのファイルはいけている。
# 返る文字列は、"令和2年4月分"
.find_date_string <- function(df){

  target <- df[[1]][1:5]
  target <- target[is.na(target) == FALSE]
  ans    <- target[stringr::str_detect(target, "平成|令和")]

  return(ans)
}

