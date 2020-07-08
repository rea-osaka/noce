# 12_13_14等の書式を持つ列名で整理されているデータについて、
# 任意番目のデータの種類毎に、データを分割する
# 分割されたデータフレームには、新たなvariable_nameで指定される列名を持つ列が追加され、
# その値として、種類毎の数字が入る。
# 12_13_14,12_14_14,13_14_14がある場合、１番目のパターンを指定すると、
# 12と13のデータフレームに分けられる
#　分けられたデータフレームはリストで繋げられて、返される。
#
# パターンがすべて違う場合、この関数を繰り返して使うと、
# 最終的に１列のみが残り、列には全てのパターンで示した属性項目が追加される
# pivot_longerをパターン文字列で作成するルーチン
.make_dividedlist <- function(datalist, pattern_index, variable_name){
  tmp_ans <- vector("list", length(datalist))
  for(i in seq_along(datalist)){
    tmp_ans[[i]] <- .divide_data(datalist[[i]], pattern_index, variable_name)
  }

  # データフレームの入ったリストのリストになっているので、
  # データフレームの入ったリストにする
  ans <- .flat_list(tmp_ans)

  return(ans)
}

# listの中にデータフレームのリストが入っているものを
# フラットなデータフレームのリストにする
# appendだと遅いらしいので、
#「先に場所確保して当てはめ」のパターンのためのルーチン
.flat_list <- function(mylist){
  .len = 0
  for(i in seq_along(mylist)){
    .len = .len + length(mylist[[i]])
  }

  ans <- vector("list", .len)

  index = 1

  for(i in seq_along(mylist)){
    for(j in seq_along(mylist[[i]])){
      ans[[index]] <- mylist[[i]][[j]]
      index = index + 1
    }
  }

  return(ans)
}



# 指定インデックスに合致するデータの
# データフレームデータを抜き出して、項目列を付ける
# 列の選択では、データフレームに複数列がある場合がある
.divide_data <- function(df, pattern_index, variable_name){

  # 数字パターンの列名を持つものを抜き出す
  # 数字パターンの入った列名の集合
  tmp_name <- names(df) %>% stringr::str_subset("^\\d+_?")

  # 数字パターンの指定インデックスにどんな種類があるかを得る
  # 数字文字列のベクトル
  vals <- .read_pattern(tmp_name, pattern_index)

  ans <- vector("list", length = length(vals))

  for(i in seq_along(vals)){

    # 検索用正規表現を作成
    match_str <- .make_pattern(pattern_index, vals[i])

    # 指定パターン列
    # プラス
    # パターン以外の必須列（地域名等）
    # のデータフレームを作る
    ans[[i]] <- df %>%
      dplyr::select(-dplyr::matches("^\\d+_?"), dplyr::matches(match_str)) %>%
      dplyr::mutate(!!variable_name := vals[i])
  }

  return(ans)
}


# "^\\d+_\\d+_\\d+"
# パターンの任意番目にどんな種類のデータがあるのか
.read_pattern <- function(col_names, index){

  if(index == 1){
    .match_str <- "^(\\d+)"
  }else{
    .match_str <- paste0("^", stringr::str_dup("\\d+_", index - 1), "(\\d+)")
  }

  ans <-
    stringr::str_match(col_names, .match_str) %>% `[`(,2) %>% unique()

  return(ans)
}

# "^\\d+_\\d+_\\d+"
# パターンの任意番目を任意の数字でマッチさせるための正規表現を作成
.make_pattern <- function(index, val){

  if(index == 1){
    ans <- paste0("^", val, "_?")
  }else{
    ans <- paste0("^", stringr::str_dup("\\d+_", index - 1), val, "_?")
  }

  return(ans)
}
