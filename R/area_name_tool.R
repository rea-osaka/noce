# 「地域」「area_code」列を追加する
#
# 元データは「000000地域名」と「地域名」２種類のパターンがある
# コードがある部分は、コードを分離してそのまま使う。
# コードがないものは、地域名から検索できるデータベースを準備してその結果をコードにする
#
# 市区町村については、元データで全てコードと一体になった名称がある。
# 一方で、「中央区」等の異なる都道府県で重複する市区町村名があり、
# 地域名からのコード検索はできないので、元データから分離する必要あり
#' @importFrom rlang .data
.add_area_item <- function(df, code_db, src_col_name = "area"){

  # areaの文字列をコードと文字に分離
  # example 01000北海道 01000 と 北海道
  area_name_v <- df[[src_col_name]] %>% .setup_areaname()
  area_code_v <- df[[src_col_name]] %>% .setup_areacode()

  # コードを含むもの
  tmp_with_code <- df %>%
    dplyr::mutate(`地域` = area_name_v,
                  area_code = area_code_v) %>%
    dplyr::filter(!is.na(.data$area_code))

  # コードを含まないもの
  tmp_without_code <- df %>%
    dplyr::mutate(`地域` = area_name_v) %>%
    dplyr::filter(is.na(area_code_v)) %>%
    dplyr::left_join(code_db, by = "地域")

  ans <-
    dplyr::bind_rows(tmp_with_code, tmp_without_code)

  return(ans)
}


# コード部分のみを取り出す
.setup_areacode <- function(areaname_col){
  ans <- areaname_col %>%
    stringr::str_match("^(\\d+)") %>% `[`(,2)

  return(ans)
}

# コード部分を消去
# 都道府県を末尾につける
.setup_areaname <- function(areaname_col){
  ans <- areaname_col %>%
    stringr::str_replace("計","") %>%
    stringr::str_replace("^\\d+","") %>%
    stringr::str_replace("^青森$","青森県") %>%
    stringr::str_replace("^岩手$","岩手県") %>%
    stringr::str_replace("^宮城$","宮城県") %>%
    stringr::str_replace("^秋田$","秋田県") %>%
    stringr::str_replace("^山形$","山形県") %>%
    stringr::str_replace("^福島$","福島県") %>%
    stringr::str_replace("^茨城$","茨城県") %>%
    stringr::str_replace("^栃木$","栃木県") %>%
    stringr::str_replace("^群馬$","群馬県") %>%
    stringr::str_replace("^埼玉$","埼玉県") %>%
    stringr::str_replace("^千葉$","千葉県") %>%
    stringr::str_replace("^神奈川$","神奈川県") %>%
    stringr::str_replace("^新潟$","新潟県") %>%
    stringr::str_replace("^富山$","富山県") %>%
    stringr::str_replace("^石川$","石川県") %>%
    stringr::str_replace("^福井$","福井県") %>%
    stringr::str_replace("^山梨$","山梨県") %>%
    stringr::str_replace("^長野$","長野県") %>%
    stringr::str_replace("^岐阜$","岐阜県") %>%
    stringr::str_replace("^静岡$","静岡県") %>%
    stringr::str_replace("^愛知$","愛知県") %>%
    stringr::str_replace("^三重$","三重県") %>%
    stringr::str_replace("^滋賀$","滋賀県") %>%
    stringr::str_replace("^京都$","京都府") %>%
    stringr::str_replace("^大阪$","大阪府") %>%
    stringr::str_replace("^兵庫$","兵庫県") %>%
    stringr::str_replace("^奈良$","奈良県") %>%
    stringr::str_replace("^和歌山$","和歌山県") %>%
    stringr::str_replace("^鳥取$","鳥取県") %>%
    stringr::str_replace("^島根$","島根県") %>%
    stringr::str_replace("^岡山$","岡山県") %>%
    stringr::str_replace("^広島$","広島県") %>%
    stringr::str_replace("^山口$","山口県") %>%
    stringr::str_replace("^徳島$","徳島県") %>%
    stringr::str_replace("^香川$","香川県") %>%
    stringr::str_replace("^愛媛$","愛媛県") %>%
    stringr::str_replace("^高知$","高知県") %>%
    stringr::str_replace("^福岡$","福岡県") %>%
    stringr::str_replace("^佐賀$","佐賀県") %>%
    stringr::str_replace("^長崎$","長崎県") %>%
    stringr::str_replace("^熊本$","熊本県") %>%
    stringr::str_replace("^大分$","大分県") %>%
    stringr::str_replace("^宮崎$","宮崎県") %>%
    stringr::str_replace("^鹿児島$","鹿児島県") %>%
    stringr::str_replace("^沖縄$","沖縄県")

  return(ans)

}

