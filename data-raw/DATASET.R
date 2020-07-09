## code to prepare `DATASET` dataset goes here
library(noce)
library(tidyverse)

#############################################################
# download target excel files from e-stats
#############################################################

dlfiles_path <- "data-raw/t15_excelfiles/"

# filedownload
# keyring::key_set("e-stat")
# noce::noce_download_excel_files(appID = keyring::key_get("e-stat"),
#                                 table = "15",
#                                 dist_path = dlfiles_path)

excelfiles <- dir(dlfiles_path, full.names = T)

jyutaku_kosuu <- 
  noce_residence_read_files(file_path_v = excelfiles)


# 戸数(tab_code == 18)、新設(cat02_code == 12)のデータのみを抜き出す。
# 必要な列のみを残す
jyutaku_kosuu <- 
  jyutaku_kosuu %>% 
  filter(tab_code == 18, cat02_code == 12) %>% 
  select(cat01_code, `利用関係`, area_code, `地域`, date, value)

usethis::use_data(jyutaku_kosuu, overwrite = TRUE)
