# ESTATから対象のデータをダウンロードしてくる。

# ※作業ディレクトリの変更
# 適時内容は書き換える
working_dir = "C:/R/script/toukei_mext_school_elementaly"

# 提供統計名	学校基本調査
# 提供分類2	初等中等教育機関・専修学校・各種学校《報告書未掲載集計》
# 提供分類3	市町村別集計
# 統計表名	市町村別集計　学校調査（小学校）




# 作業ディレクトリの指定
setwd(working_dir)

# ライブラリ
library("tidyverse")
library("readxl")
library("magrittr")

# 2018
# https://www.e-stat.go.jp/stat-search/files?page=1&query=%E5%B8%82%E7%94%BA%E6%9D%91&layout=dataset&toukei=00400001&year=20180&stat_infid=000031812534&second2=1
# https://www.e-stat.go.jp/stat-search/file-download?statInfId=000031812534&fileKind=0
curl <-"https://www.e-stat.go.jp/stat-search/file-download?statInfId=000031812534&fileKind=0"
cdestfile <- "./input/school_elementely_2018.xlsx"
download.file(curl,cdestfile,mode="wb") # mode="wb"（→バイナリ式）にしておかないと正しくダウンロードできない。

# 2017年
# https://www.e-stat.go.jp/stat-search/files?page=1&query=%E5%B8%82%E7%94%BA%E6%9D%91&layout=dataset&toukei=00400001&year=20170&second2=1
# https://www.e-stat.go.jp/stat-search/files?page=1&query=%E5%B8%82%E7%94%BA%E6%9D%91&layout=dataset&toukei=00400001&year=20170&stat_infid=000031686092&second2=1
curl <- "https://www.e-stat.go.jp/stat-search/file-download?statInfId=000031686092&fileKind=0"
cdestfile <- "./input/school_elementely_2017.xlsx"
download.file(curl,cdestfile,mode="wb")


# 2016 何さファイル名が違うじゃん。
# https://www.e-stat.go.jp/stat-search/files?page=1&query=%E5%B8%82%E7%94%BA%E6%9D%91&layout=dataset&toukei=00400001&year=20160&stat_infid=000031543953&second2=1
# https://www.e-stat.go.jp/stat-search/file-download?statInfId=000031543953&fileKind=0
curl <-"https://www.e-stat.go.jp/stat-search/file-download?statInfId=000031543953&fileKind=0"
cdestfile <- "./input/school_elementely_2016.xlsx"
download.file(curl,cdestfile,mode="wb")

# 2015年 はDBもあるのか！ そしてファイル形式がxlsだ
# https://www.e-stat.go.jp/stat-search/files?page=1&query=%E5%B8%82%E7%94%BA%E6%9D%91&layout=dataset&toukei=00400001&year=20150&stat_infid=000031392297&second2=1
# https://www.e-stat.go.jp/stat-search/file-download?statInfId=000031392297&fileKind=0
curl <-"https://www.e-stat.go.jp/stat-search/file-download?statInfId=000031392297&fileKind=0"
cdestfile <- "./input/school_elementely_2015.xls"
download.file(curl,cdestfile,mode="wb")

# 2014年 はDBもあるのか！ そしてファイル形式がxlsだ
# https://www.e-stat.go.jp/stat-search/files?page=1&query=%E5%B8%82%E7%94%BA%E6%9D%91&layout=dataset&toukei=00400001&year=20140&stat_infid=000027601765&second2=1
# https://www.e-stat.go.jp/stat-search/file-download?statInfId=000027601765&fileKind=0
curl <-"https://www.e-stat.go.jp/stat-search/file-download?statInfId=000027601765&fileKind=0"
cdestfile <- "./input/school_elementely_2014.xls"
download.file(curl,cdestfile,mode="wb")



