# ダウンロードしたデータを整形して、CSVで出力する

# ※作業ディレクトリの変更
# 適時内容は書き換える
working_dir = "C:/R/script/toukei_mext_school_elementaly"


# 学校数（関数定義）
read_school_num <- function(input_filename, f_year){

# input_filename  = "ey0318_2015.xlsx"
# f_year = 2015

input_sheet = "SYT20001"
output_filename = str_c("./output/学校基本調査_市町村別集計_小学校_学校数_",f_year,".csv")
if(f_year==2018){ # 2018年は国立という列が追加されてる、なんで毎年変わるのよ、、
	col_label <- c(
		"市町村名",NA,NA,NA,"国立_国立_本校",NA,NA,NA,
		NA,"公立_市（区）立_本校","公立_市（区）立_分校",
		NA,"公立_町立_本校","公立_町立_分校",
		NA,"公立_村立_本校","公立_村立_分校",
		NA,"公立_組合立_本校","公立_組合立_分校",
		"私立_私立_本校"
	)	
}else if(f_year<=2016){ # 2016年は私立の列がない、なんでコロコロ変わるのよ、、
	col_label <- c(
		"市町村名",NA,NA,NA,NA,NA,NA,
		NA,"公立_市（区）立_本校","公立_市（区）立_分校",
		NA,"公立_町立_本校","公立_町立_分校",
		NA,"公立_村立_本校","公立_村立_分校",
		NA,"公立_組合立_本校","公立_組合立_分校"
	)	
}else{
	col_label <- c(
		"市町村名",NA,NA,NA,NA,NA,NA,
		NA,"公立_市（区）立_本校","公立_市（区）立_分校",
		NA,"公立_町立_本校","公立_町立_分校",
		NA,"公立_村立_本校","公立_村立_分校",
		NA,"公立_組合立_本校","公立_組合立_分校",
		"私立_私立_本校"
	)	
}

read_data <- read_excel(input_filename,sheet=input_sheet, col_names=F) 

tmp <- read_data %>%
    filter(rowSums(is.na(.)) < ncol(.)) %>% # 空行の削除
    select_if(colSums(is.na(.)) < nrow(.)) %>% # 空列の削除
    # 不要列を消す＆列名を綺麗にする
    select_if(!is.na(col_label)) %>%
    set_colnames(col_label[!is.na(col_label)]) %>%	
    # 市町村名とその前に記載されているコードを分ける
    separate(市町村名,into=c("市町村ID","市町村名"),sep="\\s") %>% 
    # 県の列を追加（もっとうまいやり方がありそう）
    mutate(都道府県 = if_else(str_detect(市町村ID,"^(\\d|計).*"),NA_character_,市町村ID)) %>%
    fill(都道府県) %>%
    # 不要行を消す
    filter(str_detect(市町村ID,"^\\d.*")) %>%
    # 縦持ちにする
    gather(key="分類",value="学校数",-市町村ID, -市町村名, -都道府県) %>%
    separate(分類,into=c("公立私立","設置主体","本校分校"),sep="_") %>%
    # 年度を入れる
    mutate(年度 = f_year) %>%
    select(年度, 都道府県, 市町村ID, 市町村名 ,公立私立, 設置主体, 本校分校, 学校数)
# 書き出し
write.csv(tmp,output_filename,row.names=F)


}

# 編制方式別学級数（関数定義）
read_class_num <- function(input_filename, f_year){
	
# input_filename  = "ey0318_2015.xlsx"
# f_year = 2015

input_sheet = "SYT20032"
output_filename = str_c("./output/学校基本調査_市町村別集計_小学校_公立_編制方式別学級数_",f_year,".csv")
col_label <- c(
	"市町村名",NA,
	NA,"単式学級_1学年","単式学級_2学年","単式学級_3学年","単式学級_4学年","単式学級_5学年","単式学級_6学年",
	NA,"複式学級_2個年","複式学級_3個年","複式学級_4個年","複式学級_5個年","複式学級_6個年",
	NA,"特別支援学級_知的障害","特別支援学級_肢体不自由","特別支援学級_病弱・身体虚弱","特別支援学級_弱視","特別支援学級_難聴","特別支援学級_言語障害","特別支援学級_自閉症・情緒障害"
)

read_data <- read_excel(input_filename,sheet=input_sheet, col_names=F) 

tmp <- read_data %>%
    filter(rowSums(is.na(.)) < ncol(.)) %>% # 空行の削除
    select_if(colSums(is.na(.)) < nrow(.)) %>% # 空列の削除
    # 不要列を消す＆列名を綺麗にする
    select_if(!is.na(col_label)) %>%
    set_colnames(col_label[!is.na(col_label)]) %>%	
    # 市町村名とその前に記載されているコードを分ける
    separate(市町村名,into=c("市町村ID","市町村名"),sep="\\s") %>% 
    # 県の列を追加（もっとうまいやり方がありそう）
    mutate(都道府県 = if_else(str_detect(市町村ID,"^(\\d|計).*"),NA_character_,市町村ID)) %>%
    fill(都道府県) %>%
    # 不要行を消す
    filter(str_detect(市町村ID,"^\\d.*")) %>%
    # 縦持ちにする
    gather(key="分類",value="学級数",-市町村ID, -市町村名, -都道府県) %>%
    separate(分類,into=c("学級種別","学年種別"),sep="_") %>%
    # 年度を入れる
    mutate(年度 = f_year) %>%
    select(年度, 都道府県, 市町村ID, 市町村名,学級種別,学年種別, 学級数)
# 書き出し
write.csv(tmp,output_filename,row.names=F)


}

# 編制方式別児童数（関数定義）
read_students_num <- function(input_filename, f_year){
	
# input_filename  = "ey0318_2015.xlsx"
# f_year = 2015

input_sheet = "SYT20056"
output_filename = str_c("./output/学校基本調査_市町村別集計_小学校_公立_編制方式別児童数_",f_year,".csv")
col_label <- c(
	"市町村名",NA,
	NA,"単式学級_1学年","単式学級_2学年","単式学級_3学年","単式学級_4学年","単式学級_5学年","単式学級_6学年",
	NA,"複式学級_2個年","複式学級_3個年","複式学級_4個年","複式学級_5個年","複式学級_6個年",
	NA,"特別支援学級_知的障害","特別支援学級_肢体不自由","特別支援学級_病弱・身体虚弱","特別支援学級_弱視","特別支援学級_難聴","特別支援学級_言語障害","特別支援学級_自閉症・情緒障害"
)

read_data <- read_excel(input_filename,sheet=input_sheet, col_names=F) 

tmp <- read_data %>%
    filter(rowSums(is.na(.)) < ncol(.)) %>% # 空行の削除
    select_if(colSums(is.na(.)) < nrow(.)) %>% # 空列の削除
    # 不要列を消す＆列名を綺麗にする
    select_if(!is.na(col_label)) %>%
    set_colnames(col_label[!is.na(col_label)]) %>%	
    # 市町村名とその前に記載されているコードを分ける
    separate(市町村名,into=c("市町村ID","市町村名"),sep="\\s") %>% 
    # 県の列を追加（もっとうまいやり方がありそう）
    mutate(都道府県 = if_else(str_detect(市町村ID,"^(\\d|計).*"),NA_character_,市町村ID)) %>%
    fill(都道府県) %>%
    # 不要行を消す
    filter(str_detect(市町村ID,"^\\d.*")) %>%
    # 縦持ちにする
    gather(key="分類",value="児童数",-市町村ID, -市町村名, -都道府県) %>%
    separate(分類,into=c("学級種別","学年種別"),sep="_") %>%
    # 年度を入れる
    mutate(年度 = f_year) %>%
    select(年度, 都道府県, 市町村ID, 市町村名,学級種別,学年種別, 児童数)
# 書き出し
write.csv(tmp,output_filename,row.names=F)


}

# 学年別児童数（関数定義）
read_grade_student_num <- function(input_filename, f_year){
	
# input_filename  = "ey0318_2015.xlsx"
# f_year = 2015

input_sheet = "SYT20052"
output_filename = str_c("./output/学校基本調査_市町村別集計_小学校_公立_学年別児童数_",f_year,".csv")
col_label <- c(
	"市町村名",NA,NA,NA,
	NA,"1学年_男","1学年_女",
	NA,"2学年_男","2学年_女",
	NA,"3学年_男","3学年_女",
	NA,"4学年_男","4学年_女",
	NA,"5学年_男","5学年_女",
	NA,"6学年_男","6学年_女"
)

read_data <- read_excel(input_filename,sheet=input_sheet, col_names=F) 

tmp <- read_data %>%
    filter(rowSums(is.na(.)) < ncol(.)) %>% # 空行の削除
    select_if(colSums(is.na(.)) < nrow(.)) %>% # 空列の削除
    # 不要列を消す＆列名を綺麗にする
    select_if(!is.na(col_label)) %>%
    set_colnames(col_label[!is.na(col_label)]) %>%	
    # 市町村名とその前に記載されているコードを分ける
    separate(市町村名,into=c("市町村ID","市町村名"),sep="\\s") %>% 
    # 県の列を追加（もっとうまいやり方がありそう）
    mutate(都道府県 = if_else(str_detect(市町村ID,"^(\\d|計).*"),NA_character_,市町村ID)) %>%
    fill(都道府県) %>%
    # 不要行を消す
    filter(str_detect(市町村ID,"^\\d.*")) %>%
    # 縦持ちにする
    gather(key="分類",value="児童数",-市町村ID, -市町村名, -都道府県) %>%
    separate(分類,into=c("学年","性別"),sep="_") %>%
    # 年度を入れる
    mutate(年度 = f_year) %>%
    select(年度, 都道府県, 市町村ID, 市町村名,学年,性別, 児童数)
# 書き出し
write.csv(tmp,output_filename,row.names=F)

}

# 職名別教員数（本務者）（関数定義）
read_teacher_main_num <- function(input_filename, f_year){
	
# input_filename  = "ey0318_2015.xlsx"
# f_year = 2015

input_sheet = "SYT20074"
output_filename = str_c("./output/学校基本調査_市町村別集計_小学校_公立_職名別教員数（本務者）_",f_year,".csv")

if(f_year==2018){
	col_label <- c(
		"市町村名",NA,NA,NA,
		NA,"校長_男","校長_女",
		NA,	"副校長_男","副校長_女",
		NA,	"教頭_男","教頭_女",
		NA,	"主幹教諭_男","主幹教諭_女",
		NA,	"指導教諭_男","指導教諭_女",
		NA,	"教諭_男","教諭_女",
		NA,	"助教諭_男","助教諭_女",
		NA,	"養護教諭_男","養護教諭_女",
		NA,	"養護助教諭_男","養護助教諭_女",
		NA,	"栄養教諭_男","栄養教諭_女",
		NA,	"講師_男","講師_女",
		NA,NA,NA
	)
}else{
	col_label <- c(
		"市町村名",NA,NA,NA,
		NA,"校長_男","校長_女",
		NA,	"副校長_男","副校長_女",
		NA,	"教頭_男","教頭_女",
		NA,	"主幹教諭_男","主幹教諭_女",
		NA,	"指導教諭_男","指導教諭_女",
		NA,	"教諭_男","教諭_女",
		NA,	"助教諭_男","助教諭_女",
		NA,	"養護教諭_男","養護教諭_女",
		NA,	"養護助教諭_男","養護助教諭_女",
		NA,	"栄養教諭_男","栄養教諭_女",
		NA,	"講師_男","講師_女"
	)
}

read_data <- read_excel(input_filename,sheet=input_sheet, col_names=F) 

tmp <- read_data %>%
    filter(rowSums(is.na(.)) < ncol(.)) %>% # 空行の削除
    select_if(colSums(is.na(.)) < nrow(.)) %>% # 空列の削除
    # 不要列を消す＆列名を綺麗にする
    select_if(!is.na(col_label)) %>%
    set_colnames(col_label[!is.na(col_label)]) %>%	
    # 市町村名とその前に記載されているコードを分ける
    separate(市町村名,into=c("市町村ID","市町村名"),sep="\\s") %>% 
    # 県の列を追加（もっとうまいやり方がありそう）
    mutate(都道府県 = if_else(str_detect(市町村ID,"^(\\d|計).*"),NA_character_,市町村ID)) %>%
    fill(都道府県) %>%
    # 不要行を消す
    filter(str_detect(市町村ID,"^\\d.*")) %>%
    # 縦持ちにする
    gather(key="分類",value="教員数",-市町村ID, -市町村名, -都道府県) %>%
    separate(分類,into=c("職名","性別"),sep="_") %>%
    # 年度を入れる
    mutate(
        年度 = f_year,
        本務兼務="本務"
    ) %>%
    select(年度, 都道府県, 市町村ID, 市町村名,本務兼務,職名,性別, 教員数)
# 書き出し
write.csv(tmp,output_filename,row.names=F)

}




# 職名別教員数（兼務者）（関数定義）
read_teacher_sub_num <- function(input_filename, f_year){

# input_filename  = "ey0318_2015.xlsx"
# f_year = 2015

input_sheet = "SYT20078"
output_filename = str_c("./output/学校基本調査_市町村別集計_小学校_公立_職名別教員数（兼務者）_",f_year,".csv")

if(f_year==2018){
	col_label <- c(
		"市町村名",NA,NA,NA,
		NA,"校長_男","校長_女",
		NA,	"副校長_男","副校長_女",
		NA,	"教頭_男","教頭_女",
		NA,	"主幹教諭_男","主幹教諭_女",
		NA,	"指導教諭_男","指導教諭_女",
		NA,	"教諭_男","教諭_女",
		NA,	"助教諭_男","助教諭_女",
		NA,	"養護教諭_男","養護教諭_女",
		NA,	"養護助教諭_男","養護助教諭_女",
		NA,	"栄養教諭_男","栄養教諭_女",
		NA,	"講師_男","講師_女",
		NA,NA,NA
	)
}else{
	col_label <- c(
		"市町村名",NA,NA,NA,
		NA,"校長_男","校長_女",
		NA,	"副校長_男","副校長_女",
		NA,	"教頭_男","教頭_女",
		NA,	"主幹教諭_男","主幹教諭_女",
		NA,	"指導教諭_男","指導教諭_女",
		NA,	"教諭_男","教諭_女",
		NA,	"助教諭_男","助教諭_女",
		NA,	"養護教諭_男","養護教諭_女",
		NA,	"養護助教諭_男","養護助教諭_女",
		NA,	"栄養教諭_男","栄養教諭_女",
		NA,	"講師_男","講師_女"
	)
}

read_data <- read_excel(input_filename,sheet=input_sheet, col_names=F) 

tmp <- read_data %>%
    filter(rowSums(is.na(.)) < ncol(.)) %>% # 空行の削除
    select_if(colSums(is.na(.)) < nrow(.)) %>% # 空列の削除
    # 不要列を消す＆列名を綺麗にする
    select_if(!is.na(col_label)) %>%
    set_colnames(col_label[!is.na(col_label)]) %>%	
    # 市町村名とその前に記載されているコードを分ける
    separate(市町村名,into=c("市町村ID","市町村名"),sep="\\s") %>% 
    # 県の列を追加（もっとうまいやり方がありそう）
    mutate(都道府県 = if_else(str_detect(市町村ID,"^(\\d|計).*"),NA_character_,市町村ID)) %>%
    fill(都道府県) %>%
    # 不要行を消す
    filter(str_detect(市町村ID,"^\\d.*")) %>%
    # 縦持ちにする
    gather(key="分類",value="教員数",-市町村ID, -市町村名, -都道府県) %>%
    separate(分類,into=c("職名","性別"),sep="_") %>%
    # 年度を入れる
    mutate(
        年度 = f_year,
        本務兼務="兼務") %>%
    select(年度, 都道府県, 市町村ID, 市町村名,本務兼務,職名,性別, 教員数)
# 書き出し
write.csv(tmp,output_filename,row.names=F)

}




# 本務教員のうち教務主任等の数（関数定義）
read_teacher_role_num <- function(input_filename, f_year){
	
# input_filename  = "ey0318_2015.xlsx"
# f_year = 2015

input_sheet = "SYT20087"
output_filename = str_c("./output/学校基本調査_市町村別集計_小学校_公立_本務教員のうち教務主任等の数_",f_year,".csv")
col_label <- c(
	"市町村名",
	"一般教職員_教務主任","一般教職員_学年主任","一般教職員_保健主事","一般教職員_司書教諭","一般教職員_舎監",
	"特別支援学級担当教員_特別支援学校免許状所有者","特別支援学級担当教員_特別支援学校免許状非所有者",
	"産休代替教職員_副校長・教頭・主幹教諭・指導教諭・教諭・助教諭・講師","産休代替教職員_養護教諭・養護助教諭・栄養教諭","産休代替教職員_事務職員","産休代替教職員_学校栄養職員",
	"育児休業代替教員_副校長・教頭・主幹教諭・指導教諭・教諭・助教諭・講師","育児休業代替教員_養護教諭・養護助教諭・栄養教諭"
)

read_data <- read_excel(input_filename,sheet=input_sheet, col_names=F) 

tmp <- read_data %>%
    filter(rowSums(is.na(.)) < ncol(.)) %>% # 空行の削除
    select_if(colSums(is.na(.)) < nrow(.)) %>% # 空列の削除
    # 不要列を消す＆列名を綺麗にする
    select_if(!is.na(col_label)) %>%
    set_colnames(col_label[!is.na(col_label)]) %>%	
    # 市町村名とその前に記載されているコードを分ける
    separate(市町村名,into=c("市町村ID","市町村名"),sep="\\s") %>% 
    # 県の列を追加（もっとうまいやり方がありそう）
    mutate(都道府県 = if_else(str_detect(市町村ID,"^(\\d|計).*"),NA_character_,市町村ID)) %>%
    fill(都道府県) %>%
    # 不要行を消す
    filter(str_detect(市町村ID,"^\\d.*")) %>%
    # 縦持ちにする
    gather(key="分類",value="教員数",-市町村ID, -市町村名, -都道府県) %>%
    separate(分類,into=c("教職員種別","役種"),sep="_") %>%
    # 年度を入れる
    mutate(年度 = f_year) %>%
    select(年度, 都道府県, 市町村ID, 市町村名,教職員種別,役種, 教員数)
# 書き出し
write.csv(tmp,output_filename,row.names=F)

}

# それそれ実行
for(f_year in 2015:2018){
	# f_year = 2014
	if(f_year<=2015){ # 2015以前はファイル形式がxls形式
		filename = str_c("./input/school_elementely_",f_year,".xls")
	}else{
		filename = str_c("./input/school_elementely_",f_year,".xlsx")
	}
	print(f_year)
	# 学校数
	read_school_num(filename, f_year)
	# 編制方式別学級数
	read_class_num(filename, f_year)
	# 編制方式別児童数（関数定義）
	read_students_num(filename, f_year)
	# 学年別児童数（関数定義）
	read_grade_student_num(filename, f_year)
	# 職名別教員数（本務者）（関数定義）
	read_teacher_main_num(filename, f_year)
	# 職名別教員数（兼務者）（関数定義）
	read_teacher_sub_num(filename, f_year)
	# 本務教員のうち教務主任等の数（関数定義）
	read_teacher_role_num(filename, f_year)
}
