# ESTAT����Ώۂ̃f�[�^���_�E�����[�h���Ă���B

# ����ƃf�B���N�g���̕ύX
# �K�����e�͏���������
working_dir = "C:/R/script/toukei_mext_school_elementaly"

# �񋟓��v��	�w�Z��{����
# �񋟕���2	������������@�ցE��C�w�Z�E�e��w�Z�s�񍐏����f�ڏW�v�t
# �񋟕���3	�s�����ʏW�v
# ���v�\��	�s�����ʏW�v�@�w�Z�����i���w�Z�j




# ��ƃf�B���N�g���̎w��
setwd(working_dir)

# ���C�u����
library("tidyverse")
library("readxl")
library("magrittr")

# 2018
# https://www.e-stat.go.jp/stat-search/files?page=1&query=%E5%B8%82%E7%94%BA%E6%9D%91&layout=dataset&toukei=00400001&year=20180&stat_infid=000031812534&second2=1
# https://www.e-stat.go.jp/stat-search/file-download?statInfId=000031812534&fileKind=0
curl <-"https://www.e-stat.go.jp/stat-search/file-download?statInfId=000031812534&fileKind=0"
cdestfile <- "./input/school_elementely_2018.xlsx"
download.file(curl,cdestfile,mode="wb") # mode="wb"�i���o�C�i�����j�ɂ��Ă����Ȃ��Ɛ������_�E�����[�h�ł��Ȃ��B

# 2017�N
# https://www.e-stat.go.jp/stat-search/files?page=1&query=%E5%B8%82%E7%94%BA%E6%9D%91&layout=dataset&toukei=00400001&year=20170&second2=1
# https://www.e-stat.go.jp/stat-search/files?page=1&query=%E5%B8%82%E7%94%BA%E6%9D%91&layout=dataset&toukei=00400001&year=20170&stat_infid=000031686092&second2=1
curl <- "https://www.e-stat.go.jp/stat-search/file-download?statInfId=000031686092&fileKind=0"
cdestfile <- "./input/school_elementely_2017.xlsx"
download.file(curl,cdestfile,mode="wb")


# 2016 �����t�@�C�������Ⴄ�����B
# https://www.e-stat.go.jp/stat-search/files?page=1&query=%E5%B8%82%E7%94%BA%E6%9D%91&layout=dataset&toukei=00400001&year=20160&stat_infid=000031543953&second2=1
# https://www.e-stat.go.jp/stat-search/file-download?statInfId=000031543953&fileKind=0
curl <-"https://www.e-stat.go.jp/stat-search/file-download?statInfId=000031543953&fileKind=0"
cdestfile <- "./input/school_elementely_2016.xlsx"
download.file(curl,cdestfile,mode="wb")

# 2015�N ��DB������̂��I �����ăt�@�C���`����xls��
# https://www.e-stat.go.jp/stat-search/files?page=1&query=%E5%B8%82%E7%94%BA%E6%9D%91&layout=dataset&toukei=00400001&year=20150&stat_infid=000031392297&second2=1
# https://www.e-stat.go.jp/stat-search/file-download?statInfId=000031392297&fileKind=0
curl <-"https://www.e-stat.go.jp/stat-search/file-download?statInfId=000031392297&fileKind=0"
cdestfile <- "./input/school_elementely_2015.xls"
download.file(curl,cdestfile,mode="wb")

# 2014�N ��DB������̂��I �����ăt�@�C���`����xls��
# https://www.e-stat.go.jp/stat-search/files?page=1&query=%E5%B8%82%E7%94%BA%E6%9D%91&layout=dataset&toukei=00400001&year=20140&stat_infid=000027601765&second2=1
# https://www.e-stat.go.jp/stat-search/file-download?statInfId=000027601765&fileKind=0
curl <-"https://www.e-stat.go.jp/stat-search/file-download?statInfId=000027601765&fileKind=0"
cdestfile <- "./input/school_elementely_2014.xls"
download.file(curl,cdestfile,mode="wb")



