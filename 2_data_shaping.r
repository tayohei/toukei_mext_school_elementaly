# �_�E�����[�h�����f�[�^�𐮌`���āACSV�ŏo�͂���

# ����ƃf�B���N�g���̕ύX
# �K�����e�͏���������
working_dir = "C:/R/script/toukei_mext_school_elementaly"


# �w�Z���i�֐���`�j
read_school_num <- function(input_filename, f_year){

# input_filename  = "ey0318_2015.xlsx"
# f_year = 2015

input_sheet = "SYT20001"
output_filename = str_c("./output/�w�Z��{����_�s�����ʏW�v_���w�Z_�w�Z��_",f_year,".csv")
if(f_year==2018){ # 2018�N�͍����Ƃ����񂪒ǉ�����Ă�A�Ȃ�Ŗ��N�ς��̂�A�A
	col_label <- c(
		"�s������",NA,NA,NA,"����_����_�{�Z",NA,NA,NA,
		NA,"����_�s�i��j��_�{�Z","����_�s�i��j��_���Z",
		NA,"����_����_�{�Z","����_����_���Z",
		NA,"����_����_�{�Z","����_����_���Z",
		NA,"����_�g����_�{�Z","����_�g����_���Z",
		"����_����_�{�Z"
	)	
}else if(f_year<=2016){ # 2016�N�͎����̗񂪂Ȃ��A�Ȃ�ŃR���R���ς��̂�A�A
	col_label <- c(
		"�s������",NA,NA,NA,NA,NA,NA,
		NA,"����_�s�i��j��_�{�Z","����_�s�i��j��_���Z",
		NA,"����_����_�{�Z","����_����_���Z",
		NA,"����_����_�{�Z","����_����_���Z",
		NA,"����_�g����_�{�Z","����_�g����_���Z"
	)	
}else{
	col_label <- c(
		"�s������",NA,NA,NA,NA,NA,NA,
		NA,"����_�s�i��j��_�{�Z","����_�s�i��j��_���Z",
		NA,"����_����_�{�Z","����_����_���Z",
		NA,"����_����_�{�Z","����_����_���Z",
		NA,"����_�g����_�{�Z","����_�g����_���Z",
		"����_����_�{�Z"
	)	
}

read_data <- read_excel(input_filename,sheet=input_sheet, col_names=F) 

tmp <- read_data %>%
    filter(rowSums(is.na(.)) < ncol(.)) %>% # ��s�̍폜
    select_if(colSums(is.na(.)) < nrow(.)) %>% # ���̍폜
    # �s�v����������񖼂��Y��ɂ���
    select_if(!is.na(col_label)) %>%
    set_colnames(col_label[!is.na(col_label)]) %>%	
    # �s�������Ƃ��̑O�ɋL�ڂ���Ă���R�[�h�𕪂���
    separate(�s������,into=c("�s����ID","�s������"),sep="\\s") %>% 
    # ���̗��ǉ��i�����Ƃ��܂����������肻���j
    mutate(�s���{�� = if_else(str_detect(�s����ID,"^(\\d|�v).*"),NA_character_,�s����ID)) %>%
    fill(�s���{��) %>%
    # �s�v�s������
    filter(str_detect(�s����ID,"^\\d.*")) %>%
    # �c�����ɂ���
    gather(key="����",value="�w�Z��",-�s����ID, -�s������, -�s���{��) %>%
    separate(����,into=c("��������","�ݒu���","�{�Z���Z"),sep="_") %>%
    # �N�x������
    mutate(�N�x = f_year) %>%
    select(�N�x, �s���{��, �s����ID, �s������ ,��������, �ݒu���, �{�Z���Z, �w�Z��)
# �����o��
write.csv(tmp,output_filename,row.names=F)


}

# �Ґ������ʊw�����i�֐���`�j
read_class_num <- function(input_filename, f_year){
	
# input_filename  = "ey0318_2015.xlsx"
# f_year = 2015

input_sheet = "SYT20032"
output_filename = str_c("./output/�w�Z��{����_�s�����ʏW�v_���w�Z_����_�Ґ������ʊw����_",f_year,".csv")
col_label <- c(
	"�s������",NA,
	NA,"�P���w��_1�w�N","�P���w��_2�w�N","�P���w��_3�w�N","�P���w��_4�w�N","�P���w��_5�w�N","�P���w��_6�w�N",
	NA,"�����w��_2�N","�����w��_3�N","�����w��_4�N","�����w��_5�N","�����w��_6�N",
	NA,"���ʎx���w��_�m�I��Q","���ʎx���w��_���̕s���R","���ʎx���w��_�a��E�g�̋���","���ʎx���w��_�㎋","���ʎx���w��_�","���ʎx���w��_�����Q","���ʎx���w��_���ǁE���Q"
)

read_data <- read_excel(input_filename,sheet=input_sheet, col_names=F) 

tmp <- read_data %>%
    filter(rowSums(is.na(.)) < ncol(.)) %>% # ��s�̍폜
    select_if(colSums(is.na(.)) < nrow(.)) %>% # ���̍폜
    # �s�v����������񖼂��Y��ɂ���
    select_if(!is.na(col_label)) %>%
    set_colnames(col_label[!is.na(col_label)]) %>%	
    # �s�������Ƃ��̑O�ɋL�ڂ���Ă���R�[�h�𕪂���
    separate(�s������,into=c("�s����ID","�s������"),sep="\\s") %>% 
    # ���̗��ǉ��i�����Ƃ��܂����������肻���j
    mutate(�s���{�� = if_else(str_detect(�s����ID,"^(\\d|�v).*"),NA_character_,�s����ID)) %>%
    fill(�s���{��) %>%
    # �s�v�s������
    filter(str_detect(�s����ID,"^\\d.*")) %>%
    # �c�����ɂ���
    gather(key="����",value="�w����",-�s����ID, -�s������, -�s���{��) %>%
    separate(����,into=c("�w�����","�w�N���"),sep="_") %>%
    # �N�x������
    mutate(�N�x = f_year) %>%
    select(�N�x, �s���{��, �s����ID, �s������,�w�����,�w�N���, �w����)
# �����o��
write.csv(tmp,output_filename,row.names=F)


}

# �Ґ������ʎ������i�֐���`�j
read_students_num <- function(input_filename, f_year){
	
# input_filename  = "ey0318_2015.xlsx"
# f_year = 2015

input_sheet = "SYT20056"
output_filename = str_c("./output/�w�Z��{����_�s�����ʏW�v_���w�Z_����_�Ґ������ʎ�����_",f_year,".csv")
col_label <- c(
	"�s������",NA,
	NA,"�P���w��_1�w�N","�P���w��_2�w�N","�P���w��_3�w�N","�P���w��_4�w�N","�P���w��_5�w�N","�P���w��_6�w�N",
	NA,"�����w��_2�N","�����w��_3�N","�����w��_4�N","�����w��_5�N","�����w��_6�N",
	NA,"���ʎx���w��_�m�I��Q","���ʎx���w��_���̕s���R","���ʎx���w��_�a��E�g�̋���","���ʎx���w��_�㎋","���ʎx���w��_�","���ʎx���w��_�����Q","���ʎx���w��_���ǁE���Q"
)

read_data <- read_excel(input_filename,sheet=input_sheet, col_names=F) 

tmp <- read_data %>%
    filter(rowSums(is.na(.)) < ncol(.)) %>% # ��s�̍폜
    select_if(colSums(is.na(.)) < nrow(.)) %>% # ���̍폜
    # �s�v����������񖼂��Y��ɂ���
    select_if(!is.na(col_label)) %>%
    set_colnames(col_label[!is.na(col_label)]) %>%	
    # �s�������Ƃ��̑O�ɋL�ڂ���Ă���R�[�h�𕪂���
    separate(�s������,into=c("�s����ID","�s������"),sep="\\s") %>% 
    # ���̗��ǉ��i�����Ƃ��܂����������肻���j
    mutate(�s���{�� = if_else(str_detect(�s����ID,"^(\\d|�v).*"),NA_character_,�s����ID)) %>%
    fill(�s���{��) %>%
    # �s�v�s������
    filter(str_detect(�s����ID,"^\\d.*")) %>%
    # �c�����ɂ���
    gather(key="����",value="������",-�s����ID, -�s������, -�s���{��) %>%
    separate(����,into=c("�w�����","�w�N���"),sep="_") %>%
    # �N�x������
    mutate(�N�x = f_year) %>%
    select(�N�x, �s���{��, �s����ID, �s������,�w�����,�w�N���, ������)
# �����o��
write.csv(tmp,output_filename,row.names=F)


}

# �w�N�ʎ������i�֐���`�j
read_grade_student_num <- function(input_filename, f_year){
	
# input_filename  = "ey0318_2015.xlsx"
# f_year = 2015

input_sheet = "SYT20052"
output_filename = str_c("./output/�w�Z��{����_�s�����ʏW�v_���w�Z_����_�w�N�ʎ�����_",f_year,".csv")
col_label <- c(
	"�s������",NA,NA,NA,
	NA,"1�w�N_�j","1�w�N_��",
	NA,"2�w�N_�j","2�w�N_��",
	NA,"3�w�N_�j","3�w�N_��",
	NA,"4�w�N_�j","4�w�N_��",
	NA,"5�w�N_�j","5�w�N_��",
	NA,"6�w�N_�j","6�w�N_��"
)

read_data <- read_excel(input_filename,sheet=input_sheet, col_names=F) 

tmp <- read_data %>%
    filter(rowSums(is.na(.)) < ncol(.)) %>% # ��s�̍폜
    select_if(colSums(is.na(.)) < nrow(.)) %>% # ���̍폜
    # �s�v����������񖼂��Y��ɂ���
    select_if(!is.na(col_label)) %>%
    set_colnames(col_label[!is.na(col_label)]) %>%	
    # �s�������Ƃ��̑O�ɋL�ڂ���Ă���R�[�h�𕪂���
    separate(�s������,into=c("�s����ID","�s������"),sep="\\s") %>% 
    # ���̗��ǉ��i�����Ƃ��܂����������肻���j
    mutate(�s���{�� = if_else(str_detect(�s����ID,"^(\\d|�v).*"),NA_character_,�s����ID)) %>%
    fill(�s���{��) %>%
    # �s�v�s������
    filter(str_detect(�s����ID,"^\\d.*")) %>%
    # �c�����ɂ���
    gather(key="����",value="������",-�s����ID, -�s������, -�s���{��) %>%
    separate(����,into=c("�w�N","����"),sep="_") %>%
    # �N�x������
    mutate(�N�x = f_year) %>%
    select(�N�x, �s���{��, �s����ID, �s������,�w�N,����, ������)
# �����o��
write.csv(tmp,output_filename,row.names=F)

}

# �E���ʋ������i�{���ҁj�i�֐���`�j
read_teacher_main_num <- function(input_filename, f_year){
	
# input_filename  = "ey0318_2015.xlsx"
# f_year = 2015

input_sheet = "SYT20074"
output_filename = str_c("./output/�w�Z��{����_�s�����ʏW�v_���w�Z_����_�E���ʋ������i�{���ҁj_",f_year,".csv")

if(f_year==2018){
	col_label <- c(
		"�s������",NA,NA,NA,
		NA,"�Z��_�j","�Z��_��",
		NA,	"���Z��_�j","���Z��_��",
		NA,	"����_�j","����_��",
		NA,	"�劲���@_�j","�劲���@_��",
		NA,	"�w�����@_�j","�w�����@_��",
		NA,	"���@_�j","���@_��",
		NA,	"�����@_�j","�����@_��",
		NA,	"�{�싳�@_�j","�{�싳�@_��",
		NA,	"�{�쏕���@_�j","�{�쏕���@_��",
		NA,	"�h�{���@_�j","�h�{���@_��",
		NA,	"�u�t_�j","�u�t_��",
		NA,NA,NA
	)
}else{
	col_label <- c(
		"�s������",NA,NA,NA,
		NA,"�Z��_�j","�Z��_��",
		NA,	"���Z��_�j","���Z��_��",
		NA,	"����_�j","����_��",
		NA,	"�劲���@_�j","�劲���@_��",
		NA,	"�w�����@_�j","�w�����@_��",
		NA,	"���@_�j","���@_��",
		NA,	"�����@_�j","�����@_��",
		NA,	"�{�싳�@_�j","�{�싳�@_��",
		NA,	"�{�쏕���@_�j","�{�쏕���@_��",
		NA,	"�h�{���@_�j","�h�{���@_��",
		NA,	"�u�t_�j","�u�t_��"
	)
}

read_data <- read_excel(input_filename,sheet=input_sheet, col_names=F) 

tmp <- read_data %>%
    filter(rowSums(is.na(.)) < ncol(.)) %>% # ��s�̍폜
    select_if(colSums(is.na(.)) < nrow(.)) %>% # ���̍폜
    # �s�v����������񖼂��Y��ɂ���
    select_if(!is.na(col_label)) %>%
    set_colnames(col_label[!is.na(col_label)]) %>%	
    # �s�������Ƃ��̑O�ɋL�ڂ���Ă���R�[�h�𕪂���
    separate(�s������,into=c("�s����ID","�s������"),sep="\\s") %>% 
    # ���̗��ǉ��i�����Ƃ��܂����������肻���j
    mutate(�s���{�� = if_else(str_detect(�s����ID,"^(\\d|�v).*"),NA_character_,�s����ID)) %>%
    fill(�s���{��) %>%
    # �s�v�s������
    filter(str_detect(�s����ID,"^\\d.*")) %>%
    # �c�����ɂ���
    gather(key="����",value="������",-�s����ID, -�s������, -�s���{��) %>%
    separate(����,into=c("�E��","����"),sep="_") %>%
    # �N�x������
    mutate(
        �N�x = f_year,
        �{������="�{��"
    ) %>%
    select(�N�x, �s���{��, �s����ID, �s������,�{������,�E��,����, ������)
# �����o��
write.csv(tmp,output_filename,row.names=F)

}




# �E���ʋ������i�����ҁj�i�֐���`�j
read_teacher_sub_num <- function(input_filename, f_year){

# input_filename  = "ey0318_2015.xlsx"
# f_year = 2015

input_sheet = "SYT20078"
output_filename = str_c("./output/�w�Z��{����_�s�����ʏW�v_���w�Z_����_�E���ʋ������i�����ҁj_",f_year,".csv")

if(f_year==2018){
	col_label <- c(
		"�s������",NA,NA,NA,
		NA,"�Z��_�j","�Z��_��",
		NA,	"���Z��_�j","���Z��_��",
		NA,	"����_�j","����_��",
		NA,	"�劲���@_�j","�劲���@_��",
		NA,	"�w�����@_�j","�w�����@_��",
		NA,	"���@_�j","���@_��",
		NA,	"�����@_�j","�����@_��",
		NA,	"�{�싳�@_�j","�{�싳�@_��",
		NA,	"�{�쏕���@_�j","�{�쏕���@_��",
		NA,	"�h�{���@_�j","�h�{���@_��",
		NA,	"�u�t_�j","�u�t_��",
		NA,NA,NA
	)
}else{
	col_label <- c(
		"�s������",NA,NA,NA,
		NA,"�Z��_�j","�Z��_��",
		NA,	"���Z��_�j","���Z��_��",
		NA,	"����_�j","����_��",
		NA,	"�劲���@_�j","�劲���@_��",
		NA,	"�w�����@_�j","�w�����@_��",
		NA,	"���@_�j","���@_��",
		NA,	"�����@_�j","�����@_��",
		NA,	"�{�싳�@_�j","�{�싳�@_��",
		NA,	"�{�쏕���@_�j","�{�쏕���@_��",
		NA,	"�h�{���@_�j","�h�{���@_��",
		NA,	"�u�t_�j","�u�t_��"
	)
}

read_data <- read_excel(input_filename,sheet=input_sheet, col_names=F) 

tmp <- read_data %>%
    filter(rowSums(is.na(.)) < ncol(.)) %>% # ��s�̍폜
    select_if(colSums(is.na(.)) < nrow(.)) %>% # ���̍폜
    # �s�v����������񖼂��Y��ɂ���
    select_if(!is.na(col_label)) %>%
    set_colnames(col_label[!is.na(col_label)]) %>%	
    # �s�������Ƃ��̑O�ɋL�ڂ���Ă���R�[�h�𕪂���
    separate(�s������,into=c("�s����ID","�s������"),sep="\\s") %>% 
    # ���̗��ǉ��i�����Ƃ��܂����������肻���j
    mutate(�s���{�� = if_else(str_detect(�s����ID,"^(\\d|�v).*"),NA_character_,�s����ID)) %>%
    fill(�s���{��) %>%
    # �s�v�s������
    filter(str_detect(�s����ID,"^\\d.*")) %>%
    # �c�����ɂ���
    gather(key="����",value="������",-�s����ID, -�s������, -�s���{��) %>%
    separate(����,into=c("�E��","����"),sep="_") %>%
    # �N�x������
    mutate(
        �N�x = f_year,
        �{������="����") %>%
    select(�N�x, �s���{��, �s����ID, �s������,�{������,�E��,����, ������)
# �����o��
write.csv(tmp,output_filename,row.names=F)

}




# �{�������̂���������C���̐��i�֐���`�j
read_teacher_role_num <- function(input_filename, f_year){
	
# input_filename  = "ey0318_2015.xlsx"
# f_year = 2015

input_sheet = "SYT20087"
output_filename = str_c("./output/�w�Z��{����_�s�����ʏW�v_���w�Z_����_�{�������̂���������C���̐�_",f_year,".csv")
col_label <- c(
	"�s������",
	"��ʋ��E��_������C","��ʋ��E��_�w�N��C","��ʋ��E��_�ی��厖","��ʋ��E��_�i�����@","��ʋ��E��_�Ɋ�",
	"���ʎx���w���S������_���ʎx���w�Z�Ƌ��󏊗L��","���ʎx���w���S������_���ʎx���w�Z�Ƌ���񏊗L��",
	"�Y�x��֋��E��_���Z���E�����E�劲���@�E�w�����@�E���@�E�����@�E�u�t","�Y�x��֋��E��_�{�싳�@�E�{�쏕���@�E�h�{���@","�Y�x��֋��E��_�����E��","�Y�x��֋��E��_�w�Z�h�{�E��",
	"�玙�x�Ƒ�֋���_���Z���E�����E�劲���@�E�w�����@�E���@�E�����@�E�u�t","�玙�x�Ƒ�֋���_�{�싳�@�E�{�쏕���@�E�h�{���@"
)

read_data <- read_excel(input_filename,sheet=input_sheet, col_names=F) 

tmp <- read_data %>%
    filter(rowSums(is.na(.)) < ncol(.)) %>% # ��s�̍폜
    select_if(colSums(is.na(.)) < nrow(.)) %>% # ���̍폜
    # �s�v����������񖼂��Y��ɂ���
    select_if(!is.na(col_label)) %>%
    set_colnames(col_label[!is.na(col_label)]) %>%	
    # �s�������Ƃ��̑O�ɋL�ڂ���Ă���R�[�h�𕪂���
    separate(�s������,into=c("�s����ID","�s������"),sep="\\s") %>% 
    # ���̗��ǉ��i�����Ƃ��܂����������肻���j
    mutate(�s���{�� = if_else(str_detect(�s����ID,"^(\\d|�v).*"),NA_character_,�s����ID)) %>%
    fill(�s���{��) %>%
    # �s�v�s������
    filter(str_detect(�s����ID,"^\\d.*")) %>%
    # �c�����ɂ���
    gather(key="����",value="������",-�s����ID, -�s������, -�s���{��) %>%
    separate(����,into=c("���E�����","����"),sep="_") %>%
    # �N�x������
    mutate(�N�x = f_year) %>%
    select(�N�x, �s���{��, �s����ID, �s������,���E�����,����, ������)
# �����o��
write.csv(tmp,output_filename,row.names=F)

}

# ���ꂻ����s
for(f_year in 2015:2018){
	# f_year = 2014
	if(f_year<=2015){ # 2015�ȑO�̓t�@�C���`����xls�`��
		filename = str_c("./input/school_elementely_",f_year,".xls")
	}else{
		filename = str_c("./input/school_elementely_",f_year,".xlsx")
	}
	print(f_year)
	# �w�Z��
	read_school_num(filename, f_year)
	# �Ґ������ʊw����
	read_class_num(filename, f_year)
	# �Ґ������ʎ������i�֐���`�j
	read_students_num(filename, f_year)
	# �w�N�ʎ������i�֐���`�j
	read_grade_student_num(filename, f_year)
	# �E���ʋ������i�{���ҁj�i�֐���`�j
	read_teacher_main_num(filename, f_year)
	# �E���ʋ������i�����ҁj�i�֐���`�j
	read_teacher_sub_num(filename, f_year)
	# �{�������̂���������C���̐��i�֐���`�j
	read_teacher_role_num(filename, f_year)
}
