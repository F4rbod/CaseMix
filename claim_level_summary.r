options(repr.matrix.max.rows = 100, repr.matrix.max.cols = 300)
options(repr.plot.width = 20, repr.plot.height = 15)
options(width = 300)

numcores <- 64

library(tidyverse)
library(data.table)
library(fst)
library(comorbidity)
library(reshape)
library(dtplyr)
library(haven)
library(vroom)
library(dplyr)
`%!in%` <- Negate(`%in%`)

setDTthreads(numcores)




# diagnosis codes

office_visit_codes <- c(
  "99201", "99202", "99203", "99204", "99205", "99211", "99212", "99213", "99214",
  "99215"
)

IHD_icd_9_codes <- c(410, 411, 412, 413, 414)
IHD_icd_10_codes <- c("I20", "I21", "I22", "I23", "I24", "I25")

non_us_state_codes <- c(40, 54, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 97, 98, 99)

primary_care_specialty_codes <- c("01", "08", "11", "38")

# http://www.icd9data.com/2015/Volume1/390-459/401-405/default.htm
# https://www.icd10data.com/ICD10CM/Codes/I00-I99/I10-I16
hypertension_icd_9_codes <- c("401", "402", "403", "404", "405")
hypertension_icd_10_codes <- c("I10", "I11", "I12", "I13", "I15", "I16")

# http://www.icd9data.com/2014/Volume1/290-319/295-299/296/default.htm
# https://www.icd10data.com/ICD10CM/Codes/F01-F99/F30-F39
depression_icd_9_codes <- c("2962", "2963")
depression_icd_10_codes <- c("F32", "F33")

# http://www.icd9data.com/2015/Volume1/240-279/249-259/default.htm
# https://www.icd10data.com/ICD10CM/Codes/E00-E89/E08-E13
diabetes_icd_9_codes <- c("250")
diabetes_icd_10_codes <- c("E08", "E09", "E10", "E11", "E13")

# http://www.icd9data.com/2014/Volume1/710-739/710-719/714/default.htm
# https://www.icd10data.com/ICD10CM/Codes/M00-M99/M05-M14
arthritis_icd_9_codes <- c("714")
arthritis_icd_10_codes <- c("M05", "M06", "M07", "M08", "M09", "M10", "M11", "M12", "M13", "M14")


race_codes <- data.frame(
  race_code = seq(0, 6),
  race = c("Unknown", "White", "Black", "Other", "Asian", "Hispanic", "North American Native")
)

sex_codes <- data.frame(
  sex_code = seq(0, 2),
  sex = c("Unknown", "Male", "Female")
)





claim_carrier_all_years=read_fst("/work/postresearch/Shared/Projects/Data_fst/claim_carrier_all_years.fst",as.data.table = T)
claim_inpatient_all_years=read_fst("/work/postresearch/Shared/Projects/Data_fst/claim_inpatient_all_years.fst",as.data.table = T)
claim_outpatient_all_years=read_fst("/work/postresearch/Shared/Projects/Data_fst/claim_outpatient_all_years.fst",as.data.table = T)


conditions_claim_carrier=

claim_carrier_all_years %>%
summarise(
  
  DESY_SORT_KEY = DESY_SORT_KEY,
  
  year=year,
  
  date=date,
  
  month_year=month_year,
  
  
  PRNCPAL_DGNS_is_hypertension=(
    case_when(
      PRNCPAL_DGNS_VRSN_CD == 0 ~ substr(PRNCPAL_DGNS_CD, 0, 3) %in% hypertension_icd_10_codes,
      PRNCPAL_DGNS_VRSN_CD == 9 ~ substr(PRNCPAL_DGNS_CD, 0, 3) %in% hypertension_icd_9_codes)),
  
  
  DGNS_1_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD1 == 0 ~ substr(ICD_DGNS_CD1, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD1 == 9 ~ substr(ICD_DGNS_CD1, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_2_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD2 == 0 ~ substr(ICD_DGNS_CD2, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD2 == 9 ~ substr(ICD_DGNS_CD2, 0, 3) %in% hypertension_icd_9_codes)),

  DGNS_3_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD3 == 0 ~ substr(ICD_DGNS_CD3, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD3 == 9 ~ substr(ICD_DGNS_CD3, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_4_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD4 == 0 ~ substr(ICD_DGNS_CD4, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD4 == 9 ~ substr(ICD_DGNS_CD4, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_6_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD6 == 0 ~ substr(ICD_DGNS_CD6, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD6 == 9 ~ substr(ICD_DGNS_CD6, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_7_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD7 == 0 ~ substr(ICD_DGNS_CD7, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD7 == 9 ~ substr(ICD_DGNS_CD7, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_8_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD8 == 0 ~ substr(ICD_DGNS_CD8, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD8 == 9 ~ substr(ICD_DGNS_CD8, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_9_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD9 == 0 ~ substr(ICD_DGNS_CD9, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD9 == 9 ~ substr(ICD_DGNS_CD9, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_10_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD10 == 0 ~ substr(ICD_DGNS_CD10, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD10 == 9 ~ substr(ICD_DGNS_CD10, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_11_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD11 == 0 ~ substr(ICD_DGNS_CD11, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD11 == 9 ~ substr(ICD_DGNS_CD11, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_12_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD12 == 0 ~ substr(ICD_DGNS_CD12, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD12 == 9 ~ substr(ICD_DGNS_CD12, 0, 3) %in% hypertension_icd_9_codes)),
  
  PRNCPAL_DGNS_is_arthritis=(
    case_when(
      PRNCPAL_DGNS_VRSN_CD == 0 ~ substr(PRNCPAL_DGNS_CD, 0, 3) %in% arthritis_icd_10_codes,
      PRNCPAL_DGNS_VRSN_CD == 9 ~ substr(PRNCPAL_DGNS_CD, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_1_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD1 == 0 ~ substr(ICD_DGNS_CD1, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD1 == 9 ~ substr(ICD_DGNS_CD1, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_2_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD2 == 0 ~ substr(ICD_DGNS_CD2, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD2 == 9 ~ substr(ICD_DGNS_CD2, 0, 3) %in% arthritis_icd_9_codes)),

  DGNS_3_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD3 == 0 ~ substr(ICD_DGNS_CD3, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD3 == 9 ~ substr(ICD_DGNS_CD3, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_4_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD4 == 0 ~ substr(ICD_DGNS_CD4, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD4 == 9 ~ substr(ICD_DGNS_CD4, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_6_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD6 == 0 ~ substr(ICD_DGNS_CD6, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD6 == 9 ~ substr(ICD_DGNS_CD6, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_7_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD7 == 0 ~ substr(ICD_DGNS_CD7, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD7 == 9 ~ substr(ICD_DGNS_CD7, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_8_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD8 == 0 ~ substr(ICD_DGNS_CD8, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD8 == 9 ~ substr(ICD_DGNS_CD8, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_9_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD9 == 0 ~ substr(ICD_DGNS_CD9, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD9 == 9 ~ substr(ICD_DGNS_CD9, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_10_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD10 == 0 ~ substr(ICD_DGNS_CD10, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD10 == 9 ~ substr(ICD_DGNS_CD10, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_11_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD11 == 0 ~ substr(ICD_DGNS_CD11, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD11 == 9 ~ substr(ICD_DGNS_CD11, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_12_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD12 == 0 ~ substr(ICD_DGNS_CD12, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD12 == 9 ~ substr(ICD_DGNS_CD12, 0, 3) %in% arthritis_icd_9_codes)),
  
  PRNCPAL_DGNS_is_IHD=(
    case_when(
      PRNCPAL_DGNS_VRSN_CD == 0 ~ substr(PRNCPAL_DGNS_CD, 0, 3) %in% IHD_icd_10_codes,
      PRNCPAL_DGNS_VRSN_CD == 9 ~ substr(PRNCPAL_DGNS_CD, 0, 3) %in% IHD_icd_9_codes)),
  
    DGNS_1_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD1 == 0 ~ substr(ICD_DGNS_CD1, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD1 == 9 ~ substr(ICD_DGNS_CD1, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_2_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD2 == 0 ~ substr(ICD_DGNS_CD2, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD2 == 9 ~ substr(ICD_DGNS_CD2, 0, 3) %in% IHD_icd_9_codes)),

  DGNS_3_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD3 == 0 ~ substr(ICD_DGNS_CD3, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD3 == 9 ~ substr(ICD_DGNS_CD3, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_4_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD4 == 0 ~ substr(ICD_DGNS_CD4, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD4 == 9 ~ substr(ICD_DGNS_CD4, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_6_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD6 == 0 ~ substr(ICD_DGNS_CD6, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD6 == 9 ~ substr(ICD_DGNS_CD6, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_7_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD7 == 0 ~ substr(ICD_DGNS_CD7, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD7 == 9 ~ substr(ICD_DGNS_CD7, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_8_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD8 == 0 ~ substr(ICD_DGNS_CD8, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD8 == 9 ~ substr(ICD_DGNS_CD8, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_9_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD9 == 0 ~ substr(ICD_DGNS_CD9, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD9 == 9 ~ substr(ICD_DGNS_CD9, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_10_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD10 == 0 ~ substr(ICD_DGNS_CD10, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD10 == 9 ~ substr(ICD_DGNS_CD10, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_11_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD11 == 0 ~ substr(ICD_DGNS_CD11, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD11 == 9 ~ substr(ICD_DGNS_CD11, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_12_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD12 == 0 ~ substr(ICD_DGNS_CD12, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD12 == 9 ~ substr(ICD_DGNS_CD12, 0, 3) %in% IHD_icd_9_codes)),
  
  PRNCPAL_DGNS_is_diabetes=(
    case_when(
      PRNCPAL_DGNS_VRSN_CD == 0 ~ substr(PRNCPAL_DGNS_CD, 0, 3) %in% diabetes_icd_10_codes,
      PRNCPAL_DGNS_VRSN_CD == 9 ~ substr(PRNCPAL_DGNS_CD, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_1_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD1 == 0 ~ substr(ICD_DGNS_CD1, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD1 == 9 ~ substr(ICD_DGNS_CD1, 0, 3) %in% diabetes_icd_9_codes)),

  DGNS_1_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD1 == 0 ~ substr(ICD_DGNS_CD1, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD1 == 9 ~ substr(ICD_DGNS_CD1, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_2_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD2 == 0 ~ substr(ICD_DGNS_CD2, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD2 == 9 ~ substr(ICD_DGNS_CD2, 0, 3) %in% diabetes_icd_9_codes)),

  DGNS_3_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD3 == 0 ~ substr(ICD_DGNS_CD3, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD3 == 9 ~ substr(ICD_DGNS_CD3, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_4_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD4 == 0 ~ substr(ICD_DGNS_CD4, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD4 == 9 ~ substr(ICD_DGNS_CD4, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_6_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD6 == 0 ~ substr(ICD_DGNS_CD6, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD6 == 9 ~ substr(ICD_DGNS_CD6, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_7_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD7 == 0 ~ substr(ICD_DGNS_CD7, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD7 == 9 ~ substr(ICD_DGNS_CD7, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_8_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD8 == 0 ~ substr(ICD_DGNS_CD8, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD8 == 9 ~ substr(ICD_DGNS_CD8, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_9_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD9 == 0 ~ substr(ICD_DGNS_CD9, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD9 == 9 ~ substr(ICD_DGNS_CD9, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_10_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD10 == 0 ~ substr(ICD_DGNS_CD10, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD10 == 9 ~ substr(ICD_DGNS_CD10, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_11_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD11 == 0 ~ substr(ICD_DGNS_CD11, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD11 == 9 ~ substr(ICD_DGNS_CD11, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_12_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD12 == 0 ~ substr(ICD_DGNS_CD12, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD12 == 9 ~ substr(ICD_DGNS_CD12, 0, 3) %in% diabetes_icd_9_codes)),
  
  PRNCPAL_DGNS_is_depression=(
    case_when(
      PRNCPAL_DGNS_VRSN_CD == 0 ~ substr(PRNCPAL_DGNS_CD, 0, 3) %in% depression_icd_10_codes,
      PRNCPAL_DGNS_VRSN_CD == 9 ~ substr(PRNCPAL_DGNS_CD, 0, 4) %in% depression_icd_9_codes)),
  
    DGNS_1_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD1 == 0 ~ substr(ICD_DGNS_CD1, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD1 == 9 ~ substr(ICD_DGNS_CD1, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_2_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD2 == 0 ~ substr(ICD_DGNS_CD2, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD2 == 9 ~ substr(ICD_DGNS_CD2, 0, 4) %in% depression_icd_9_codes)),

  DGNS_3_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD3 == 0 ~ substr(ICD_DGNS_CD3, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD3 == 9 ~ substr(ICD_DGNS_CD3, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_4_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD4 == 0 ~ substr(ICD_DGNS_CD4, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD4 == 9 ~ substr(ICD_DGNS_CD4, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_6_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD6 == 0 ~ substr(ICD_DGNS_CD6, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD6 == 9 ~ substr(ICD_DGNS_CD6, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_7_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD7 == 0 ~ substr(ICD_DGNS_CD7, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD7 == 9 ~ substr(ICD_DGNS_CD7, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_8_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD8 == 0 ~ substr(ICD_DGNS_CD8, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD8 == 9 ~ substr(ICD_DGNS_CD8, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_9_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD9 == 0 ~ substr(ICD_DGNS_CD9, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD9 == 9 ~ substr(ICD_DGNS_CD9, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_10_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD10 == 0 ~ substr(ICD_DGNS_CD10, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD10 == 9 ~ substr(ICD_DGNS_CD10, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_11_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD11 == 0 ~ substr(ICD_DGNS_CD11, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD11 == 9 ~ substr(ICD_DGNS_CD11, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_12_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD12 == 0 ~ substr(ICD_DGNS_CD12, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD12 == 9 ~ substr(ICD_DGNS_CD12, 0, 4) %in% depression_icd_9_codes))
)%>%
as.data.table()

conditions_claim_carrier[is.na(conditions_claim_carrier)] <- 0

conditions_claim_carrier=
conditions_claim_carrier%>%
summarise(
  
  DESY_SORT_KEY = DESY_SORT_KEY,
  
  year=year,
  
  date=date,
  
  month_year=month_year,
  

  is_hypertension=
  PRNCPAL_DGNS_is_hypertension | DGNS_1_is_hypertension | DGNS_2_is_hypertension | DGNS_3_is_hypertension | DGNS_4_is_hypertension | DGNS_6_is_hypertension | DGNS_7_is_hypertension | 
  DGNS_8_is_hypertension | DGNS_9_is_hypertension | DGNS_10_is_hypertension | DGNS_11_is_hypertension | DGNS_12_is_hypertension,
  
  is_arthritis=
  PRNCPAL_DGNS_is_arthritis | DGNS_1_is_arthritis | DGNS_2_is_arthritis | DGNS_3_is_arthritis | DGNS_4_is_arthritis | DGNS_6_is_arthritis | DGNS_7_is_arthritis | 
  DGNS_8_is_arthritis | DGNS_9_is_arthritis | DGNS_10_is_arthritis | DGNS_11_is_arthritis | DGNS_12_is_arthritis,

  
  is_IHD=
  PRNCPAL_DGNS_is_IHD | DGNS_1_is_IHD | DGNS_2_is_IHD | DGNS_3_is_IHD | DGNS_4_is_IHD | DGNS_6_is_IHD | DGNS_7_is_IHD | 
  DGNS_8_is_IHD | DGNS_9_is_IHD | DGNS_10_is_IHD | DGNS_11_is_IHD | DGNS_12_is_IHD,
  
  
  is_diabetes=
  PRNCPAL_DGNS_is_diabetes | DGNS_1_is_diabetes | DGNS_2_is_diabetes | DGNS_3_is_diabetes | DGNS_4_is_diabetes | DGNS_6_is_diabetes | DGNS_7_is_diabetes | 
  DGNS_8_is_diabetes | DGNS_9_is_diabetes | DGNS_10_is_diabetes | DGNS_11_is_diabetes | DGNS_12_is_diabetes,


  is_depression=
  PRNCPAL_DGNS_is_depression | DGNS_1_is_depression | DGNS_2_is_depression | DGNS_3_is_depression | DGNS_4_is_depression | DGNS_6_is_depression | DGNS_7_is_depression | 
  DGNS_8_is_depression | DGNS_9_is_depression | DGNS_10_is_depression | DGNS_11_is_depression | DGNS_12_is_depression
)%>%
as.data.table()



head(conditions_claim_carrier)





conditions_claim_inpatient=
claim_inpatient_all_years %>%
summarise(
  
  DESY_SORT_KEY = DESY_SORT_KEY,
  
  year=year,
  
  date=date,
  
  month_year=month_year,
  
  PRNCPAL_DGNS_is_hypertension=(
    case_when(
      PRNCPAL_DGNS_VRSN_CD == 0 ~ substr(PRNCPAL_DGNS_CD, 0, 3) %in% hypertension_icd_10_codes,
      PRNCPAL_DGNS_VRSN_CD == 9 ~ substr(PRNCPAL_DGNS_CD, 0, 3) %in% hypertension_icd_9_codes)),
  PRNCPAL_DGNS_is_arthritis=(
    case_when(
      PRNCPAL_DGNS_VRSN_CD == 0 ~ substr(PRNCPAL_DGNS_CD, 0, 3) %in% arthritis_icd_10_codes,
      PRNCPAL_DGNS_VRSN_CD == 9 ~ substr(PRNCPAL_DGNS_CD, 0, 3) %in% arthritis_icd_9_codes)),
  PRNCPAL_DGNS_is_IHD=(
    case_when(
      PRNCPAL_DGNS_VRSN_CD == 0 ~ substr(PRNCPAL_DGNS_CD, 0, 3) %in% IHD_icd_10_codes,
      PRNCPAL_DGNS_VRSN_CD == 9 ~ substr(PRNCPAL_DGNS_CD, 0, 3) %in% IHD_icd_9_codes)),
  PRNCPAL_DGNS_is_diabetes=(
    case_when(
      PRNCPAL_DGNS_VRSN_CD == 0 ~ substr(PRNCPAL_DGNS_CD, 0, 3) %in% diabetes_icd_10_codes,
      PRNCPAL_DGNS_VRSN_CD == 9 ~ substr(PRNCPAL_DGNS_CD, 0, 3) %in% diabetes_icd_9_codes)),
  PRNCPAL_DGNS_is_depression=(
    case_when(
      PRNCPAL_DGNS_VRSN_CD == 0 ~ substr(PRNCPAL_DGNS_CD, 0, 3) %in% depression_icd_10_codes,
      PRNCPAL_DGNS_VRSN_CD == 9 ~ substr(PRNCPAL_DGNS_CD, 0, 4) %in% depression_icd_9_codes)),
  
  ADMTG_DGNS_is_hypertension=(
    case_when(
      ADMTG_DGNS_VRSN_CD == 0 ~ substr(ADMTG_DGNS_CD, 0, 3) %in% hypertension_icd_10_codes,
      ADMTG_DGNS_VRSN_CD == 9 ~ substr(ADMTG_DGNS_CD, 0, 3) %in% hypertension_icd_9_codes)),
  ADMTG_DGNS_is_arthritis=(
    case_when(
      ADMTG_DGNS_VRSN_CD == 0 ~ substr(ADMTG_DGNS_CD, 0, 3) %in% arthritis_icd_10_codes,
      ADMTG_DGNS_VRSN_CD == 9 ~ substr(ADMTG_DGNS_CD, 0, 3) %in% arthritis_icd_9_codes)),
  ADMTG_DGNS_is_IHD=(
    case_when(
      ADMTG_DGNS_VRSN_CD == 0 ~ substr(ADMTG_DGNS_CD, 0, 3) %in% IHD_icd_10_codes,
      ADMTG_DGNS_VRSN_CD == 9 ~ substr(ADMTG_DGNS_CD, 0, 3) %in% IHD_icd_9_codes)),
  ADMTG_DGNS_is_diabetes=(
    case_when(
      ADMTG_DGNS_VRSN_CD == 0 ~ substr(ADMTG_DGNS_CD, 0, 3) %in% diabetes_icd_10_codes,
      ADMTG_DGNS_VRSN_CD == 9 ~ substr(ADMTG_DGNS_CD, 0, 3) %in% diabetes_icd_9_codes)),
  ADMTG_DGNS_is_depression=(
    case_when(
      ADMTG_DGNS_VRSN_CD == 0 ~ substr(ADMTG_DGNS_CD, 0, 3) %in% depression_icd_10_codes,
      ADMTG_DGNS_VRSN_CD == 9 ~ substr(ADMTG_DGNS_CD, 0, 4) %in% depression_icd_9_codes)),
  
  
  DGNS_1_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD1 == 0 ~ substr(ICD_DGNS_CD1, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD1 == 9 ~ substr(ICD_DGNS_CD1, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_2_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD2 == 0 ~ substr(ICD_DGNS_CD2, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD2 == 9 ~ substr(ICD_DGNS_CD2, 0, 3) %in% hypertension_icd_9_codes)),

  DGNS_3_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD3 == 0 ~ substr(ICD_DGNS_CD3, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD3 == 9 ~ substr(ICD_DGNS_CD3, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_4_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD4 == 0 ~ substr(ICD_DGNS_CD4, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD4 == 9 ~ substr(ICD_DGNS_CD4, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_6_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD6 == 0 ~ substr(ICD_DGNS_CD6, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD6 == 9 ~ substr(ICD_DGNS_CD6, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_7_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD7 == 0 ~ substr(ICD_DGNS_CD7, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD7 == 9 ~ substr(ICD_DGNS_CD7, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_8_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD8 == 0 ~ substr(ICD_DGNS_CD8, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD8 == 9 ~ substr(ICD_DGNS_CD8, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_9_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD9 == 0 ~ substr(ICD_DGNS_CD9, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD9 == 9 ~ substr(ICD_DGNS_CD9, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_10_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD10 == 0 ~ substr(ICD_DGNS_CD10, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD10 == 9 ~ substr(ICD_DGNS_CD10, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_11_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD11 == 0 ~ substr(ICD_DGNS_CD11, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD11 == 9 ~ substr(ICD_DGNS_CD11, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_12_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD12 == 0 ~ substr(ICD_DGNS_CD12, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD12 == 9 ~ substr(ICD_DGNS_CD12, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_13_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD13 == 0 ~ substr(ICD_DGNS_CD13, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD13 == 9 ~ substr(ICD_DGNS_CD13, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_14_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD14 == 0 ~ substr(ICD_DGNS_CD14, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD14 == 9 ~ substr(ICD_DGNS_CD14, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_15_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD15 == 0 ~ substr(ICD_DGNS_CD15, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD15 == 9 ~ substr(ICD_DGNS_CD15, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_16_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD16 == 0 ~ substr(ICD_DGNS_CD16, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD16 == 9 ~ substr(ICD_DGNS_CD16, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_17_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD17 == 0 ~ substr(ICD_DGNS_CD17, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD17 == 9 ~ substr(ICD_DGNS_CD17, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_18_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD18 == 0 ~ substr(ICD_DGNS_CD18, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD18 == 9 ~ substr(ICD_DGNS_CD18, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_19_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD19 == 0 ~ substr(ICD_DGNS_CD19, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD19 == 9 ~ substr(ICD_DGNS_CD19, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_20_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD20 == 0 ~ substr(ICD_DGNS_CD20, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD20 == 9 ~ substr(ICD_DGNS_CD20, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_21_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD21 == 0 ~ substr(ICD_DGNS_CD21, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD21 == 9 ~ substr(ICD_DGNS_CD21, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_22_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD22 == 0 ~ substr(ICD_DGNS_CD22, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD22 == 9 ~ substr(ICD_DGNS_CD22, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_23_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD23 == 0 ~ substr(ICD_DGNS_CD23, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD23 == 9 ~ substr(ICD_DGNS_CD23, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_24_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD24 == 0 ~ substr(ICD_DGNS_CD24, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD24 == 9 ~ substr(ICD_DGNS_CD24, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_25_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD25 == 0 ~ substr(ICD_DGNS_CD25, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD25 == 9 ~ substr(ICD_DGNS_CD25, 0, 3) %in% hypertension_icd_9_codes)),
  
  

  DGNS_1_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD1 == 0 ~ substr(ICD_DGNS_CD1, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD1 == 9 ~ substr(ICD_DGNS_CD1, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_2_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD2 == 0 ~ substr(ICD_DGNS_CD2, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD2 == 9 ~ substr(ICD_DGNS_CD2, 0, 3) %in% arthritis_icd_9_codes)),

  DGNS_3_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD3 == 0 ~ substr(ICD_DGNS_CD3, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD3 == 9 ~ substr(ICD_DGNS_CD3, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_4_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD4 == 0 ~ substr(ICD_DGNS_CD4, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD4 == 9 ~ substr(ICD_DGNS_CD4, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_6_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD6 == 0 ~ substr(ICD_DGNS_CD6, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD6 == 9 ~ substr(ICD_DGNS_CD6, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_7_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD7 == 0 ~ substr(ICD_DGNS_CD7, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD7 == 9 ~ substr(ICD_DGNS_CD7, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_8_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD8 == 0 ~ substr(ICD_DGNS_CD8, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD8 == 9 ~ substr(ICD_DGNS_CD8, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_9_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD9 == 0 ~ substr(ICD_DGNS_CD9, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD9 == 9 ~ substr(ICD_DGNS_CD9, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_10_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD10 == 0 ~ substr(ICD_DGNS_CD10, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD10 == 9 ~ substr(ICD_DGNS_CD10, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_11_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD11 == 0 ~ substr(ICD_DGNS_CD11, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD11 == 9 ~ substr(ICD_DGNS_CD11, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_12_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD12 == 0 ~ substr(ICD_DGNS_CD12, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD12 == 9 ~ substr(ICD_DGNS_CD12, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_13_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD13 == 0 ~ substr(ICD_DGNS_CD13, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD13 == 9 ~ substr(ICD_DGNS_CD13, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_14_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD14 == 0 ~ substr(ICD_DGNS_CD14, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD14 == 9 ~ substr(ICD_DGNS_CD14, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_15_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD15 == 0 ~ substr(ICD_DGNS_CD15, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD15 == 9 ~ substr(ICD_DGNS_CD15, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_16_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD16 == 0 ~ substr(ICD_DGNS_CD16, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD16 == 9 ~ substr(ICD_DGNS_CD16, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_17_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD17 == 0 ~ substr(ICD_DGNS_CD17, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD17 == 9 ~ substr(ICD_DGNS_CD17, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_18_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD18 == 0 ~ substr(ICD_DGNS_CD18, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD18 == 9 ~ substr(ICD_DGNS_CD18, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_19_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD19 == 0 ~ substr(ICD_DGNS_CD19, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD19 == 9 ~ substr(ICD_DGNS_CD19, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_20_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD20 == 0 ~ substr(ICD_DGNS_CD20, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD20 == 9 ~ substr(ICD_DGNS_CD20, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_21_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD21 == 0 ~ substr(ICD_DGNS_CD21, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD21 == 9 ~ substr(ICD_DGNS_CD21, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_22_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD22 == 0 ~ substr(ICD_DGNS_CD22, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD22 == 9 ~ substr(ICD_DGNS_CD22, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_23_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD23 == 0 ~ substr(ICD_DGNS_CD23, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD23 == 9 ~ substr(ICD_DGNS_CD23, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_24_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD24 == 0 ~ substr(ICD_DGNS_CD24, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD24 == 9 ~ substr(ICD_DGNS_CD24, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_25_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD25 == 0 ~ substr(ICD_DGNS_CD25, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD25 == 9 ~ substr(ICD_DGNS_CD25, 0, 3) %in% arthritis_icd_9_codes)),






  DGNS_1_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD1 == 0 ~ substr(ICD_DGNS_CD1, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD1 == 9 ~ substr(ICD_DGNS_CD1, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_2_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD2 == 0 ~ substr(ICD_DGNS_CD2, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD2 == 9 ~ substr(ICD_DGNS_CD2, 0, 3) %in% IHD_icd_9_codes)),

  DGNS_3_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD3 == 0 ~ substr(ICD_DGNS_CD3, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD3 == 9 ~ substr(ICD_DGNS_CD3, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_4_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD4 == 0 ~ substr(ICD_DGNS_CD4, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD4 == 9 ~ substr(ICD_DGNS_CD4, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_6_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD6 == 0 ~ substr(ICD_DGNS_CD6, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD6 == 9 ~ substr(ICD_DGNS_CD6, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_7_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD7 == 0 ~ substr(ICD_DGNS_CD7, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD7 == 9 ~ substr(ICD_DGNS_CD7, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_8_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD8 == 0 ~ substr(ICD_DGNS_CD8, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD8 == 9 ~ substr(ICD_DGNS_CD8, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_9_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD9 == 0 ~ substr(ICD_DGNS_CD9, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD9 == 9 ~ substr(ICD_DGNS_CD9, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_10_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD10 == 0 ~ substr(ICD_DGNS_CD10, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD10 == 9 ~ substr(ICD_DGNS_CD10, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_11_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD11 == 0 ~ substr(ICD_DGNS_CD11, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD11 == 9 ~ substr(ICD_DGNS_CD11, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_12_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD12 == 0 ~ substr(ICD_DGNS_CD12, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD12 == 9 ~ substr(ICD_DGNS_CD12, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_13_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD13 == 0 ~ substr(ICD_DGNS_CD13, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD13 == 9 ~ substr(ICD_DGNS_CD13, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_14_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD14 == 0 ~ substr(ICD_DGNS_CD14, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD14 == 9 ~ substr(ICD_DGNS_CD14, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_15_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD15 == 0 ~ substr(ICD_DGNS_CD15, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD15 == 9 ~ substr(ICD_DGNS_CD15, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_16_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD16 == 0 ~ substr(ICD_DGNS_CD16, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD16 == 9 ~ substr(ICD_DGNS_CD16, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_17_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD17 == 0 ~ substr(ICD_DGNS_CD17, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD17 == 9 ~ substr(ICD_DGNS_CD17, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_18_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD18 == 0 ~ substr(ICD_DGNS_CD18, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD18 == 9 ~ substr(ICD_DGNS_CD18, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_19_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD19 == 0 ~ substr(ICD_DGNS_CD19, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD19 == 9 ~ substr(ICD_DGNS_CD19, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_20_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD20 == 0 ~ substr(ICD_DGNS_CD20, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD20 == 9 ~ substr(ICD_DGNS_CD20, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_21_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD21 == 0 ~ substr(ICD_DGNS_CD21, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD21 == 9 ~ substr(ICD_DGNS_CD21, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_22_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD22 == 0 ~ substr(ICD_DGNS_CD22, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD22 == 9 ~ substr(ICD_DGNS_CD22, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_23_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD23 == 0 ~ substr(ICD_DGNS_CD23, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD23 == 9 ~ substr(ICD_DGNS_CD23, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_24_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD24 == 0 ~ substr(ICD_DGNS_CD24, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD24 == 9 ~ substr(ICD_DGNS_CD24, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_25_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD25 == 0 ~ substr(ICD_DGNS_CD25, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD25 == 9 ~ substr(ICD_DGNS_CD25, 0, 3) %in% IHD_icd_9_codes)),


  DGNS_1_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD1 == 0 ~ substr(ICD_DGNS_CD1, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD1 == 9 ~ substr(ICD_DGNS_CD1, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_2_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD2 == 0 ~ substr(ICD_DGNS_CD2, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD2 == 9 ~ substr(ICD_DGNS_CD2, 0, 3) %in% diabetes_icd_9_codes)),

  DGNS_3_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD3 == 0 ~ substr(ICD_DGNS_CD3, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD3 == 9 ~ substr(ICD_DGNS_CD3, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_4_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD4 == 0 ~ substr(ICD_DGNS_CD4, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD4 == 9 ~ substr(ICD_DGNS_CD4, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_6_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD6 == 0 ~ substr(ICD_DGNS_CD6, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD6 == 9 ~ substr(ICD_DGNS_CD6, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_7_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD7 == 0 ~ substr(ICD_DGNS_CD7, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD7 == 9 ~ substr(ICD_DGNS_CD7, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_8_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD8 == 0 ~ substr(ICD_DGNS_CD8, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD8 == 9 ~ substr(ICD_DGNS_CD8, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_9_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD9 == 0 ~ substr(ICD_DGNS_CD9, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD9 == 9 ~ substr(ICD_DGNS_CD9, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_10_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD10 == 0 ~ substr(ICD_DGNS_CD10, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD10 == 9 ~ substr(ICD_DGNS_CD10, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_11_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD11 == 0 ~ substr(ICD_DGNS_CD11, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD11 == 9 ~ substr(ICD_DGNS_CD11, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_12_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD12 == 0 ~ substr(ICD_DGNS_CD12, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD12 == 9 ~ substr(ICD_DGNS_CD12, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_13_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD13 == 0 ~ substr(ICD_DGNS_CD13, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD13 == 9 ~ substr(ICD_DGNS_CD13, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_14_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD14 == 0 ~ substr(ICD_DGNS_CD14, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD14 == 9 ~ substr(ICD_DGNS_CD14, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_15_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD15 == 0 ~ substr(ICD_DGNS_CD15, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD15 == 9 ~ substr(ICD_DGNS_CD15, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_16_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD16 == 0 ~ substr(ICD_DGNS_CD16, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD16 == 9 ~ substr(ICD_DGNS_CD16, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_17_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD17 == 0 ~ substr(ICD_DGNS_CD17, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD17 == 9 ~ substr(ICD_DGNS_CD17, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_18_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD18 == 0 ~ substr(ICD_DGNS_CD18, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD18 == 9 ~ substr(ICD_DGNS_CD18, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_19_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD19 == 0 ~ substr(ICD_DGNS_CD19, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD19 == 9 ~ substr(ICD_DGNS_CD19, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_20_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD20 == 0 ~ substr(ICD_DGNS_CD20, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD20 == 9 ~ substr(ICD_DGNS_CD20, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_21_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD21 == 0 ~ substr(ICD_DGNS_CD21, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD21 == 9 ~ substr(ICD_DGNS_CD21, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_22_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD22 == 0 ~ substr(ICD_DGNS_CD22, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD22 == 9 ~ substr(ICD_DGNS_CD22, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_23_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD23 == 0 ~ substr(ICD_DGNS_CD23, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD23 == 9 ~ substr(ICD_DGNS_CD23, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_24_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD24 == 0 ~ substr(ICD_DGNS_CD24, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD24 == 9 ~ substr(ICD_DGNS_CD24, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_25_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD25 == 0 ~ substr(ICD_DGNS_CD25, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD25 == 9 ~ substr(ICD_DGNS_CD25, 0, 3) %in% diabetes_icd_9_codes)),


  DGNS_1_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD1 == 0 ~ substr(ICD_DGNS_CD1, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD1 == 9 ~ substr(ICD_DGNS_CD1, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_2_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD2 == 0 ~ substr(ICD_DGNS_CD2, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD2 == 9 ~ substr(ICD_DGNS_CD2, 0, 4) %in% depression_icd_9_codes)),

  DGNS_3_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD3 == 0 ~ substr(ICD_DGNS_CD3, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD3 == 9 ~ substr(ICD_DGNS_CD3, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_4_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD4 == 0 ~ substr(ICD_DGNS_CD4, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD4 == 9 ~ substr(ICD_DGNS_CD4, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_6_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD6 == 0 ~ substr(ICD_DGNS_CD6, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD6 == 9 ~ substr(ICD_DGNS_CD6, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_7_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD7 == 0 ~ substr(ICD_DGNS_CD7, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD7 == 9 ~ substr(ICD_DGNS_CD7, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_8_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD8 == 0 ~ substr(ICD_DGNS_CD8, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD8 == 9 ~ substr(ICD_DGNS_CD8, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_9_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD9 == 0 ~ substr(ICD_DGNS_CD9, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD9 == 9 ~ substr(ICD_DGNS_CD9, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_10_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD10 == 0 ~ substr(ICD_DGNS_CD10, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD10 == 9 ~ substr(ICD_DGNS_CD10, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_11_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD11 == 0 ~ substr(ICD_DGNS_CD11, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD11 == 9 ~ substr(ICD_DGNS_CD11, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_12_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD12 == 0 ~ substr(ICD_DGNS_CD12, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD12 == 9 ~ substr(ICD_DGNS_CD12, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_13_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD13 == 0 ~ substr(ICD_DGNS_CD13, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD13 == 9 ~ substr(ICD_DGNS_CD13, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_14_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD14 == 0 ~ substr(ICD_DGNS_CD14, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD14 == 9 ~ substr(ICD_DGNS_CD14, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_15_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD15 == 0 ~ substr(ICD_DGNS_CD15, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD15 == 9 ~ substr(ICD_DGNS_CD15, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_16_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD16 == 0 ~ substr(ICD_DGNS_CD16, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD16 == 9 ~ substr(ICD_DGNS_CD16, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_17_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD17 == 0 ~ substr(ICD_DGNS_CD17, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD17 == 9 ~ substr(ICD_DGNS_CD17, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_18_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD18 == 0 ~ substr(ICD_DGNS_CD18, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD18 == 9 ~ substr(ICD_DGNS_CD18, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_19_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD19 == 0 ~ substr(ICD_DGNS_CD19, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD19 == 9 ~ substr(ICD_DGNS_CD19, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_20_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD20 == 0 ~ substr(ICD_DGNS_CD20, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD20 == 9 ~ substr(ICD_DGNS_CD20, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_21_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD21 == 0 ~ substr(ICD_DGNS_CD21, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD21 == 9 ~ substr(ICD_DGNS_CD21, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_22_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD22 == 0 ~ substr(ICD_DGNS_CD22, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD22 == 9 ~ substr(ICD_DGNS_CD22, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_23_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD23 == 0 ~ substr(ICD_DGNS_CD23, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD23 == 9 ~ substr(ICD_DGNS_CD23, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_24_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD24 == 0 ~ substr(ICD_DGNS_CD24, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD24 == 9 ~ substr(ICD_DGNS_CD24, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_25_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD25 == 0 ~ substr(ICD_DGNS_CD25, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD25 == 9 ~ substr(ICD_DGNS_CD25, 0, 4) %in% depression_icd_9_codes)),


  
)%>%
as.data.table()

conditions_claim_inpatient[is.na(conditions_claim_inpatient)] <- 0

conditions_claim_inpatient=
conditions_claim_inpatient%>%
summarise(
  
  DESY_SORT_KEY = DESY_SORT_KEY,
  
  year=year,
  
  date=date,
  
  month_year=month_year,
  

  is_hypertension=
  ADMTG_DGNS_is_hypertension | PRNCPAL_DGNS_is_hypertension | DGNS_1_is_hypertension | DGNS_2_is_hypertension | DGNS_3_is_hypertension | DGNS_4_is_hypertension | DGNS_6_is_hypertension | DGNS_7_is_hypertension | 
  DGNS_8_is_hypertension | DGNS_9_is_hypertension | DGNS_10_is_hypertension | DGNS_11_is_hypertension | DGNS_12_is_hypertension| DGNS_13_is_hypertension | DGNS_14_is_hypertension | DGNS_15_is_hypertension |
  DGNS_16_is_hypertension | DGNS_17_is_hypertension | DGNS_18_is_hypertension | DGNS_19_is_hypertension | DGNS_20_is_hypertension | DGNS_21_is_hypertension | DGNS_22_is_hypertension | DGNS_23_is_hypertension|
  DGNS_24_is_hypertension | DGNS_25_is_hypertension,
  
  is_arthritis=
  ADMTG_DGNS_is_arthritis | PRNCPAL_DGNS_is_arthritis | DGNS_1_is_arthritis | DGNS_2_is_arthritis | DGNS_3_is_arthritis | DGNS_4_is_arthritis | DGNS_6_is_arthritis | DGNS_7_is_arthritis | 
  DGNS_8_is_arthritis | DGNS_9_is_arthritis | DGNS_10_is_arthritis | DGNS_11_is_arthritis | DGNS_12_is_arthritis| DGNS_13_is_arthritis | DGNS_14_is_arthritis | DGNS_15_is_arthritis |
  DGNS_16_is_arthritis | DGNS_17_is_arthritis | DGNS_18_is_arthritis | DGNS_19_is_arthritis | DGNS_20_is_arthritis | DGNS_21_is_arthritis | DGNS_22_is_arthritis | DGNS_23_is_arthritis|
  DGNS_24_is_arthritis | DGNS_25_is_arthritis,
  

  
  is_IHD=
  ADMTG_DGNS_is_IHD | PRNCPAL_DGNS_is_IHD | DGNS_1_is_IHD | DGNS_2_is_IHD | DGNS_3_is_IHD | DGNS_4_is_IHD | DGNS_6_is_IHD | DGNS_7_is_IHD | 
  DGNS_8_is_IHD | DGNS_9_is_IHD | DGNS_10_is_IHD | DGNS_11_is_IHD | DGNS_12_is_IHD| DGNS_13_is_IHD | DGNS_14_is_IHD | DGNS_15_is_IHD |
  DGNS_16_is_IHD | DGNS_17_is_IHD | DGNS_18_is_IHD | DGNS_19_is_IHD | DGNS_20_is_IHD | DGNS_21_is_IHD | DGNS_22_is_IHD | DGNS_23_is_IHD|
  DGNS_24_is_IHD | DGNS_25_is_IHD,
  
  
  
  is_diabetes=
  ADMTG_DGNS_is_diabetes | PRNCPAL_DGNS_is_diabetes | DGNS_1_is_diabetes | DGNS_2_is_diabetes | DGNS_3_is_diabetes | DGNS_4_is_diabetes | DGNS_6_is_diabetes | DGNS_7_is_diabetes | 
  DGNS_8_is_diabetes | DGNS_9_is_diabetes | DGNS_10_is_diabetes | DGNS_11_is_diabetes | DGNS_12_is_diabetes| DGNS_13_is_diabetes | DGNS_14_is_diabetes | DGNS_15_is_diabetes |
  DGNS_16_is_diabetes | DGNS_17_is_diabetes | DGNS_18_is_diabetes | DGNS_19_is_diabetes | DGNS_20_is_diabetes | DGNS_21_is_diabetes | DGNS_22_is_diabetes | DGNS_23_is_diabetes|
  DGNS_24_is_diabetes | DGNS_25_is_diabetes,
  


  is_depression=
  ADMTG_DGNS_is_depression | PRNCPAL_DGNS_is_depression | DGNS_1_is_depression | DGNS_2_is_depression | DGNS_3_is_depression | DGNS_4_is_depression | DGNS_6_is_depression | DGNS_7_is_depression | 
  DGNS_8_is_depression | DGNS_9_is_depression | DGNS_10_is_depression | DGNS_11_is_depression | DGNS_12_is_depression| DGNS_13_is_depression | DGNS_14_is_depression | DGNS_15_is_depression |
  DGNS_16_is_depression | DGNS_17_is_depression | DGNS_18_is_depression | DGNS_19_is_depression | DGNS_20_is_depression | DGNS_21_is_depression | DGNS_22_is_depression | DGNS_23_is_depression|
  DGNS_24_is_depression | DGNS_25_is_depression,
  
)%>%
as.data.table()


tail(conditions_claim_inpatient)






conditions_claim_outpatient=
claim_outpatient_all_years %>%
summarise(
  
  DESY_SORT_KEY = DESY_SORT_KEY,
  
  year=year,
  
  date=date,
  
  month_year=month_year,
  
  PRNCPAL_DGNS_is_hypertension=(
    case_when(
      PRNCPAL_DGNS_VRSN_CD == 0 ~ substr(PRNCPAL_DGNS_CD, 0, 3) %in% hypertension_icd_10_codes,
      PRNCPAL_DGNS_VRSN_CD == 9 ~ substr(PRNCPAL_DGNS_CD, 0, 3) %in% hypertension_icd_9_codes)),
  PRNCPAL_DGNS_is_arthritis=(
    case_when(
      PRNCPAL_DGNS_VRSN_CD == 0 ~ substr(PRNCPAL_DGNS_CD, 0, 3) %in% arthritis_icd_10_codes,
      PRNCPAL_DGNS_VRSN_CD == 9 ~ substr(PRNCPAL_DGNS_CD, 0, 3) %in% arthritis_icd_9_codes)),
  PRNCPAL_DGNS_is_IHD=(
    case_when(
      PRNCPAL_DGNS_VRSN_CD == 0 ~ substr(PRNCPAL_DGNS_CD, 0, 3) %in% IHD_icd_10_codes,
      PRNCPAL_DGNS_VRSN_CD == 9 ~ substr(PRNCPAL_DGNS_CD, 0, 3) %in% IHD_icd_9_codes)),
  PRNCPAL_DGNS_is_diabetes=(
    case_when(
      PRNCPAL_DGNS_VRSN_CD == 0 ~ substr(PRNCPAL_DGNS_CD, 0, 3) %in% diabetes_icd_10_codes,
      PRNCPAL_DGNS_VRSN_CD == 9 ~ substr(PRNCPAL_DGNS_CD, 0, 3) %in% diabetes_icd_9_codes)),
  PRNCPAL_DGNS_is_depression=(
    case_when(
      PRNCPAL_DGNS_VRSN_CD == 0 ~ substr(PRNCPAL_DGNS_CD, 0, 3) %in% depression_icd_10_codes,
      PRNCPAL_DGNS_VRSN_CD == 9 ~ substr(PRNCPAL_DGNS_CD, 0, 4) %in% depression_icd_9_codes)),
  
  
  DGNS_1_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD1 == 0 ~ substr(ICD_DGNS_CD1, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD1 == 9 ~ substr(ICD_DGNS_CD1, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_2_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD2 == 0 ~ substr(ICD_DGNS_CD2, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD2 == 9 ~ substr(ICD_DGNS_CD2, 0, 3) %in% hypertension_icd_9_codes)),

  DGNS_3_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD3 == 0 ~ substr(ICD_DGNS_CD3, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD3 == 9 ~ substr(ICD_DGNS_CD3, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_4_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD4 == 0 ~ substr(ICD_DGNS_CD4, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD4 == 9 ~ substr(ICD_DGNS_CD4, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_6_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD6 == 0 ~ substr(ICD_DGNS_CD6, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD6 == 9 ~ substr(ICD_DGNS_CD6, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_7_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD7 == 0 ~ substr(ICD_DGNS_CD7, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD7 == 9 ~ substr(ICD_DGNS_CD7, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_8_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD8 == 0 ~ substr(ICD_DGNS_CD8, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD8 == 9 ~ substr(ICD_DGNS_CD8, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_9_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD9 == 0 ~ substr(ICD_DGNS_CD9, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD9 == 9 ~ substr(ICD_DGNS_CD9, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_10_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD10 == 0 ~ substr(ICD_DGNS_CD10, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD10 == 9 ~ substr(ICD_DGNS_CD10, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_11_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD11 == 0 ~ substr(ICD_DGNS_CD11, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD11 == 9 ~ substr(ICD_DGNS_CD11, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_12_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD12 == 0 ~ substr(ICD_DGNS_CD12, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD12 == 9 ~ substr(ICD_DGNS_CD12, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_13_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD13 == 0 ~ substr(ICD_DGNS_CD13, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD13 == 9 ~ substr(ICD_DGNS_CD13, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_14_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD14 == 0 ~ substr(ICD_DGNS_CD14, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD14 == 9 ~ substr(ICD_DGNS_CD14, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_15_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD15 == 0 ~ substr(ICD_DGNS_CD15, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD15 == 9 ~ substr(ICD_DGNS_CD15, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_16_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD16 == 0 ~ substr(ICD_DGNS_CD16, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD16 == 9 ~ substr(ICD_DGNS_CD16, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_17_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD17 == 0 ~ substr(ICD_DGNS_CD17, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD17 == 9 ~ substr(ICD_DGNS_CD17, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_18_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD18 == 0 ~ substr(ICD_DGNS_CD18, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD18 == 9 ~ substr(ICD_DGNS_CD18, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_19_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD19 == 0 ~ substr(ICD_DGNS_CD19, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD19 == 9 ~ substr(ICD_DGNS_CD19, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_20_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD20 == 0 ~ substr(ICD_DGNS_CD20, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD20 == 9 ~ substr(ICD_DGNS_CD20, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_21_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD21 == 0 ~ substr(ICD_DGNS_CD21, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD21 == 9 ~ substr(ICD_DGNS_CD21, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_22_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD22 == 0 ~ substr(ICD_DGNS_CD22, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD22 == 9 ~ substr(ICD_DGNS_CD22, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_23_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD23 == 0 ~ substr(ICD_DGNS_CD23, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD23 == 9 ~ substr(ICD_DGNS_CD23, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_24_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD24 == 0 ~ substr(ICD_DGNS_CD24, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD24 == 9 ~ substr(ICD_DGNS_CD24, 0, 3) %in% hypertension_icd_9_codes)),
  
  DGNS_25_is_hypertension=(
    case_when(
      ICD_DGNS_VRSN_CD25 == 0 ~ substr(ICD_DGNS_CD25, 0, 3) %in% hypertension_icd_10_codes,
      ICD_DGNS_VRSN_CD25 == 9 ~ substr(ICD_DGNS_CD25, 0, 3) %in% hypertension_icd_9_codes)),
  
  

  DGNS_1_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD1 == 0 ~ substr(ICD_DGNS_CD1, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD1 == 9 ~ substr(ICD_DGNS_CD1, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_2_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD2 == 0 ~ substr(ICD_DGNS_CD2, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD2 == 9 ~ substr(ICD_DGNS_CD2, 0, 3) %in% arthritis_icd_9_codes)),

  DGNS_3_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD3 == 0 ~ substr(ICD_DGNS_CD3, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD3 == 9 ~ substr(ICD_DGNS_CD3, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_4_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD4 == 0 ~ substr(ICD_DGNS_CD4, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD4 == 9 ~ substr(ICD_DGNS_CD4, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_6_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD6 == 0 ~ substr(ICD_DGNS_CD6, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD6 == 9 ~ substr(ICD_DGNS_CD6, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_7_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD7 == 0 ~ substr(ICD_DGNS_CD7, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD7 == 9 ~ substr(ICD_DGNS_CD7, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_8_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD8 == 0 ~ substr(ICD_DGNS_CD8, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD8 == 9 ~ substr(ICD_DGNS_CD8, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_9_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD9 == 0 ~ substr(ICD_DGNS_CD9, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD9 == 9 ~ substr(ICD_DGNS_CD9, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_10_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD10 == 0 ~ substr(ICD_DGNS_CD10, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD10 == 9 ~ substr(ICD_DGNS_CD10, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_11_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD11 == 0 ~ substr(ICD_DGNS_CD11, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD11 == 9 ~ substr(ICD_DGNS_CD11, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_12_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD12 == 0 ~ substr(ICD_DGNS_CD12, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD12 == 9 ~ substr(ICD_DGNS_CD12, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_13_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD13 == 0 ~ substr(ICD_DGNS_CD13, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD13 == 9 ~ substr(ICD_DGNS_CD13, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_14_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD14 == 0 ~ substr(ICD_DGNS_CD14, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD14 == 9 ~ substr(ICD_DGNS_CD14, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_15_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD15 == 0 ~ substr(ICD_DGNS_CD15, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD15 == 9 ~ substr(ICD_DGNS_CD15, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_16_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD16 == 0 ~ substr(ICD_DGNS_CD16, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD16 == 9 ~ substr(ICD_DGNS_CD16, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_17_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD17 == 0 ~ substr(ICD_DGNS_CD17, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD17 == 9 ~ substr(ICD_DGNS_CD17, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_18_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD18 == 0 ~ substr(ICD_DGNS_CD18, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD18 == 9 ~ substr(ICD_DGNS_CD18, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_19_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD19 == 0 ~ substr(ICD_DGNS_CD19, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD19 == 9 ~ substr(ICD_DGNS_CD19, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_20_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD20 == 0 ~ substr(ICD_DGNS_CD20, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD20 == 9 ~ substr(ICD_DGNS_CD20, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_21_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD21 == 0 ~ substr(ICD_DGNS_CD21, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD21 == 9 ~ substr(ICD_DGNS_CD21, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_22_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD22 == 0 ~ substr(ICD_DGNS_CD22, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD22 == 9 ~ substr(ICD_DGNS_CD22, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_23_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD23 == 0 ~ substr(ICD_DGNS_CD23, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD23 == 9 ~ substr(ICD_DGNS_CD23, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_24_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD24 == 0 ~ substr(ICD_DGNS_CD24, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD24 == 9 ~ substr(ICD_DGNS_CD24, 0, 3) %in% arthritis_icd_9_codes)),
  
  DGNS_25_is_arthritis=(
    case_when(
      ICD_DGNS_VRSN_CD25 == 0 ~ substr(ICD_DGNS_CD25, 0, 3) %in% arthritis_icd_10_codes,
      ICD_DGNS_VRSN_CD25 == 9 ~ substr(ICD_DGNS_CD25, 0, 3) %in% arthritis_icd_9_codes)),






  DGNS_1_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD1 == 0 ~ substr(ICD_DGNS_CD1, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD1 == 9 ~ substr(ICD_DGNS_CD1, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_2_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD2 == 0 ~ substr(ICD_DGNS_CD2, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD2 == 9 ~ substr(ICD_DGNS_CD2, 0, 3) %in% IHD_icd_9_codes)),

  DGNS_3_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD3 == 0 ~ substr(ICD_DGNS_CD3, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD3 == 9 ~ substr(ICD_DGNS_CD3, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_4_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD4 == 0 ~ substr(ICD_DGNS_CD4, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD4 == 9 ~ substr(ICD_DGNS_CD4, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_6_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD6 == 0 ~ substr(ICD_DGNS_CD6, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD6 == 9 ~ substr(ICD_DGNS_CD6, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_7_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD7 == 0 ~ substr(ICD_DGNS_CD7, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD7 == 9 ~ substr(ICD_DGNS_CD7, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_8_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD8 == 0 ~ substr(ICD_DGNS_CD8, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD8 == 9 ~ substr(ICD_DGNS_CD8, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_9_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD9 == 0 ~ substr(ICD_DGNS_CD9, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD9 == 9 ~ substr(ICD_DGNS_CD9, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_10_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD10 == 0 ~ substr(ICD_DGNS_CD10, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD10 == 9 ~ substr(ICD_DGNS_CD10, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_11_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD11 == 0 ~ substr(ICD_DGNS_CD11, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD11 == 9 ~ substr(ICD_DGNS_CD11, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_12_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD12 == 0 ~ substr(ICD_DGNS_CD12, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD12 == 9 ~ substr(ICD_DGNS_CD12, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_13_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD13 == 0 ~ substr(ICD_DGNS_CD13, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD13 == 9 ~ substr(ICD_DGNS_CD13, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_14_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD14 == 0 ~ substr(ICD_DGNS_CD14, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD14 == 9 ~ substr(ICD_DGNS_CD14, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_15_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD15 == 0 ~ substr(ICD_DGNS_CD15, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD15 == 9 ~ substr(ICD_DGNS_CD15, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_16_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD16 == 0 ~ substr(ICD_DGNS_CD16, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD16 == 9 ~ substr(ICD_DGNS_CD16, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_17_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD17 == 0 ~ substr(ICD_DGNS_CD17, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD17 == 9 ~ substr(ICD_DGNS_CD17, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_18_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD18 == 0 ~ substr(ICD_DGNS_CD18, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD18 == 9 ~ substr(ICD_DGNS_CD18, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_19_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD19 == 0 ~ substr(ICD_DGNS_CD19, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD19 == 9 ~ substr(ICD_DGNS_CD19, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_20_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD20 == 0 ~ substr(ICD_DGNS_CD20, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD20 == 9 ~ substr(ICD_DGNS_CD20, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_21_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD21 == 0 ~ substr(ICD_DGNS_CD21, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD21 == 9 ~ substr(ICD_DGNS_CD21, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_22_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD22 == 0 ~ substr(ICD_DGNS_CD22, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD22 == 9 ~ substr(ICD_DGNS_CD22, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_23_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD23 == 0 ~ substr(ICD_DGNS_CD23, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD23 == 9 ~ substr(ICD_DGNS_CD23, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_24_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD24 == 0 ~ substr(ICD_DGNS_CD24, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD24 == 9 ~ substr(ICD_DGNS_CD24, 0, 3) %in% IHD_icd_9_codes)),
  
  DGNS_25_is_IHD=(
    case_when(
      ICD_DGNS_VRSN_CD25 == 0 ~ substr(ICD_DGNS_CD25, 0, 3) %in% IHD_icd_10_codes,
      ICD_DGNS_VRSN_CD25 == 9 ~ substr(ICD_DGNS_CD25, 0, 3) %in% IHD_icd_9_codes)),


  DGNS_1_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD1 == 0 ~ substr(ICD_DGNS_CD1, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD1 == 9 ~ substr(ICD_DGNS_CD1, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_2_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD2 == 0 ~ substr(ICD_DGNS_CD2, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD2 == 9 ~ substr(ICD_DGNS_CD2, 0, 3) %in% diabetes_icd_9_codes)),

  DGNS_3_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD3 == 0 ~ substr(ICD_DGNS_CD3, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD3 == 9 ~ substr(ICD_DGNS_CD3, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_4_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD4 == 0 ~ substr(ICD_DGNS_CD4, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD4 == 9 ~ substr(ICD_DGNS_CD4, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_6_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD6 == 0 ~ substr(ICD_DGNS_CD6, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD6 == 9 ~ substr(ICD_DGNS_CD6, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_7_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD7 == 0 ~ substr(ICD_DGNS_CD7, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD7 == 9 ~ substr(ICD_DGNS_CD7, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_8_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD8 == 0 ~ substr(ICD_DGNS_CD8, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD8 == 9 ~ substr(ICD_DGNS_CD8, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_9_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD9 == 0 ~ substr(ICD_DGNS_CD9, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD9 == 9 ~ substr(ICD_DGNS_CD9, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_10_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD10 == 0 ~ substr(ICD_DGNS_CD10, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD10 == 9 ~ substr(ICD_DGNS_CD10, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_11_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD11 == 0 ~ substr(ICD_DGNS_CD11, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD11 == 9 ~ substr(ICD_DGNS_CD11, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_12_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD12 == 0 ~ substr(ICD_DGNS_CD12, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD12 == 9 ~ substr(ICD_DGNS_CD12, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_13_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD13 == 0 ~ substr(ICD_DGNS_CD13, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD13 == 9 ~ substr(ICD_DGNS_CD13, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_14_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD14 == 0 ~ substr(ICD_DGNS_CD14, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD14 == 9 ~ substr(ICD_DGNS_CD14, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_15_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD15 == 0 ~ substr(ICD_DGNS_CD15, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD15 == 9 ~ substr(ICD_DGNS_CD15, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_16_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD16 == 0 ~ substr(ICD_DGNS_CD16, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD16 == 9 ~ substr(ICD_DGNS_CD16, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_17_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD17 == 0 ~ substr(ICD_DGNS_CD17, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD17 == 9 ~ substr(ICD_DGNS_CD17, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_18_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD18 == 0 ~ substr(ICD_DGNS_CD18, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD18 == 9 ~ substr(ICD_DGNS_CD18, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_19_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD19 == 0 ~ substr(ICD_DGNS_CD19, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD19 == 9 ~ substr(ICD_DGNS_CD19, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_20_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD20 == 0 ~ substr(ICD_DGNS_CD20, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD20 == 9 ~ substr(ICD_DGNS_CD20, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_21_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD21 == 0 ~ substr(ICD_DGNS_CD21, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD21 == 9 ~ substr(ICD_DGNS_CD21, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_22_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD22 == 0 ~ substr(ICD_DGNS_CD22, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD22 == 9 ~ substr(ICD_DGNS_CD22, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_23_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD23 == 0 ~ substr(ICD_DGNS_CD23, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD23 == 9 ~ substr(ICD_DGNS_CD23, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_24_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD24 == 0 ~ substr(ICD_DGNS_CD24, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD24 == 9 ~ substr(ICD_DGNS_CD24, 0, 3) %in% diabetes_icd_9_codes)),
  
  DGNS_25_is_diabetes=(
    case_when(
      ICD_DGNS_VRSN_CD25 == 0 ~ substr(ICD_DGNS_CD25, 0, 3) %in% diabetes_icd_10_codes,
      ICD_DGNS_VRSN_CD25 == 9 ~ substr(ICD_DGNS_CD25, 0, 3) %in% diabetes_icd_9_codes)),


  DGNS_1_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD1 == 0 ~ substr(ICD_DGNS_CD1, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD1 == 9 ~ substr(ICD_DGNS_CD1, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_2_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD2 == 0 ~ substr(ICD_DGNS_CD2, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD2 == 9 ~ substr(ICD_DGNS_CD2, 0, 4) %in% depression_icd_9_codes)),

  DGNS_3_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD3 == 0 ~ substr(ICD_DGNS_CD3, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD3 == 9 ~ substr(ICD_DGNS_CD3, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_4_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD4 == 0 ~ substr(ICD_DGNS_CD4, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD4 == 9 ~ substr(ICD_DGNS_CD4, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_6_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD6 == 0 ~ substr(ICD_DGNS_CD6, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD6 == 9 ~ substr(ICD_DGNS_CD6, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_7_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD7 == 0 ~ substr(ICD_DGNS_CD7, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD7 == 9 ~ substr(ICD_DGNS_CD7, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_8_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD8 == 0 ~ substr(ICD_DGNS_CD8, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD8 == 9 ~ substr(ICD_DGNS_CD8, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_9_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD9 == 0 ~ substr(ICD_DGNS_CD9, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD9 == 9 ~ substr(ICD_DGNS_CD9, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_10_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD10 == 0 ~ substr(ICD_DGNS_CD10, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD10 == 9 ~ substr(ICD_DGNS_CD10, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_11_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD11 == 0 ~ substr(ICD_DGNS_CD11, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD11 == 9 ~ substr(ICD_DGNS_CD11, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_12_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD12 == 0 ~ substr(ICD_DGNS_CD12, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD12 == 9 ~ substr(ICD_DGNS_CD12, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_13_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD13 == 0 ~ substr(ICD_DGNS_CD13, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD13 == 9 ~ substr(ICD_DGNS_CD13, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_14_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD14 == 0 ~ substr(ICD_DGNS_CD14, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD14 == 9 ~ substr(ICD_DGNS_CD14, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_15_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD15 == 0 ~ substr(ICD_DGNS_CD15, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD15 == 9 ~ substr(ICD_DGNS_CD15, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_16_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD16 == 0 ~ substr(ICD_DGNS_CD16, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD16 == 9 ~ substr(ICD_DGNS_CD16, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_17_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD17 == 0 ~ substr(ICD_DGNS_CD17, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD17 == 9 ~ substr(ICD_DGNS_CD17, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_18_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD18 == 0 ~ substr(ICD_DGNS_CD18, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD18 == 9 ~ substr(ICD_DGNS_CD18, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_19_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD19 == 0 ~ substr(ICD_DGNS_CD19, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD19 == 9 ~ substr(ICD_DGNS_CD19, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_20_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD20 == 0 ~ substr(ICD_DGNS_CD20, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD20 == 9 ~ substr(ICD_DGNS_CD20, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_21_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD21 == 0 ~ substr(ICD_DGNS_CD21, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD21 == 9 ~ substr(ICD_DGNS_CD21, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_22_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD22 == 0 ~ substr(ICD_DGNS_CD22, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD22 == 9 ~ substr(ICD_DGNS_CD22, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_23_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD23 == 0 ~ substr(ICD_DGNS_CD23, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD23 == 9 ~ substr(ICD_DGNS_CD23, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_24_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD24 == 0 ~ substr(ICD_DGNS_CD24, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD24 == 9 ~ substr(ICD_DGNS_CD24, 0, 4) %in% depression_icd_9_codes)),
  
  DGNS_25_is_depression=(
    case_when(
      ICD_DGNS_VRSN_CD25 == 0 ~ substr(ICD_DGNS_CD25, 0, 3) %in% depression_icd_10_codes,
      ICD_DGNS_VRSN_CD25 == 9 ~ substr(ICD_DGNS_CD25, 0, 4) %in% depression_icd_9_codes)),


  
)%>%
as.data.table()

conditions_claim_outpatient[is.na(conditions_claim_outpatient)] <- 0

conditions_claim_outpatient=
conditions_claim_outpatient%>%
summarise(
  
  DESY_SORT_KEY = DESY_SORT_KEY,
  
  year=year,
  
  date=date,
  
  month_year=month_year,
  

  is_hypertension=
 PRNCPAL_DGNS_is_hypertension | DGNS_1_is_hypertension | DGNS_2_is_hypertension | DGNS_3_is_hypertension | DGNS_4_is_hypertension | DGNS_6_is_hypertension | DGNS_7_is_hypertension | 
  DGNS_8_is_hypertension | DGNS_9_is_hypertension | DGNS_10_is_hypertension | DGNS_11_is_hypertension | DGNS_12_is_hypertension| DGNS_13_is_hypertension | DGNS_14_is_hypertension | DGNS_15_is_hypertension |
  DGNS_16_is_hypertension | DGNS_17_is_hypertension | DGNS_18_is_hypertension | DGNS_19_is_hypertension | DGNS_20_is_hypertension | DGNS_21_is_hypertension | DGNS_22_is_hypertension | DGNS_23_is_hypertension|
  DGNS_24_is_hypertension | DGNS_25_is_hypertension,
  
  is_arthritis=
 PRNCPAL_DGNS_is_arthritis | DGNS_1_is_arthritis | DGNS_2_is_arthritis | DGNS_3_is_arthritis | DGNS_4_is_arthritis | DGNS_6_is_arthritis | DGNS_7_is_arthritis | 
  DGNS_8_is_arthritis | DGNS_9_is_arthritis | DGNS_10_is_arthritis | DGNS_11_is_arthritis | DGNS_12_is_arthritis| DGNS_13_is_arthritis | DGNS_14_is_arthritis | DGNS_15_is_arthritis |
  DGNS_16_is_arthritis | DGNS_17_is_arthritis | DGNS_18_is_arthritis | DGNS_19_is_arthritis | DGNS_20_is_arthritis | DGNS_21_is_arthritis | DGNS_22_is_arthritis | DGNS_23_is_arthritis|
  DGNS_24_is_arthritis | DGNS_25_is_arthritis,
  

  
  is_IHD=
 PRNCPAL_DGNS_is_IHD | DGNS_1_is_IHD | DGNS_2_is_IHD | DGNS_3_is_IHD | DGNS_4_is_IHD | DGNS_6_is_IHD | DGNS_7_is_IHD | 
  DGNS_8_is_IHD | DGNS_9_is_IHD | DGNS_10_is_IHD | DGNS_11_is_IHD | DGNS_12_is_IHD| DGNS_13_is_IHD | DGNS_14_is_IHD | DGNS_15_is_IHD |
  DGNS_16_is_IHD | DGNS_17_is_IHD | DGNS_18_is_IHD | DGNS_19_is_IHD | DGNS_20_is_IHD | DGNS_21_is_IHD | DGNS_22_is_IHD | DGNS_23_is_IHD|
  DGNS_24_is_IHD | DGNS_25_is_IHD,
  
  
  
  is_diabetes=
 PRNCPAL_DGNS_is_diabetes | DGNS_1_is_diabetes | DGNS_2_is_diabetes | DGNS_3_is_diabetes | DGNS_4_is_diabetes | DGNS_6_is_diabetes | DGNS_7_is_diabetes | 
  DGNS_8_is_diabetes | DGNS_9_is_diabetes | DGNS_10_is_diabetes | DGNS_11_is_diabetes | DGNS_12_is_diabetes| DGNS_13_is_diabetes | DGNS_14_is_diabetes | DGNS_15_is_diabetes |
  DGNS_16_is_diabetes | DGNS_17_is_diabetes | DGNS_18_is_diabetes | DGNS_19_is_diabetes | DGNS_20_is_diabetes | DGNS_21_is_diabetes | DGNS_22_is_diabetes | DGNS_23_is_diabetes|
  DGNS_24_is_diabetes | DGNS_25_is_diabetes,
  


  is_depression=
 PRNCPAL_DGNS_is_depression | DGNS_1_is_depression | DGNS_2_is_depression | DGNS_3_is_depression | DGNS_4_is_depression | DGNS_6_is_depression | DGNS_7_is_depression | 
  DGNS_8_is_depression | DGNS_9_is_depression | DGNS_10_is_depression | DGNS_11_is_depression | DGNS_12_is_depression| DGNS_13_is_depression | DGNS_14_is_depression | DGNS_15_is_depression |
  DGNS_16_is_depression | DGNS_17_is_depression | DGNS_18_is_depression | DGNS_19_is_depression | DGNS_20_is_depression | DGNS_21_is_depression | DGNS_22_is_depression | DGNS_23_is_depression|
  DGNS_24_is_depression | DGNS_25_is_depression,
  
)%>%
as.data.table()


head(conditions_claim_outpatient)



claim_level_all_data=rbind(conditions_claim_carrier,conditions_claim_inpatient)
claim_level_all_data=rbind(claim_level_all_data,conditions_claim_outpatient)





summarise_claim_level <- function(data, time_frame = 365) {
  data %>%
    group_by(DESY_SORT_KEY, year) %>%
    summarise(
      hypertension = sum(is_hypertension, na.rm = T) > 0,
      arthritis = sum(is_arthritis, na.rm = T) > 0,
      IHD = sum(is_IHD, na.rm = T) > 0,
      diabetes = sum(is_diabetes, na.rm = T) > 0,
      depression = sum(is_depression, na.rm = T) > 0,
    ) %>%
    as.data.table()
}

summary_claim_level_patient_by_year <- summarise_claim_level(claim_level_all_data)


tail(summary_claim_level_patient_by_year)


write_fst(summary_claim_level_patient_by_year, "/work/postresearch/Shared/Projects/Farbod/CaseMix/summary_claim_level_patient_by_year.fst")
