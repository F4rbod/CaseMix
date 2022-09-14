numcores <- 10

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


claim_carrier_2013=vroom(file="/work/postresearch/Shared/Data_raw/Medicare/Claims/CSV/car_claimsj_lds_5_2013.csv" , num_threads = numcores) %>% as.data.table
claim_carrier_2014=vroom(file="/work/postresearch/Shared/Data_raw/Medicare/Claims/CSV/car_claimsj_lds_5_2014.csv" , num_threads = numcores) %>% as.data.table
claim_carrier_2015=vroom(file="/work/postresearch/Shared/Data_raw/Medicare/Claims/CSV/car_claimsj_lds_5_2015.csv" , num_threads = numcores) %>% as.data.table
claim_carrier_2016=vroom(file="/work/postresearch/Shared/Data_raw/Medicare/Claims/CSV/car_claimsk_lds_5_2016.csv" , num_threads = numcores) %>% as.data.table
claim_carrier_2017=vroom(file="/work/postresearch/Shared/Data_raw/Medicare/Claims/CSV/car_claimsk_lds_5_2017.csv" , num_threads = numcores) %>% as.data.table
claim_carrier_2018=vroom(file="/work/postresearch/Shared/Data_raw/Medicare/Claims/CSV/car_claimsk_lds_5_2018.csv" , num_threads = numcores) %>% as.data.table
claim_carrier_2019=vroom(file="/work/postresearch/Shared/Data_raw/Medicare/Claims/CSV/car_claimsk_lds_5_2019.csv" , num_threads = numcores) %>% as.data.table
claim_carrier_2020=vroom(file="/work/postresearch/Shared/Data_raw/Medicare/Claims/CSV/car_claimsk_lds_5_2020.csv" , num_threads = numcores) %>% as.data.table

claim_inpatient_2013=vroom(file="/work/postresearch/Shared/Data_raw/Medicare/Claims/CSV/inp_claimsj_lds_5_2013.csv" , num_threads = numcores) %>% as.data.table
claim_inpatient_2014=vroom(file="/work/postresearch/Shared/Data_raw/Medicare/Claims/CSV/inp_claimsj_lds_5_2014.csv" , num_threads = numcores) %>% as.data.table
claim_inpatient_2015=vroom(file="/work/postresearch/Shared/Data_raw/Medicare/Claims/CSV/inp_claimsj_lds_5_2015.csv" , num_threads = numcores) %>% as.data.table
claim_inpatient_2016=vroom(file="/work/postresearch/Shared/Data_raw/Medicare/Claims/CSV/inp_claimsk_lds_5_2016.csv" , num_threads = numcores) %>% as.data.table
claim_inpatient_2017=vroom(file="/work/postresearch/Shared/Data_raw/Medicare/Claims/CSV/inp_claimsk_lds_5_2017.csv" , num_threads = numcores) %>% as.data.table
claim_inpatient_2018=vroom(file="/work/postresearch/Shared/Data_raw/Medicare/Claims/CSV/inp_claimsk_lds_5_2018.csv" , num_threads = numcores) %>% as.data.table
claim_inpatient_2019=vroom(file="/work/postresearch/Shared/Data_raw/Medicare/Claims/CSV/inp_claimsk_lds_5_2019.csv" , num_threads = numcores) %>% as.data.table
claim_inpatient_2020=vroom(file="/work/postresearch/Shared/Data_raw/Medicare/Claims/CSV/inp_claimsk_lds_5_2020.csv" , num_threads = numcores) %>% as.data.table

claim_outpatient_2013=vroom(file="/work/postresearch/Shared/Data_raw/Medicare/Claims/CSV/out_claimsj_lds_5_2013.csv" , num_threads = numcores) %>% as.data.table
claim_outpatient_2014=vroom(file="/work/postresearch/Shared/Data_raw/Medicare/Claims/CSV/out_claimsj_lds_5_2014.csv" , num_threads = numcores) %>% as.data.table
claim_outpatient_2015=vroom(file="/work/postresearch/Shared/Data_raw/Medicare/Claims/CSV/out_claimsj_lds_5_2015.csv" , num_threads = numcores) %>% as.data.table
claim_outpatient_2016=vroom(file="/work/postresearch/Shared/Data_raw/Medicare/Claims/CSV/out_claimsk_lds_5_2016.csv" , num_threads = numcores) %>% as.data.table
claim_outpatient_2017=vroom(file="/work/postresearch/Shared/Data_raw/Medicare/Claims/CSV/out_claimsk_lds_5_2017.csv" , num_threads = numcores) %>% as.data.table
claim_outpatient_2018=vroom(file="/work/postresearch/Shared/Data_raw/Medicare/Claims/CSV/out_claimsk_lds_5_2018.csv" , num_threads = numcores) %>% as.data.table
claim_outpatient_2019=vroom(file="/work/postresearch/Shared/Data_raw/Medicare/Claims/CSV/out_claimsk_lds_5_2019.csv" , num_threads = numcores) %>% as.data.table
claim_outpatient_2020=vroom(file="/work/postresearch/Shared/Data_raw/Medicare/Claims/CSV/out_claimsk_lds_5_2020.csv" , num_threads = numcores) %>% as.data.table




colnames_claim_carrier_2011_2015=
c("DESY_SORT_KEY","CLAIM_NO","CLM_THRU_DT","NCH_NEAR_LINE_REC_IDENT_CD","NCH_CLM_TYPE_CD","CLM_DISP_CD","CARR_NUM","CARR_CLM_PMT_DNL_CD","CLM_PMT_AMT","CARR_CLM_PRMRY_PYR_PD_AMT","RFR_PHYSN_UPIN","RFR_PHYSN_NPI","CARR_CLM_PRVDR_ASGNMT_IND_SW","PROV_PMT","BENE_PMT","SBMTCHRG","ALOWCHRG","DEDAPPLY","HCPCS_YR","CARR_CLM_RFRNG_PIN_NUM","PRNCPAL_DGNS_CD","PRNCPAL_DGNS_VRSN_CD","ICD_DGNS_CD1","ICD_DGNS_VRSN_CD1","ICD_DGNS_CD2","ICD_DGNS_VRSN_CD2","ICD_DGNS_CD3","ICD_DGNS_VRSN_CD3","ICD_DGNS_CD4","ICD_DGNS_VRSN_CD4","ICD_DGNS_CD5","ICD_DGNS_VRSN_CD5","ICD_DGNS_CD6","ICD_DGNS_VRSN_CD6","ICD_DGNS_CD7","ICD_DGNS_VRSN_CD7","ICD_DGNS_CD8","ICD_DGNS_VRSN_CD8","ICD_DGNS_CD9","ICD_DGNS_VRSN_CD9","ICD_DGNS_CD10","ICD_DGNS_VRSN_CD10","ICD_DGNS_CD11","ICD_DGNS_VRSN_CD11","ICD_DGNS_CD12","ICD_DGNS_VRSN_CD12","DOB_DT","GNDR_CD","BENE_RACE_CD","BENE_CNTY_CD","BENE_STATE_CD","CWF_BENE_MDCR_STUS_CD")

colnames_claim_carrier_2016_2020=
c("DESY_SORT_KEY","CLAIM_NO","CLM_THRU_DT","NCH_NEAR_LINE_REC_IDENT_CD","NCH_CLM_TYPE_CD","CLM_DISP_CD","CARR_NUM","CARR_CLM_PMT_DNL_CD","CLM_PMT_AMT","CARR_CLM_PRMRY_PYR_PD_AMT","RFR_PHYSN_UPIN","RFR_PHYSN_NPI","CARR_CLM_PRVDR_ASGNMT_IND_SW","NCH_CLM_PRVDR_PMT_AMT","NCH_CLM_BENE_PMT_AMT","NCH_CARR_CLM_SBMTD_CHRG_AMT","NCH_CARR_CLM_ALOWD_AMT","CARR_CLM_CASH_DDCTBL_APLD_AMT","CARR_CLM_HCPCS_YR_CD","CARR_CLM_RFRNG_PIN_NUM","PRNCPAL_DGNS_CD","PRNCPAL_DGNS_VRSN_CD","ICD_DGNS_CD1","ICD_DGNS_VRSN_CD1","ICD_DGNS_CD2","ICD_DGNS_VRSN_CD2","ICD_DGNS_CD3","ICD_DGNS_VRSN_CD3","ICD_DGNS_CD4","ICD_DGNS_VRSN_CD4","ICD_DGNS_CD5","ICD_DGNS_VRSN_CD5","ICD_DGNS_CD6","ICD_DGNS_VRSN_CD6","ICD_DGNS_CD7","ICD_DGNS_VRSN_CD7","ICD_DGNS_CD8","ICD_DGNS_VRSN_CD8","ICD_DGNS_CD9","ICD_DGNS_VRSN_CD9","ICD_DGNS_CD10","ICD_DGNS_VRSN_CD10","ICD_DGNS_CD11","ICD_DGNS_VRSN_CD11","ICD_DGNS_CD12","ICD_DGNS_VRSN_CD12","DOB_DT","GNDR_CD","BENE_RACE_CD","BENE_CNTY_CD","BENE_STATE_CD","CWF_BENE_MDCR_STUS_CD","CLM_BENE_PD_AMT","CPO_PRVDR_NUM","CPO_ORG_NPI_NUM","CARR_CLM_BLG_NPI_NUM","ACO_ID_NUM")

colnames_claim_inpatient_2011_2015=
c("DESY_SORT_KEY","CLAIM_NO","PRVDR_NUM","CLM_THRU_DT","NCH_NEAR_LINE_REC_IDENT_CD","NCH_CLM_TYPE_CD","CLAIM_QUERY_CODE","CLM_FAC_TYPE_CD","CLM_SRVC_CLSFCTN_TYPE_CD","CLM_FREQ_CD","FI_NUM","CLM_MDCR_NON_PMT_RSN_CD","CLM_PMT_AMT","NCH_PRMRY_PYR_CLM_PD_AMT","NCH_PRMRY_PYR_CD","FI_CLM_ACTN_CD","PRVDR_STATE_CD","ORG_NPI_NUM","AT_PHYSN_UPIN","AT_PHYSN_NPI","OP_PHYSN_UPIN","OP_PHYSN_NPI","OT_PHYSN_UPIN","OT_PHYSN_NPI","CLM_MCO_PD_SW","PTNT_DSCHRG_STUS_CD","CLM_PPS_IND_CD","CLM_TOT_CHRG_AMT","CLM_ADMSN_DT","CLM_IP_ADMSN_TYPE_CD","CLM_SRC_IP_ADMSN_CD","NCH_PTNT_STATUS_IND_CD","CLM_PASS_THRU_PER_DIEM_AMT","NCH_BENE_IP_DDCTBL_AMT","NCH_BENE_PTA_COINSRNC_LBLTY_AM","NCH_BENE_BLOOD_DDCTBL_LBLTY_AM","NCH_PROFNL_CMPNT_CHRG_AMT","NCH_IP_NCVRD_CHRG_AMT","CLM_TOT_PPS_CPTL_AMT","CLM_PPS_CPTL_FSP_AMT","CLM_PPS_CPTL_OUTLIER_AMT","CLM_PPS_CPTL_DSPRPRTNT_SHR_AMT","CLM_PPS_CPTL_IME_AMT","CLM_PPS_CPTL_EXCPTN_AMT","CLM_PPS_OLD_CPTL_HLD_HRMLS_AMT","CLM_PPS_CPTL_DRG_WT_NUM","CLM_UTLZTN_DAY_CNT","BENE_TOT_COINSRNC_DAYS_CNT","BENE_LRD_USED_CNT","CLM_NON_UTLZTN_DAYS_CNT","NCH_BLOOD_PNTS_FRNSHD_QTY","NCH_VRFD_NCVRD_STAY_FROM_DT","NCH_VRFD_NCVRD_STAY_THRU_DT","NCH_BENE_MDCR_BNFTS_EXHTD_DT_I","NCH_BENE_DSCHRG_DT","CLM_DRG_CD","CLM_DRG_OUTLIER_STAY_CD","NCH_DRG_OUTLIER_APRVD_PMT_AMT","ADMTG_DGNS_CD","ADMTG_DGNS_VRSN_CD","PRNCPAL_DGNS_CD","PRNCPAL_DGNS_VRSN_CD","ICD_DGNS_CD1","ICD_DGNS_VRSN_CD1","CLM_POA_IND_SW1","ICD_DGNS_CD2","ICD_DGNS_VRSN_CD2","CLM_POA_IND_SW2","ICD_DGNS_CD3","ICD_DGNS_VRSN_CD3","CLM_POA_IND_SW3","ICD_DGNS_CD4","ICD_DGNS_VRSN_CD4","CLM_POA_IND_SW4","ICD_DGNS_CD5","ICD_DGNS_VRSN_CD5","CLM_POA_IND_SW5","ICD_DGNS_CD6","ICD_DGNS_VRSN_CD6","CLM_POA_IND_SW6","ICD_DGNS_CD7","ICD_DGNS_VRSN_CD7","CLM_POA_IND_SW7","ICD_DGNS_CD8","ICD_DGNS_VRSN_CD8","CLM_POA_IND_SW8","ICD_DGNS_CD9","ICD_DGNS_VRSN_CD9","CLM_POA_IND_SW9","ICD_DGNS_CD10","ICD_DGNS_VRSN_CD10","CLM_POA_IND_SW10","ICD_DGNS_CD11","ICD_DGNS_VRSN_CD11","CLM_POA_IND_SW11","ICD_DGNS_CD12","ICD_DGNS_VRSN_CD12","CLM_POA_IND_SW12","ICD_DGNS_CD13","ICD_DGNS_VRSN_CD13","CLM_POA_IND_SW13","ICD_DGNS_CD14","ICD_DGNS_VRSN_CD14","CLM_POA_IND_SW14","ICD_DGNS_CD15","ICD_DGNS_VRSN_CD15","CLM_POA_IND_SW15","ICD_DGNS_CD16","ICD_DGNS_VRSN_CD16","CLM_POA_IND_SW16","ICD_DGNS_CD17","ICD_DGNS_VRSN_CD17","CLM_POA_IND_SW17","ICD_DGNS_CD18","ICD_DGNS_VRSN_CD18","CLM_POA_IND_SW18","ICD_DGNS_CD19","ICD_DGNS_VRSN_CD19","CLM_POA_IND_SW19","ICD_DGNS_CD20","ICD_DGNS_VRSN_CD20","CLM_POA_IND_SW20","ICD_DGNS_CD21","ICD_DGNS_VRSN_CD21","CLM_POA_IND_SW21","ICD_DGNS_CD22","ICD_DGNS_VRSN_CD22","CLM_POA_IND_SW22","ICD_DGNS_CD23","ICD_DGNS_VRSN_CD23","CLM_POA_IND_SW23","ICD_DGNS_CD24","ICD_DGNS_VRSN_CD24","CLM_POA_IND_SW24","ICD_DGNS_CD25","ICD_DGNS_VRSN_CD25","CLM_POA_IND_SW25","FST_DGNS_E_CD","FST_DGNS_E_VRSN_CD","ICD_DGNS_E_CD1","ICD_DGNS_E_VRSN_CD1","CLM_E_POA_IND_SW1","ICD_DGNS_E_CD2","ICD_DGNS_E_VRSN_CD2","CLM_E_POA_IND_SW2","ICD_DGNS_E_CD3","ICD_DGNS_E_VRSN_CD3","CLM_E_POA_IND_SW3","ICD_DGNS_E_CD4","ICD_DGNS_E_VRSN_CD4","CLM_E_POA_IND_SW4","ICD_DGNS_E_CD5","ICD_DGNS_E_VRSN_CD5","CLM_E_POA_IND_SW5","ICD_DGNS_E_CD6","ICD_DGNS_E_VRSN_CD6","CLM_E_POA_IND_SW6","ICD_DGNS_E_CD7","ICD_DGNS_E_VRSN_CD7","CLM_E_POA_IND_SW7","ICD_DGNS_E_CD8","ICD_DGNS_E_VRSN_CD8","CLM_E_POA_IND_SW8","ICD_DGNS_E_CD9","ICD_DGNS_E_VRSN_CD9","CLM_E_POA_IND_SW9","ICD_DGNS_E_CD10","ICD_DGNS_E_VRSN_CD10","CLM_E_POA_IND_SW10","ICD_DGNS_E_CD11","ICD_DGNS_E_VRSN_CD11","CLM_E_POA_IND_SW11","ICD_DGNS_E_CD12","ICD_DGNS_E_VRSN_CD12","CLM_E_POA_IND_SW12","ICD_PRCDR_CD1","ICD_PRCDR_VRSN_CD1","PRCDR_DT1","ICD_PRCDR_CD2","ICD_PRCDR_VRSN_CD2","PRCDR_DT2","ICD_PRCDR_CD3","ICD_PRCDR_VRSN_CD3","PRCDR_DT3","ICD_PRCDR_CD4","ICD_PRCDR_VRSN_CD4","PRCDR_DT4","ICD_PRCDR_CD5","ICD_PRCDR_VRSN_CD5","PRCDR_DT5","ICD_PRCDR_CD6","ICD_PRCDR_VRSN_CD6","PRCDR_DT6","ICD_PRCDR_CD7","ICD_PRCDR_VRSN_CD7","PRCDR_DT7","ICD_PRCDR_CD8","ICD_PRCDR_VRSN_CD8","PRCDR_DT8","ICD_PRCDR_CD9","ICD_PRCDR_VRSN_CD9","PRCDR_DT9","ICD_PRCDR_CD10","ICD_PRCDR_VRSN_CD10","PRCDR_DT10","ICD_PRCDR_CD11","ICD_PRCDR_VRSN_CD11","PRCDR_DT11","ICD_PRCDR_CD12","ICD_PRCDR_VRSN_CD12","PRCDR_DT12","ICD_PRCDR_CD13","ICD_PRCDR_VRSN_CD13","PRCDR_DT13","ICD_PRCDR_CD14","ICD_PRCDR_VRSN_CD14","PRCDR_DT14","ICD_PRCDR_CD15","ICD_PRCDR_VRSN_CD15","PRCDR_DT15","ICD_PRCDR_CD16","ICD_PRCDR_VRSN_CD16","PRCDR_DT16","ICD_PRCDR_CD17","ICD_PRCDR_VRSN_CD17","PRCDR_DT17","ICD_PRCDR_CD18","ICD_PRCDR_VRSN_CD18","PRCDR_DT18","ICD_PRCDR_CD19","ICD_PRCDR_VRSN_CD19","PRCDR_DT19","ICD_PRCDR_CD20","ICD_PRCDR_VRSN_CD20","PRCDR_DT20","ICD_PRCDR_CD21","ICD_PRCDR_VRSN_CD21","PRCDR_DT21","ICD_PRCDR_CD22","ICD_PRCDR_VRSN_CD22","PRCDR_DT22","ICD_PRCDR_CD23","ICD_PRCDR_VRSN_CD23","PRCDR_DT23","ICD_PRCDR_CD24","ICD_PRCDR_VRSN_CD24","PRCDR_DT24","ICD_PRCDR_CD25","ICD_PRCDR_VRSN_CD25","PRCDR_DT25","DOB_DT","GNDR_CD","BENE_RACE_CD","BENE_CNTY_CD","BENE_STATE_CD","CWF_BENE_MDCR_STUS_CD")

colnames_claim_inpatient_2016_2020=
c("DESY_SORT_KEY","CLAIM_NO","PRVDR_NUM","CLM_THRU_DT","NCH_NEAR_LINE_REC_IDENT_CD","NCH_CLM_TYPE_CD","CLAIM_QUERY_CODE","CLM_FAC_TYPE_CD","CLM_SRVC_CLSFCTN_TYPE_CD","CLM_FREQ_CD","FI_NUM","CLM_MDCR_NON_PMT_RSN_CD","CLM_PMT_AMT","NCH_PRMRY_PYR_CLM_PD_AMT","NCH_PRMRY_PYR_CD","FI_CLM_ACTN_CD","PRVDR_STATE_CD","ORG_NPI_NUM","AT_PHYSN_UPIN","AT_PHYSN_NPI","AT_PHYSN_SPCLTY_CD","OP_PHYSN_UPIN","OP_PHYSN_NPI","OP_PHYSN_SPCLTY_CD","OT_PHYSN_UPIN","OT_PHYSN_NPI","OT_PHYSN_SPCLTY_CD","RNDRNG_PHYSN_NPI","RNDRNG_PHYSN_SPCLTY_CD","CLM_MCO_PD_SW","PTNT_DSCHRG_STUS_CD","CLM_PPS_IND_CD","CLM_TOT_CHRG_AMT","CLM_ADMSN_DT","CLM_IP_ADMSN_TYPE_CD","CLM_SRC_IP_ADMSN_CD","NCH_PTNT_STATUS_IND_CD","CLM_PASS_THRU_PER_DIEM_AMT","NCH_BENE_IP_DDCTBL_AMT","NCH_BENE_PTA_COINSRNC_LBLTY_AM","NCH_BENE_BLOOD_DDCTBL_LBLTY_AM","NCH_PROFNL_CMPNT_CHRG_AMT","NCH_IP_NCVRD_CHRG_AMT","CLM_TOT_PPS_CPTL_AMT","CLM_PPS_CPTL_FSP_AMT","CLM_PPS_CPTL_OUTLIER_AMT","CLM_PPS_CPTL_DSPRPRTNT_SHR_AMT","CLM_PPS_CPTL_IME_AMT","CLM_PPS_CPTL_EXCPTN_AMT","CLM_PPS_OLD_CPTL_HLD_HRMLS_AMT","CLM_PPS_CPTL_DRG_WT_NUM","CLM_UTLZTN_DAY_CNT","BENE_TOT_COINSRNC_DAYS_CNT","BENE_LRD_USED_CNT","CLM_NON_UTLZTN_DAYS_CNT","NCH_BLOOD_PNTS_FRNSHD_QTY","NCH_VRFD_NCVRD_STAY_FROM_DT","NCH_VRFD_NCVRD_STAY_THRU_DT","NCH_BENE_MDCR_BNFTS_EXHTD_DT_I","NCH_BENE_DSCHRG_DT","CLM_DRG_CD","CLM_DRG_OUTLIER_STAY_CD","NCH_DRG_OUTLIER_APRVD_PMT_AMT","ADMTG_DGNS_CD","PRNCPAL_DGNS_CD","ICD_DGNS_CD1","CLM_POA_IND_SW1","ICD_DGNS_CD2","CLM_POA_IND_SW2","ICD_DGNS_CD3","CLM_POA_IND_SW3","ICD_DGNS_CD4","CLM_POA_IND_SW4","ICD_DGNS_CD5","CLM_POA_IND_SW5","ICD_DGNS_CD6","CLM_POA_IND_SW6","ICD_DGNS_CD7","CLM_POA_IND_SW7","ICD_DGNS_CD8","CLM_POA_IND_SW8","ICD_DGNS_CD9","CLM_POA_IND_SW9","ICD_DGNS_CD10","CLM_POA_IND_SW10","ICD_DGNS_CD11","CLM_POA_IND_SW11","ICD_DGNS_CD12","CLM_POA_IND_SW12","ICD_DGNS_CD13","CLM_POA_IND_SW13","ICD_DGNS_CD14","CLM_POA_IND_SW14","ICD_DGNS_CD15","CLM_POA_IND_SW15","ICD_DGNS_CD16","CLM_POA_IND_SW16","ICD_DGNS_CD17","CLM_POA_IND_SW17","ICD_DGNS_CD18","CLM_POA_IND_SW18","ICD_DGNS_CD19","CLM_POA_IND_SW19","ICD_DGNS_CD20","CLM_POA_IND_SW20","ICD_DGNS_CD21","CLM_POA_IND_SW21","ICD_DGNS_CD22","CLM_POA_IND_SW22","ICD_DGNS_CD23","CLM_POA_IND_SW23","ICD_DGNS_CD24","CLM_POA_IND_SW24","ICD_DGNS_CD25","CLM_POA_IND_SW25","FST_DGNS_E_CD","ICD_DGNS_E_CD1","CLM_E_POA_IND_SW1","ICD_DGNS_E_CD2","CLM_E_POA_IND_SW2","ICD_DGNS_E_CD3","CLM_E_POA_IND_SW3","ICD_DGNS_E_CD4","CLM_E_POA_IND_SW4","ICD_DGNS_E_CD5","CLM_E_POA_IND_SW5","ICD_DGNS_E_CD6","CLM_E_POA_IND_SW6","ICD_DGNS_E_CD7","CLM_E_POA_IND_SW7","ICD_DGNS_E_CD8","CLM_E_POA_IND_SW8","ICD_DGNS_E_CD9","CLM_E_POA_IND_SW9","ICD_DGNS_E_CD10","CLM_E_POA_IND_SW10","ICD_DGNS_E_CD11","CLM_E_POA_IND_SW11","ICD_DGNS_E_CD12","CLM_E_POA_IND_SW12","ICD_PRCDR_CD1","PRCDR_DT1","ICD_PRCDR_CD2","PRCDR_DT2","ICD_PRCDR_CD3","PRCDR_DT3","ICD_PRCDR_CD4","PRCDR_DT4","ICD_PRCDR_CD5","PRCDR_DT5","ICD_PRCDR_CD6","PRCDR_DT6","ICD_PRCDR_CD7","PRCDR_DT7","ICD_PRCDR_CD8","PRCDR_DT8","ICD_PRCDR_CD9","PRCDR_DT9","ICD_PRCDR_CD10","PRCDR_DT10","ICD_PRCDR_CD11","PRCDR_DT11","ICD_PRCDR_CD12","PRCDR_DT12","ICD_PRCDR_CD13","PRCDR_DT13","ICD_PRCDR_CD14","PRCDR_DT14","ICD_PRCDR_CD15","PRCDR_DT15","ICD_PRCDR_CD16","PRCDR_DT16","ICD_PRCDR_CD17","PRCDR_DT17","ICD_PRCDR_CD18","PRCDR_DT18","ICD_PRCDR_CD19","PRCDR_DT19","ICD_PRCDR_CD20","PRCDR_DT20","ICD_PRCDR_CD21","PRCDR_DT21","ICD_PRCDR_CD22","PRCDR_DT22","ICD_PRCDR_CD23","PRCDR_DT23","ICD_PRCDR_CD24","PRCDR_DT24","ICD_PRCDR_CD25","PRCDR_DT25","DOB_DT","GNDR_CD","BENE_RACE_CD","BENE_CNTY_CD","BENE_STATE_CD","CWF_BENE_MDCR_STUS_CD","CLM_TRTMT_AUTHRZTN_NUM","CLM_PRCR_RTRN_CD","CLM_IP_LOW_VOL_PMT_AMT","CLM_CARE_IMPRVMT_MODEL_CD1","CLM_CARE_IMPRVMT_MODEL_CD2","CLM_CARE_IMPRVMT_MODEL_CD3","CLM_CARE_IMPRVMT_MODEL_CD4","CLM_BNDLD_MODEL_1_DSCNT_PCT","CLM_BASE_OPRTG_DRG_AMT","CLM_VBP_PRTCPNT_IND_CD","CLM_VBP_ADJSTMT_PCT","CLM_HRR_PRTCPNT_IND_CD","CLM_HRR_ADJSTMT_PCT","CLM_MODEL_4_READMSN_IND_CD","CLM_UNCOMPD_CARE_PMT_AMT","CLM_BNDLD_ADJSTMT_PMT_AMT","CLM_VBP_ADJSTMT_PMT_AMT","CLM_HRR_ADJSTMT_PMT_AMT","EHR_PYMT_ADJSTMT_AMT","PPS_STD_VAL_PYMT_AMT","FINL_STD_AMT","HAC_PGM_RDCTN_IND_SW","EHR_PGM_RDCTN_IND_SW","CLM_SITE_NTRL_PYMT_CST_AMT","CLM_SITE_NTRL_PYMT_IPPS_AMT","CLM_FULL_STD_PYMT_AMT","CLM_SS_OUTLIER_STD_PYMT_AMT","CLM_NEXT_GNRTN_ACO_IND_CD1","CLM_NEXT_GNRTN_ACO_IND_CD2","CLM_NEXT_GNRTN_ACO_IND_CD3","CLM_NEXT_GNRTN_ACO_IND_CD4","CLM_NEXT_GNRTN_ACO_IND_CD5","ACO_ID_NUM")

colnames_claim_outpatient_2011_2015=
c("DESY_SORT_KEY","CLAIM_NO","PRVDR_NUM","CLM_THRU_DT","NCH_NEAR_LINE_REC_IDENT_CD","NCH_CLM_TYPE_CD","CLAIM_QUERY_CODE","CLM_FAC_TYPE_CD","CLM_SRVC_CLSFCTN_TYPE_CD","CLM_FREQ_CD","FI_NUM","CLM_MDCR_NON_PMT_RSN_CD","CLM_PMT_AMT","NCH_PRMRY_PYR_CLM_PD_AMT","NCH_PRMRY_PYR_CD","PRVDR_STATE_CD","ORG_NPI_NUM","AT_PHYSN_UPIN","AT_PHYSN_NPI","OP_PHYSN_UPIN","OP_PHYSN_NPI","OT_PHYSN_UPIN","OT_PHYSN_NPI","CLM_MCO_PD_SW","PTNT_DSCHRG_STUS_CD","CLM_TOT_CHRG_AMT","NCH_BENE_BLOOD_DDCTBL_LBLTY_AM","NCH_PROFNL_CMPNT_CHRG_AMT","PRNCPAL_DGNS_CD","PRNCPAL_DGNS_VRSN_CD","ICD_DGNS_CD1","ICD_DGNS_VRSN_CD1","ICD_DGNS_CD2","ICD_DGNS_VRSN_CD2","ICD_DGNS_CD3","ICD_DGNS_VRSN_CD3","ICD_DGNS_CD4","ICD_DGNS_VRSN_CD4","ICD_DGNS_CD5","ICD_DGNS_VRSN_CD5","ICD_DGNS_CD6","ICD_DGNS_VRSN_CD6","ICD_DGNS_CD7","ICD_DGNS_VRSN_CD7","ICD_DGNS_CD8","ICD_DGNS_VRSN_CD8","ICD_DGNS_CD9","ICD_DGNS_VRSN_CD9","ICD_DGNS_CD10","ICD_DGNS_VRSN_CD10","ICD_DGNS_CD11","ICD_DGNS_VRSN_CD11","ICD_DGNS_CD12","ICD_DGNS_VRSN_CD12","ICD_DGNS_CD13","ICD_DGNS_VRSN_CD13","ICD_DGNS_CD14","ICD_DGNS_VRSN_CD14","ICD_DGNS_CD15","ICD_DGNS_VRSN_CD15","ICD_DGNS_CD16","ICD_DGNS_VRSN_CD16","ICD_DGNS_CD17","ICD_DGNS_VRSN_CD17","ICD_DGNS_CD18","ICD_DGNS_VRSN_CD18","ICD_DGNS_CD19","ICD_DGNS_VRSN_CD19","ICD_DGNS_CD20","ICD_DGNS_VRSN_CD20","ICD_DGNS_CD21","ICD_DGNS_VRSN_CD21","ICD_DGNS_CD22","ICD_DGNS_VRSN_CD22","ICD_DGNS_CD23","ICD_DGNS_VRSN_CD23","ICD_DGNS_CD24","ICD_DGNS_VRSN_CD24","ICD_DGNS_CD25","ICD_DGNS_VRSN_CD25","FST_DGNS_E_CD","FST_DGNS_E_VRSN_CD","ICD_DGNS_E_CD1","ICD_DGNS_E_VRSN_CD1","ICD_DGNS_E_CD2","ICD_DGNS_E_VRSN_CD2","ICD_DGNS_E_CD3","ICD_DGNS_E_VRSN_CD3","ICD_DGNS_E_CD4","ICD_DGNS_E_VRSN_CD4","ICD_DGNS_E_CD5","ICD_DGNS_E_VRSN_CD5","ICD_DGNS_E_CD6","ICD_DGNS_E_VRSN_CD6","ICD_DGNS_E_CD7","ICD_DGNS_E_VRSN_CD7","ICD_DGNS_E_CD8","ICD_DGNS_E_VRSN_CD8","ICD_DGNS_E_CD9","ICD_DGNS_E_VRSN_CD9","ICD_DGNS_E_CD10","ICD_DGNS_E_VRSN_CD10","ICD_DGNS_E_CD11","ICD_DGNS_E_VRSN_CD11","ICD_DGNS_E_CD12","ICD_DGNS_E_VRSN_CD12","ICD_PRCDR_CD1","ICD_PRCDR_VRSN_CD1","PRCDR_DT1","ICD_PRCDR_CD2","ICD_PRCDR_VRSN_CD2","PRCDR_DT2","ICD_PRCDR_CD3","ICD_PRCDR_VRSN_CD3","PRCDR_DT3","ICD_PRCDR_CD4","ICD_PRCDR_VRSN_CD4","PRCDR_DT4","ICD_PRCDR_CD5","ICD_PRCDR_VRSN_CD5","PRCDR_DT5","ICD_PRCDR_CD6","ICD_PRCDR_VRSN_CD6","PRCDR_DT6","ICD_PRCDR_CD7","ICD_PRCDR_VRSN_CD7","PRCDR_DT7","ICD_PRCDR_CD8","ICD_PRCDR_VRSN_CD8","PRCDR_DT8","ICD_PRCDR_CD9","ICD_PRCDR_VRSN_CD9","PRCDR_DT9","ICD_PRCDR_CD10","ICD_PRCDR_VRSN_CD10","PRCDR_DT10","ICD_PRCDR_CD11","ICD_PRCDR_VRSN_CD11","PRCDR_DT11","ICD_PRCDR_CD12","ICD_PRCDR_VRSN_CD12","PRCDR_DT12","ICD_PRCDR_CD13","ICD_PRCDR_VRSN_CD13","PRCDR_DT13","ICD_PRCDR_CD14","ICD_PRCDR_VRSN_CD14","PRCDR_DT14","ICD_PRCDR_CD15","ICD_PRCDR_VRSN_CD15","PRCDR_DT15","ICD_PRCDR_CD16","ICD_PRCDR_VRSN_CD16","PRCDR_DT16","ICD_PRCDR_CD17","ICD_PRCDR_VRSN_CD17","PRCDR_DT17","ICD_PRCDR_CD18","ICD_PRCDR_VRSN_CD18","PRCDR_DT18","ICD_PRCDR_CD19","ICD_PRCDR_VRSN_CD19","PRCDR_DT19","ICD_PRCDR_CD20","ICD_PRCDR_VRSN_CD20","PRCDR_DT20","ICD_PRCDR_CD21","ICD_PRCDR_VRSN_CD21","PRCDR_DT21","ICD_PRCDR_CD22","ICD_PRCDR_VRSN_CD22","PRCDR_DT22","ICD_PRCDR_CD23","ICD_PRCDR_VRSN_CD23","PRCDR_DT23","ICD_PRCDR_CD24","ICD_PRCDR_VRSN_CD24","PRCDR_DT24","ICD_PRCDR_CD25","ICD_PRCDR_VRSN_CD25","PRCDR_DT25","RSN_VISIT_CD1","RSN_VISIT_VRSN_CD1","RSN_VISIT_CD2","RSN_VISIT_VRSN_CD2","RSN_VISIT_CD3","RSN_VISIT_VRSN_CD3","NCH_BENE_PTB_DDCTBL_AMT","NCH_BENE_PTB_COINSRNC_AMT","CLM_OP_PRVDR_PMT_AMT","CLM_OP_BENE_PMT_AMT","DOB_DT","GNDR_CD","BENE_RACE_CD","BENE_CNTY_CD","BENE_STATE_CD","CWF_BENE_MDCR_STUS_CD","FI_CLM_ACTN_CD")

colnames_claim_outpatient_2016_2020=
c("DESY_SORT_KEY","CLAIM_NO","PRVDR_NUM","CLM_THRU_DT","NCH_NEAR_LINE_REC_IDENT_CD","NCH_CLM_TYPE_CD","CLAIM_QUERY_CODE","CLM_FAC_TYPE_CD","CLM_SRVC_CLSFCTN_TYPE_CD","CLM_FREQ_CD","FI_NUM","CLM_MDCR_NON_PMT_RSN_CD","CLM_PMT_AMT","NCH_PRMRY_PYR_CLM_PD_AMT","NCH_PRMRY_PYR_CD","PRVDR_STATE_CD","ORG_NPI_NUM","SRVC_LOC_NPI_NUM","AT_PHYSN_UPIN","AT_PHYSN_NPI","AT_PHYSN_SPCLTY_CD","OP_PHYSN_UPIN","OP_PHYSN_NPI","OP_PHYSN_SPCLTY_CD","OT_PHYSN_UPIN","OT_PHYSN_NPI","OT_PHYSN_SPCLTY_CD","RNDRNG_PHYSN_NPI","RNDRNG_PHYSN_SPCLTY_CD","RFR_PHYSN_NPI","RFR_PHYSN_SPCLTY_CD","CLM_MCO_PD_SW","PTNT_DSCHRG_STUS_CD","CLM_TOT_CHRG_AMT","NCH_BENE_BLOOD_DDCTBL_LBLTY_AM","NCH_PROFNL_CMPNT_CHRG_AMT","PRNCPAL_DGNS_CD","ICD_DGNS_CD1","ICD_DGNS_CD2","ICD_DGNS_CD3","ICD_DGNS_CD4","ICD_DGNS_CD5","ICD_DGNS_CD6","ICD_DGNS_CD7","ICD_DGNS_CD8","ICD_DGNS_CD9","ICD_DGNS_CD10","ICD_DGNS_CD11","ICD_DGNS_CD12","ICD_DGNS_CD13","ICD_DGNS_CD14","ICD_DGNS_CD15","ICD_DGNS_CD16","ICD_DGNS_CD17","ICD_DGNS_CD18","ICD_DGNS_CD19","ICD_DGNS_CD20","ICD_DGNS_CD21","ICD_DGNS_CD22","ICD_DGNS_CD23","ICD_DGNS_CD24","ICD_DGNS_CD25","FST_DGNS_E_CD","ICD_DGNS_E_CD1","ICD_DGNS_E_CD2","ICD_DGNS_E_CD3","ICD_DGNS_E_CD4","ICD_DGNS_E_CD5","ICD_DGNS_E_CD6","ICD_DGNS_E_CD7","ICD_DGNS_E_CD8","ICD_DGNS_E_CD9","ICD_DGNS_E_CD10","ICD_DGNS_E_CD11","ICD_DGNS_E_CD12","ICD_PRCDR_CD1","PRCDR_DT1","ICD_PRCDR_CD2","PRCDR_DT2","ICD_PRCDR_CD3","PRCDR_DT3","ICD_PRCDR_CD4","PRCDR_DT4","ICD_PRCDR_CD5","PRCDR_DT5","ICD_PRCDR_CD6","PRCDR_DT6","ICD_PRCDR_CD7","PRCDR_DT7","ICD_PRCDR_CD8","PRCDR_DT8","ICD_PRCDR_CD9","PRCDR_DT9","ICD_PRCDR_CD10","PRCDR_DT10","ICD_PRCDR_CD11","PRCDR_DT11","ICD_PRCDR_CD12","PRCDR_DT12","ICD_PRCDR_CD13","PRCDR_DT13","ICD_PRCDR_CD14","PRCDR_DT14","ICD_PRCDR_CD15","PRCDR_DT15","ICD_PRCDR_CD16","PRCDR_DT16","ICD_PRCDR_CD17","PRCDR_DT17","ICD_PRCDR_CD18","PRCDR_DT18","ICD_PRCDR_CD19","PRCDR_DT19","ICD_PRCDR_CD20","PRCDR_DT20","ICD_PRCDR_CD21","PRCDR_DT21","ICD_PRCDR_CD22","PRCDR_DT22","ICD_PRCDR_CD23","PRCDR_DT23","ICD_PRCDR_CD24","PRCDR_DT24","ICD_PRCDR_CD25","PRCDR_DT25","RSN_VISIT_CD1","RSN_VISIT_CD2","RSN_VISIT_CD3","NCH_BENE_PTB_DDCTBL_AMT","NCH_BENE_PTB_COINSRNC_AMT","CLM_OP_PRVDR_PMT_AMT","CLM_OP_BENE_PMT_AMT","DOB_DT","GNDR_CD","BENE_RACE_CD","BENE_CNTY_CD","BENE_STATE_CD","CWF_BENE_MDCR_STUS_CD","FI_CLM_ACTN_CD","NCH_BLOOD_PNTS_FRNSHD_QTY","CLM_TRTMT_AUTHRZTN_NUM","CLM_PRCR_RTRN_CD","CLM_OP_TRANS_TYPE_CD","CLM_OP_ESRD_MTHD_CD","CLM_NEXT_GNRTN_ACO_IND_CD1","CLM_NEXT_GNRTN_ACO_IND_CD2","CLM_NEXT_GNRTN_ACO_IND_CD3","CLM_NEXT_GNRTN_ACO_IND_CD4","CLM_NEXT_GNRTN_ACO_IND_CD5","ACO_ID_NUM")





colnames(claim_carrier_2013)=colnames_claim_carrier_2011_2015
colnames(claim_carrier_2014)=colnames_claim_carrier_2011_2015
colnames(claim_carrier_2015)=colnames_claim_carrier_2011_2015
colnames(claim_carrier_2016)=colnames_claim_carrier_2016_2020
colnames(claim_carrier_2017)=colnames_claim_carrier_2016_2020
colnames(claim_carrier_2018)=colnames_claim_carrier_2016_2020
colnames(claim_carrier_2019)=colnames_claim_carrier_2016_2020
colnames(claim_carrier_2020)=colnames_claim_carrier_2016_2020


colnames(claim_inpatient_2013)=colnames_claim_inpatient_2011_2015
colnames(claim_inpatient_2014)=colnames_claim_inpatient_2011_2015
colnames(claim_inpatient_2015)=colnames_claim_inpatient_2011_2015
colnames(claim_inpatient_2016)=colnames_claim_inpatient_2016_2020
colnames(claim_inpatient_2017)=colnames_claim_inpatient_2016_2020
colnames(claim_inpatient_2018)=colnames_claim_inpatient_2016_2020
colnames(claim_inpatient_2019)=colnames_claim_inpatient_2016_2020
colnames(claim_inpatient_2020)=colnames_claim_inpatient_2016_2020


colnames(claim_outpatient_2013)=colnames_claim_outpatient_2011_2015
colnames(claim_outpatient_2014)=colnames_claim_outpatient_2011_2015
colnames(claim_outpatient_2015)=colnames_claim_outpatient_2011_2015
colnames(claim_outpatient_2016)=colnames_claim_outpatient_2016_2020
colnames(claim_outpatient_2017)=colnames_claim_outpatient_2016_2020
colnames(claim_outpatient_2018)=colnames_claim_outpatient_2016_2020
colnames(claim_outpatient_2019)=colnames_claim_outpatient_2016_2020
colnames(claim_outpatient_2020)=colnames_claim_outpatient_2016_2020


claim_inpatient_2016[,":="(ADMTG_DGNS_VRSN_CD =0, PRNCPAL_DGNS_VRSN_CD=0, ICD_DGNS_VRSN_CD1=0, ICD_DGNS_VRSN_CD2=0, ICD_DGNS_VRSN_CD3=0, ICD_DGNS_VRSN_CD4=0, ICD_DGNS_VRSN_CD5=0, ICD_DGNS_VRSN_CD6=0, ICD_DGNS_VRSN_CD7=0, ICD_DGNS_VRSN_CD8=0, ICD_DGNS_VRSN_CD9=0, ICD_DGNS_VRSN_CD10=0, ICD_DGNS_VRSN_CD11=0, ICD_DGNS_VRSN_CD12=0, ICD_DGNS_VRSN_CD13=0, ICD_DGNS_VRSN_CD14=0, ICD_DGNS_VRSN_CD15=0, ICD_DGNS_VRSN_CD16=0, ICD_DGNS_VRSN_CD17=0, ICD_DGNS_VRSN_CD18=0, ICD_DGNS_VRSN_CD19=0, ICD_DGNS_VRSN_CD20=0, ICD_DGNS_VRSN_CD21=0, ICD_DGNS_VRSN_CD22=0, ICD_DGNS_VRSN_CD23=0, ICD_DGNS_VRSN_CD24=0, ICD_DGNS_VRSN_CD25=0)]

claim_inpatient_2017[,":="(ADMTG_DGNS_VRSN_CD =0, PRNCPAL_DGNS_VRSN_CD=0, ICD_DGNS_VRSN_CD1=0, ICD_DGNS_VRSN_CD2=0, ICD_DGNS_VRSN_CD3=0, ICD_DGNS_VRSN_CD4=0, ICD_DGNS_VRSN_CD5=0, ICD_DGNS_VRSN_CD6=0, ICD_DGNS_VRSN_CD7=0, ICD_DGNS_VRSN_CD8=0, ICD_DGNS_VRSN_CD9=0, ICD_DGNS_VRSN_CD10=0, ICD_DGNS_VRSN_CD11=0, ICD_DGNS_VRSN_CD12=0, ICD_DGNS_VRSN_CD13=0, ICD_DGNS_VRSN_CD14=0, ICD_DGNS_VRSN_CD15=0, ICD_DGNS_VRSN_CD16=0, ICD_DGNS_VRSN_CD17=0, ICD_DGNS_VRSN_CD18=0, ICD_DGNS_VRSN_CD19=0, ICD_DGNS_VRSN_CD20=0, ICD_DGNS_VRSN_CD21=0, ICD_DGNS_VRSN_CD22=0, ICD_DGNS_VRSN_CD23=0, ICD_DGNS_VRSN_CD24=0, ICD_DGNS_VRSN_CD25=0)]

claim_inpatient_2018[,":="(ADMTG_DGNS_VRSN_CD =0, PRNCPAL_DGNS_VRSN_CD=0, ICD_DGNS_VRSN_CD1=0, ICD_DGNS_VRSN_CD2=0, ICD_DGNS_VRSN_CD3=0, ICD_DGNS_VRSN_CD4=0, ICD_DGNS_VRSN_CD5=0, ICD_DGNS_VRSN_CD6=0, ICD_DGNS_VRSN_CD7=0, ICD_DGNS_VRSN_CD8=0, ICD_DGNS_VRSN_CD9=0, ICD_DGNS_VRSN_CD10=0, ICD_DGNS_VRSN_CD11=0, ICD_DGNS_VRSN_CD12=0, ICD_DGNS_VRSN_CD13=0, ICD_DGNS_VRSN_CD14=0, ICD_DGNS_VRSN_CD15=0, ICD_DGNS_VRSN_CD16=0, ICD_DGNS_VRSN_CD17=0, ICD_DGNS_VRSN_CD18=0, ICD_DGNS_VRSN_CD19=0, ICD_DGNS_VRSN_CD20=0, ICD_DGNS_VRSN_CD21=0, ICD_DGNS_VRSN_CD22=0, ICD_DGNS_VRSN_CD23=0, ICD_DGNS_VRSN_CD24=0, ICD_DGNS_VRSN_CD25=0)]

claim_inpatient_2019[,":="(ADMTG_DGNS_VRSN_CD =0, PRNCPAL_DGNS_VRSN_CD=0, ICD_DGNS_VRSN_CD1=0, ICD_DGNS_VRSN_CD2=0, ICD_DGNS_VRSN_CD3=0, ICD_DGNS_VRSN_CD4=0, ICD_DGNS_VRSN_CD5=0, ICD_DGNS_VRSN_CD6=0, ICD_DGNS_VRSN_CD7=0, ICD_DGNS_VRSN_CD8=0, ICD_DGNS_VRSN_CD9=0, ICD_DGNS_VRSN_CD10=0, ICD_DGNS_VRSN_CD11=0, ICD_DGNS_VRSN_CD12=0, ICD_DGNS_VRSN_CD13=0, ICD_DGNS_VRSN_CD14=0, ICD_DGNS_VRSN_CD15=0, ICD_DGNS_VRSN_CD16=0, ICD_DGNS_VRSN_CD17=0, ICD_DGNS_VRSN_CD18=0, ICD_DGNS_VRSN_CD19=0, ICD_DGNS_VRSN_CD20=0, ICD_DGNS_VRSN_CD21=0, ICD_DGNS_VRSN_CD22=0, ICD_DGNS_VRSN_CD23=0, ICD_DGNS_VRSN_CD24=0, ICD_DGNS_VRSN_CD25=0)]

claim_inpatient_2020[,":="(ADMTG_DGNS_VRSN_CD =0, PRNCPAL_DGNS_VRSN_CD=0, ICD_DGNS_VRSN_CD1=0, ICD_DGNS_VRSN_CD2=0, ICD_DGNS_VRSN_CD3=0, ICD_DGNS_VRSN_CD4=0, ICD_DGNS_VRSN_CD5=0, ICD_DGNS_VRSN_CD6=0, ICD_DGNS_VRSN_CD7=0, ICD_DGNS_VRSN_CD8=0, ICD_DGNS_VRSN_CD9=0, ICD_DGNS_VRSN_CD10=0, ICD_DGNS_VRSN_CD11=0, ICD_DGNS_VRSN_CD12=0, ICD_DGNS_VRSN_CD13=0, ICD_DGNS_VRSN_CD14=0, ICD_DGNS_VRSN_CD15=0, ICD_DGNS_VRSN_CD16=0, ICD_DGNS_VRSN_CD17=0, ICD_DGNS_VRSN_CD18=0, ICD_DGNS_VRSN_CD19=0, ICD_DGNS_VRSN_CD20=0, ICD_DGNS_VRSN_CD21=0, ICD_DGNS_VRSN_CD22=0, ICD_DGNS_VRSN_CD23=0, ICD_DGNS_VRSN_CD24=0, ICD_DGNS_VRSN_CD25=0)]





claim_outpatient_2016[,":="(PRNCPAL_DGNS_VRSN_CD=0, ICD_DGNS_VRSN_CD1=0, ICD_DGNS_VRSN_CD2=0, ICD_DGNS_VRSN_CD3=0, ICD_DGNS_VRSN_CD4=0, ICD_DGNS_VRSN_CD5=0, ICD_DGNS_VRSN_CD6=0, ICD_DGNS_VRSN_CD7=0, ICD_DGNS_VRSN_CD8=0, ICD_DGNS_VRSN_CD9=0, ICD_DGNS_VRSN_CD10=0, ICD_DGNS_VRSN_CD11=0, ICD_DGNS_VRSN_CD12=0, ICD_DGNS_VRSN_CD13=0, ICD_DGNS_VRSN_CD14=0, ICD_DGNS_VRSN_CD15=0, ICD_DGNS_VRSN_CD16=0, ICD_DGNS_VRSN_CD17=0, ICD_DGNS_VRSN_CD18=0, ICD_DGNS_VRSN_CD19=0, ICD_DGNS_VRSN_CD20=0, ICD_DGNS_VRSN_CD21=0, ICD_DGNS_VRSN_CD22=0, ICD_DGNS_VRSN_CD23=0, ICD_DGNS_VRSN_CD24=0, ICD_DGNS_VRSN_CD25=0)]

claim_outpatient_2017[,":="( PRNCPAL_DGNS_VRSN_CD=0, ICD_DGNS_VRSN_CD1=0, ICD_DGNS_VRSN_CD2=0, ICD_DGNS_VRSN_CD3=0, ICD_DGNS_VRSN_CD4=0, ICD_DGNS_VRSN_CD5=0, ICD_DGNS_VRSN_CD6=0, ICD_DGNS_VRSN_CD7=0, ICD_DGNS_VRSN_CD8=0, ICD_DGNS_VRSN_CD9=0, ICD_DGNS_VRSN_CD10=0, ICD_DGNS_VRSN_CD11=0, ICD_DGNS_VRSN_CD12=0, ICD_DGNS_VRSN_CD13=0, ICD_DGNS_VRSN_CD14=0, ICD_DGNS_VRSN_CD15=0, ICD_DGNS_VRSN_CD16=0, ICD_DGNS_VRSN_CD17=0, ICD_DGNS_VRSN_CD18=0, ICD_DGNS_VRSN_CD19=0, ICD_DGNS_VRSN_CD20=0, ICD_DGNS_VRSN_CD21=0, ICD_DGNS_VRSN_CD22=0, ICD_DGNS_VRSN_CD23=0, ICD_DGNS_VRSN_CD24=0, ICD_DGNS_VRSN_CD25=0)]

claim_outpatient_2018[,":="( PRNCPAL_DGNS_VRSN_CD=0, ICD_DGNS_VRSN_CD1=0, ICD_DGNS_VRSN_CD2=0, ICD_DGNS_VRSN_CD3=0, ICD_DGNS_VRSN_CD4=0, ICD_DGNS_VRSN_CD5=0, ICD_DGNS_VRSN_CD6=0, ICD_DGNS_VRSN_CD7=0, ICD_DGNS_VRSN_CD8=0, ICD_DGNS_VRSN_CD9=0, ICD_DGNS_VRSN_CD10=0, ICD_DGNS_VRSN_CD11=0, ICD_DGNS_VRSN_CD12=0, ICD_DGNS_VRSN_CD13=0, ICD_DGNS_VRSN_CD14=0, ICD_DGNS_VRSN_CD15=0, ICD_DGNS_VRSN_CD16=0, ICD_DGNS_VRSN_CD17=0, ICD_DGNS_VRSN_CD18=0, ICD_DGNS_VRSN_CD19=0, ICD_DGNS_VRSN_CD20=0, ICD_DGNS_VRSN_CD21=0, ICD_DGNS_VRSN_CD22=0, ICD_DGNS_VRSN_CD23=0, ICD_DGNS_VRSN_CD24=0, ICD_DGNS_VRSN_CD25=0)]

claim_outpatient_2019[,":="( PRNCPAL_DGNS_VRSN_CD=0, ICD_DGNS_VRSN_CD1=0, ICD_DGNS_VRSN_CD2=0, ICD_DGNS_VRSN_CD3=0, ICD_DGNS_VRSN_CD4=0, ICD_DGNS_VRSN_CD5=0, ICD_DGNS_VRSN_CD6=0, ICD_DGNS_VRSN_CD7=0, ICD_DGNS_VRSN_CD8=0, ICD_DGNS_VRSN_CD9=0, ICD_DGNS_VRSN_CD10=0, ICD_DGNS_VRSN_CD11=0, ICD_DGNS_VRSN_CD12=0, ICD_DGNS_VRSN_CD13=0, ICD_DGNS_VRSN_CD14=0, ICD_DGNS_VRSN_CD15=0, ICD_DGNS_VRSN_CD16=0, ICD_DGNS_VRSN_CD17=0, ICD_DGNS_VRSN_CD18=0, ICD_DGNS_VRSN_CD19=0, ICD_DGNS_VRSN_CD20=0, ICD_DGNS_VRSN_CD21=0, ICD_DGNS_VRSN_CD22=0, ICD_DGNS_VRSN_CD23=0, ICD_DGNS_VRSN_CD24=0, ICD_DGNS_VRSN_CD25=0)]

claim_outpatient_2020[,":="( PRNCPAL_DGNS_VRSN_CD=0, ICD_DGNS_VRSN_CD1=0, ICD_DGNS_VRSN_CD2=0, ICD_DGNS_VRSN_CD3=0, ICD_DGNS_VRSN_CD4=0, ICD_DGNS_VRSN_CD5=0, ICD_DGNS_VRSN_CD6=0, ICD_DGNS_VRSN_CD7=0, ICD_DGNS_VRSN_CD8=0, ICD_DGNS_VRSN_CD9=0, ICD_DGNS_VRSN_CD10=0, ICD_DGNS_VRSN_CD11=0, ICD_DGNS_VRSN_CD12=0, ICD_DGNS_VRSN_CD13=0, ICD_DGNS_VRSN_CD14=0, ICD_DGNS_VRSN_CD15=0, ICD_DGNS_VRSN_CD16=0, ICD_DGNS_VRSN_CD17=0, ICD_DGNS_VRSN_CD18=0, ICD_DGNS_VRSN_CD19=0, ICD_DGNS_VRSN_CD20=0, ICD_DGNS_VRSN_CD21=0, ICD_DGNS_VRSN_CD22=0, ICD_DGNS_VRSN_CD23=0, ICD_DGNS_VRSN_CD24=0, ICD_DGNS_VRSN_CD25=0)]




choose_columns = function(data_list,columns) {
  require(data.table)
  data_list = lapply(data_list, function (data) { data [, ..columns] } )
  result = rbindlist(data_list)
  return(result)
}

claim_carrier_all_years = choose_columns(
  list(
  claim_carrier_2013,
  claim_carrier_2014,
  claim_carrier_2015,
  claim_carrier_2016,
  claim_carrier_2017,
  claim_carrier_2018,
  claim_carrier_2019,
  claim_carrier_2020),
  columns=c("DESY_SORT_KEY","CLAIM_NO","CLM_THRU_DT","PRNCPAL_DGNS_CD","PRNCPAL_DGNS_VRSN_CD","ICD_DGNS_CD1","ICD_DGNS_VRSN_CD1","ICD_DGNS_CD2","ICD_DGNS_VRSN_CD2","ICD_DGNS_CD3","ICD_DGNS_VRSN_CD3","ICD_DGNS_CD4","ICD_DGNS_VRSN_CD4","ICD_DGNS_CD5","ICD_DGNS_VRSN_CD5","ICD_DGNS_CD6","ICD_DGNS_VRSN_CD6","ICD_DGNS_CD7","ICD_DGNS_VRSN_CD7","ICD_DGNS_CD8","ICD_DGNS_VRSN_CD8","ICD_DGNS_CD9","ICD_DGNS_VRSN_CD9","ICD_DGNS_CD10","ICD_DGNS_VRSN_CD10","ICD_DGNS_CD11","ICD_DGNS_VRSN_CD11","ICD_DGNS_CD12","ICD_DGNS_VRSN_CD12"))

claim_carrier_all_years[,date := as.IDate(as.character(CLM_THRU_DT), "%Y%m%d")][order(date)]
claim_carrier_all_years[,month_year := format(date, "%Y-%m")]
claim_carrier_all_years[,DESY_SORT_KEY := as.integer(DESY_SORT_KEY)]


claim_inpatient_all_years = choose_columns(
  list(
  claim_inpatient_2013,
  claim_inpatient_2014,
  claim_inpatient_2015,
  claim_inpatient_2016,
  claim_inpatient_2017,
  claim_inpatient_2018,
  claim_inpatient_2019,
  claim_inpatient_2020),
  columns=c("DESY_SORT_KEY","CLAIM_NO","CLM_THRU_DT","ADMTG_DGNS_CD","ADMTG_DGNS_VRSN_CD","PRNCPAL_DGNS_CD","PRNCPAL_DGNS_VRSN_CD","ICD_DGNS_CD1","ICD_DGNS_VRSN_CD1","CLM_POA_IND_SW1","ICD_DGNS_CD2","ICD_DGNS_VRSN_CD2","CLM_POA_IND_SW2","ICD_DGNS_CD3","ICD_DGNS_VRSN_CD3","CLM_POA_IND_SW3","ICD_DGNS_CD4","ICD_DGNS_VRSN_CD4","CLM_POA_IND_SW4","ICD_DGNS_CD5","ICD_DGNS_VRSN_CD5","CLM_POA_IND_SW5","ICD_DGNS_CD6","ICD_DGNS_VRSN_CD6","CLM_POA_IND_SW6","ICD_DGNS_CD7","ICD_DGNS_VRSN_CD7","CLM_POA_IND_SW7","ICD_DGNS_CD8","ICD_DGNS_VRSN_CD8","CLM_POA_IND_SW8","ICD_DGNS_CD9","ICD_DGNS_VRSN_CD9","CLM_POA_IND_SW9","ICD_DGNS_CD10","ICD_DGNS_VRSN_CD10","CLM_POA_IND_SW10","ICD_DGNS_CD11","ICD_DGNS_VRSN_CD11","CLM_POA_IND_SW11","ICD_DGNS_CD12","ICD_DGNS_VRSN_CD12","CLM_POA_IND_SW12","ICD_DGNS_CD13","ICD_DGNS_VRSN_CD13","CLM_POA_IND_SW13","ICD_DGNS_CD14","ICD_DGNS_VRSN_CD14","CLM_POA_IND_SW14","ICD_DGNS_CD15","ICD_DGNS_VRSN_CD15","CLM_POA_IND_SW15","ICD_DGNS_CD16","ICD_DGNS_VRSN_CD16","CLM_POA_IND_SW16","ICD_DGNS_CD17","ICD_DGNS_VRSN_CD17","CLM_POA_IND_SW17","ICD_DGNS_CD18","ICD_DGNS_VRSN_CD18","CLM_POA_IND_SW18","ICD_DGNS_CD19","ICD_DGNS_VRSN_CD19","CLM_POA_IND_SW19","ICD_DGNS_CD20","ICD_DGNS_VRSN_CD20","CLM_POA_IND_SW20","ICD_DGNS_CD21","ICD_DGNS_VRSN_CD21","CLM_POA_IND_SW21","ICD_DGNS_CD22","ICD_DGNS_VRSN_CD22","CLM_POA_IND_SW22","ICD_DGNS_CD23","ICD_DGNS_VRSN_CD23","CLM_POA_IND_SW23","ICD_DGNS_CD24","ICD_DGNS_VRSN_CD24","CLM_POA_IND_SW24","ICD_DGNS_CD25","ICD_DGNS_VRSN_CD25","CLM_POA_IND_SW25"))

claim_inpatient_all_years[,date := as.IDate(as.character(CLM_THRU_DT), "%Y%m%d")][order(date)]
claim_inpatient_all_years[,month_year := format(date, "%Y-%m")]
claim_inpatient_all_years[,DESY_SORT_KEY := as.integer(DESY_SORT_KEY)]


claim_outpatient_all_years = choose_columns(
  list(
  claim_outpatient_2013,
  claim_outpatient_2014,
  claim_outpatient_2015,
  claim_outpatient_2016,
  claim_outpatient_2017,
  claim_outpatient_2018,
  claim_outpatient_2019,
  claim_outpatient_2020),
  columns=c("DESY_SORT_KEY","CLAIM_NO","CLM_THRU_DT","PRNCPAL_DGNS_CD","PRNCPAL_DGNS_VRSN_CD","ICD_DGNS_CD1","ICD_DGNS_VRSN_CD1","ICD_DGNS_CD2","ICD_DGNS_VRSN_CD2","ICD_DGNS_CD3","ICD_DGNS_VRSN_CD3","ICD_DGNS_CD4","ICD_DGNS_VRSN_CD4","ICD_DGNS_CD5","ICD_DGNS_VRSN_CD5","ICD_DGNS_CD6","ICD_DGNS_VRSN_CD6","ICD_DGNS_CD7","ICD_DGNS_VRSN_CD7","ICD_DGNS_CD8","ICD_DGNS_VRSN_CD8","ICD_DGNS_CD9","ICD_DGNS_VRSN_CD9","ICD_DGNS_CD10","ICD_DGNS_VRSN_CD10","ICD_DGNS_CD11","ICD_DGNS_VRSN_CD11","ICD_DGNS_CD12","ICD_DGNS_VRSN_CD12","ICD_DGNS_CD13","ICD_DGNS_VRSN_CD13","ICD_DGNS_CD14","ICD_DGNS_VRSN_CD14","ICD_DGNS_CD15","ICD_DGNS_VRSN_CD15","ICD_DGNS_CD16","ICD_DGNS_VRSN_CD16","ICD_DGNS_CD17","ICD_DGNS_VRSN_CD17","ICD_DGNS_CD18","ICD_DGNS_VRSN_CD18","ICD_DGNS_CD19","ICD_DGNS_VRSN_CD19","ICD_DGNS_CD20","ICD_DGNS_VRSN_CD20","ICD_DGNS_CD21","ICD_DGNS_VRSN_CD21","ICD_DGNS_CD22","ICD_DGNS_VRSN_CD22","ICD_DGNS_CD23","ICD_DGNS_VRSN_CD23","ICD_DGNS_CD24","ICD_DGNS_VRSN_CD24","ICD_DGNS_CD25","ICD_DGNS_VRSN_CD25"))

claim_outpatient_all_years[,date := as.IDate(as.character(CLM_THRU_DT), "%Y%m%d")][order(date)]
claim_outpatient_all_years[,month_year := format(date, "%Y-%m")]
claim_outpatient_all_years[,DESY_SORT_KEY := as.integer(DESY_SORT_KEY)]



write_fst(claim_carrier_all_years, "/work/postresearch/Shared/Projects/Data_fst/claim_carrier_all_years.fst")
write_fst(claim_inpatient_all_years, "/work/postresearch/Shared/Projects/Data_fst/claim_inpatient_all_years.fst")
write_fst(claim_outpatient_all_years, "/work/postresearch/Shared/Projects/Data_fst/claim_outpatient_all_years.fst")
