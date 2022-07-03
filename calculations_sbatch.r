options(repr.matrix.max.rows=100, repr.matrix.max.cols=300)
options(repr.plot.width = 20, repr.plot.height = 15)
options(width=300)

numcores=110

install.packages("tidyverse")
install.packages("data.table")
install.packages("fst")
#install.packages("comorbidity")
install.packages("reshape")
install.packages("dtplyr")
install.packages("haven")
install.packages("vroom")
#install.packages("dplyr")



library(tidyverse)
library(data.table)
library(fst)
library(comorbidity)
library(reshape)
library(dtplyr)
library(haven)
library(vroom)
library(dplyr)
`%!in%` = Negate(`%in%`)

setDTthreads(numcores)









#diagnosis codes

office_visit_codes=c("99201","99202","99203","99204","99205","99211","99212","99213","99214"
                     ,"99215")

IHD_icd_9_codes=c(410, 411, 412,413,414)
IHD_icd_10_codes=c("I20", "I21", "I22", "I23", "I24", "I25")

non_us_state_codes=c(40,54,56,57,58,59,60,61,62,63,64,65,66,97,98,99)

primary_care_specialty_codes=c("01", "08", "11", "38")

#http://www.icd9data.com/2015/Volume1/390-459/401-405/default.htm
#https://www.icd10data.com/ICD10CM/Codes/I00-I99/I10-I16
hypertension_icd_9_codes=c("401","402","403","404","405")
hypertension_icd_10_codes=c("I10","I11","I12","I13","I15","I16")

#http://www.icd9data.com/2014/Volume1/290-319/295-299/296/default.htm
#https://www.icd10data.com/ICD10CM/Codes/F01-F99/F30-F39
depression_icd_9_codes=c("2962","2963")
depression_icd_10_codes=c("F32","F33")

#http://www.icd9data.com/2015/Volume1/240-279/249-259/default.htm
#https://www.icd10data.com/ICD10CM/Codes/E00-E89/E08-E13
diabetes_icd_9_codes=c("250")
diabetes_icd_10_codes=c("E08","E09","E10","E11","E13")

#http://www.icd9data.com/2014/Volume1/710-739/710-719/714/default.htm
#https://www.icd10data.com/ICD10CM/Codes/M00-M99/M05-M14
arthritis_icd_9_codes=c("714")
arthritis_icd_10_codes=c("M05","M06","M07","M08","M09","M10","M11","M12","M13","M14")









carrier_data_all_years = read_fst(
    "carrier_data_all_years.fst", as.data.table = T)
mbsf_data = read_fst(
  "/work/postresearch/Shared/Projects/Data_fst/mbsf_data", as.data.table = T)









yearly_calculator_patient_conditions = function(data) {
  
  #requirements
  require(data.table)
  require(dtplyr)
  require(tidyverse)
  require(lubridate)
  
  data %>%
    mutate(
      is_office_visit = HCPCS_CD %in% office_visit_codes,
      
      is_by_primary_care_physician= PRVDR_SPCLTY %in% primary_care_specialty_codes,

      is_hypertension= if_else(
        LINE_ICD_DGNS_VRSN_CD == 0,
        substr(LINE_ICD_DGNS_CD, 0, 3) %in% hypertension_icd_10_codes,
        if_else(
          LINE_ICD_DGNS_VRSN_CD == 9,
          substr(LINE_ICD_DGNS_CD, 0, 3) %in% hypertension_icd_9_codes,NA)),
      
      is_arthritis= if_else(
        LINE_ICD_DGNS_VRSN_CD == 0,
        substr(LINE_ICD_DGNS_CD, 0, 3) %in% arthritis_icd_10_codes,
        if_else(
          LINE_ICD_DGNS_VRSN_CD == 9,
          substr(LINE_ICD_DGNS_CD, 0, 3) %in% arthritis_icd_9_codes,NA)),
      
      is_IHD = if_else(
        LINE_ICD_DGNS_VRSN_CD == 0,
        substr(LINE_ICD_DGNS_CD, 0, 3) %in% IHD_icd_10_codes,
        if_else(
          LINE_ICD_DGNS_VRSN_CD == 9,
          substr(LINE_ICD_DGNS_CD, 0, 3) %in% IHD_icd_9_codes,NA)),
      
      is_diabetes= if_else(
        LINE_ICD_DGNS_VRSN_CD == 0,
        substr(LINE_ICD_DGNS_CD, 0, 3) %in% diabetes_icd_10_codes,
        if_else(
          LINE_ICD_DGNS_VRSN_CD == 9,
          substr(LINE_ICD_DGNS_CD, 0, 3) %in% diabetes_icd_9_codes,NA)),
      
      is_depression= if_else(
        LINE_ICD_DGNS_VRSN_CD == 0,
        substr(LINE_ICD_DGNS_CD, 0, 3) %in% depression_icd_10_codes,
        if_else(
          LINE_ICD_DGNS_VRSN_CD == 9,
          substr(LINE_ICD_DGNS_CD, 0, 4) %in% depression_icd_9_codes,NA))

      
    ) %>%
    as.data.table()
}

yearly_patient_conditions_carrier=yearly_calculator_patient_conditions(carrier_data_all_years)
head(yearly_patient_conditions_carrier)









summarise_carrier = function(data, time_frame = 365){
  
  data%>%
    group_by(DESY_SORT_KEY,year) %>%
    summarise(
      #tot_allowed_carrier = sum(na.rm = T, LINE_ALOWD_CHRG_AMT),
      
      #office_visit_count = sum(na.rm = T, is_office_visit),
      
      #office_visit_cost_carrier = sum(na.rm = T, LINE_ALOWD_CHRG_AMT * is_office_visit),
      
      distinct_clinicians = length(unique(PRF_PHYSN_NPI)),
      
      distinct_primary_care_physicians = length(.[is_by_primary_care_physician, unique(PRF_PHYSN_NPI)]),

      hypertension = sum(is_hypertension, na.rm = T) > 0,
      
      arthritis = sum(is_arthritis, na.rm = T) > 0,
      
      IHD = sum(is_IHD, na.rm = T) > 0,
      
      diabetes = sum(is_diabetes, na.rm = T) > 0,
  
      depression = sum(is_depression, na.rm = T) > 0,
      
      icd_9_pure = ifelse(prod(LINE_ICD_DGNS_VRSN_CD, na.rm = T) == 0, F, T),
      
      icd_10_pure = ifelse(sum(LINE_ICD_DGNS_VRSN_CD, na.rm = T) == 0, T, F),
      
    ) %>%
    as.data.table()
}


summary_patient_by_year = summarise_carrier(yearly_patient_conditions_carrier)
head(summary_patient_by_year)









add_patient_characteristics = function(mbsf_data,summary_data){
  require(dtplyr)
  require(lubridate)
  require(tidyverse)
  data = left_join(summary_data,mbsf_data,by=c("DESY_SORT_KEY","year")) %>% as.data.frame()
  
  data %>%
  mutate(
    year_of_death=substr(DATE_OF_DEATH,0,4)
  )%>%
  as.data.table()
}

summary_with_patient_characteristics=add_patient_characteristics(mbsf_data,summary_patient_by_year)
head(summary_with_patient_characteristics)









#adding most common physicians
add_patient_NPI=function(data, summary_data, time_frame = 365){

  comorbidity_and_phys_data =
    inner_join(data, summary_data[, c("DESY_SORT_KEY","year",
                                "icd_9_pure",
                                "icd_10_pure")], by = c("DESY_SORT_KEY","year")) %>%
    as.data.table()
  
  patient_NPI_count_finder = function(data) {
    result = data %>%
      mutate(is_office_visit = HCPCS_CD %in% office_visit_codes)%>%
      group_by(DESY_SORT_KEY, year, PRF_PHYSN_NPI) %>%
      summarise(n = sum(is_office_visit,na.rm=T)) %>%
      filter(n>0)%>%
      arrange(.by_group = T, desc(n))
  }
  
  patient_NPI_counts = patient_NPI_count_finder(comorbidity_and_phys_data)
  
  patient_NPI_counts = left_join(patient_NPI_counts,
                                 distinct(data[, .(PRF_PHYSN_NPI, PRVDR_SPCLTY)]), by ="PRF_PHYSN_NPI")
  
  find_most_common = function(data) {
    data %>%
      group_by(DESY_SORT_KEY,year) %>%
      arrange(.by_group = T, desc(n)) %>%
      slice(1) %>%
      as.data.table()
  }
  
  find_most_common_by_specialty = function(data, specialty_code) {
    data %>%
      filter(PRVDR_SPCLTY %in% specialty_code) %>%
      group_by(DESY_SORT_KEY,year) %>%
      arrange(.by_group = T, desc(n)) %>%
      slice(1) %>%
      as.data.table()
  }
  
  most_common_physician = find_most_common(patient_NPI_counts)
  most_common_primary_care_physician = find_most_common_by_specialty(patient_NPI_counts,
                                                                     specialty_code = c("01", "08", "11", "38"))
  most_common_physician = data.frame(most_common_physician) %>%
    rename_with( ~ paste0("most_common_physician_", .x))
  most_common_primary_care_physician = data.frame(most_common_primary_care_physician) %>%
    rename_with( ~ paste0("most_common_primary_care_physician_", .x))
  
  summary_data = left_join(
    summary_data,
    most_common_physician,
    by = c("DESY_SORT_KEY" = "most_common_physician_DESY_SORT_KEY")
  )
  summary_data = left_join(
    summary_data,
    most_common_primary_care_physician,
    by = c("DESY_SORT_KEY" = "most_common_primary_care_physician_DESY_SORT_KEY")
  )%>%
  as.data.table()

}

summary_with_npi=add_patient_NPI(data = carrier_data_all_years, summary_data = summary_with_patient_characteristics)
head(summary_with_npi)









exclusive_hospital_code_finder = function (data,
                                           threshold=0.05,
                                           integrated_place_of_service_codes = c("19", "22"),
                                           all_place_of_service_codes = c("11", "19", "22")){
  require(dtplyr)
  require(tidyverse)
  
  result = data %>%
  filter(LINE_PLACE_OF_SRVC_CD %in% all_place_of_service_codes)%>%
  group_by(HCPCS_CD) %>%
  summarise(prp_in_facility = nrow(.[LINE_PLACE_OF_SRVC_CD %in% integrated_place_of_service_codes])/n()
           )%>%
  as.data.table
  
  exclusive_codes = result[prp_in_facility<threshold | prp_in_facility>(1-threshold),HCPCS_CD]
  

  return(exclusive_codes)
  
}

exclusive_hospital_codes=exclusive_hospital_code_finder(carrier_data_all_years)









#calculate and add physician integration data
#this only uses visits to see if a physician is integrated or not (codde list)

physician_integration_finder = function(data,
                                        integrated_place_of_service_codes = c("19", "22"),
                                        all_place_of_service_codes = c("11", "19", "22"),
                                        #integration_threshold = 0.5,
                                        office_code_list = c(
                                          "99201",
                                          "99202",
                                          "99203",
                                          "99204",
                                          "99205",
                                          "99211",
                                          "99212",
                                          "99213",
                                          "99214",
                                          "99215"
                                        ),
                                       exclusive_hospital_codes) {
  require(dtplyr)
  require(tidyverse)
  
  #data = subset(data, HCPCS_CD %in% code_list)
  result = data %>%
  mutate(
    is_facility = LINE_PLACE_OF_SRVC_CD %in% integrated_place_of_service_codes,
    is_all = LINE_PLACE_OF_SRVC_CD %in% all_place_of_service_codes,
    is_office_visit = HCPCS_CD %in% office_code_list,
    has_non_exclusive_code = HCPCS_CD %!in% exclusive_hospital_codes
  ) %>%
  group_by(PRF_PHYSN_NPI, year) %>%
  summarise(
    in_facility_visits_count = sum(is_facility*is_office_visit, na.rm = T),
    in_all_visits_count = sum(is_all*is_office_visit, na.rm = T),
    in_facility_non_exclusive_HCPCS_count = sum(is_facility*has_non_exclusive_code, na.rm = T),
    in_all_non_exclusive_HCPCS_count = sum(is_all*has_non_exclusive_code, na.rm = T),
    in_facility_count = sum(is_facility, na.rm = T),
    in_all_count = sum(is_all, na.rm = T)
  ) %>%
  mutate(
    in_facility_visits_prp = in_facility_visits_count / in_all_visits_count,
    in_facility_non_exclusive_HCPCS_prp = in_facility_non_exclusive_HCPCS_count / in_all_non_exclusive_HCPCS_count,    
    in_facility_prp = in_facility_count / in_all_count
  )%>%
  as.data.table()
}

physician_integration_stats = physician_integration_finder(carrier_data_all_years,exclusive_hospital_codes=exclusive_hospital_codes)
tail(physician_integration_stats)









#rename columns
rename_last = function(data, how_many, new_names) {
  total_cols = ncol(data)
  setnames(data, (total_cols - how_many + 1):(total_cols), new_names)
}
add_integration_status=function(data, physician_integration_stats){
  
  data_selected=data[,c("DESY_SORT_KEY",
               "year",
               "most_common_physician_PRF_PHYSN_NPI",
               "most_common_primary_care_physician_PRF_PHYSN_NPI"
              )]
  
  most_common_physician = left_join(
    data_selected,
    physician_integration_stats,
    by = c(
      "most_common_physician_PRF_PHYSN_NPI" = "PRF_PHYSN_NPI", "year" = "year")
  ) %>% as.data.table()
  
  most_common_physician=most_common_physician[,-c("most_common_primary_care_physician_PRF_PHYSN_NPI")]
  
  rename_last(
    most_common_physician,
    ncol(physician_integration_stats)-2,
    paste("most_common_physician_",colnames(physician_integration_stats)[3:ncol(physician_integration_stats)],sep="")
    )
    
    
  most_common_primary_care = left_join(
    data_selected,
    physician_integration_stats,
    by = c(
      "most_common_primary_care_physician_PRF_PHYSN_NPI" = "PRF_PHYSN_NPI", "year" = "year")
  ) %>% as.data.table()
  
  most_common_primary_care=most_common_primary_care[,-c("most_common_physician_PRF_PHYSN_NPI")]  

  rename_last(
    most_common_primary_care,
    ncol(physician_integration_stats)-2,
    paste("most_common_primary_care_physician_",colnames(physician_integration_stats)[3:ncol(physician_integration_stats)],sep="")
    )
  
  physician_data=
  full_join(most_common_physician[,-("most_common_physician_PRF_PHYSN_NPI")],
            most_common_primary_care[,-("most_common_primary_care_physician_PRF_PHYSN_NPI")],
            by=c("DESY_SORT_KEY","year")
           )
  result=full_join(data,
                   physician_data,
                   by=c("DESY_SORT_KEY","year")
           )%>%
  as.data.table
  
  
  return(result)
}

summary_with_physician_integration_stats=add_integration_status(data = summary_with_npi,
                                                                physician_integration_stats = physician_integration_stats)




head(summary_with_physician_integration_stats)


write.fst(summary_with_physician_integration_stats,"calculation_results.fst")