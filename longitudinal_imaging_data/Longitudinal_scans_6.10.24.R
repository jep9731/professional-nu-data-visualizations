# Import libraries --------------------------------------------------------
library(tidyverse)
library(readxl)
library(writexl)

# Import files ------------------------------------------------------------
mastersheet <- read_excel("/Volumes/fsmresfiles/CNADC/Imaging_Core/imaging_core_docs/Recruitment/IC Goals/Y4/imaging_diagnosis_mastersheet_2024-06-10.xlsx")

# Separating databases ---------------------------------------------------------
## MRI
mastersheet_mri <- mastersheet[which(mastersheet$modality == "mri"), ]

## Amyloid PET
mastersheet_amyloid_pet <- mastersheet[which(mastersheet$modality == "amyloid"), ]

## Tau PET
mastersheet_tau_pet <- mastersheet[which(mastersheet$modality == "tau"), ]

# Creating binary variable ------------------------------------------------
## MRI
mastersheet_mri <- mastersheet_mri %>%
  mutate(age_at_scan = round(time_length(difftime(as.Date(scan_date), as.Date(dob_stub)), "years"), 
                             digits = 2)) %>% # Creates new column that calculates age at scan with 2 decimal places
  mutate(age_dummy = ifelse(age_at_scan >= 80.00, 1, 0)) # Creates new column with dummy variables (1 = 80+ age, 0 = everyone else)
print(mastersheet_mri[53:54]) # Verify it was successful

## Amyloid PET
mastersheet_amyloid_pet <- mastersheet_amyloid_pet %>%
  mutate(age_at_scan = round(time_length(difftime(as.Date(scan_date),as.Date(dob_stub)), "years"),
                             digits = 2)) %>% # Creates new column that calculates age at scan with 2 decimal places
  mutate(age_dummy = ifelse(age_at_scan >= 80.00, 1, 0)) # Creates new column with dummy variables (1 = 80+ age, 0 = everyone else)
print(mastersheet_amyloid_pet[53:54]) # Verify it was successful

## Tau PET
mastersheet_tau_pet <- mastersheet_tau_pet %>%
  mutate(age_at_scan = round(time_length(difftime(as.Date(scan_date),as.Date(dob_stub)), "years"),
                             digits = 2)) %>% # Creates new column that calculates age at scan with 2 decimal places
  mutate(age_dummy = ifelse(age_at_scan >= 80.00, 1, 0)) # Creates new column with dummy variables (1 = 80+ age, 0 = everyone else)
print(mastersheet_tau_pet[53:54]) # Verify it was successful

# Keeping most recent scans ------------------------------------------------------
## MRI
mastersheet_mri_updated <- mastersheet_mri[order(mastersheet_mri$ptid, 
                                                 mastersheet_mri$scan_date, 
                                                 decreasing = FALSE), ] # Sort ptids and MRI scan dates in deceasing order
print(mastersheet_mri_updated)

mastersheet_mri_most_recent <- mastersheet_mri_updated[!duplicated(mastersheet_mri_updated$ptid, 
                                                                   fromLast = TRUE), ] # Keeps most recent instance of duplicated rows

mastersheet_mri_most_recent <- mastersheet_mri_most_recent %>%
  select(global_id:cv.current_diagnosis, 
         age_at_scan, 
         age_dummy) # Gets rid of unnecessary information
print(mastersheet_mri_most_recent)

## Amyloid PET
mastersheet_amyloid_pet_updated <- mastersheet_amyloid_pet[order(mastersheet_amyloid_pet$ptid, 
                                                                 mastersheet_amyloid_pet$scan_date, 
                                                                 decreasing = FALSE), ] # Sort ptids and aPET scan dates in decreasing order
print(mastersheet_amyloid_pet_updated)

mastersheet_amyloid_pet_most_recent <- mastersheet_amyloid_pet_updated[!duplicated(mastersheet_amyloid_pet_updated$ptid, 
                                                                                   fromLast = TRUE), ] # Keeps most recent instance of duplicated rows

mastersheet_amyloid_pet_most_recent <- mastersheet_amyloid_pet_most_recent %>%
  select(global_id:cv.current_diagnosis, 
         age_at_scan, 
         age_dummy) # Gets rid of unnecessary information
print(mastersheet_amyloid_pet_most_recent)

## Tau PET
mastersheet_tau_pet_updated <- mastersheet_tau_pet[order(mastersheet_tau_pet$ptid, 
                                                         mastersheet_tau_pet$scan_date, 
                                                         decreasing = FALSE), ] # Sort ptids and tPET scan dates in decreasing order
print(mastersheet_tau_pet_updated)

mastersheet_tau_pet_most_recent <- mastersheet_tau_pet_updated[!duplicated(mastersheet_tau_pet_updated$ptid, 
                                                                           fromLast = TRUE), ] # Keeps most recent instance of duplicated rows

mastersheet_tau_pet_most_recent <- mastersheet_tau_pet_most_recent %>%
  select(global_id:cv.current_diagnosis, 
         age_at_scan, 
         age_dummy) # Gets rid of unnecessary information
print(mastersheet_tau_pet_most_recent)

# Determine time difference -----------------------------------------------
## MRI
mastersheet_mri_most_recent_years <- mastersheet_mri_most_recent %>%
  mutate(date_diff = round(time_length(difftime(as.Date(Sys.Date()), as.Date(scan_date)), "years"), 
                           digits = 2) # Calculates the time difference between now & date of scan
  )
print(mastersheet_mri_most_recent_years)

## Aymloid PET
mastersheet_amyloid_pet_most_recent_years <- mastersheet_amyloid_pet_most_recent %>%
  mutate(date_diff = round(time_length(difftime(as.Date(Sys.Date()), as.Date(scan_date)), "years"), 
                           digits = 2) # Calculates the time difference between now & date of scan
  )
print(mastersheet_amyloid_pet_most_recent_years)

## Tau PET
mastersheet_tau_pet_most_recent_years <- mastersheet_tau_pet_most_recent %>%
  mutate(date_diff = round(time_length(difftime(as.Date(Sys.Date()), as.Date(scan_date)), "years"), 
                           digits = 2) # Calculates the time difference between now & date of scan
  )
print(mastersheet_tau_pet_most_recent_years)

# Clean datasets ----------------------------------------------------------
## MRI
mastersheet_mri_final <- mastersheet_mri_most_recent_years %>%
  mutate(current_age = round(time_length(difftime(as.Date(Sys.Date()), as.Date(dob_stub)), "years"), 
                             digits = 2)) %>% # Calculates current age
  filter(cv.core_participant_status == "Actively Followed") # Filtering by active participants
print(mastersheet_mri_final)

## Amyloid PET
mastersheet_amyloid_pet_final <- mastersheet_amyloid_pet_most_recent_years %>% 
  mutate(current_age = round(time_length(difftime(as.Date(Sys.Date()), as.Date(dob_stub)), "years"), 
                             digits = 2)) %>% # Calculates current age
  filter(cv.core_participant_status == "Actively Followed") # Filtering by active participants
print(mastersheet_amyloid_pet_final)

## Tau PET
mastersheet_tau_pet_final <- mastersheet_tau_pet_most_recent_years %>%
  mutate(current_age = round(time_length(difftime(as.Date(Sys.Date()), as.Date(dob_stub)), "years"), 
                             digits = 2)) %>% # Calculates current age
  filter(cv.core_participant_status == "Actively Followed") # Filtering by active participants
print(mastersheet_tau_pet_final)

# Graphs -------------------------------------------------------
## MRI - Total
ggplot(data = mastersheet_mri_final, aes(x = factor(date_diff))) +
  geom_bar(stat="count", width=0.7, fill="steelblue") +
  geom_text(aes(label = after_stat(count)), stat = "count", colour = "black", vjust = -0.3, size = 5) +
  labs(title = "Number of Years Since Most Recent MRI Scan",
       x = "Years Since Most Recent Scan",
       y = "Count") +
  theme_bw() +
  theme(plot.title = element_text(size=16, face="bold", hjust=0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))

## MRI - Study Affiliation
unique_study_affiliation_mri <- sort(unique(mastersheet_mri_final$study_affiliation))
print(unique_study_affiliation_mri)

ggplot(data=mastersheet_mri_final, aes(x=factor(date_diff), fill=study_affiliation)) +
  geom_bar(width=0.7, color = "black") +
  coord_flip() +
  geom_text(aes(label = after_stat(count)), stat = "count", color = "black", position = position_stack(vjust = 0.5), size = 5) +
  labs(title = "Number of Years Since Most Recent MRI Scan By Study Affiliation",
       x = "Years Since Most Recent Scan",
       y = "Count") +
  theme_bw() +
  scale_fill_discrete(name = "Study Affiliation", unique_study_affiliation_mri) +
  theme(legend.text = element_text(size = 12), 
        legend.title = element_text(size = 12),
        legend.position = "right",
        legend.background = element_rect(fill=NA, linewidth =0.5, linetype="solid"),
        plot.title = element_text(size=16, face="bold", hjust=0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))

## MRI - DX
unique_dx_mri <- sort(unique(mastersheet_mri_final$diagnosis_time_of_visit))
print(unique_dx_mri)
unique_dx_mri[[1]] <- "AD" # Changing DAT to AD for updated nomenclature
print(unique_dx_mri)

ggplot(data=mastersheet_mri_final, aes(x=factor(date_diff), fill=diagnosis_time_of_visit)) +
  geom_bar(width=0.7, color = "black") +
  coord_flip() +
  geom_text(aes(label = after_stat(count)), stat = "count", color = "black", position = position_stack(vjust = 0.5), size = 5) +
  labs(title = "Number of Years Since Most Recent MRI Scan By Diagnosis At Scan",
       x = "Years Since Most Recent Scan",
       y = "Count") +
  theme_bw() +
  scale_fill_discrete(name = "Diagnosis At Scan", labels = c(unique_dx_mri)) +
  theme(legend.text = element_text(size = 12), 
        legend.title = element_text(size = 12),
        legend.position = c(.92, .85),
        legend.background = element_rect(fill=NA, linewidth=0.5, linetype="solid"),
        plot.title = element_text(size=16, face="bold", hjust=0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))

## Amyloid PET - Total
ggplot(data = mastersheet_amyloid_pet_final, aes(x = factor(date_diff))) +
  geom_bar(stat="count", width=0.7, fill="steelblue") +
  geom_text(aes(label = after_stat(count)), stat = "count", colour = "black", vjust = -0.3, size = 5) +
  labs(title = "Number of Years Since Most Recent Amyloid PET Scan",
       x = "Years Since Most Recent Scan",
       y = "Count") +
  theme_bw() +
  theme(plot.title = element_text(size=16, face="bold", hjust=0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))

## Amyloid PET - Study Affiliation
unique_study_affiliation_amyloid_pet <- sort(unique(mastersheet_amyloid_pet_final$study_affiliation))
print(unique_study_affiliation_amyloid_pet)

ggplot(data = mastersheet_amyloid_pet_final, aes(x=factor(date_diff), fill=study_affiliation)) +
  geom_bar(width=0.7, color = "black") +
  coord_flip() +
  geom_text(aes(label = after_stat(count)), stat = "count", color = "black", position = position_stack(vjust = 0.5), size = 5) +
  labs(title = "Number of Years Since Most Recent Amyloid PET Scan By Study Affiliation",
       x = "Years Since Most Recent Scan",
       y = "Count") +
  theme_bw() +
  scale_fill_discrete(name = "Study Affiliation", labels = unique_study_affiliation_amyloid_pet) +
  theme(legend.text = element_text(size = 12), 
        legend.title = element_text(size = 12),
        legend.position = c(.92, .83),
        legend.background = element_rect(fill=NA, linewidth=0.5, linetype="solid"),
        plot.title = element_text(size=16, face="bold", hjust=0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))

## Amyloid PET - DX
unique_dx_amyloid_pet <- sort(unique(mastersheet_amyloid_pet_final$diagnosis_time_of_visit))
print(unique_dx_amyloid_pet)
unique_dx_amyloid_pet[[1]] <- "AD" # Changing DAT to AD for updated nomenclature
print(unique_dx_amyloid_pet)

ggplot(data = mastersheet_amyloid_pet_final, aes(x=factor(date_diff), fill=diagnosis_time_of_visit)) +
  geom_bar(width=0.7, color = "black") +
  coord_flip() +
  geom_text(aes(label = after_stat(count)), stat = "count", color = "black", position = position_stack(vjust = 0.5), size = 5) +
  labs(title = "Number of Years Since Most Recent Amyloid PET Scan By Diagnosis At Scan",
       x = "Years Since Most Recent Scan",
       y = "Count") +
  theme_bw() +
  scale_fill_discrete(name = "Diagnosis At Scan", labels = unique_dx_amyloid_pet) +
  theme(legend.text = element_text(size = 12), 
        legend.title = element_text(size = 12),
        legend.position = c(.92, .83),
        legend.background = element_rect(fill=NA, linewidth=0.5, linetype="solid"),
        plot.title = element_text(size=16, face="bold", hjust=0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))

## Tau PET - Total
ggplot(data = mastersheet_tau_pet_final, aes(x = factor(date_diff))) +
  geom_bar(stat="count", width=0.7, fill="steelblue") +
  geom_text(aes(label = after_stat(count)), stat = "count", colour = "black", vjust = -0.3, size = 5) +
  labs(title = "Number of Years Since Most Recent Tau PET Scan",
       x = "Years Since Most Recent Scan",
       y = "Count") +
  theme_bw() +
  theme(plot.title = element_text(size=16, face="bold", hjust=0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))

## Tau PET - Study Affiliation
unique_study_affiliation_tau_pet <- sort(unique(mastersheet_tau_pet_final$study_affiliation))
print(unique_study_affiliation_tau_pet)

ggplot(data = mastersheet_tau_pet_final, aes(x=factor(date_diff), fill=study_affiliation)) +
  geom_bar(width=0.7, color = "black") +
  coord_flip() +
  geom_text(aes(label = after_stat(count)), stat = "count", color = "black", position = position_stack(vjust = 0.5), size = 5) +
  labs(title = "Number of Years Since Most Recent Tau PET Scan By Study Affiliation",
       x = "Years Since Most Recent Scan",
       y = "Count") +
  theme_bw() +
  scale_fill_discrete(name = "Study Affiliation", labels = unique_study_affiliation_tau_pet) +
  theme(legend.text = element_text(size = 12), 
        legend.title = element_text(size = 12),
        legend.position = c(.92, .86),
        legend.background = element_rect(fill=NA, linewidth=0.5, linetype="solid"),
        plot.title = element_text(size=16, face="bold", hjust=0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))

## Tau PET - DX
unique_dx_tau_pet <- sort(unique(mastersheet_tau_pet_final$diagnosis_time_of_visit))
print(unique_dx_tau_pet)
unique_dx_tau_pet[[1]] <- "AD" # Changing DAT to AD for updated nomenclature
print(unique_dx_tau_pet)

ggplot(data = mastersheet_tau_pet_final, aes(x=factor(date_diff), fill=diagnosis_time_of_visit)) +
  geom_bar(width=0.7, color = "black") +
  coord_flip() +
  geom_text(aes(label = after_stat(count)), stat = "count", color = "black", position = position_stack(vjust = 0.5), size = 5) +
  labs(title = "Number of Years Since Most Recent Tau PET Scan By Diagnosis At Scan",
       x = "Years Since Most Recent Scan",
       y = "Count") +
  theme_bw() +
  scale_fill_discrete(name = "Diagnosis At Scan", labels = unique_dx_tau_pet) +
  theme(legend.text = element_text(size = 12), 
        legend.title = element_text(size = 12),
        legend.position = c(.92, .86),
        legend.background = element_rect(fill=NA, linewidth=0.5, linetype="solid"),
        plot.title = element_text(size=16, face="bold", hjust=0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))

# PPA -------------------------------------------------------------
## MRI
mastersheet_ppa_mri <- mastersheet_mri_final %>% 
  filter(study_affiliation == "PPA",
         diagnosis_time_of_visit == "PPA",
         date_diff >= 5) # Filtering by study affiliation and date diff of over 5
print(mastersheet_ppa_mri)

mastersheet_ppa_mri_total <- data.frame(number_of_ppa_mri_scanned_5_or_more_yrs_ago=length(mastersheet_ppa_mri$global_id)) # Creates table with total number of PPA scanned 5+ years ago
print(mastersheet_ppa_mri_total)

mastersheet_ppa_mri_total_id <- data.frame(global_id = mastersheet_ppa_mri$global_id, 
                                       ptid = mastersheet_ppa_mri$ptid, 
                                       subject_id = mastersheet_ppa_mri$case_num_ppa,
                                       scan_date = mastersheet_ppa_mri$scan_date,
                                       years_since_most_recent_scan = mastersheet_ppa_mri$date_diff,
                                       modality_mri = c("mri")) # Creates table with IDs, scan date, and date diff
print(mastersheet_ppa_mri_total_id)

## aPET
mastersheet_ppa_apet <- mastersheet_amyloid_pet_final %>%
  filter(study_affiliation == "PPA",
         diagnosis_time_of_visit == "PPA",
         date_diff >= 5) # Filtering by study affiliation and date diff of over 5
print(mastersheet_ppa_apet)

mastersheet_ppa_apet_total <- data.frame(number_of_ppa_apet_scanned_5_or_more_yrs_ago=length(mastersheet_ppa_apet$global_id)) # Creates table with total number of PPA scanned 5+ years ago
print(mastersheet_ppa_apet_total)

mastersheet_ppa_apet_total_id <- data.frame(global_id = mastersheet_ppa_apet$global_id, 
                                           ptid = mastersheet_ppa_apet$ptid,
                                           subject_id = mastersheet_ppa_apet$case_num_ppa,
                                           scan_date = mastersheet_ppa_apet$scan_date,
                                           years_since_most_recent_scan = mastersheet_ppa_apet$date_diff,
                                           modality_amyloid = c("amyloid")) # Creates table with IDs, scan date, and date diff
print(mastersheet_ppa_apet_total_id)

## tPET
mastersheet_ppa_tpet <- mastersheet_tau_pet_final %>%
  filter(study_affiliation == "PPA",
         diagnosis_time_of_visit == "PPA",
         date_diff >= 5) # Filtering by study affiliation and date diff of over 5
print(mastersheet_ppa_tpet)

mastersheet_ppa_tpet_total <- data.frame(number_of_ppa_tpet_scanned_5_or_more_yrs_ago=length(mastersheet_ppa_tpet$global_id)) # Creates table with total number of PPA scanned 5+ years ago
print(mastersheet_ppa_tpet_total)

mastersheet_ppa_tpet_total_id <- data.frame(global_id = mastersheet_ppa_tpet$global_id, 
                                            ptid = mastersheet_ppa_tpet$ptid,
                                            subject_id = mastersheet_ppa_tpet$case_num_ppa,
                                            scan_date = mastersheet_ppa_tpet$scan_date,
                                            years_since_most_recent_scan = mastersheet_ppa_tpet$date_diff,
                                            modality_tau = c("tau")) # Creates table with IDs, scan date, and date diff
print(mastersheet_ppa_tpet_total_id)

## Final table
table_5 <- data.frame(scanned_5_or_more_yrs_ago=sum(mastersheet_ppa_mri_total, 
                                                    mastersheet_ppa_apet_total, 
                                                    mastersheet_ppa_tpet_total)) # Creates combined table
print(table_5)

scanned_ppa_id_list <- list(mastersheet_ppa_mri_total_id, 
                            mastersheet_ppa_apet_total_id, 
                            mastersheet_ppa_tpet_total_id) # Creates a list of ID tables

scanned_ppa_id <- scanned_ppa_id_list %>% 
  reduce(full_join, by=c("global_id", "subject_id", "ptid", "scan_date", "years_since_most_recent_scan")) # Joins the list of ID tables
print(scanned_ppa_id)

# Elderly NC (SA & non-SA) --------------------------------------------------------------
## MRI
mastersheet_elderly_control_mri <- mastersheet_mri_final %>% 
  filter(cv.current_diagnosis == "Normal Control",
         age_dummy == 1,
         date_diff >= 5) # Filtering by current diagnosis, current age of 80+, and date diff of over 5
print(mastersheet_elderly_control_mri)

mastersheet_elderly_control_mri_total <- data.frame(number_of_elderly_nc_mri_scanned_5_or_more_yrs_ago=length(mastersheet_elderly_control_mri$global_id)) # Creates table with total number of SA scanned 5+ years ago
print(mastersheet_elderly_control_mri_total)

mastersheet_elderly_control_mri_total_id <- data.frame(global_id = mastersheet_elderly_control_mri$global_id,
                                          ptid = mastersheet_elderly_control_mri$ptid,
                                          study_affiliation = mastersheet_elderly_control_mri$study_affiliation,
                                          scan_date = mastersheet_elderly_control_mri$scan_date,
                                          age_at_scan = mastersheet_elderly_control_mri$age_at_scan,
                                          current_age = mastersheet_elderly_control_mri$current_age,
                                          years_since_most_recent_scan = mastersheet_elderly_control_mri$date_diff,
                                          modality_mri = c("mri")) # Creates table with IDs, scan date, and date diff
print(mastersheet_elderly_control_mri_total_id)

## aPET
mastersheet_elderly_control_apet <- mastersheet_amyloid_pet_final %>% 
  filter(cv.current_diagnosis == "Normal Control",
         age_dummy == 1, 
         date_diff >= 5) # Filtering by current diagnosis, current age of 80+, and date diff of over 5
print(mastersheet_elderly_control_apet)

mastersheet_elderly_control_apet_total <- data.frame(number_of_elderly_nc_apet_scanned_5_or_more_yrs_ago=length(mastersheet_elderly_control_apet$global_id)) # Creates table with total number of SA scanned 5+ years ago
print(mastersheet_elderly_control_apet_total)

mastersheet_elderly_control_apet_total_id <- data.frame(global_id = mastersheet_elderly_control_apet$global_id,
                                           ptid = mastersheet_elderly_control_apet$ptid,
                                           study_affiliation = mastersheet_elderly_control_apet$study_affiliation,
                                           scan_date = mastersheet_elderly_control_apet$scan_date,
                                           age_at_scan = mastersheet_elderly_control_apet$age_at_scan,
                                           current_age = mastersheet_elderly_control_apet$current_age,
                                           years_since_most_recent_scan = mastersheet_elderly_control_apet$date_diff,
                                           modality_amyloid = c("amyloid")) # Creates table with IDs, scan date, and date diff
print(mastersheet_elderly_control_apet_total_id)

## tPET
mastersheet_elderly_control_tpet <- mastersheet_tau_pet_final %>% 
  filter(cv.current_diagnosis == "Normal Control",
         age_dummy == 1, 
         date_diff >= 5) # Filtering by diagnosis, current age 80+, and date diff of over 5
print(mastersheet_elderly_control_tpet)

mastersheet_elderly_control_tpet_total <- data.frame(number_of_elderly_nc_tpet_scanned_5_or_more_yrs_ago=length(mastersheet_elderly_control_tpet$global_id)) # Creates table with total number of SA scanned 5+ years ago
print(mastersheet_elderly_control_tpet_total)

## Final table
table_6 <- data.frame(scanned_5_or_more_yrs_ago=sum(mastersheet_elderly_control_mri_total, 
                                                    mastersheet_elderly_control_apet_total, 
                                                    mastersheet_elderly_control_tpet_total)) # Creates combined table
print(table_6)

scanned_elderly_nc_id_list <- list(mastersheet_elderly_control_mri_total_id, 
                                   mastersheet_elderly_control_apet_total_id) # Creates a list of ID tables

scanned_elderly_nc_id <- scanned_elderly_nc_id_list %>% 
  reduce(full_join, by=c("global_id","ptid", "study_affiliation", "scan_date", "age_at_scan", "current_age", "years_since_most_recent_scan")) # Joins the list of ID tables
print(scanned_elderly_nc_id)

# Normal Controls (Middle Age) ----------------------------------------
## MRI
mastersheet_middle_age_nc_mri <- mastersheet_mri_final %>%
  filter(cv.current_diagnosis == "Normal Control",
         age_dummy == 0,
         current_age < 80,
         date_diff >= 5) # Filtering by diagnosis, age less than 80, and date diff of over 5
print(mastersheet_middle_age_nc_mri)

mastersheet_middle_age_nc_mri_total <- data.frame(number_of_middle_age_nc_mri_scanned_5_or_more_yrs_ago=length(mastersheet_middle_age_nc_mri$global_id)) # Creates table with total number of middle age NC scanned 5+ years ago
print(mastersheet_middle_age_nc_mri_total)

mastersheet_middle_age_nc_mri_total_id <- data.frame(global_id = mastersheet_middle_age_nc_mri$global_id,
                                                     ptid = mastersheet_middle_age_nc_mri$ptid,
                                                     study_affiliation = mastersheet_middle_age_nc_mri$study_affiliation,
                                                     scan_date = mastersheet_middle_age_nc_mri$scan_date,
                                                     age_at_scan = mastersheet_middle_age_nc_mri$age_at_scan,
                                                     current_age = mastersheet_middle_age_nc_mri$current_age,
                                                     years_since_most_recent_scan = mastersheet_middle_age_nc_mri$date_diff,
                                                     modality_mri = c("mri")) # Creates table with IDs, scan date, and date diff
print(mastersheet_middle_age_nc_mri_total_id)

## aPET
mastersheet_middle_age_nc_apet <- mastersheet_amyloid_pet_final %>%
  filter(cv.current_diagnosis == "Normal Control",
         age_dummy == 0,
         current_age < 80,
         date_diff >= 5) # Filtering by diagnosis, age less than 80, and date diff of over 5
print(mastersheet_middle_age_nc_apet)

mastersheet_middle_age_nc_apet_total <- data.frame(number_of_middle_age_nc_apet_scanned_5_or_more_yrs_ago=length(mastersheet_middle_age_nc_apet$global_id)) # Creates table with total number of middle age NC scanned 5+ years ago
print(mastersheet_middle_age_nc_apet_total)

mastersheet_middle_age_nc_apet_total_id <- data.frame(global_id = mastersheet_middle_age_nc_apet$global_id,
                                                     ptid = mastersheet_middle_age_nc_apet$ptid,
                                                     study_affiliation = mastersheet_middle_age_nc_apet$study_affiliation,
                                                     scan_date = mastersheet_middle_age_nc_apet$scan_date,
                                                     age_at_scan = mastersheet_middle_age_nc_apet$age_at_scan,
                                                     current_age = mastersheet_middle_age_nc_apet$current_age,
                                                     years_since_most_recent_scan = mastersheet_middle_age_nc_apet$date_diff,
                                                     modality_amyloid = c("amyloid")) # Creates table with IDs, scan date, and date diff
print(mastersheet_middle_age_nc_apet_total_id)

## tPET
mastersheet_middle_age_nc_tpet <- mastersheet_tau_pet_final %>%
  filter(cv.current_diagnosis == "Normal Control",
         age_dummy == 0,
         current_age < 80,
         date_diff >= 5) # Filtering by diagnosis, age less than 80, and date diff of over 5
print(mastersheet_middle_age_nc_tpet)

mastersheet_middle_age_nc_tpet_total <- data.frame(number_of_middle_age_nc_tpet_scanned_5_or_more_yrs_ago=length(mastersheet_middle_age_nc_tpet$global_id)) # Creates table with total number of middle age NC scanned 5+ years ago
print(mastersheet_middle_age_nc_tpet_total)

mastersheet_middle_age_nc_tpet_total_id <- data.frame(global_id = mastersheet_middle_age_nc_tpet$global_id,
                                                      ptid = mastersheet_middle_age_nc_tpet$ptid,
                                                      study_affiliation = mastersheet_middle_age_nc_tpet$study_affiliation,
                                                      scan_date = mastersheet_middle_age_nc_tpet$scan_date,
                                                      age_at_scan = mastersheet_middle_age_nc_tpet$age_at_scan,
                                                      current_age = mastersheet_middle_age_nc_tpet$current_age,
                                                      years_since_most_recent_scan = mastersheet_middle_age_nc_tpet$date_diff,
                                                      modality_tau = c("tau")) # Creates table with IDs, scan date, and date diff
print(mastersheet_middle_age_nc_tpet_total_id)

## Final table
table_7 <- data.frame(scanned_5_or_more_yrs_ago=sum(mastersheet_middle_age_nc_mri_total, 
                                                    mastersheet_middle_age_nc_apet_total, 
                                                    mastersheet_middle_age_nc_tpet_total)) # Creates combined table
print(table_7)

scanned_middle_age_nc_id_list <- list(mastersheet_middle_age_nc_mri_total_id, 
                                      mastersheet_middle_age_nc_apet_total_id, 
                                      mastersheet_middle_age_nc_tpet_total_id) # Creates a list of ID tables

scanned_middle_age_nc_id <- scanned_middle_age_nc_id_list %>% 
  reduce(full_join, by=c("global_id", "ptid", "study_affiliation", "scan_date", "age_at_scan", "current_age","years_since_most_recent_scan")) # Joins the list of ID tables
print(scanned_middle_age_nc_id)

# Combined data table -----------------------------------------------------
combined_tables_2 <- rbind(table_5, table_6, table_7) # Combines tables

combined_tables_2 <- combined_tables_2 %>%
  mutate(Diagnosis = c("PPA", "Elderly NC", "Middle Age NC"), .before = scanned_5_or_more_yrs_ago) # Edit table to distinguish between diagnosis groups
print(combined_tables_2)

# Import data table from IC_goals_Y4.R script and merge data tables from this script
combined_tables_1 <- read_excel("/Volumes/fsmresfiles/CNADC/Imaging_Core/imaging_core_docs/Recruitment/IC Goals/Y4/tables/never_scanned.xlsx")
combined <- merge(combined_tables_1, combined_tables_2, by = "Diagnosis")
print(combined)

# Export data table to excel ----------------------------------------------
write_xlsx(combined_tables_2, "/Volumes/fsmresfiles/CNADC/Imaging_Core/imaging_core_docs/Recruitment/IC Goals/Y4/tables/scanned_over_5_or_more_yrs_ago_table.xlsx")
write_xlsx(combined, "/Volumes/fsmresfiles/CNADC/Imaging_Core/imaging_core_docs/Recruitment/IC Goals/Y4/tables/combined_table.xlsx")
write_xlsx(scanned_ppa_id, "/Volumes/fsmresfiles/CNADC/Imaging_Core/imaging_core_docs/Recruitment/IC Goals/Y4/tables/ppa_scanned_over_5_or_more_yrs.xlsx")
write_xlsx(scanned_elderly_nc_id, "/Volumes/fsmresfiles/CNADC/Imaging_Core/imaging_core_docs/Recruitment/IC Goals/Y4/tables/elderly_age_nc_scanned_over_5_or_more_yrs.xlsx")
write_xlsx(scanned_middle_age_nc_id, "/Volumes/fsmresfiles/CNADC/Imaging_Core/imaging_core_docs/Recruitment/IC Goals/Y4/tables/middle_age_nc_scanned_over_5_or_more_yrs.xlsx")
