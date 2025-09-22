# Data import and merge ID questions --------------------------------------

## Set up ------------------------------------------------------------------

library(ridl)
library(labelled)
library(dplyr)
library(tidyverse)
library(forcats)
library(stringr)
library(readr)
library(readxl)
library(writexl)
library(haven)
library(lubridate)
library(fusen)
library(purrr)
library(rlang)
library(DT)
library(robotoolbox)
library(dm)
library(fs)
library(httr)
library(ckanr)

#source(.Renviron)

## Functions

### Define valid refugee documents by country
ref_id <- function(ID_09var, ID_09_vals) {
  case_when(({{ID_09var}} %in% ID_09_vals) ~ "1",
            TRUE ~ "2")
}

### Country column
country_col <- function(df, country_name) {
  df |> mutate(Country = country_name)
}

# SSD ---------------------------------------------------------------------

### RIDL API

# key = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJqdGkiOiJHSE9wd1ZrSWpFdEwyd2hncDNDdlBwX2tWRjVLcU5HV2wxRnBEMURZRXV3IiwiaWF0IjoxNzMwMTI2ODgyLCJleHAiOjE3NjE2NjI4ODJ9.UP14-smHbzSU1aqbMkfxNOCieoFEQEM1B7pphmiGHEM"
# 
# ckanr_setup(url = "https://ridl.unhcr.org/", key)
# 
# res <- package_search(q = "south sudan", fq = "tags:fds")
# 
# res$results
# dataset_id <- res$results[[1]]$id
# dataset_details <- package_show(id = dataset_id)
# 
# resource_url <- dataset_details$resources[[1]]$url
# GET(resource_url, add_headers(Authorization = key), write_disk("ssd_data.zip"))

#unzip("ssd_data.zip", exdir = "data/extracted")

roster_id_ssd <- readRDS("data/extracted/hhroster_ind.rds")
main_id_ssd <- readRDS("data/extracted/hhmain_ind.rds")
ra_id_ssd <- readRDS("data/extracted/rmember_ind.rds")

#### Remove downloaded data
if (file.exists("ssd_data.zip")) {
  file.remove("ssd_data.zip")
}
if (dir.exists("extracted")) {
  unlink("extracted", recursive = TRUE)
}

### Select variables
roster_id_ssd <- roster_id_ssd |> 
  select(`_uuid`, mPosition = rosterposition, HH_03,
         ageYears, sex = HH_02,
         cob = HH_06, citizenship, arrivalYear = HH_00a_year_comb,
         ID_00, ID_01a, ID_01b, ID_02, ID_03, ID_04, 
         ID_05, ID_06, ID_06b, ID_09, popgroup, HH_Educ03, HH_Educ18)

ra_id_ssd <- ra_id_ssd |> 
  select(`_uuid`, mPosition = rosterposition, 
         ID_00RA = ID_00_random, ID_01bRA = ID_01b_random, ID_02RA = ID_02_random, 
         ID_03RA = ID_03_random, ID_04RA = ID_04_random, ID_05RA = ID_05_random, 
         ID_06RA = ID_06_random, ID_06bRA = ID_06b_random, ID_09RA = ID_09_random,
         FI01, FI02, FI10,
         EMP01, EMP02, EMP03, EMP04, EMP05,
         EMP06a, EMP06b, EMP06c,
         EMP09, EMP10_7,
         EMP10, EMP10_4, EMP10_5, EMP10_6, EMP10_6a,
         EMP10_11, EMP10_12, EMP25a, 
         #EMP25aa, 
         EMP29
  ) |>
  mutate(across(everything(), as.character))


roster_id_ssd <- roster_id_ssd |>
  mutate(
    eduLevelCurrent = case_when(
      HH_Educ03 %in% c(1:6, 22:25) ~ 1,
      HH_Educ03 %in% c(7:8, 26) ~ 2,
      HH_Educ03 %in% c(9:12, 27) ~ 3,
      HH_Educ03 %in% c(13:15) ~ 4,
      HH_Educ03 %in% c(16:21) ~ 5),
    eduLevelCurrent = labelled(eduLevelCurrent,
                               labels = c(
                                 "Primary" = 1,
                                 "Lower Secondary" = 2,
                                 "Upper Secondary" = 3,
                                 "Post-secondary, non-tertiary" = 4,
                                 "Tertiary" = 5),
                               label = "Current level of education")
  )

roster_id_ssd <- roster_id_ssd |>
  mutate(
    eduLevelHighest = case_when(
      HH_Educ18 %in% c(21, 50, 79, 143, 173, 288, 312) ~ 3,
      HH_Educ18 %in% c(12:21, 41:50, 70:79, 131:143, 162:173, 278:288, 309:312) | eduLevelCurrent %in% c(4,5) ~ 3,
      HH_Educ18 %in% c(8:21, 37:50, 66:79, 124:143, 152:173, 181:189, 209:218, 268:288, 302:312) | eduLevelCurrent %in% c(3:5)~ 2,
      HH_Educ18 %in% c(6:21, 25:27, 35:50, 64:79, 123:143, 151:173, 180:189, 208:218, 267:288, 300:312) | eduLevelCurrent %in% c(2:5) ~ 1,
    TRUE ~ 0),
    eduLevelHighest = labelled(eduLevelHighest,
                               labels = c(
                                 "Primary" = 1,
                                 "Lower Secondary" = 2,
                                 "Upper Secondary" = 3,
                                 "None" = 0),
                               label = "Highest completed level of education")
  )

main_id_ssd <- main_id_ssd |>
  select(`_uuid`,
         hhsize,
         Intro_03a_NUTS1 = Intro_03a,
         Intro_03b_NUTS2 = Intro_03b,
         Intro_03c_NUTS3 = Intro_03c,
         Intro_08,
         DH_10_NUTS1 = DH_10_state,
         DH_10_NUTS2 = DH_10_province,
         DH_13,
         ends_with("Land10"),
         ends_with("Land14"),
         ends_with("Land15"),
         Food11aa = Food_div1,  
         Food12a = Food_div2,   
         Food13a = Food_div3,   
         Food14a = Food_div4,   
         Food15a = Food_div5,   
         Food16a = Food_div6,   
         Food17a = Food_div7,  
         Food18a = Food_div8,
         ScProtec01,
         stratum = samp_strata) |>
  rename(Land10_1 = Plot1_Land10,
         Land10_2 = Plot2_Land10,
         Land10_3 = Plot3_Land10,
         Land10_4 = Plot4_Land10,
         Land14_1 = Plot1_Land14,
         Land14_2 = Plot2_Land14,
         Land14_3 = Plot3_Land14,
         Land14_4 = Plot4_Land14,
         Land15_1 = Plot1_Land15,
         Land15_2 = Plot2_Land15,
         Land15_3 = Plot3_Land15,
         Land15_4 = Plot4_Land15)

roster_id_ssd <- merge(roster_id_ssd, main_id_ssd, by = "_uuid")

### Add binary variables
roster_id_ssd <- roster_id_ssd |> mutate(ID_09Ref = ref_id(ID_09, c("I", "M")),
                                         mPosition = as.numeric(mPosition),
                                         across(c(sex, cob, ID_00:ID_09), as.character))

ra_id_ssd <- ra_id_ssd |> mutate(ID_09RARef = ref_id(ID_09RA, c("I", "M")),
                                 mPosition = as.numeric(mPosition)) 

### Add labels
roster_id_ssd <- roster_id_ssd |> mutate(stratum = labelled::to_factor(stratum))

### Add country column
roster_id_ssd <- roster_id_ssd |> country_col("South Sudan")

# CMR ---------------------------------------------------------------------

roster_id_cmr <- readRDS("data/cmr/hhroster.rds")
main_id_cmr <- readRDS("data/cmr/main.rds")
ra_id_cmr <- readRDS("data/cmr/RA_adult.rds")

roster_id_cmr <- roster_id_cmr |>
  mutate(ID_00_lbl = case_when(!is.na(ID_00_specify) ~ ID_00_specify,
                               !is.na(ID_00) & is.na(ID_00_specify) ~ "CMR",
                               TRUE ~ NA)) |>
  select(`_uuid` = uuid, mPosition = rosterposition, HH_03,
         ID_00, ID_01a, ID_01a_2, ID_01b, ID_01b_2, ID_02, 
         #ID_02b, 
         ID_03, ID_04,
         ID_05, ID_06, ID_06b, ID_09,
         #DH_10_NUTS1, DH_10_NUTS2,
         arrivalYear = HH_00a_year, 
         Intro_03a_NUTS1, Intro_03b_NUTS2, Intro_03c_NUTS3,
         ageYears = HH_04, sex = HH_02, 
         Intro_03a_NUTS1, Intro_03b_NUTS2, Intro_03c_NUTS3, 
         cob = HH_06, citizenship = ID_00_lbl, HH_Educ02a, HH_Educ03, HH_Educ17, HH_Educ18, HH_Educ17_other)

roster_id_cmr <- roster_id_cmr %>%
  mutate(primary_complete_cur = case_when(
    HH_Educ02a == 1 & HH_Educ03 %in% 7:20 ~ 1, #Currently enrolled in secondary school or higher, therefore primary is completed. 
    TRUE ~ NA_real_))

roster_id_cmr <- roster_id_cmr %>%
  mutate(primary_complete_past = case_when(
    HH_Educ18 %in% 6:20 ~ 1, #Primary education completed in Pakistan
    TRUE ~ NA_real_))

roster_id_cmr <- roster_id_cmr %>%
  mutate(primary_complete = case_when(
    primary_complete_cur == 1 | primary_complete_past == 1 ~ 1, #Primary completed for those currently is school (sec or higer) or those who indicated that they have completed primary school in the past
    TRUE ~ NA_real_))

#Step 2: Create a variable assessing completion of lower secondary school
#For those currently in school 
roster_id_cmr <- roster_id_cmr %>%
  mutate(lowseco_complete_cur = case_when(
    HH_Educ02a == 1 & HH_Educ03 %in% 9:20 ~ 1, #Currently enrolled in higher secondary school or higher, therefore lower secondary is completed. 
    TRUE ~ NA_real_))

roster_id_cmr <- roster_id_cmr %>%
  mutate(lowersec_complete_past = case_when(
    HH_Educ18 %in% 8:20 ~ 1, #Lower Secondary education completed in Pakistan
    TRUE ~ NA_real_))

roster_id_cmr <- roster_id_cmr %>%
  mutate(lowersec_complete = case_when(
    lowersec_complete_past == 1 | lowseco_complete_cur == 1 ~ 1, #Secondary completed for those currently is school (higher sec or higer) or those who indicated that they have completed lower secondary school in the past
    TRUE ~ NA_real_))

#Step 3: Create a variable assessing completion of upper secondary school
#For those currently in school 
roster_id_cmr <- roster_id_cmr %>%
  mutate(upperseco_complete_cur = case_when(HH_Educ03 %in% 14:20 ~ 1, #Currently enrolled in tertiary education, therefore higher secondary is completed. 
                                            TRUE ~ NA_real_))

roster_id_cmr <- roster_id_cmr %>%
  mutate(uppersec_complete_past = case_when(
    HH_Educ18 %in% 13:20 ~ 1, #Upper secondary education completed in Pakistan
    HH_Educ18 %in% 11:12 ~ 1, 
    TRUE ~ NA_real_))

roster_id_cmr <- roster_id_cmr %>%
  mutate(uppersec_complete = case_when(
    upperseco_complete_cur == 1 | uppersec_complete_past == 1 ~ 1, #Secondary completed for those currently is tertiary school or those who indicated that they have completed upper secondary school in the past
    TRUE ~ NA_real_))

roster_id_cmr <- roster_id_cmr |>
  mutate(
    eduLevelHighest = case_when(
      uppersec_complete == 1 ~ 3,
      lowersec_complete == 1 ~ 2,
      primary_complete == 1 ~ 1,
      TRUE ~ 0),
    eduLevelHighest = labelled(eduLevelHighest,
                               labels = c(
                                 "Primary" = 1,
                                 "Lower Secondary" = 2,
                                 "Upper Secondary" = 3,
                                 "None" = 0),
                               label = "Highest completed level of education")
  )  |> select(-c(HH_Educ02a, HH_Educ03, HH_Educ17, HH_Educ18, HH_Educ17_other,
                  primary_complete_cur, primary_complete_past,
                  lowseco_complete_cur, lowersec_complete_past,
                  upperseco_complete_cur, uppersec_complete_past))


ra_id_cmr <- ra_id_cmr |>
  select(`_uuid` = uuid, mPosition = rosterposition,
         ID_00RA, ID_01aRA, ID_01a_2RA, ID_01bRA, ID_01b_2RA, ID_02RA, 
         #ID_02bRA, 
         ID_03RA, 
         ID_04RA,
         DH_13,
         ID_05RA, ID_06RA, ID_06bRA, ID_09RA,
         FI01, FI02, FI10,
         EMP01, EMP02, EMP03, EMP04, EMP05,
         EMP06a, EMP06b, EMP06c,
         EMP09, EMP10_7,
         EMP10, EMP10_4, EMP10_5, EMP10_6, EMP10_6a,
         EMP10_11, EMP10_12, EMP25a, EMP25aa, EMP29
  )

main_id_cmr <- main_id_cmr |>
  select(`_uuid` = uuid,
         hhsize = HHmembersize,
         Intro_08,
         starts_with("Land10"),
         starts_with("Land14"),
         starts_with("Land15"),
         ScProtec01,
         Food11aa,  
         Food12a,   
         Food13a,   
         Food14a,   
         Food15a,   
         Food16a,   
         Food17a,  
         Food18a,
         stratum = samp_strat)

roster_id_cmr <- merge(roster_id_cmr, main_id_cmr, by = "_uuid")

roster_id_cmr <- merge(roster_id_cmr, ra_id_cmr |> select(`_uuid`, DH_13), by = "_uuid")

roster_id_cmr <- roster_id_cmr |>
  mutate(
    ID_09 = as.character(ID_09),
    ID_09 = case_when(
      ID_09 == "1" ~ "A",
      ID_09 == "2" ~ "B",
      ID_09 == "3" ~ "C",
      ID_09 == "4" ~ "D",
      ID_09 == "5" ~ "E",
      ID_09 == "6" ~ "F",
      ID_09 == "7" ~ "G",
      ID_09 == "8" ~ "H",
      ID_09 == "9" ~ "I",
      ID_09 == "10" ~ "J",
      ID_09 == "11" ~ "K",
      ID_09 == "12" ~ "L",
      ID_09 == "13" ~ "M",
      ID_09 == "14" ~ "N",
      ID_09 == "15" ~ "O",
      ID_09 == "16" ~ "P",
      ID_09 == "17" ~ "Q",
      ID_09 == "18" ~ "R",
      ID_09 == "19" ~ "S",
      ID_09 %in% c("96", "98", "99") ~ "DK"
    )
  )

ra_id_cmr <- ra_id_cmr |>
  mutate(
    ID_09RA = as.character(ID_09RA),
    ID_09RA = case_when(
      ID_09RA == "1" ~ "A",
      ID_09RA == "2" ~ "B",
      ID_09RA == "3" ~ "C",
      ID_09RA == "4" ~ "D",
      ID_09RA == "5" ~ "E",
      ID_09RA == "6" ~ "F",
      ID_09RA == "7" ~ "G",
      ID_09RA == "8" ~ "H",
      ID_09RA == "9" ~ "I",
      ID_09RA == "10" ~ "J",
      ID_09RA == "11" ~ "K",
      ID_09RA == "12" ~ "L",
      ID_09RA == "13" ~ "M",
      ID_09RA == "14" ~ "N",
      ID_09RA == "15" ~ "O",
      ID_09RA == "16" ~ "P",
      ID_09RA == "17" ~ "Q",
      ID_09RA == "18" ~ "R",
      ID_09RA == "19" ~ "S",
      ID_09RA %in% c("96", "98", "99") ~ "DK"
    )
  )

### Add binary variables
roster_id_cmr <- roster_id_cmr |> mutate(ID_09Ref = ref_id(ID_09, c("I", "M")))
ra_id_cmr <- ra_id_cmr |> mutate(ID_09RARef = ref_id(ID_09RA, c("I", "M"))) 

### Add country column
roster_id_cmr <- roster_id_cmr |> country_col("Cameroon")

# PAK ---------------------------------------------------------------------

roster_id_pak <- readRDS("data/pak/hhroster.rds")
main_id_pak <- readRDS("data/pak/main.rds")
ra_id_pak <- readRDS("data/pak/RA_adult.rds")

roster_id_pak <- roster_id_pak |>
  mutate(ID_00_lbl = case_when(!is.na(ID_00_specify) ~ ID_00_specify,
                               !is.na(ID_00) & is.na(ID_00_specify) ~ "CMR",
                               TRUE ~ NA)) |>
  select(`_uuid` = uuid, mPosition = rosterposition, HH_03,
         ID_00, ID_01a, ID_01a_2, ID_01b, ID_01b_2, ID_02, 
         #ID_02b, 
         ID_03, ID_04,
         ID_05, ID_06, ID_06b, ID_09,
         #DH_10_NUTS1, DH_10_NUTS2,
         arrivalYear = HH_00a_year, 
         Intro_03a_NUTS1, Intro_03b_NUTS2, Intro_03c_NUTS3,
         ageYears = HH_04, sex = HH_02, 
         Intro_03a_NUTS1, Intro_03b_NUTS2, Intro_03c_NUTS3, 
         cob = HH_06, citizenship = ID_00_lbl, HH_Educ02a, HH_Educ03, HH_Educ17, HH_Educ18, HH_Educ17_other)

roster_id_pak <- roster_id_pak %>%
  mutate(primary_complete_cur = case_when(
    HH_Educ02a == 1 & HH_Educ03 %in% 6:19 ~ 1, #Currently enrolled in secondary school or higher, therefore primary is completed. 
    TRUE ~ NA_real_))

roster_id_pak <- roster_id_pak %>%
  mutate(primary_complete_past = case_when(
    HH_Educ17 == 1 & HH_Educ18 %in% 5:19 ~ 1, #Primary education completed in Pakistan
    HH_Educ17 == 2 & HH_Educ17_other == "AFG" & HH_Educ18 %in% 6:19 ~ 1, #Primary education completed in Afghanistan
    HH_Educ17 == 2 & HH_Educ17_other == "IRN" & HH_Educ18 %in% 6:15 ~ 1, #Primary education completed in Iran
    HH_Educ17 == 2 & (!is.na(HH_Educ17_other) & HH_Educ17_other != "IRN" & HH_Educ17_other != "AFG") & HH_Educ18 %in% 1:12 ~ 1, #Primary education completed somewhere else
    HH_Educ17 %in% c(3,98,99) & HH_Educ18 %in% 1:12 ~ 1, 
    TRUE ~ NA_real_))

roster_id_pak <- roster_id_pak %>%
  mutate(primary_complete = case_when(
    primary_complete_cur == 1 | primary_complete_past == 1 ~ 1, #Primary completed for those currently is school (sec or higer) or those who indicated that they have completed primary school in the past
    TRUE ~ NA_real_))

#Step 2: Create a variable assessing completion of lower secondary school
#For those currently in school 
roster_id_pak <- roster_id_pak %>%
  mutate(lowseco_complete_cur = case_when(
    HH_Educ02a == 1 & HH_Educ03 %in% 9:19 ~ 1, #Currently enrolled in higher secondary school or higher, therefore lower secondary is completed. 
    TRUE ~ NA_real_))

roster_id_pak <- roster_id_pak %>%
  mutate(lowersec_complete_past = case_when(
    HH_Educ17 == 1 & HH_Educ18 %in% 8:19 ~ 1, #Lower Secondary education completed in Pakistan
    HH_Educ17 == 2 & HH_Educ17_other == "AFG" & HH_Educ18 %in% 9:19 ~ 1, #Lower Secondary education completed in Afghanistan
    HH_Educ17 == 2 & HH_Educ17_other == "IRN" & HH_Educ18 %in% 9:15 ~ 1, #Lower Secondary education completed in Iran
    HH_Educ17 == 2 & (!is.na(HH_Educ17_other) & HH_Educ17_other != "IRN" & HH_Educ17_other != "AFG") & HH_Educ18 %in% 4:12 ~ 1, #Lower Secondary education completed somewhere else
    HH_Educ17 %in% c(3,98,99) & HH_Educ18 %in% 4:12 ~ 1, 
    TRUE ~ NA_real_))

roster_id_pak <- roster_id_pak %>%
  mutate(lowersec_complete = case_when(
    lowersec_complete_past == 1 | lowseco_complete_cur == 1 ~ 1, #Secondary completed for those currently is school (higher sec or higer) or those who indicated that they have completed lower secondary school in the past
    TRUE ~ NA_real_))

#Step 3: Create a variable assessing completion of upper secondary school
#For those currently in school 
roster_id_pak <- roster_id_pak %>%
  mutate(upperseco_complete_cur = case_when(
    HH_Educ02a == 1 & HH_Educ03 %in% 13:19 ~ 1, #Currently enrolled in tertiary education, therefore higher secondary is completed. 
    TRUE ~ NA_real_))

roster_id_pak <- roster_id_pak %>%
  mutate(uppersec_complete_past = case_when(
    HH_Educ17 == 1 & HH_Educ18 %in% 12:19 ~ 1, #Upper secondary education completed in Pakistan
    HH_Educ17 == 2 & HH_Educ17_other == "AFG" & HH_Educ18 %in% 12:19 ~ 1, #Upper secondary education completed in Afghanistan
    HH_Educ17 == 2 & HH_Educ17_other == "IRN" & HH_Educ18 %in% 12:15 ~ 1, #Upper secondary education completed in Iran
    HH_Educ17 == 2 & (!is.na(HH_Educ17_other) & HH_Educ17_other != "IRN" & HH_Educ17_other != "AFG") & HH_Educ18 %in% 11:12 ~ 1, #Upper secondary education completed somewhere else
    HH_Educ17 %in% c(3,98,99) & HH_Educ18 %in% 11:12 ~ 1, 
    TRUE ~ NA_real_))

roster_id_pak <- roster_id_pak %>%
  mutate(uppersec_complete = case_when(
    upperseco_complete_cur == 1 | uppersec_complete_past == 1 ~ 1, #Secondary completed for those currently is tertiary school or those who indicated that they have completed upper secondary school in the past
    TRUE ~ NA_real_))

roster_id_pak <- roster_id_pak |>
  mutate(
  eduLevelHighest = case_when(
    uppersec_complete == 1 ~ 3,
    lowersec_complete == 1 ~ 2,
    primary_complete == 1 ~ 1,
    TRUE ~ 0),
    eduLevelHighest = labelled(eduLevelHighest,
                             labels = c(
                               "Primary" = 1,
                               "Lower Secondary" = 2,
                               "Upper Secondary" = 3,
                               "None" = 0),
                             label = "Highest completed level of education")
)  |> select(-c(HH_Educ02a, HH_Educ03, HH_Educ17, HH_Educ18, HH_Educ17_other,
                primary_complete_cur, primary_complete_past,
                lowseco_complete_cur, lowersec_complete_past,
                upperseco_complete_cur, uppersec_complete_past))

ra_id_pak <- ra_id_pak |>
  select(`_uuid` = uuid, mPosition = rosterposition,
         ID_00RA, ID_01aRA, ID_01a_2RA, ID_01bRA, ID_01b_2RA, ID_02RA, 
         #ID_02bRA, 
         ID_03RA, 
         ID_04RA,
         ID_05RA, ID_06RA, ID_06bRA, ID_09RA,
         DH_13,
         FI01, FI02, FI10,
         EMP01, EMP02, EMP03, EMP04, EMP05,
         EMP06a, EMP06b, EMP06c,
         EMP09, EMP10_7,
         EMP10, EMP10_4, EMP10_5, EMP10_6, EMP10_6a,
         EMP10_11, EMP10_12, EMP25a, EMP25aa, EMP29
  )

main_id_pak <- main_id_pak |>
  select(`_uuid` = uuid,
         hhsize = HHmembersize,
         Intro_08,
         starts_with("Land10"),
         starts_with("Land14"),
         starts_with("Land15"),
         ScProtec01,
         Food11aa,  
         Food12a,   
         Food13a,   
         Food14a,   
         Food15a,   
         Food16a,   
         Food17a,  
         Food18a,
         stratum = samp_strat)

roster_id_pak <- merge(roster_id_pak, main_id_pak, by = "_uuid")
roster_id_pak <- merge(roster_id_pak, ra_id_pak |> select(`_uuid`, DH_13), by = "_uuid")


roster_id_pak <- roster_id_pak |>
  mutate(
    ID_09 = as.character(ID_09),
    ID_09 = case_when(
      ID_09 == "1" ~ "A",
      ID_09 == "2" ~ "B",
      ID_09 == "3" ~ "C",
      ID_09 == "4" ~ "D",
      ID_09 == "5" ~ "E",
      ID_09 == "6" ~ "F",
      ID_09 == "7" ~ "G",
      ID_09 == "8" ~ "H",
      ID_09 == "9" ~ "I",
      ID_09 == "10" ~ "J",
      ID_09 == "11" ~ "K",
      ID_09 == "12" ~ "L",
      ID_09 == "13" ~ "M",
      ID_09 == "14" ~ "N",
      ID_09 == "15" ~ "O",
      ID_09 == "16" ~ "P",
      ID_09 == "17" ~ "Q",
      ID_09 == "18" ~ "R",
      ID_09 == "19" ~ "S",
      ID_09 %in% c("96", "98", "99") ~ "DK"
    )
  )

ra_id_pak <- ra_id_pak |>
  mutate(
    ID_09RA = as.character(ID_09RA),
    ID_09RA = case_when(
      ID_09RA == "1" ~ "A",
      ID_09RA == "2" ~ "B",
      ID_09RA == "3" ~ "C",
      ID_09RA == "4" ~ "D",
      ID_09RA == "5" ~ "E",
      ID_09RA == "6" ~ "F",
      ID_09RA == "7" ~ "G",
      ID_09RA == "8" ~ "H",
      ID_09RA == "9" ~ "I",
      ID_09RA == "10" ~ "J",
      ID_09RA == "11" ~ "K",
      ID_09RA == "12" ~ "L",
      ID_09RA == "13" ~ "M",
      ID_09RA == "14" ~ "N",
      ID_09RA == "15" ~ "O",
      ID_09RA == "16" ~ "P",
      ID_09RA == "17" ~ "Q",
      ID_09RA == "18" ~ "R",
      ID_09RA == "19" ~ "S",
      ID_09RA %in% c("96", "98", "99") ~ "DK"
    )
  )

### Add binary variables
roster_id_pak <- roster_id_pak |> mutate(ID_09Ref = ref_id(ID_09, c("I", "J", "K", "N")))
ra_id_pak <- ra_id_pak |> mutate(ID_09RARef = ref_id(ID_09RA, c("I", "J", "K", "N"))) 

### Add country column
roster_id_pak <- roster_id_pak |> country_col("Pakistan")

# Save --------------------------------------------------------------------

saveRDS(roster_id_pak, "data/roster_id_pak.rds") 
saveRDS(roster_id_ssd, "data/roster_id_ssd.rds") 
saveRDS(roster_id_cmr, "data/roster_id_cmr.rds") 
saveRDS(ra_id_pak, "data/ra_id_pak.rds") 
saveRDS(ra_id_ssd, "data/ra_id_ssd.rds") 
saveRDS(ra_id_cmr, "data/ra_id_cmr.rds")
