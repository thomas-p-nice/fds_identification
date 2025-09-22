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

# Load data
roster_id_pak <- read_rds("data/roster_id_pak.rds") 
roster_id_ssd <- read_rds("data/roster_id_ssd.rds") 
roster_id_cmr <- read_rds("data/roster_id_cmr.rds") 
ra_id_pak <- read_rds("data/ra_id_pak.rds") 
ra_id_ssd <- read_rds("data/ra_id_ssd.rds") 
ra_id_cmr <- read_rds("data/ra_id_cmr.rds")

# Redefine conflicting labels ---------------------------------------------
# Set as follows, with SSD & CMR to start and then adding PAK categories

## ID_09
ID_09_labels <- c("No documents"="A", "Tourist visa"="B", "Student visa"="C", 
                  "Work visa"="D", "Humanitarian visa"="E", 
                  "Regional free movement agreement (e.g. Mercosur, EU)"="F",
                  "Permanent resident"="G", "Asylum applicant certificate"="H", 
                  "Refugee card"="I", "Recognized stateless person"="J", 
                  "Complementary and subsidiary protection"="K", 
                  "Temporary protection"="L", "NADRA Proof of Registration card"="P", 
                  "Refugee enrollment document"="M", "NADRA Family Information Certificate"="Q", 
                  "Birth certificate/Birth records"="R",
                  "UNHCR SHAARP /SEHER reception registration slip"="S", 
                  "ACC card"="T", "Don't know"="DK", "Other"="OT")

roster_id_ssd <- roster_id_ssd |>
  mutate(
    ID_09 = labelled(ID_09, labels = ID_09_labels)
  )

roster_id_cmr <- roster_id_cmr |>
  mutate(
    ID_09 = labelled(ID_09, labels = ID_09_labels)
  )

roster_id_pak <- roster_id_pak |>
  mutate(ID_09 = case_when(ID_09 == "J" ~ "P", # Relabel NADRA Proof of Registration card 
                           ID_09 == "K" ~ "Q", # Relabel Family Information Certificate
                           ID_09 == "L" ~ "R", # Relabel Birth certificate/Birth records
                           ID_09 == "M" ~ "S", # Relabel UNHCR SHAARP /SEHER reception registration slip
                           ID_09 == "N" ~ "T", # Relabel ACC card
                           ID_09 == "O" ~ "J", # Relabel recognized stateless person
                           TRUE ~ ID_09),
         ID_09 = labelled(ID_09, labels = ID_09_labels))


ra_id_ssd <- ra_id_ssd |>
  mutate(
    ID_09RA = labelled(ID_09RA, labels = ID_09_labels)
  )

ra_id_cmr <- ra_id_cmr |>
  mutate(
    ID_09RA = labelled(ID_09RA, labels = ID_09_labels)
  )

ra_id_pak <- ra_id_pak |>
  mutate(ID_09RA = case_when(ID_09RA == "J" ~ "P", # Relabel NADRA Proof of Registration card 
                             ID_09RA == "K" ~ "Q", # Relabel Family Information Certificate
                             ID_09RA == "L" ~ "R", # Relabel Birth certificate/Birth records
                             ID_09RA == "M" ~ "S", # Relabel UNHCR SHAARP /SEHER reception registration slip
                             ID_09RA == "N" ~ "T", # Relabel ACC card
                             ID_09RA == "O" ~ "J", # Relabel recognized stateless person
                             TRUE ~ ID_09RA),
         ID_09RA = labelled(ID_09RA, labels = ID_09_labels))     

## cob
cob_labels <- c("Host country" = "1",
                "Other country" = "2",
                "Stateless" = "3",
                "Don't know" = "98",
                "Refuse to answer" = "99")

roster_id_ssd <- roster_id_ssd |>
  mutate(cob = as.character(cob),
         cob = labelled(cob, 
                        label = "Country of birth",
                        labels = cob_labels))

roster_id_cmr <- roster_id_cmr |>
  mutate(cob = as.character(cob),
         cob = labelled(cob, 
                        label = "Country of birth",
                        labels = cob_labels))

roster_id_pak <- roster_id_pak |>
  mutate(cob = as.character(cob),
         cob = labelled(cob, 
                        label = "Country of birth",
                        labels = cob_labels))

## ID_00
roster_id_ssd <- roster_id_ssd |>
  mutate(ID_00 = as.character(ID_00),
         ID_00 = labelled(ID_00, 
                        label = "Nationality",
                        labels = cob_labels))

roster_id_cmr <- roster_id_cmr |>
  mutate(ID_00 = as.character(ID_00),
         ID_00 = labelled(ID_00, 
                          label = "Nationality",
                          labels = cob_labels))

roster_id_pak <- roster_id_pak |>
  mutate(ID_00 = as.character(ID_00),
         ID_00 = labelled(ID_00, 
                          label = "Nationality",
                          labels = cob_labels))

## ID_01a
roster_id_ssd <- roster_id_ssd |>
  mutate(ID_01a = labelled::to_factor(ID_01a))

roster_id_cmr <- roster_id_cmr |>
  mutate(ID_01a = labelled::to_factor(ID_01a))

roster_id_pak <- roster_id_pak |>
  mutate(ID_01a = labelled::to_factor(ID_01a))

## ID_01b
roster_id_ssd <- roster_id_ssd |>
  mutate(ID_01b = labelled::to_factor(ID_01b))

roster_id_cmr <- roster_id_cmr |>
  mutate(ID_01b = labelled::to_factor(ID_01b))

roster_id_pak <- roster_id_pak |>
  mutate(ID_01b = labelled::to_factor(ID_01b))

## ID_02
ID_02_labels <- c("Security, conflict, violence" = "1",
                  "Fear of persecution" = "2",
                  "Human rights violation" = "3",
                  "Natural/man-made disaster" = "4",
                  "Eviction" = "5",
                  "Personal reasons" = "6",
                  "Other" = "96",
                  "Don't know" = "98",
                  "Refuse to answer" = "99")
### Roster
roster_id_ssd <- roster_id_ssd |>
  mutate(ID_02 = as.character(ID_02),
         ID_02 = labelled(ID_02, 
                          label = "Reason for fleeing",
                          labels = ID_02_labels))

roster_id_cmr <- roster_id_cmr |>
  mutate(ID_02 = as.character(ID_02),
         ID_02 = case_when(ID_02 == "5" ~ "6",
                           TRUE ~ ID_02)) |> # In CMR eviction isn't in the levels, so need to move "personal reasons" to level 6
  mutate(ID_02 = labelled(ID_02, 
                          label = "Reason for fleeing",
                          labels = ID_02_labels))

roster_id_pak <- roster_id_pak |>
  mutate(ID_02 = as.character(ID_02),
         ID_02 = labelled(ID_02, 
                          label = "Reason for fleeing",
                          labels = ID_02_labels))

### Roster recovery question

# roster_id_cmr <- roster_id_cmr |>
#   mutate(ID_02b = as.character(ID_02b),
#          ID_02b = labelled(ID_02b, 
#                            label = "Reason for fleeing",
#                            labels = ID_02_labels))

# ## ID_03
# roster_id_ssd <- roster_id_ssd |>
#   mutate(ID_03 = labelled::to_factor(ID_03))
# 
# roster_id_cmr <- roster_id_cmr |>
#   mutate(ID_03 = labelled::to_factor(ID_03))
# 
# roster_id_pak <- roster_id_pak |>
#   mutate(ID_03 = labelled::to_factor(ID_03))
# 
# ## ID_04
# roster_id_ssd <- roster_id_ssd |>
#   mutate(ID_04 = labelled::to_factor(ID_04))
# 
# roster_id_cmr <- roster_id_cmr |>
#   mutate(ID_04 = labelled::to_factor(ID_04))
# 
# roster_id_pak <- roster_id_pak |>
#   mutate(ID_04 = labelled::to_factor(ID_04))
# 
# ## ID_05
# roster_id_ssd <- roster_id_ssd |>
#   mutate(ID_05 = labelled::to_factor(ID_05))
# 
# roster_id_cmr <- roster_id_cmr |>
#   mutate(ID_05 = labelled::to_factor(ID_05))
# 
# roster_id_pak <- roster_id_pak |>
#   mutate(ID_05 = labelled::to_factor(ID_05))
# 
# ## ID_06
# roster_id_ssd <- roster_id_ssd |>
#   mutate(ID_06 = labelled::to_factor(ID_06))
# 
# roster_id_cmr <- roster_id_cmr |>
#   mutate(ID_06 = labelled::to_factor(ID_06))
# 
# roster_id_pak <- roster_id_pak |>
#   mutate(ID_06 = labelled::to_factor(ID_06))
# 
# ## ID_06b
# roster_id_ssd <- roster_id_ssd |>
#   mutate(ID_06b = labelled::to_factor(ID_06b))
# 
# roster_id_cmr <- roster_id_cmr |>
#   mutate(ID_06b = labelled::to_factor(ID_06b))
# 
# roster_id_pak <- roster_id_pak |>
#   mutate(ID_06b = labelled::to_factor(ID_06b))

### RA
ra_id_ssd <- ra_id_ssd |>
  mutate(ID_02RA = as.character(ID_02RA),
         ID_02RA = labelled(ID_02RA, 
                            label = "Reason for fleeing",
                            labels = ID_02_labels))

ra_id_cmr <- ra_id_cmr |>
  mutate(ID_02RA = as.character(ID_02RA),
         ID_02RA = case_when(ID_02RA == "5" ~ "6",
                             TRUE ~ ID_02RA)) |> # In CMR eviction isn't in the levels, so need to move "personal reasons" to level 6
  mutate(ID_02RA = labelled(ID_02RA, 
                            label = "Reason for fleeing",
                            labels = ID_02_labels))

ra_id_pak <- ra_id_pak |>
  mutate(ID_02RA = as.character(ID_02RA),
         ID_02RA = labelled(ID_02RA, 
                            label = "Reason for fleeing",
                            labels = ID_02_labels))
# 
# roster_id_pak <- roster_id_pak |>
#   mutate(DH_10_NUTS1 = to_factor(DH_10_NUTS1),
#          DH_10_NUTS2 = to_factor(DH_10_NUTS2))

# roster_id_cmr <- roster_id_cmr |>
#   mutate(DH_10_NUTS1 = to_factor(DH_10_NUTS1),
#          DH_10_NUTS2 = to_factor(DH_10_NUTS2))

roster_id_pak <- roster_id_pak |>
  mutate(Intro_03a_NUTS1 = to_factor(Intro_03a_NUTS1),
         Intro_03b_NUTS2 = to_factor(Intro_03b_NUTS2),
         Intro_03c_NUTS3 = to_factor(Intro_03c_NUTS3))

roster_id_cmr <- roster_id_cmr |>
  mutate(Intro_03a_NUTS1 = to_factor(Intro_03a_NUTS1),
         Intro_03b_NUTS2 = to_factor(Intro_03b_NUTS2),
         Intro_03c_NUTS3 = to_factor(Intro_03c_NUTS3))

roster_id_ssd <- roster_id_ssd |>
  mutate(Intro_03a_NUTS1 = to_factor(Intro_03a_NUTS1),
         Intro_03b_NUTS2 = to_factor(Intro_03b_NUTS2),
         Intro_03c_NUTS3 = to_factor(Intro_03c_NUTS3))

# Combine dataframes ------------------------------------------------------

roster_id <- roster_id_ssd |> 
  mutate(across(where(is.double), as.character)) |> 
  bind_rows(
    roster_id_pak |> mutate(across(where(is.double), as.character)),
    roster_id_cmr |> mutate(across(where(is.double), as.character))
  )

ra_id <- ra_id_ssd |> 
  mutate(across(where(is.double), as.character)) |> 
  bind_rows(
    ra_id_pak |> mutate(across(where(is.double), as.character)),
    ra_id_cmr |> mutate(across(where(is.double), as.character))
  )


## Merge random adult values from roster with random adult table: Lot of missings for highest mpositions
ra_id <- left_join(ra_id, roster_id, by = c("_uuid", "mPosition"))
#anti_join(ra_id |> select("_uuid", "mPosition"), ra_id1 |> select("_uuid", "mPosition")) |> View()

### Add labels for refugee document variable
ref_doc_label <- function(var) {
  labelled({{var}},
           labels = c("Refugee document" = "1",
                      "No refugee document" = "2"),
           label = "Refugee document")
}

roster_id <- roster_id |> mutate(ID_09Ref = ref_doc_label(ID_09Ref))
ra_id <- ra_id |> mutate(ID_09RARef = ref_doc_label(ID_09RARef),
                         ID_09RARef = ref_doc_label(ID_09RARef))

# Labels ------------------------------------------------------------------

# Combine RA and RA responses to only have one column
ra_id <- ra_id |>
  mutate(ID_00RA = case_when(Country == "Pakistan" ~ ID_00RA,
                             TRUE ~ ID_00RA),
         ID_01aRA = case_when(Country == "Pakistan" ~ ID_01aRA,
                              TRUE ~ ID_01aRA),
         ID_01a_2RA = case_when(Country == "Pakistan" ~ ID_01a_2RA,
                                TRUE ~ ID_01a_2RA),
         ID_01bRA = case_when(Country == "Pakistan" ~ ID_01bRA,
                              TRUE ~ ID_01bRA),
         ID_01b_2RA = case_when(Country == "Pakistan" ~ ID_01b_2RA,
                                TRUE ~ ID_01b_2RA),
         ID_02RA = case_when(Country == "Pakistan" ~ ID_02RA,
                             TRUE ~ ID_02RA),
         # ID_02bRA = case_when(Country == "Pakistan" ~ ID_02bRA,
         #                     TRUE ~ ID_02bRA),
         ID_03RA = case_when(Country == "Pakistan" ~ ID_03RA,
                             TRUE ~ ID_03RA),
         # ID_04RA = case_when(Country == "Pakistan" ~ ID_04RA,
         #                     TRUE ~ ID_04RA),
         ID_05RA = case_when(Country == "Pakistan" ~ ID_05RA,
                             TRUE ~ ID_05RA),
         ID_06RA = case_when(Country == "Pakistan" ~ ID_06RA,
                             TRUE ~ ID_06RA),
         ID_06bRA = case_when(Country == "Pakistan" ~ ID_06bRA,
                              TRUE ~ ID_06bRA),
         ID_09RA = case_when(Country == "Pakistan" ~ ID_09RA,
                             TRUE ~ ID_09RA),
         ID_09RARef = case_when(Country == "Pakistan" ~ ID_09RARef,
                                TRUE ~ ID_09RARef))

ra_id <- ra_id |> 
  mutate(ID_09RARef = as.character(ID_09RARef),
         ID_09RARef = labelled(ID_09RARef, 
                               label = "Valid refugee document",
                               labels = c("Valid refugee document" = "1",
                                          "No valid refugee document" = "2")))

# Had to flee/abandon home
roster_id <- roster_id |> mutate(ID_01b = case_when(ID_01b  == "1" ~ "1",
                                                    TRUE ~ "2"),
                                 ID_01b = labelled(ID_01b,
                                                   label = "Had to flee",
                                                   labels = c("Had to flee" = "1",
                                                              "Never had to flee" = "2")))

roster_id <- roster_id |> mutate(ID_01b_2 = case_when(ID_01b_2  == "1" ~ "1",
                                                      TRUE ~ "2"),
                                 ID_01b_2 = labelled(ID_01b_2,
                                                     label = "Had to abandon home",
                                                     labels = c("Had to abandon home" = "1",
                                                                "Never had to abandon home" = "2")))

roster_id <- roster_id |> mutate(ID_01b_combine = case_when(ID_01b  == "1" | ID_01b_2 == "1" ~ "1",
                                                            TRUE ~ "2"),
                                 ID_01b_combine = labelled(ID_01b_combine,
                                                           label = "Had to flee/abandon home",
                                                           labels = c("Had to flee/abandon home" = "1",
                                                                      "Never had to flee/abandon home" = "2")))

ra_id <- ra_id |> mutate(ID_01b = case_when(ID_01b  == "1" ~ "1",
                                            TRUE ~ "2"),
                         ID_01b = labelled(ID_01b,
                                           label = "Had to flee",
                                           labels = c("Had to flee" = "1",
                                                      "Never had to flee" = "2")))

ra_id <- ra_id |> mutate(ID_01b_2 = case_when(ID_01b_2  == "1" ~ "1",
                                              TRUE ~ "2"),
                         ID_01b_2 = labelled(ID_01b_2,
                                             label = "Had to abandon home",
                                             labels = c("Had to abandon home" = "1",
                                                        "Never had to abandon home" = "2")))

ra_id <- ra_id |> mutate(ID_01bRA = case_when(ID_01bRA  == "1" ~ "1",
                                              TRUE ~ "2"),
                         ID_01bRA = labelled(ID_01bRA,
                                             label = "Had to flee",
                                             labels = c("Had to flee" = "1",
                                                        "Never had to flee" = "2")))

ra_id <- ra_id |> mutate(ID_01b_2RA = case_when(ID_01b_2RA  == "1" ~ "1",
                                                TRUE ~ "2"),
                         ID_01b_2RA = labelled(ID_01b_2RA,
                                               label = "Had to abandon home",
                                               labels = c("Had to abandon home" = "1",
                                                          "Never had to abandon home" = "2")))

ra_id <- ra_id |> mutate(ID_01b_combine = case_when(ID_01b  == "1" | ID_01b_2 == "1" ~ "1",
                                                    TRUE ~ "2"),
                         ID_01b_combine = labelled(ID_01b_combine,
                                                   label = "Had to flee/abandon home",
                                                   labels = c("Had to flee/abandon home" = "1",
                                                              "Never had to flee/abandon home" = "2")))

ra_id <- ra_id |> mutate(ID_01b_combineRA = case_when(ID_01bRA  == "1" | ID_01b_2RA == "1" ~ "1",
                                                      TRUE ~ "2"),
                         ID_01b_combineRA = labelled(ID_01b_combineRA,
                                                     label = "Had to flee/abandon home",
                                                     labels = c("Had to flee/abandon home" = "1",
                                                                "Never had to flee/abandon home" = "2")))

# Crossed a border
roster_id <- roster_id |> mutate(ID_03_simp = case_when(ID_03 == "1" ~ "1",
                                                        TRUE ~ "2"),
                                 ID_03_simp = labelled(ID_03_simp,
                                                       label = "Crossed border",
                                                       labels = c("Crossed border" = "1",
                                                                  "Did not cross border" = "2")))

ra_id <- ra_id |> mutate(ID_03_simp = case_when(ID_03 == "1" ~ "1",
                                                TRUE ~ "2"),
                         ID_03_simp = labelled(ID_03_simp,
                                               label = "Crossed border",
                                               labels = c("Crossed border" = "1",
                                                          "Did not cross border" = "2")))

ra_id <- ra_id |> mutate(ID_03_simpRA = case_when(ID_03RA == "1" ~ "1",
                                                  TRUE ~ "2"),
                         ID_03_simpRA = labelled(ID_03_simpRA,
                                                 label = "Crossed border",
                                                 labels = c("Crossed border" = "1",
                                                            "Did not cross border" = "2")))

# Applied for protection
roster_id <- roster_id |> mutate(ID_05_simp = case_when(ID_05 %in% c("1", "3") ~ "1",
                                                        TRUE ~ "2"),
                                 ID_05_simp = labelled(ID_05_simp,
                                                       label = "Applied for international protection/prima facie recognition",
                                                       labels = c("Applied for international protection/prima facie recognition" = "1",
                                                                  "Did not apply for international protection/prima facie recognition" = "2")))

ra_id <- ra_id |> mutate(ID_05_simp = case_when(ID_05 %in% c("1", "3") ~ "1",
                                                TRUE ~ "2"),
                         ID_05_simp = labelled(ID_05_simp,
                                               label = "Applied for international protection/prima facie recognition",
                                               labels = c("Applied for international protection/prima facie recognition" = "1",
                                                          "Did not apply for international protection/prima facie recognition" = "2")))

ra_id <- ra_id |> mutate(ID_05_simpRA = case_when(ID_05RA %in% c("1", "3") ~ "1",
                                                  TRUE ~ "2"),
                         ID_05_simpRA = labelled(ID_05_simpRA,
                                                 label = "Applied for international protection/prima facie recognition",
                                                 labels = c("Applied for international protection/prima facie recognition" = "1",
                                                            "Did not apply for international protection/prima facie recognition" = "2")))

# add variable on required international protection based on ID_02
roster_id <- roster_id |> mutate(ID_02_valid = case_when(ID_02  %in% c("1", "2", "3", "4") ~ "1",
                                                         ID_02  %in% c("5", "6") ~ "2"),
                                 ID_02_valid = labelled(ID_02_valid,
                                                        label = "Required international protection",
                                                        labels = c("Required international protection" = "1",
                                                                   "Did not require international protection" = "2")))

ra_id <- ra_id |> mutate(ID_02_valid = case_when(ID_02  %in% c("1", "2", "3", "4") ~ "1",
                                                 ID_02  %in% c("5", "6") ~ "2"),
                         ID_02_valid = labelled(ID_02_valid,
                                                label = "Required international protection",
                                                labels = c("Required international protection" = "1",
                                                           "Did not require international protection" = "2")))

ra_id <- ra_id |> mutate(ID_02_validRA = case_when(ID_02RA  %in% c("1", "2", "3", "4") ~ "1",
                                                   TRUE ~ "2"),
                         ID_02_validRA = labelled(ID_02_validRA,
                                                  label = "Required international protection",
                                                  labels = c("Required international protection" = "1",
                                                             "Did not require international protection" = "2")))

# roster_id <- roster_id |> mutate(ID_02b_valid = case_when(ID_02b  %in% c("1", "2", "3", "4") ~ "1",
#                                                           ID_02b  %in% c("5", "6") ~ "2"),
#                                  ID_02b_valid = labelled(ID_02b_valid,
#                                                          label = "Required international protection (recovery question)",
#                                                          labels = c("Required international protection (recovery question)" = "1",
#                                                                     "Did not require international protection (recovery question)" = "2")))
# 
# ra_id <- ra_id |> mutate(ID_02b_valid = case_when(ID_02b  %in% c("1", "2", "3", "4") ~ "1",
#                                                   ID_02b  %in% c("5", "6") ~ "2"),
#                          ID_02b_valid = labelled(ID_02b_valid,
#                                                  label = "Required international protection (recovery question)",
#                                                  labels = c("Required international protection (recovery question)" = "1",
#                                                             "Did not require international protection (recovery question)" = "2")))
# 
# ra_id <- ra_id |> mutate(ID_02b_validRA = case_when(ID_02bRA  %in% c("1", "2", "3", "4") ~ "1",
#                                                     TRUE ~ "2"),
#                          ID_02b_validRA = labelled(ID_02b_validRA,
#                                                    label = "Required international protection (recovery question)",
#                                                    labels = c("Required international protection (recovery question)" = "1",
#                                                               "Did not require international protection (recovery question)" = "2")))

roster_id <- roster_id |>
  mutate(ID_09Ref = as.character(ID_09Ref),
         ID_09Ref = labelled(ID_09Ref, 
                             label = "Valid refugee document",
                             labels = c("Valid refugee document" = "1",
                                        "No valid refugee document" = "2")))

ra_id <- ra_id |>
  mutate(ID_09Ref = as.character(ID_09Ref),
         ID_09Ref = labelled(ID_09Ref, 
                             label = "Valid refugee document",
                             labels = c("Valid refugee document" = "1",
                                        "No valid refugee document" = "2")))

roster_id <- roster_id |>
  mutate(cob = as.character(cob),
         cob = labelled(cob, 
                        label = "Country of birth",
                        labels = c("Host country" = "1",
                                   "Other country" = "2",
                                   "Stateless" = "3",
                                   "Don't know" = "98",
                                   "Refuse to answer" = "99")))

ra_id <- ra_id |>
  mutate(cob = as.character(cob),
         cob = labelled(cob, 
                        label = "Country of birth",
                        labels = c("Host country" = "1",
                                   "Other country" = "2",
                                   "Stateless" = "3",
                                   "Don't know" = "98",
                                   "Refuse to answer" = "99")))

# Clean variables
roster_id <- roster_id |>
  mutate(HH_03 = labelled::to_factor(HH_03),
    arrivalYear = case_when(arrivalYear == 9998 ~ NA_character_,
                                     arrivalYear < 1950 ~ NA_character_,
                                     TRUE ~ arrivalYear),
         arrivalYear = as.numeric(arrivalYear),
         citizenship = case_when(citizenship == "" ~ NA_character_,
                                     TRUE ~ citizenship)) |>
  mutate(HH_03 = case_when(
    HH_03 == 1 ~ "The household head himself/herself",
    HH_03 == 2 ~ "Wife/Husband/Partner",
    HH_03 == 3 ~ "Biological child",
    HH_03 == 4 ~ "Step child/Adopted child",
    HH_03 == 5 ~ "Grandchild",
    HH_03 == 6 ~ "Niece/Nephew",
    HH_03 == 7 ~ "Mother/Father",
    HH_03 == 8 ~ "Sister/Brother",
    HH_03 == 9 ~ "Son/Daughter-in-law",
    HH_03 == 10 ~ "Brother/Sister-in-law",
    HH_03 == 11 ~ "Grandfather/mother",
    HH_03 == 12 ~ "Father/Mother-in-law",
    HH_03 == 13 ~ "Uncle/Aunt",
    HH_03 == 14 ~ "Cousin",
    HH_03 == 15 ~ "Travel companion/friend",
    HH_03 == 16 ~ "Servant or servant’s relative",
    HH_03 == 17 ~ "Lodger or lodger’s relative",
    TRUE ~ HH_03,
  )) |>
  filter(!grepl("Hosts|Host", stratum))

ra_id <- ra_id |>
  mutate(arrivalYear = case_when(arrivalYear == 9998 ~ NA_character_,
                                 arrivalYear < 1950 ~ NA_character_,
                                 TRUE ~ arrivalYear),
         arrivalYear = as.numeric(arrivalYear),
         citizenship = case_when(citizenship == "" ~ NA_character_,
                                 TRUE ~ citizenship)) |>
  filter(!grepl("Hosts|Host", stratum))

roster_id <- roster_id %>% 
  mutate(
    sex = case_when(sex %in% c("2", "Female") ~ "Female", TRUE ~ "Male"),
    hhead = case_when(mPosition == "1" ~ "Household head", TRUE ~ "Not household head"),
    separated = case_when(
      DH_13 == "1" ~ "Not separated",
      DH_13 %in% c("2","3") ~ "Separated",
      TRUE ~ NA_character_
    ),
    cob = labelled::to_factor(cob),
    `Separated households` = case_when(
      DH_13 == 1 ~ 0,
      DH_13 %in% c(2,3) ~ 1,
      TRUE ~ NA_real_
    ),
    Female = case_when(sex %in% c("2", "Female") ~ 1, TRUE ~ 0),
    ageYears = as.numeric(ageYears),
    arrivalYear = as.numeric(arrivalYear)
  )  |>  mutate(
    eduLevelHighest = labelled(eduLevelHighest,
                               labels = c(
                                 "Primary" = "1",
                                 "Lower Secondary" = "2",
                                 "Upper Secondary" = "3",
                                 "None" = "0"),
                               label = "Highest completed level of education")) |>
  group_by(`_uuid`) %>%
  mutate(eduLevelHoH = max(eduLevelHighest[mPosition == "1"], na.rm = TRUE),
         sexHoH = first(sex[mPosition == "1"])
) %>%
  ungroup()


ra_id <- ra_id %>% 
  mutate(
    sex = case_when(sex %in% c("2", "Female") ~ "Female", TRUE ~ "Male"),
    hhead = case_when(mPosition == "1" ~ "Household head", TRUE ~ "Not household head"),
    cob = labelled::to_factor(cob),
    Female = case_when(sex %in% c("2", "Female") ~ 1, TRUE ~ 0),
    ageYears = as.numeric(ageYears),
    arrivalYear = as.numeric(arrivalYear)
  ) 
# Save --------------------------------------------------------------------

saveRDS(roster_id, "data/roster_id.rds")
saveRDS(ra_id, "data/ra_id.rds")
