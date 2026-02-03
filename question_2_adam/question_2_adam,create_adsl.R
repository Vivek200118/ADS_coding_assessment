############################################################
# Question 2: ADaM ADSL Creation
# Description: Create subject-level ADaM dataset using admiral
############################################################
# Start log
sink("question_2_adam_run_log.txt", split = TRUE)

cat("ADSL creation started at:", Sys.time(), "\n\n")

# Clear environment
rm(list = ls())

# Load required libraries
library(admiral)
library(dplyr)
library(pharmaversesdtm)
library(pharmaverseadam)
library(lubridate)
# Load SDTM datasets
dm <- pharmaversesdtm::dm
vs <- pharmaversesdtm::vs
ex <- pharmaversesdtm::ex
ae <- pharmaversesdtm::ae
ds <- pharmaversesdtm::ds
# ----------------------------------------------------------
# Create ADSL base from DM
# ----------------------------------------------------------

adsl <- dm %>%
  select(
    STUDYID,
    USUBJID,
    SUBJID,
    SITEID,
    AGE,
    SEX,
    ARM,
    ARMCD,
    RACE,
    COUNTRY
  )
# ----------------------------------------------------------
# Derive AGEGR9 and AGEGR9N
# ----------------------------------------------------------

adsl <- adsl %>%
  mutate(
    AGEGR9 = case_when(
      AGE < 18 ~ "<18",
      AGE >= 18 & AGE <= 50 ~ "18 - 50",
      AGE > 50 ~ ">50",
      TRUE ~ NA_character_
    ),
    AGEGR9N = case_when(
      AGE < 18 ~ 1,
      AGE >= 18 & AGE <= 50 ~ 2,
      AGE > 50 ~ 3,
      TRUE ~ NA_real_
    )
  )
table(adsl$AGEGR9, useNA = "ifany")
# ----------------------------------------------------------
# Prepare EX data for TRTSDTM derivation
# ----------------------------------------------------------

ex_trt <- ex %>%
  filter(
    (EXDOSE > 0) |
      (EXDOSE == 0 & grepl("PLACEBO", toupper(EXTRT)))
  ) %>%
  filter(!is.na(EXSTDTC))
# Impute missing time as 00:00:00 and create datetime
ex_trt <- ex_trt %>%
  mutate(
    TRTSTMF = ifelse(nchar(EXSTDTC) == 10, "Y", NA_character_),
    EXSTDTC_IMP = ifelse(
      nchar(EXSTDTC) == 10,
      paste0(EXSTDTC, "T00:00:00"),
      EXSTDTC
    ),
    EXSTDTC_DT = ymd_hms(EXSTDTC_IMP)
  )
# Get first treatment datetime per subject
trt_start <- ex_trt %>%
  arrange(USUBJID, EXSTDTC_DT) %>%
  group_by(USUBJID) %>%
  slice(1) %>%
  ungroup() %>%
  select(USUBJID, TRTSDTM = EXSTDTC_DT, TRTSTMF)
# Merge TRTSDTM and TRTSTMF into ADSL
adsl <- adsl %>%
  left_join(trt_start, by = "USUBJID")
adsl %>%
  select(USUBJID, TRTSDTM, TRTSTMF) %>%
  head()
# ----------------------------------------------------------
# Derive ITTFL
# ----------------------------------------------------------

adsl <- adsl %>%
  mutate(
    ITTFL = ifelse(!is.na(TRTSDTM), "Y", "N")
  )
table(adsl$ITTFL)
# ----------------------------------------------------------
# Derive LSTALVDT (Last Available Assessment Date)
# ----------------------------------------------------------

vs_dates <- vs %>%
  filter(!is.na(VSDTC)) %>%
  mutate(ADT = ymd(VSDTC)) %>%
  select(USUBJID, ADT)

ae_dates <- ae %>%
  filter(!is.na(AESTDTC)) %>%
  mutate(ADT = ymd(AESTDTC)) %>%
  select(USUBJID, ADT)

ds_dates <- ds %>%
  filter(!is.na(DSSTDTC)) %>%
  mutate(ADT = ymd(DSSTDTC)) %>%
  select(USUBJID, ADT)

ex_dates <- ex %>%
  filter(!is.na(EXSTDTC)) %>%
  mutate(ADT = ymd(EXSTDTC)) %>%
  select(USUBJID, ADT)

all_dates <- bind_rows(vs_dates, ae_dates, ds_dates, ex_dates)

lstavldt <- all_dates %>%
  group_by(USUBJID) %>%
  summarise(
    LSTAVLDT = max(ADT, na.rm = TRUE),
    .groups = "drop"
  )

adsl <- adsl %>%
  left_join(lstavldt, by = "USUBJID")
saveRDS(adsl, "C:/Users/srigi/OneDrive/Desktop/Q2 adam/adsl.rds")
test <- readRDS("C:/Users/srigi/OneDrive/Desktop/Q2 adam/adsl.rds")
str(test)
adsl <- readRDS("C:/Users/srigi/OneDrive/Desktop/Q2 adam/adsl.rds")
View(adsl)
cat("\nADSL creation completed successfully at:", Sys.time(), "\n")

# Stop log
sink()



