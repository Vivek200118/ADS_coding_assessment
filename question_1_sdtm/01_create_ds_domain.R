############################################################
# Question 1: SDTM DS Domain Creation
# Description: Create SDTM Disposition (DS) domain
############################################################

# Clear environment
rm(list = ls())

# Load required libraries
library(sdtm.oak)
library(dplyr)
library(pharmaverseraw)

# Load raw DS data
ds_raw <- pharmaverseraw::ds_raw

# View structure of raw data
str(ds_raw)

# ----------------------------------------------------------
# Study Controlled Terminology for DS domain
# ----------------------------------------------------------

study_ct <- data.frame(
  stringsAsFactors = FALSE,
  codelist_code = c(
    "C66727","C66727","C66727","C66727","C66727",
    "C66727","C66727","C66727","C66727","C66727"
  ),
  term_code = c(
    "C41331","C25250","C28554","C48226","C48227",
    "C48250","C142185","C49628","C49632","C49634"
  ),
  term_value = c(
    "ADVERSE EVENT","COMPLETED","DEATH","LACK OF EFFICACY",
    "LOST TO FOLLOW-UP","PHYSICIAN DECISION",
    "PROTOCOL VIOLATION","SCREEN FAILURE",
    "STUDY TERMINATED BY SPONSOR","WITHDRAWAL BY SUBJECT"
  ),
  collected_value = c(
    "Adverse Event","Complete","Dead","Lack of Efficacy",
    "Lost To Follow-Up","Physician Decision",
    "Protocol Violation","Trial Screen Failure",
    "Study Terminated By Sponsor","Withdrawal by Subject"
  ),
  term_preferred_term = c(
    "AE","Completed","Died",
    NA,NA,NA,"Violation",
    "Failure to Meet Inclusion/Exclusion Criteria",
    NA,"Dropout"
  ),
  term_synonyms = c(
    "ADVERSE EVENT","COMPLETE","Death",
    NA,NA,NA,NA,NA,NA,"Discontinued Participation"
  )
)
# ----------------------------------------------------------
# Create SDTM DS domain
# ----------------------------------------------------------
library(stringr)

ds_sdtm <- ds_raw %>%
  mutate(
    STUDYID  = STUDY,
    DOMAIN   = "DS",
    USUBJID  = paste(STUDY, PATNUM, sep = "-"),
    DSTERM   = IT.DSTERM,
    DSCAT    = "DISPOSITION EVENT",
    VISIT    = INSTANCE,
    VISITNUM = case_when(
      INSTANCE == "Baseline" ~ 0,
      grepl("Week", INSTANCE) ~ as.numeric(str_extract(INSTANCE, "\\d+")),
      TRUE ~ NA_real_
    ),
    DSDTC    = DSDTCOL,
    DSSTDTC  = DSDTCOL
  )
# ----------------------------------------------------------
# Map DSDECOD using controlled terminology
# ----------------------------------------------------------

ds_sdtm <- ds_sdtm %>%
  left_join(
    study_ct,
    by = c("DSTERM" = "collected_value")
  ) %>%
  mutate(
    DSDECOD = term_value
  )
# ----------------------------------------------------------
# Create DSSEQ
# ----------------------------------------------------------

ds_sdtm <- ds_sdtm %>%
  arrange(USUBJID, DSSTDTC) %>%
  group_by(USUBJID) %>%
  mutate(DSSEQ = row_number()) %>%
  ungroup()
# ----------------------------------------------------------
# ----------------------------------------------------------
# ----------------------------------------------------------
# Create DSSTDY (Study Day) - safe handling of missing dates
# ----------------------------------------------------------

ds_sdtm <- ds_sdtm %>%
  mutate(
    DSSTDTC_DATE = as.Date(DSSTDTC, format = "%d-%m-%Y")
  ) %>%
  group_by(USUBJID) %>%
  mutate(
    DSSTDY = ifelse(
      !is.na(DSSTDTC_DATE),
      as.numeric(DSSTDTC_DATE - min(DSSTDTC_DATE, na.rm = TRUE) + 1),
      NA_real_
    )
  ) %>%
  ungroup() %>%
  select(-DSSTDTC_DATE)
saveRDS(
  ds_sdtm,
  "C:/Users/srigi/OneDrive/Desktop/Q1 sdtm/ds_sdtm.rds"
)
ds_q1 <- readRDS("C:/Users/srigi/OneDrive/Desktop/Q1 sdtm/ds_sdtm.rds")
View(ds_q1)
str(ds_q1)
nrow(ds_q1)
names(ds_q1)
table(ds_q1$DOMAIN)
table(ds_q1$DSDECOD, useNA = "ifany")








