############################################################
# Question 3 – AE Summary Table (FDA Table 10)
############################################################

rm(list = ls())

library(dplyr)
library(gtsummary)
library(pharmaverseadam)
library(gt)

# Load ADAE
adae <- pharmaverseadam::adae

# Keep Treatment-Emergent AEs
adae_te <- adae %>%
  filter(TRTEMFL == "Y")

# Sanity check
table(adae_te$TRT01A, useNA = "ifany")

# FDA Table 10 style AE summary
ae_table <- adae_te %>%
  select(AESOC, AETERM, TRT01A) %>%
  tbl_summary(
    by = TRT01A,
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing = "no"
  ) %>%
  add_overall() %>%
  modify_header(
    label ~ "**Primary SOC / Preferred Term**"
  ) %>%
  bold_labels()

# Display table
ae_table

# Save output
dir.create("question_3_tlg", showWarnings = FALSE)

ae_table %>%
  as_gt() %>%
  gtsave("question_3_tlg/ae_summary_table.html")
