############################################################
# Question 3 – AE Visualizations
############################################################

rm(list = ls())
install.packages("binom")

library(dplyr)
library(ggplot2)
library(pharmaverseadam)
library(binom)

# Load ADAE
adae <- pharmaverseadam::adae

# Keep Treatment-Emergent AEs
adae_te <- adae %>%
  filter(TRTEMFL == "Y")

# Create output directory on Desktop
out_dir <- "C:/Users/srigi/OneDrive/Desktop/question_3_tlg"
dir.create(out_dir, showWarnings = FALSE)

############################################################
# Plot 1: AE Severity Distribution by Treatment
############################################################

p1_data <- adae_te %>%
  count(TRT01A, AESEV)

p1 <- ggplot(p1_data, aes(x = TRT01A, y = n, fill = AESEV)) +
  geom_bar(stat = "identity") +
  labs(
    title = "AE Severity Distribution by Treatment",
    x = "Treatment Arm",
    y = "Count of AEs",
    fill = "Severity"
  ) +
  theme_minimal()

ggsave(
  filename = paste0(out_dir, "/ae_severity_by_treatment.png"),
  plot = p1,
  width = 8,
  height = 6,
  dpi = 300
)

############################################################
# Plot 2: Top 10 Most Frequent AEs with 95% CI
############################################################

# Subject-level AE incidence
ae_subjects <- adae_te %>%
  distinct(USUBJID, AETERM)

# Total number of subjects
n_subj <- n_distinct(adae_te$USUBJID)

# Top 10 AEs
top10_ae <- ae_subjects %>%
  count(AETERM, sort = TRUE) %>%
  slice_head(n = 10) %>%
  mutate(
    pct = n / n_subj,
    ci = binom.confint(n, n_subj, method = "exact"),
    lower = ci$lower,
    upper = ci$upper
  )

p2 <- ggplot(top10_ae, aes(x = pct, y = reorder(AETERM, pct))) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Top 10 Most Frequent Adverse Events",
    subtitle = paste0("n = ", n_subj, " subjects; 95% Clopper-Pearson CI"),
    x = "Percentage of Patients",
    y = "Adverse Event"
  ) +
  theme_minimal()

ggsave(
  filename = paste0(out_dir, "/top10_ae_with_ci.png"),
  plot = p2,
  width = 8,
  height = 6,
  dpi = 300
)
