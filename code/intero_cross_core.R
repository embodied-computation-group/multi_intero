# Script to plot pairwise correlations for all intero variables.
# Micah G. Allen, 2025

# Required packages:
# install.packages(c("readr", "dplyr", "RColorBrewer", "GGally", "ggplot2", "apaTables"))

library(readr)
library(dplyr)
library(RColorBrewer)
library(GGally)
library(apaTables)

# 1) Read your data. Replace path as appropriate.
alldata <- read_csv("data/subject_data_vmp.csv")

# 2) Subset only the RRST and HRD variables.
int_data <- alldata %>%
  select(
    RRST_conf,
    RRST_slope,
    RRST_thresh,
    HRD_conf,
    HRD_slope,
    HRD_thresh
  )

# Optional: rename for clarity
int_data <- int_data %>%
  rename(
    "RRST Confidence" = RRST_conf,
    "RRST Slope" = RRST_slope,
    "RRST Threshold" = RRST_thresh,
    "HRDT Confidence" = HRD_conf,
    "HRDT Slope" = HRD_slope,
    "HRDT Threshold" = HRD_thresh
  )

# ----------------------------------------------------------------------------
# Figure 1: Scatterplot Matrix
# ----------------------------------------------------------------------------


N <- nrow(na.omit(int_data))
N_hrdt <- nrow(na.omit(int_data[ ,'HRDT Threshold']))
N_rrst <- nrow(na.omit(int_data[ ,'RRST Threshold']))

# Improved scatterplot matrix with adjustments
ggpairs(
  data = int_data,
  lower = list(continuous = "smooth"),
  upper = list(continuous = "cor"),
  diag  = list(continuous = "densityDiag"),
  aes(alpha = 0.7)
) +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12, color = "black")
  ) +
  labs(title = "Interoception Variable Scatterplot Matrix",
       subtitle = paste("Pairwise relationships among RRST and HRDT variables", "(crossmodal N =", N, ", HRDT N =", N_hrdt, ", RRST N =", N_rrst, ")")
  )

pval_fdr <- psych::corr.test(int_data, method = "pearson", adjust = "fdr")$p
rval_fdr <- psych::corr.test(int_data, method = "pearson", adjust = "fdr")$r

# Save the scatterplot matrix
ggsave("figs/scatter_matrix_interoception.png", width = 12, height = 12)

# ----------------------------------------------------------------------------
# APA Style Correlation Matrix Table
# ----------------------------------------------------------------------------

# Generate and export an APA style formatted correlation matrix to a Word file.
apa.cor.table(int_data, filename = "docs/APA_Correlation_Matrix.doc", table.number = 1)
