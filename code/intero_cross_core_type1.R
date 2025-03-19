# Script to plot pairwise correlations for perceptual (type 1) intero & extero variables.
# Leah Banellis & Micah G. Allen, 2025

# Required packages:
# install.packages(c("readr", "dplyr", "RColorBrewer", "GGally", "ggplot2", "apaTables"))
rm(list = ls())

library(readr)
library(dplyr)
library(RColorBrewer)
library(GGally)
library(apaTables)
source("code/get_all_bad_subjects.R")

# Read your data. Replace path as appropriate.
alldata <- read_csv("data/mergedvmpdata_all.csv") 

# Subset only the RRST and HRDT variables.
int_data <- alldata %>%
  dplyr::select(
    rrst_estimated_mean_alpha,
    rrst_estimated_mean_beta,
    hrd_estimated_mean_alpha,
    hrd_estimated_mean_beta,
    extero_estimated_mean_alpha,
    extero_estimated_mean_beta
  )

# compute absolute hrdt (intero and extero) threshold 
int_data[ ,'hrd_estimated_mean_alpha'] <- abs(int_data[ ,'hrd_estimated_mean_alpha'])
int_data[ ,'extero_estimated_mean_alpha'] <- abs(int_data[ ,'extero_estimated_mean_alpha'])

# Invert scale of hrd/extero slope (so higher = steeper slope (same as RRST)) (for both intero & extero)
int_data[ ,'hrd_estimated_mean_beta'] <- -(int_data[ ,'hrd_estimated_mean_beta'])
int_data[ ,'extero_estimated_mean_beta'] <- -(int_data[ ,'extero_estimated_mean_beta'])


# Rename for clarity
int_data <- int_data %>%
  rename(
    "Respiratory Precision" = rrst_estimated_mean_beta,
    "Respiratory Sensitivity" = rrst_estimated_mean_alpha,
    "Cardiac Precision" = hrd_estimated_mean_beta,
    "Cardiac Sensitivity" = hrd_estimated_mean_alpha,
    "Auditory Precision" = extero_estimated_mean_beta,
    "Auditory Sensitivity" = extero_estimated_mean_alpha
  )

# ----------------------------------------------------------------------------
# Figure 1: Scatterplot Matrix
# ----------------------------------------------------------------------------


N <- nrow(na.omit(int_data))
N_hrdt <- nrow(na.omit(int_data[ ,'Cardiac Sensitivity']))
N_extero <- nrow(na.omit(int_data[ ,'Auditory Sensitivity']))
N_rrst <- nrow(na.omit(int_data[ ,'Respiratory Sensitivity']))

# Scatterplot Matrix 
ggpairs(
  data = int_data,
  lower = list(continuous = "smooth"),
  upper = list(continuous = wrap("cor", size = 7)),
  diag  = list(continuous = "densityDiag"),
  aes(alpha = 0.7)
) +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 16, angle = 50, hjust = 1),  # x-axis text size
    axis.text.y = element_text(size = 16),  # y-axis text size
    strip.text = element_text(size = 18, color = "black"),
    plot.title = element_text(size = 24, face = "bold"),  # title size
    plot.subtitle = element_text(size = 20)  # subtitle size
  ) +
  labs(title = "Interoception & Exteroception Type-1 Variable Scatterplot Matrix",
       subtitle = paste("Pairwise relationships among Respiratory (RRST), Cardiac (HRDT-Intero) and Auditory (HRDT-Extero) variables"))

# Save the scatterplot matrix
ggsave("figs/scatter_matrix_type1.png", width = 20, height = 20)

# ----------------------------------------------------------------------------
# APA Style Correlation Matrix Table
# ----------------------------------------------------------------------------

# Generate and export an APA style formatted correlation matrix to a Word file.
apa.cor.table(int_data, filename = "docs/APA_Correlation_Matrix_Type1.doc", table.number = 1)

