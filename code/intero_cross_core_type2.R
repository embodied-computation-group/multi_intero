# Script to plot pairwise correlations for metacognitice (type 2) intero & extero variables.
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

# Read data.
alldata <- read_csv("data/mergedvmpdata_all.csv")
# load metacognition 
metad_indiv <- read.csv('~/Git/multi-intero/data/metad_indivfits.csv')

# merge
alldata <- merge(alldata, metad_indiv, by.x = "participant_id", by.y = "V1", all = TRUE)

# Subset only the RRST and HRD variables.
int_data <- alldata %>%
  dplyr::select(
    rrst_mean_confidence,
    mratio_rrst,
    hrd_mean_confidence,
    mratio_hrd,
    extero_mean_confidence,
    mratio_extero,
  )

# Rename for clarity
int_data <- int_data %>%
  rename(
    "Respiratory Confidence" = rrst_mean_confidence,
    "Respiratory Metacognition" = mratio_rrst,
    "Cardiac Confidence" = hrd_mean_confidence,
    "Cardiac Metacognition" = mratio_hrd,
    "Auditory Confidence" = extero_mean_confidence,
    "Auditory Metacognition" = mratio_extero,
  )

# Replace negative M-Ratio values with NA 
cols_to_modify <- c("Cardiac Metacognition", "Auditory Metacognition", "Respiratory Metacognition")
int_data[cols_to_modify] <- lapply(int_data[cols_to_modify], function(x) ifelse(x < 0, NA, x))

# remove high outliers from M-Ratio (MAD-based outlier removal)
int_data[cols_to_modify] <- lapply(int_data[cols_to_modify], function(x) {
  median_x <- median(x, na.rm = TRUE)
  mad_x <- mad(x, na.rm = TRUE)
  x[abs(x - median_x) > 3 * mad_x] <- NA
  return(x) 
})

# ----------------------------------------------------------------------------
# Figure 1: Scatterplot Matrix
# ----------------------------------------------------------------------------


#N <- nrow(na.omit(int_data))
N_hrdt <- sum(!is.na(int_data[ ,"Cardiac Confidence"]))
N_extero <- sum(!is.na(int_data[ ,"Auditory Confidence"]))
N_rrst <- sum(!is.na(int_data[ ,"Respiratory Confidence"]))
N_hrdt_metacog <- sum(!is.na(int_data[ ,"Cardiac Metacognition"]))
N_extero_metacog <- sum(!is.na(int_data[ ,"Auditory Metacognition"]))
N_rrst_metacog <- sum(!is.na(int_data[ ,"Respiratory Metacognition"]))


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
    axis.text.x = element_text(size = 16, angle = 50, hjust = 1),  # Increase x-axis text size
    axis.text.y = element_text(size = 16),  # Increase y-axis text size
    strip.text = element_text(size = 16, color = "black"),
    plot.title = element_text(size = 24, face = "bold"),  # Adjust title size
    plot.subtitle = element_text(size = 20)  # Adjust subtitle size
  ) +
  labs(title = "Interoception & Exteroception Type-2 Variable Scatterplot Matrix",
       subtitle = paste("Pairwise relationships among Respiratory (RRST), Cardiac (HRDT-Intero) and Auditory (HRDT-Extero) variables") #"( crossmodal N =", N, ", HRDT/Extero N =", N_hrdt, ", RRST N =", N_rrst,")
  )

# Save the scatterplot matrix
ggsave("figs/scatter_matrix_type2.png", width = 20, height = 20)


# ----------------------------------------------------------------------------
# APA Style Correlation Matrix Table
# ----------------------------------------------------------------------------

# Generate and export an APA style formatted correlation matrix to a Word file.
apa.cor.table(int_data, filename = "docs/APA_Correlation_Matrix_Type2.doc", table.number = 1)
