# Script to plot pairwise correlations for all intero/extero variables.
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

# Read your data. 
alldata <- read_csv("data/mergedvmpdata_all.csv")
# load metacognition data
metad_indiv <- read.csv('~/Git/multi-intero/data/metad_indivfits.csv')
# merge
alldata <- merge(alldata, metad_indiv, by.x = "participant_id", by.y = "V1", all = TRUE)
#colSums(!is.na(alldata))

# Subset only the RRST and HRD variables.
int_data <- alldata %>%
  dplyr::select(
    #rrst_Accuracy,
    rrst_estimated_mean_alpha,
    rrst_estimated_mean_beta,
    rrst_mean_confidence,
    mratio_rrst,
    #hrd_Accuracy,
    hrd_estimated_mean_alpha,
    hrd_estimated_mean_beta,
    hrd_mean_confidence,
    mratio_hrd,
    #extero_Accuracy,
    extero_estimated_mean_alpha,
    extero_estimated_mean_beta,
    extero_mean_confidence,
    mratio_extero
  )

#colSums(!is.na(int_data))

# # Loop through each column in int_data and plot histograms
# for (col in names(int_data)) {
#   hist(na.omit(int_data[[col]]), main = col, xlab = "Value", col = "lightblue", border = "black")
# }

# compute absolute hrd (intero and extero) threshold 
int_data[ ,'hrd_estimated_mean_alpha'] <- abs(int_data[ ,'hrd_estimated_mean_alpha'])
int_data[ ,'extero_estimated_mean_alpha'] <- abs(int_data[ ,'extero_estimated_mean_alpha'])

# Invert scale of hrd/extero slope (so higher = steeper slope (same as RRST)) (for both intero & extero)
int_data[ ,'hrd_estimated_mean_beta'] <- -(int_data[ ,'hrd_estimated_mean_beta'])
int_data[ ,'extero_estimated_mean_beta'] <- -(int_data[ ,'extero_estimated_mean_beta'])

# Rename for clarity
int_data <- int_data %>%
  rename(
    #"Respiratory Accuracy" = rrst_Accuracy,
    "Respiratory Sensitivity" = rrst_estimated_mean_alpha,
    "Respiratory Precision" = rrst_estimated_mean_beta,
    "Respiratory Confidence" = rrst_mean_confidence,
    "Respiratory Metacognition" = mratio_rrst,
    #"Cardiac Accuracy" = hrd_Accuracy,
    "Cardiac Sensitivity" = hrd_estimated_mean_alpha,
    "Cardiac Precision" = hrd_estimated_mean_beta,
    "Cardiac Confidence" = hrd_mean_confidence,
    "Cardiac Metacognition" = mratio_hrd,
    #"Auditory Accuracy" = extero_Accuracy,
    "Auditory Sensitivity" = extero_estimated_mean_alpha,
    "Auditory Precision" = extero_estimated_mean_beta,
    "Auditory Confidence" = extero_mean_confidence,
    "Auditory Metacognition" = mratio_extero
  )

# Replace negative M-Ratio values with NA 
cols_to_modify <- c("Cardiac Metacognition", "Auditory Metacognition", "Respiratory Metacognition")
int_data[cols_to_modify] <- lapply(int_data[cols_to_modify], function(x) ifelse(x < 0, NA, x))

#cols_to_modify <- colnames(int_data)
# remove high outliers from M-Ratio (MAD-based outlier removal)
int_data[cols_to_modify] <- lapply(int_data[cols_to_modify], function(x) {
  median_x <- median(x, na.rm = TRUE)
  mad_x <- mad(x, na.rm = TRUE)
  x[abs(x - median_x) > 3 * mad_x] <- NA
  return(x) 
})


# Loop through each column in int_data and plot histograms
for (col in names(int_data)) {
  hist(na.omit(int_data[[col]]), main = col, xlab = "Value", col = "lightblue", border = "black")
}

# ----------------------------------------------------------------------------
# Figure 1: Scatterplot Matrix
# ----------------------------------------------------------------------------

#N <- nrow(na.omit(int_data))
N_hrdt <- sum(!is.na(int_data[ ,"Cardiac Confidence"])) # same as thresh & slope
N_extero <- sum(!is.na(int_data[ ,"Auditory Confidence"])) # same as thresh & slope
N_rrst <- sum(!is.na(int_data[ ,"Respiratory Confidence"])) # same as thresh & slope
N_hrdt_metacog <- sum(!is.na(int_data[ ,"Cardiac Metacognition"]))
N_extero_metacog <- sum(!is.na(int_data[ ,"Auditory Metacognition"]))
N_rrst_metacog <- sum(!is.na(int_data[ ,"Respiratory Metacognition"]))


# full FDR-corrected corr matrix
library(ggcorrplot)
cormatrix_full <- function(cordata, title, subtitle) {
  pval_fdr <- psych::corr.test(cordata, method = "pearson", adjust = "fdr")$p
  rval_fdr <- psych::corr.test(cordata, method = "pearson", adjust = "fdr")$r
  
  cp1 <- ggcorrplot(rval_fdr, p.mat = pval_fdr, hc.order=FALSE,
                    type='full', insig='blank', method = "square", 
                    sig.level = 0.05, outline.col = "black",
                    colors = c("#6D9EC1", "white", "#EA5F21FF"),
                    ggtheme = theme_minimal()) +  # Apply theme inside ggcorrplot()
    geom_hline(yintercept = ncol(rval_fdr) + .5, linetype = 3, size = 1) +
    geom_vline(xintercept = .5, linetype = 3, size = 1) +
    geom_abline(intercept = 0, slope = 1, linetype = 3, size = 1) +
    ggtitle(title) +
    labs(subtitle = subtitle) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),  # Set full white bg
      panel.background = element_rect(fill = "white", color = NA), # Ensure no grey panels
      axis.text.x = element_text(hjust = 1, size = 11),  # Adjust label text size
      axis.text.y = element_text(size = 11),  # Adjust y-axis label size
      plot.title = element_text(size = 14, face = "bold"),  # Bigger title
      plot.subtitle = element_text(size = 12)  # Bigger subtitle
    )
  
  return(cp1)
}

p_fdr <- round(psych::corr.test(int_data, method = "pearson", adjust = "fdr")$p,3)
r_fdr <- round(psych::corr.test(int_data, method = "pearson", adjust = "fdr")$r,3)

cormatrix_full(int_data, "Interoception & Exteroception Correlation Matrix", "Pearson Correlations, upper tri FDR-corrected")
# Save the plot using ggsave
ggsave(filename = "~/Git/multi-intero/figs/cormatrix_all.png", width = 8, height = 8, dpi = 300)


# Generate and export an APA style formatted correlation matrix to a Word file.
apa.cor.table(int_data, filename = "docs/APA_Correlation_Matrix_All.doc", table.number = 1)


# Descriptives Table
library(flextable)

desc_stats <- data.frame(
  Variable = colnames(int_data),
  Mean = round(colMeans(int_data, na.rm = TRUE), 3),
  SD = round(apply(int_data, 2, sd, na.rm = TRUE), 3),
  Median = round(apply(int_data, 2, median, na.rm = TRUE), 3),
  Range = apply(int_data, 2, function(x) paste0(round(min(x, na.rm = TRUE), 3), " - ", round(max(x, na.rm = TRUE), 3))),
  CI_95 = apply(int_data, 2, function(x) {
    x <- na.omit(x)
    ci <- t.test(x)$conf.int  # Compute 95% CI
    paste0(round(ci[1], 3), ", ", round(ci[2], 3))
  })
)

# Format the table in APA style
apa_table <- flextable(desc_stats) %>%
  set_header_labels(Variable = "Variable", Mean = "Mean", SD = "Standard Deviation", CI_95 = "95% CI") %>%
  autofit()

# Print the table
apa_table
save_as_docx(apa_table, path = "docs/APA_Descriptives_fullsample.docx")


# rename to fit on plot
int_data <- int_data %>%
  rename(
    "Resp Sensitivity" = "Respiratory Sensitivity",
    "Resp Precision" = "Respiratory Precision",
    "Resp Confidence" = "Respiratory Confidence",
    "Resp Metacognition" = "Respiratory Metacognition",
    "Card Sensitivity" = "Cardiac Sensitivity",
    "Card Precision" = "Cardiac Precision",
    "Card Confidence" = "Cardiac Confidence",
    "Card Metacognition" = "Cardiac Metacognition",
    "Aud Sensitivity" = "Auditory Sensitivity",
    "Aud Precision" = "Auditory Precision",
    "Aud Confidence" = "Auditory Confidence",
    "Aud Metacognition" = "Auditory Metacognition"
  )
# Improved scatterplot matrix with adjustments
ggpairs(
  data = int_data,
  lower = list(continuous = "smooth"),
  #upper = list(continuous = "cor"),
  upper = list(continuous = wrap("cor", size = 6)),
  diag  = list(continuous = "densityDiag"),
  aes(alpha = 0.7)
) +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 14, angle = 50, hjust = 1),  # Increase x-axis text size
    axis.text.y = element_text(size = 14),  # Increase y-axis text size
    strip.text = element_text(size = 11, color = "black"),
    plot.title = element_text(size = 18, face = "bold"),  # Adjust title size
    plot.subtitle = element_text(size = 15)  # Adjust subtitle size
  ) +
  labs(title = "Interoception & Exteroception Variable Scatterplot Matrix",
       subtitle = paste("Pairwise relationships among Respiratory (RRST), Cardiac (HRDT-Intero) and Auditory (HRDT-Extero) variables"))

# Save the scatterplot matrix
ggsave("figs/scatter_matrix_ALL.png", width = 20, height = 20)

