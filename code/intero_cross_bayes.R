## Script to calculate Null Bayes Factors for all pairwise correlations, and make a heatmap.
## Leah Banellis & Micah G. Allen, 2025

# Load required libraries
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(readr)
library(dplyr)
library(BayesFactor)

# Read data. 
alldata <- read_csv("data/mergedvmpdata_all.csv")
# load metacognition data
metad_indiv <- read.csv('~/Git/multi-intero/data/metad_indivfits.csv')
# merge
alldata <- merge(alldata, metad_indiv, by.x = "participant_id", by.y = "V1", all = TRUE)
cat("Data successfully read. Dimensions:", dim(alldata), "\n\n")

# Subset interoception & exteroception variables (RRST and HRD measures)
cat("Subsetting interoception variables...\n")
int_data <- alldata %>%
  dplyr::select(
    rrst_estimated_mean_alpha,
    rrst_estimated_mean_beta,
    rrst_mean_confidence,
    mratio_rrst,
    hrd_estimated_mean_alpha,
    hrd_estimated_mean_beta,
    hrd_mean_confidence,
    mratio_hrd
  )

# compute absolute hrd (intero and extero) threshold 
int_data[ ,'hrd_estimated_mean_alpha'] <- abs(int_data[ ,'hrd_estimated_mean_alpha'])

# Invert scale of hrd/extero slope (so higher = steeper slope (same as RRST)) (for both intero & extero)
int_data[ ,'hrd_estimated_mean_beta'] <- -(int_data[ ,'hrd_estimated_mean_beta'])

# Rename for clarity
int_data <- int_data %>%
  rename(
    "Respiratory Confidence" = rrst_mean_confidence,
    "Respiratory Metacognition" = mratio_rrst,
    "Respiratory Precision" = rrst_estimated_mean_beta,
    "Respiratory Sensitivity" = rrst_estimated_mean_alpha,
    "Cardiac Confidence" = hrd_mean_confidence,
    "Cardiac Metacognition" = mratio_hrd,
    "Cardiac Precision" = hrd_estimated_mean_beta,
    "Cardiac Sensitivity" = hrd_estimated_mean_alpha
  )
cat("Subset successful. Dimensions:", dim(int_data), "\n\n")


# Replace negative M-Ratio values with NA 
cols_to_modify <- c("Cardiac Metacognition", "Respiratory Metacognition")
int_data[cols_to_modify] <- lapply(int_data[cols_to_modify], function(x) ifelse(x < 0, NA, x))

#cols_to_modify <- colnames(int_data)
# remove high outliers from M-Ratio (MAD-based outlier removal to selected columns)
int_data[cols_to_modify] <- lapply(int_data[cols_to_modify], function(x) {
  median_x <- median(x, na.rm = TRUE)
  mad_x <- mad(x, na.rm = TRUE)
  x[abs(x - median_x) > 3 * mad_x] <- NA
  return(x) 
})


# Function to compute Bayes factors favoring the null (BF01)
cat("Computing Bayes factors (BF01)...\n")
make_bf_mat_null <- function(df) {
  df <- as.data.frame(df)
  n <- ncol(df)
  if (n == 0) stop("Error: No columns available for computing Bayes factors.")
  BF.mat <- matrix(NA, n, n)
  colnames(BF.mat) <- colnames(df)
  rownames(BF.mat) <- colnames(df)
  
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tryCatch({
        bf_object <- correlationBF(x = df[[i]], y = df[[j]])
        bf_10 <- extractBF(bf_object)$bf   # BF10
        bf_01 <- 1 / bf_10                 # BF01 = reciprocal
        BF.mat[i, j] <- bf_01
        BF.mat[j, i] <- bf_01
      }, error = function(e) {
        cat(sprintf("Warning: Error computing BF for columns %s and %s: %s\n",
                    colnames(df)[i], colnames(df)[j], e$message))
      })
    }
  }
  diag(BF.mat) <- NA
  BF.mat
}

bf01_df <- tryCatch(
  as.data.frame(make_bf_mat_null(int_data)),
  error = function(e) {
    stop("Error: Failed to compute BF01 matrix. Check your data for issues.")
  }
)
cat("BF01 matrix successfully computed.\n\n")


# remove upper tri for plot
bf01_df[upper.tri(bf01_df)]<- NA

# Prepare data for heatmap
cat("Step 5: Preparing data for heatmap...\n")
bf01_df$Row <- rownames(bf01_df)

# Reshape data into long format and remove NA diagonal values
bf01_long <- bf01_df %>%
  melt(id.vars = "Row", variable.name = "Column", value.name = "Value") %>%
  filter(!is.na(Value))  # Remove rows with NA values (diagonal)

# Reorder the factors to ensure proper descending order in both axes
bf01_long$Row <- factor(bf01_long$Row, levels = rev(rownames(bf01_df)))
bf01_long$Column <- factor(bf01_long$Column, levels = colnames(bf01_df)[-ncol(bf01_df)])  # Exclude 'Row'

# Set the factor levels for Row and Column explicitly
bf01_long$Row <- factor(bf01_long$Row, levels = rev(levels(bf01_long$Column)))
bf01_long$Column <- factor(bf01_long$Column, levels = levels(bf01_long$Column))


# 6) Create heatmap
heatmap_plot <- ggplot(bf01_long, aes(x = Column, y = Row, fill = Value)) +
  geom_tile(color = "grey") +
  geom_text(aes(label = sprintf("%.2f", Value)), size = 3, color = "black") +
  scale_fill_gradientn(
    colors = brewer.pal(9, "YlGnBu")[1:7],  # Unidimensional color scale 
    values = scales::rescale(c(min(bf01_long$Value), 3, max(bf01_long$Value))), 
    name = "BF01"
  ) +
  labs(title = "Null Bayes Factor Heatmap", x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  ) +
  coord_fixed()

# Print the heatmap
print(heatmap_plot)


# Save the heatmap
tryCatch({
  ggsave(filename = "figs/intero_corr_bayes.png", height = 7, width = 7, bg = 'white')
  cat("Heatmap saved successfully.\n")
}, error = function(e) {
  cat("Error: Failed to save heatmap. Check your file system permissions.\n")
})
