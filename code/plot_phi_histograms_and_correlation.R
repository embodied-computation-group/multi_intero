plot_phi_histograms_and_correlation <- function(phi_df) {
  library(dplyr)
  library(ggplot2)
  library(patchwork) # For combining plots
  
  # Remove any rows where either phi is NA
  phi_df_clean <- phi_df %>%
    drop_na(phi_hrd, phi_rrst)
  
  # Plot histogram of phi_hrd
  p1 <- ggplot(phi_df_clean, aes(x = phi_hrd)) +
    geom_histogram(binwidth = 0.05, color = "black", fill = "lightblue") +
    labs(
      title = "Histogram of phi_hrd",
      x = "phi_hrd",
      y = "Count"
    ) +
    xlim(-1, 1)
  
  # Plot histogram of phi_rrst
  p2 <- ggplot(phi_df_clean, aes(x = phi_rrst)) +
    geom_histogram(binwidth = 0.05, color = "black", fill = "lightblue") +
    labs(
      title = "Histogram of phi_rrst",
      x = "phi_rrst",
      y = "Count"
    ) +
    xlim(-1, 1)
  
  # Correlation test
  cor_val <- cor(phi_df_clean$phi_hrd, phi_df_clean$phi_rrst, use = "complete.obs")
  cat("Correlation between phi_hrd and phi_rrst:", cor_val, "\n")
  
  # Return a combined plot of the two histograms
  p1 + p2
}

# Example usage, given phi_df from previous function:
# phi_df <- calculate_phi_correlations(hrd_data, rrst_data)
# plot_phi_histograms_and_correlation(phi_df)
