plot_confidence_sd_hist <- function(data, confidence_col = "Confidence", binwidth = 1, title = "Confidence SD Distribution") {
  library(dplyr)
  
  subject_sd <- data %>%
    group_by(participant_id) %>%
    summarize(confidence_sd = sd(.data[[confidence_col]], na.rm = TRUE))
  
  ggplot(subject_sd, aes(x = confidence_sd)) +
    geom_histogram(binwidth = binwidth, color = "black", fill = "lightblue") +
    labs(
      x = "Standard Deviation of Confidence Ratings",
      y = "Number of Subjects",
      title = title
    )
}

# Example usage:
a <- plot_confidence_sd_hist(hrd_data, title = "HRD Data - Confidence SD")
b <- plot_confidence_sd_hist(rrst_data, title = "RRST Data - Confidence SD")

# Combine plots with separate titles
a + b