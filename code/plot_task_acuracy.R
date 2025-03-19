plot_mean_accuracy_hist <- function(data, accuracy_col = "ResponseCorrect", binwidth = 0.01, title = "Mean Accuracy Distribution") {
  library(dplyr)
  library(ggplot2)
  
  # Ensure ResponseCorrect is numeric
  data <- data %>%
    mutate(!!accuracy_col := as.numeric(.data[[accuracy_col]]))
  
  subject_accuracy <- data %>%
    group_by(participant_id) %>%
    summarize(mean_accuracy = mean(.data[[accuracy_col]], na.rm = TRUE))
  
  ggplot(subject_accuracy, aes(x = mean_accuracy)) +
    geom_histogram(binwidth = binwidth, color = "black", fill = "lightblue") +
    labs(
      x = "Mean Accuracy",
      y = "Number of Subjects",
      title = title
    ) +
    scale_x_continuous(limits = c(0, 1))  # Accuracy should be between 0 and 1
}

# Example usage:
a <- plot_mean_accuracy_hist(hrd_data, title = "HRD Data - Mean Accuracy")
b <- plot_mean_accuracy_hist(rrst_data, title = "RRST Data - Mean Accuracy")

# Combine plots with separate titles
library(patchwork)
a + b
