reject_low_confidence_sd <- function(data, confidence_col = "Confidence", percentile_threshold = 5) {
  library(dplyr)
  
  # Ensure Confidence column is numeric
  data <- data %>%
    mutate(!!confidence_col := suppressWarnings(as.numeric(.data[[confidence_col]])))
  
  # Compute standard deviation of confidence for each subject
  subject_sd <- data %>%
    group_by(participant_id) %>%
    summarize(confidence_sd = sd(.data[[confidence_col]], na.rm = TRUE), .groups = "drop")
  
  # Remove subjects with all NA confidence values
  subject_sd <- subject_sd %>%
    filter(!is.na(confidence_sd))
  
  # Determine the threshold based on the given percentile
  sd_threshold <- quantile(subject_sd$confidence_sd, probs = percentile_threshold / 100, na.rm = TRUE)
  
  # Identify subjects below the threshold
  low_sd_subjects <- subject_sd %>%
    filter(confidence_sd <= sd_threshold) %>%
    pull(participant_id)
  
  cat("Rejecting", length(low_sd_subjects), "subjects with low confidence SD (below", sd_threshold, ")\n")
  
  return(low_sd_subjects)
}

reject_low_accuracy <- function(data, accuracy_col = "ResponseCorrect", accuracy_threshold = 0.6) {
  library(dplyr)
  
  # Ensure ResponseCorrect column is numeric
  data <- data %>%
    mutate(!!accuracy_col := suppressWarnings(as.numeric(.data[[accuracy_col]])))
  
  # Compute mean accuracy for each subject
  subject_accuracy <- data %>%
    group_by(participant_id) %>%
    summarize(mean_accuracy = mean(.data[[accuracy_col]], na.rm = TRUE), .groups = "drop")
  
  # Remove subjects with all NA accuracy values
  subject_accuracy <- subject_accuracy %>%
    filter(!is.na(mean_accuracy))
  
  # Identify subjects below the accuracy threshold
  low_accuracy_subjects <- subject_accuracy %>%
    filter(mean_accuracy < accuracy_threshold) %>%
    pull(participant_id)
  
  cat("Rejecting", length(low_accuracy_subjects), "subjects with mean accuracy below", accuracy_threshold * 100, "%\n")
  
  return(low_accuracy_subjects)
}
