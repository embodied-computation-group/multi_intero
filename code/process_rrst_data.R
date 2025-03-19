process_rrst_data <- function(rrst_data, all_bad_subjects = NULL) {
  library(dplyr)
  
  # Exclude pre-identified bad subjects if provided
  if (!is.null(all_bad_subjects)) {
    rrst_data <- rrst_data %>% filter(!participant_id %in% all_bad_subjects)
  }
  
  temp <- rrst_data %>%
    mutate(Confidence = as.numeric(Confidence)) %>%    # Ensure Confidence is numeric
    group_by(participant_id) %>% 
    mutate(ConfidenceQuartile = bin_confidence_quantiles(Confidence, 4)) %>%  # Bin confidence into quartiles per subject
    ungroup() %>% 
    mutate(
      Signal = if_else(SignalInterval == 1, 0, 1),        # Convert SignalInterval to 0/1
      Response = if_else(Decision == 1, 1, 0)             # Convert Decision to 0/1
    ) %>% 
    dplyr::select(participant_id, Signal, Response, ResponseCorrect, ConfidenceQuartile)
  
  # Identify subjects whose binning failed (i.e., all ConfidenceQuartile values are NA)
  excluded_subjects <- temp %>%
    group_by(participant_id) %>%
    summarize(all_na = all(is.na(ConfidenceQuartile))) %>%
    filter(all_na) %>%
    pull(participant_id)
  
  cat("Excluded", length(excluded_subjects), "subjects for bad bins:\n")
  print(excluded_subjects)
  
  processed_rrst_data <- temp %>%
    filter(!participant_id %in% excluded_subjects) %>%
    na.omit()
  
  return(processed_rrst_data)
}
