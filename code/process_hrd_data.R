process_hrd_data <- function(hrd_data) {
  # Process the raw HRD data and bin confidence ratings
  temp <- hrd_data %>%
    dplyr::select(participant_id, nTrials, Condition, Decision, Confidence) %>% 
    mutate(
      Signal = if_else(Condition == "More", 1, 0),
      Response = if_else(Decision == "More", 1, 0),
      Confidence = as.numeric(Confidence)
    ) %>%
    group_by(participant_id) %>% 
    mutate(ConfidenceQuartile = bin_confidence_quantiles(Confidence, 4)) %>% 
    ungroup()
  
  # Identify participants for whom binning failed (i.e. all NA)
  excluded_subjects <- temp %>%
    group_by(participant_id) %>%
    summarize(all_na = all(is.na(ConfidenceQuartile))) %>%
    filter(all_na) %>%
    pull(participant_id)
  
  cat("Excluded", length(excluded_subjects), "subjects for bad bins:\n")
  print(excluded_subjects)
  
  # Exclude those subjects and remove any remaining NA rows
  processed_hrd_data <- temp %>%
    filter(!participant_id %in% excluded_subjects) %>%
    na.omit()
  
  return(processed_hrd_data)
}
