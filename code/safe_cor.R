safe_cor <- function(x, y) {
  # Convert to numeric
  x_num <- suppressWarnings(as.numeric(x))
  y_num <- suppressWarnings(as.numeric(y))
  
  # Identify rows with both values not NA
  complete_idx <- which(!is.na(x_num) & !is.na(y_num))
  
  # If fewer than 2 complete pairs, return NA
  if (length(complete_idx) < 2) {
    return(NA_real_)
  } else {
    return(cor(x_num[complete_idx], y_num[complete_idx]))
  }
}

phi_hrd <- hrd_data %>%
  group_by(participant_id) %>%
  summarize(phi_hrd = safe_cor(Confidence, ResponseCorrect),
            .groups = "drop")

phi_rrst <- rrst_data %>%
  group_by(participant_id) %>%
  summarize(phi_rrst = safe_cor(Confidence, ResponseCorrect),
            .groups = "drop")

# Join the two, possibly with all participants
phi_df <- full_join(phi_hrd, phi_rrst, by = "participant_id")

# Inspect how many participants have non-NA correlations for both tasks
phi_df %>%
  filter(!is.na(phi_hrd) & !is.na(phi_rrst)) %>%
  count()
