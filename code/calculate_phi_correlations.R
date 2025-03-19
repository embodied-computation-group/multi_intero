calculate_phi_correlations <- function(hrd_data, rrst_data) {
  library(dplyr)
  
  # Helper function: returns NA if fewer than 2 valid numeric pairs
  safe_cor <- function(x, y) {
    x_num <- as.numeric(x)
    y_num <- as.numeric(y)
    valid_idx <- which(!is.na(x_num) & !is.na(y_num))
    if (length(valid_idx) < 2) {
      return(NA_real_)
    } else {
      return(cor(x_num[valid_idx], y_num[valid_idx]))
    }
  }
  
  # Compute within-subject correlation of Confidence & Accuracy for HRD
  phi_hrd <- hrd_data %>%
    group_by(participant_id) %>%
    summarize(phi_hrd = safe_cor(Confidence, ResponseCorrect), .groups = "drop")
  
  # Compute within-subject correlation of Confidence & Accuracy for RRST
  phi_rrst <- rrst_data %>%
    group_by(participant_id) %>%
    summarize(phi_rrst = safe_cor(Confidence, ResponseCorrect), .groups = "drop")
  
  # Join results by participant_id
  full_join(phi_hrd, phi_rrst, by = "participant_id")
}

# Example usage:
# (Assuming you have already cleaned/converted Confidence and ResponseCorrect to numeric or 0/1.)
# phi_df <- calculate_phi_correlations(hrd_data, rrst_data)
