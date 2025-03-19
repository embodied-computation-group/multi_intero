library(dplyr)
library(readr)

get_all_bad_subjects <- function(hrd_badid_path, rrst_badid_path, hrd_data, rrst_data,
                                 exclude_badid = TRUE,
                                 exclude_low_conf_sd = TRUE,
                                 exclude_low_accuracy = TRUE) {
  all_excluded <- character(0)
  
  if (exclude_badid) {
    hrd_badid <- read_csv(hrd_badid_path)
    rrst_badid <- read_csv(rrst_badid_path)
    
    hrd_bad_subjects <- hrd_badid %>%
      group_by(participant_id) %>%
      summarize(is_bad = any(bad_ids == 1)) %>%
      filter(is_bad) %>%
      pull(participant_id)
    
    rrst_bad_subjects <- rrst_badid %>%
      group_by(participant_id) %>%
      summarize(is_bad = any(bad_ids == 1)) %>%
      filter(is_bad) %>%
      pull(participant_id)
    
    all_excluded <- union(all_excluded, union(hrd_bad_subjects, rrst_bad_subjects))
  }
  
  if (exclude_low_conf_sd) {
    bad_subjects_low_sd_hrd <- reject_low_confidence_sd(hrd_data)
    bad_subjects_low_sd_rrst <- reject_low_confidence_sd(rrst_data)
    all_excluded <- union(all_excluded, union(bad_subjects_low_sd_hrd, bad_subjects_low_sd_rrst))
  }
  
  if (exclude_low_accuracy) {
    bad_subjects_low_accuracy_hrd <- reject_low_accuracy(hrd_data)
    bad_subjects_low_accuracy_rrst <- reject_low_accuracy(rrst_data)
    all_excluded <- union(all_excluded, union(bad_subjects_low_accuracy_hrd, bad_subjects_low_accuracy_rrst))
  }
  
  cat("Total excluded subjects:", length(all_excluded), "\n")
  print(all_excluded)
  
  return(all_excluded)
}
