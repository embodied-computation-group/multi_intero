identify_bad_subjects <- function(data, min_rating = 1, max_rating = 100, threshold = 0.95) {
  library(dplyr)
  
  bad_subjects <- data %>%
    group_by(participant_id) %>%
    summarize(extreme_fraction = mean(Confidence %in% c(min_rating, max_rating), na.rm = TRUE)) %>%
    filter(extreme_fraction > threshold) %>%
    pull(participant_id)
  
  cat("Found", length(bad_subjects), "subjects with extreme confidence ratings.\n")
  return(bad_subjects)
}