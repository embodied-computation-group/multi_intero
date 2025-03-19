# helper function for metacognition model
# Micah G. Allen, 2025
# note that N ratings is hard coded to 4... change if using different binning strategy.

process_task <- function(task_data, nRatings = 4) {
  library(dplyr)
  
  # Get unique participant IDs
  participants <- unique(task_data$participant_id)
  
  # Initialize lists to store participant-level counts
  nR_S1_list <- list()
  nR_S2_list <- list()
  
  # Loop through each participant
  for (p in participants) {
    # Subset data for the participant
    participant_data <- task_data %>% filter(participant_id == p)
    
    # Extract inputs for trials2counts
    stimID <- participant_data$Signal
    response <- participant_data$Response
    rating <- participant_data$ConfidenceQuartile
    
    # Call trials2counts
    counts <- trials2counts(stimID, response, rating, nRatings)
    
    # Append counts to the lists
    nR_S1_list[[p]] <- counts[[1]]
    nR_S2_list[[p]] <- counts[[2]]
  }
  
  # Combine counts into data frames where each column is a participant
  nR_S1 <- as.data.frame(do.call(cbind, nR_S1_list))
  nR_S2 <- as.data.frame(do.call(cbind, nR_S2_list))
  
  # Name the columns by participant IDs
  colnames(nR_S1) <- participants
  colnames(nR_S2) <- participants
  
  # Return the results
  return(list(nR_S1 = nR_S1, nR_S2 = nR_S2))
}
