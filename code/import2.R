# Get unique participant IDs
participants <- unique(selected_data$participant_id)

# Initialize lists to store counts
nR_S1_list <- list()
nR_S2_list <- list()

# Loop through each participant
for (p in participants) {
  # Subset data for the participant
  participant_data <- selected_data %>% filter(participant_id == p)
  
  # Extract inputs for trials2counts
  stimID <- participant_data$Condition
  response <- participant_data$Decision
  rating <- participant_data$ConfidenceQuartile
  
  # Call trials2counts
  counts <- trials2counts(stimID, response, rating, nRatings = 4)
  
  # Store results in the lists (each participant as a row)
  nR_S1_list[[p]] <- counts[[1]]
  nR_S2_list[[p]] <- counts[[2]]
}

# Combine results into data frames (each row is a participant)
nR_S1 <- do.call(rbind, nR_S1_list)
nR_S2 <- do.call(rbind, nR_S2_list)

# Add participant IDs as row names
rownames(nR_S1) <- participants
rownames(nR_S2) <- participants

# Output the results
print("nR_S1:")
print(nR_S1)

print("nR_S2:")
print(nR_S2)
