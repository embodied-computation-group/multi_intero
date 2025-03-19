# Initialize the nRatings
nRatings <- 4  # Adjust based on your confidence quartiles

# Get unique participants
participants <- unique(selected_data$participant_id)

# Initialize empty lists to store counts for nR_S1 and nR_S2
nR_S1_list <- list()
nR_S2_list <- list()

# Loop through each participant
for (p in participants) {
  # Subset data for the participant
  participant_data <- selected_data %>% filter(participant_id == p)
  
  # Extract required inputs for trials2counts
  stimID <- participant_data$Condition  # Stimulus: 0 = S1, 1 = S2
  response <- participant_data$Decision  # Response: 0 = S1, 1 = S2
  rating <- participant_data$ConfidenceQuartile  # Confidence (1 to nRatings)
  
  # Call trials2counts
  counts <- trials2counts(stimID, response, rating, nRatings)
  
  # Store the results
  nR_S1_list[[p]] <- counts$nR_S1
  nR_S2_list[[p]] <- counts$nR_S2
}

# Combine the results into data frames
nR_S1 <- do.call(cbind, nR_S1_list)
nR_S2 <- do.call(cbind, nR_S2_list)

# Name the columns by participant
colnames(nR_S1) <- participants
colnames(nR_S2) <- participants

# Output the results
print("nR_S1:")
print(nR_S1)

print("nR_S2:")
print(nR_S2)
