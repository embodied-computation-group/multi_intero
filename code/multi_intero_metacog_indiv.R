## this script preprocesses the data and runs the multi-intero metacognition analyses using the metad indiv model
## Leah Banellis & Micah G. Allen, 2025

library(readr)
library(tidyverse)
library(ggpubr)

# Source the custom functions from the 'code/' directory:
source("code/get_all_bad_subjects.R")
source("code/process_hrd_data.R")
source("code/process_rrst_data.R")
source("code/process_task.R")
source("code/fit_metad_groupcorr.R")
source("code/subject_cleaning.R")
source("code/bin_confidence_quantiles.R")
source("HMeta-d/R/trials2counts.R")


# Load data
hrd_data <- read_csv("data/raw_hrd.csv", na = c("n/a", "NA", ""))
hrd_data <- hrd_data[hrd_data$session==1, ] # select session-1 only for hrd
rrst_data <- read_csv("data/raw_rrst.csv", na = c("n/a", "NA", ""))
#length(unique(hrd_data$participant_id))
#length(unique(rrst_data$participant_id))
#common_participants <- intersect(unique(rrst_data$participant_id), unique(hrd_data$participant_id))

## there was an error in the trial data where the accuracy was coded differently depending on cohort, the following
## corrects this
hrd_data <- hrd_data %>%
  mutate(
    # Convert Confidence to numeric everywhere
    Confidence = as.numeric(Confidence),
    
    # Handle ResponseCorrect by cohort
    ResponseCorrect = case_when(
      # vmp1 can have "0", "0.0", "1", "1.0", or "n/a"
      cohort == "vmp1" & (ResponseCorrect == "0"   | ResponseCorrect == "0.0") ~ 0,
      cohort == "vmp1" & (ResponseCorrect == "1"   | ResponseCorrect == "1.0") ~ 1,
      cohort == "vmp1" & ResponseCorrect == "n/a"                              ~ NA_real_,
      
      # vmp2 can have "False", "True", or "n/a"
      cohort == "vmp2" & ResponseCorrect == "False"                           ~ 0,
      cohort == "vmp2" & ResponseCorrect == "True"                            ~ 1,
      cohort == "vmp2" & ResponseCorrect == "n/a"                              ~ NA_real_,
      
      # Fallback for anything unexpected
      TRUE                                                                    ~ NA_real_
    )
  )

# Define file paths for bad subject IDs
hrd_badid_path  <- "~/Git/multi-intero/data/HRD_badid.csv"
rrst_badid_path <- "~/Git/multi-intero/data/rrst_badid.csv"
hrd_badid <- read.csv(hrd_badid_path)
hrd_badid <- unique(hrd_badid[hrd_badid$bad_ids==1, 'participant_id'])
rrst_badid <- read.csv(rrst_badid_path)
rrst_badid <- unique(rrst_badid[rrst_badid$bad_ids==1, 'participant_id'])

# remove bad_subjects from hrd/rrst separately
clean_hrd_data  <- hrd_data  %>% filter(!participant_id %in% hrd_badid)
clean_rrst_data <- rrst_data %>% filter(!participant_id %in% rrst_badid)
#length(unique(clean_hrd_data$participant_id))
#length(unique(clean_rrst_data$participant_id))

# # merge all bad subjects
# all_bad_subjects <- get_all_bad_subjects(hrd_badid_path, rrst_badid_path, hrd_data, rrst_data,
#                                          exclude_badid = TRUE,
#                                          exclude_low_conf_sd = FALSE,
#                                          exclude_low_accuracy = FALSE)
# 
# # Exclude the bad subjects from your datasets.
# clean_hrd_data  <- hrd_data  %>% filter(!participant_id %in% all_bad_subjects)
# clean_rrst_data <- rrst_data %>% filter(!participant_id %in% all_bad_subjects)


# separate intero and extero conditions from HRD
clean_extero_data <- clean_hrd_data[clean_hrd_data$Modality=='Extero', ]
clean_hrd_data <- clean_hrd_data[clean_hrd_data$Modality=='Intero', ]


#processed HRD data 
processed_hrd_data <- process_hrd_data(clean_hrd_data) # bin confidence ratings (0-100) into 1-4 quartiles
# View the processed data:
head(processed_hrd_data)

# Process RRST data
processed_rrst_data <- process_rrst_data(clean_rrst_data) # bin confidence ratings (0-100) into 1-4 quartiles
head(processed_rrst_data)

#processed Extero data 
processed_extero_data <- process_hrd_data(clean_extero_data) # bin confidence ratings (0-100) into 1-4 quartiles
# View the processed data:
head(processed_extero_data)

# Glimpse processed data
glimpse(processed_hrd_data)
glimpse(processed_rrst_data)
glimpse(processed_extero_data)

# Glimpse filtered data
glimpse(processed_hrd_data)
glimpse(processed_rrst_data)
glimpse(processed_extero_data)

# Process tasks
rrst_results <- process_task(processed_rrst_data, nRatings = 4) # compute counts (each response type & confidence rating pair)
hrd_results <- process_task(processed_hrd_data, nRatings = 4) # compute counts (each response type & confidence rating pair)
extero_results <- process_task(processed_extero_data, nRatings = 4) # compute counts (each response type & confidence rating pair)

# Extract results
nR_S1_rrst <- rrst_results$nR_S1 # hrdt counts for each response type (response s1 or s2) & confidence rating (1-4) for stim1
nR_S2_rrst <- rrst_results$nR_S2 # hrdt counts for each response type (response s1 or s2) & confidence rating (1-4) for stim2

nR_S1_hrd <- hrd_results$nR_S1 # rrst counts for each response type (response s1 or s2) & confidence rating (1-4) for stim1
nR_S2_hrd <- hrd_results$nR_S2 # rrst counts for each response type (response s1 or s2) & confidence rating (1-4) for stim2

nR_S1_extero <- extero_results$nR_S1 # extero counts for each response type (response s1 or s2) & confidence rating (1-4) for stim1
nR_S2_extero <- extero_results$nR_S2 # extero counts for each response type (response s1 or s2) & confidence rating (1-4) for stim2


# Fit the model (individually) - will take some time
source("HMeta-d/R/fit_metad_indiv.R")

# fit meta-d individually for HRDT
d1_hrd <- numeric(ncol(nR_S1_hrd))
metad_hrd <- numeric(ncol(nR_S1_hrd))
mratio_hrd <- numeric(ncol(nR_S1_hrd))
# fit meta-d each subj HRDT
for (p in 1:ncol(nR_S1_hrd)) {
  output_indiv_hrd <- fit_metad_indiv(nR_S1 = nR_S1_hrd[ ,p], nR_S2 = nR_S2_hrd[ ,p])
  
  ## Model output - each subj
  output = output_indiv_hrd[[1]]
  d1_hrd[p] = output_indiv_hrd[[2]]$d1
  
  # Compute mean statistics
  Value <- summary(output)
  stat <- data.frame(mean = Value[["statistics"]][, "Mean"]) %>%
    rownames_to_column(var = "name")
  
  # Compute metacognitive ratio
  metad_hrd[p] <- stat$mean[stat$name == "meta_d"]
  mratio_hrd[p] <- metad_hrd[p] / d1_hrd[p]
  
}


# fit meta-d individually for RRST
d1_rrst <- numeric(ncol(nR_S1_rrst))
metad_rrst <- numeric(ncol(nR_S1_rrst))
mratio_rrst <- numeric(ncol(nR_S1_rrst))
# fit meta-d each subj RRST
for (p in 1:ncol(nR_S1_rrst)) {
  output_indiv_rrst <- fit_metad_indiv(nR_S1 = nR_S1_rrst[ ,p], nR_S2 = nR_S2_rrst[ ,p])
  
  ## Model output - each subj
  output = output_indiv_rrst[[1]]
  d1_rrst[p] = output_indiv_rrst[[2]]$d1
  
  # Compute mean statistics
  Value <- summary(output)
  stat <- data.frame(mean = Value[["statistics"]][, "Mean"]) %>%
    rownames_to_column(var = "name")
  
  # Compute metacognitive ratio
  metad_rrst[p] <- stat$mean[stat$name == "meta_d"]
  mratio_rrst[p] <- metad_rrst[p] / d1_rrst[p]
  
}


# fit meta-d individually for Extero
d1_extero <- numeric(ncol(nR_S1_extero))
metad_extero <- numeric(ncol(nR_S1_extero))
mratio_extero <- numeric(ncol(nR_S1_extero))
# fit meta-d each subj HRDT
for (p in 1:ncol(nR_S1_extero)) {
  output_indiv_extero <- fit_metad_indiv(nR_S1 = nR_S1_extero[ ,p], nR_S2 = nR_S2_extero[ ,p])
  
  ## Model output - each subj
  output = output_indiv_extero[[1]]
  d1_extero[p] = output_indiv_extero[[2]]$d1
  
  # Compute mean statistics
  Value <- summary(output)
  stat <- data.frame(mean = Value[["statistics"]][, "Mean"]) %>%
    rownames_to_column(var = "name")
  
  # Compute metacognitive ratio
  metad_extero[p] <- stat$mean[stat$name == "meta_d"]
  mratio_extero[p] <- metad_extero[p] / d1_extero[p]
  
}

#alldata <- merge(alldata, metad_indiv, by.x = "participant_id", by.y = "X", all = TRUE)

# merge metacognitive data and save 
metad_hrdextero <- cbind(colnames(nR_S1_hrd), d1_hrd, metad_hrd, mratio_hrd, d1_extero, metad_extero, mratio_extero)
metad_rrst <-  cbind(colnames(nR_S1_rrst), d1_rrst, metad_rrst, mratio_rrst)

metad_indiv <- merge(metad_hrdextero, metad_rrst, by.x = "V1", by.y = "V1", all = TRUE)
#metad_indiv <- cbind(colnames(nR_S1_hrd), d1_hrd, metad_hrd, mratio_hrd, d1_rrst, metad_rrst, mratio_rrst, d1_extero, metad_extero, mratio_extero)

write.csv(metad_indiv, '~/Git/multi-intero/data/metad_indivfits.csv', row.names = FALSE)


