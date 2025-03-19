## this script preprocesses the data and runs the multi-intero metacognition analyses using the H-metad model
## Micah G. Allen, 2025


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
rrst_data <- read_csv("data/raw_rrst.csv", na = c("n/a", "NA", ""))

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

# Assume hrd_data and rrst_data have already been read in appropriately
all_bad_subjects <- get_all_bad_subjects(hrd_badid_path, rrst_badid_path, hrd_data, rrst_data,
                                         exclude_badid = TRUE,
                                         exclude_low_conf_sd = FALSE,
                                         exclude_low_accuracy = FALSE)

# Exclude the bad subjects from your datasets.
clean_hrd_data  <- hrd_data  %>% filter(!participant_id %in% all_bad_subjects)
clean_rrst_data <- rrst_data %>% filter(!participant_id %in% all_bad_subjects)

# separate intero and extero conditions from HRD
#clean_extero_data <- clean_hrd_data[clean_hrd_data$Modality=='Extero', ]
clean_hrd_data <- clean_hrd_data[clean_hrd_data$Modality=='Intero', ]


#processed HRD data 
processed_hrd_data <- process_hrd_data(clean_hrd_data)

# View the processed data:
head(processed_hrd_data)

# Process RRST data
processed_rrst_data <- process_rrst_data(clean_rrst_data)
head(processed_rrst_data)

# Glimpse processed data
glimpse(processed_hrd_data)
glimpse(processed_rrst_data)

# Find common participants
common_participants <- intersect(
  processed_hrd_data$participant_id,
  processed_rrst_data$participant_id
)

# Filter both datasets to include only common participants
processed_hrd_data <- processed_hrd_data %>%
  filter(participant_id %in% common_participants)

processed_rrst_data <- processed_rrst_data %>%
  filter(participant_id %in% common_participants)

# Glimpse filtered data
glimpse(processed_hrd_data)
glimpse(processed_rrst_data)

# Process tasks
rrst_results <- process_task(processed_rrst_data, nRatings = 4)
hrd_results <- process_task(processed_hrd_data, nRatings = 4)

# Extract results
nR_S1_rrst <- rrst_results$nR_S1 # counts for each response type (response s1 or s2) & confidence rating (1-4) for stim1
nR_S2_rrst <- rrst_results$nR_S2 # counts for each response type (response s1 or s2) & confidence rating (1-4) for stim2

nR_S1_hrd <- hrd_results$nR_S1
nR_S2_hrd <- hrd_results$nR_S2

# Ensure participant order is identical in both tasks
all.equal(colnames(nR_S1_hrd), colnames(nR_S1_rrst))  # Should return TRUE
all.equal(colnames(nR_S2_hrd), colnames(nR_S2_rrst))  # Should return TRUE

# Create model inputs
nR_S1 <- list(nR_S1_hrd, nR_S1_rrst)
nR_S2 <- list(nR_S2_hrd, nR_S2_rrst)

# Fit the model - will take some time
source("code/fit_metad_groupcorr.R")

# Fits the model if not already done, otherwise just loads the RDS file - note this file is too large for github

if (file.exists("data/output.rds")) {
  output <- readRDS("data/output.rds")
} else {
  output <- fit_metad_groupcorr(nR_S1 = nR_S1, nR_S2 = nR_S2)
  saveRDS(output, "data/output_intero.rds")
}


## Model output ------------------------------------------------------------

# Values 
Value <- summary(output)
stat <- data.frame(mean = Value$statistics[,"Mean"])
stat %<>%
  rownames_to_column(var = "name")

# Rhat 
Value <- gelman.diag(output, confidence = 0.95)
Rhat <- data.frame(conv = Value$psrf)

# HDI 
HDI <- data.frame(HPDinterval(output, prob = 0.95))
HDI %<>%
  rownames_to_column(var = "name")

# All values in the same dataframe
Fit <- stat %>%
  cbind(lower = HDI$lower,
        upper = HDI$upper,
        Rhat = Rhat[,1])

## Plots ---------------------------------------------------------

# Plot trace mcmc
#traceplot(output)  # takes forever... can be a supplemental fig 

# mcmc values in df for plot posterior distributions
mcmc.sample <- ggs(output)

# Plot posterior distribution for rho value
Rho_plot <- mcmc.sample %>%
  filter(Parameter == "rho") %>% 
  ggplot(aes(value)) +
  geom_histogram(binwidth = 0.03, fill = "blue", colour = "grey", alpha = 0.5) +
  geom_vline(xintercept = stat$mean[stat$name == "rho"],linetype="dashed", linewidth = 1.5) +
  annotate("segment", x = HDI$lower[HDI$name == "rho"], y = 50, 
           xend = HDI$upper[HDI$name == "rho"], yend = 50, 
           colour = "white", linewidth = 2.5) +
  xlim(c(-1, 1)) +
  ylab("Sample count") +
  xlab(expression(paste(rho, " value")))

Rho_plot

# what is the mean correlation?
Fit$mean



##

# Values (mean and CI)
Value <- summary(output)
stat <- data.frame(mean = Value$statistics[,"Mean"])
stat %<>%
  rownames_to_column(var = "name") %>% 
  cbind(CILow = Value$quantiles[,"2.5%"]) %>% 
  cbind(CIUp = Value$quantiles[,"97.5%"])

# HDI function -> not working
HDI <- data.frame(HPDinterval(output, prob = 0.95))
HDI %<>%
  rownames_to_column(var = "name")

##
mcmc.rho <- mcmc.sample %>% 
  filter(Parameter == "rho")

Rho1 <- mcmc.sample %>%
  filter(Parameter == "rho") %>% 
  ggplot(aes(value)) +
  geom_histogram(binwidth = 0.03, fill = "blue", colour = "grey", alpha = 0.5) +
  geom_vline(xintercept = stat$mean[stat$name == "rho"],linetype="dashed", size = 1.5) +
  geom_segment(aes(x = HDI$lower[HDI$name == "rho"], y = 50, xend = HDI$upper[HDI$name == "rho"], yend = 50), colour = "white", size = 2.5) +
  theme_minimal(base_size = 14)+
  theme(
  panel.grid.major = element_line(color = "gray80"),  # Light gridlines
  panel.grid.minor = element_blank()
)+
  ylab("Sample count") +
  xlab(expression(paste(rho, " value")))+
  ggtitle("Posterior Samples and 95% HDI")
print(Rho1)

# Extract M-ratio values and adapt the structure for tasks
Mratio_indiv <- stat %>%
  # Filter rows where names start with "M"
  filter(grepl("^M", name)) %>%
  # Add task information based on patterns in "name"
  mutate(
    task = case_when(
      grepl("1]", name) ~ "HRD",  # Task 1 corresponds to HRD
      grepl("2]", name) ~ "RRST", # Task 2 corresponds to RRST
      TRUE ~ "Other"             # Catch any other cases
    ),
    # Extract participant ID as an integer
    pp = name %>%
      str_extract(regex("\\d+(?=,)")) %>% 
      as.integer()
  ) %>%
  # Reshape the data so participants (pp) are rows and tasks are columns
  dcast(pp ~ task, value.var = "mean")

# Output the resulting structure
glimpse(Mratio_indiv)

# Hierarchically estimated correlation from your JAGS model 
rho_est <-stat$mean[stat$name == "rho"]

slope <- rho_est * (sd(Mratio_indiv$RRST) / sd(Mratio_indiv$HRD))
intercept <- mean(Mratio_indiv$RRST) - slope * mean(Mratio_indiv$HRD)



# Scatter plot of HRD vs. RRST
scatter_plot <- ggplot(Mratio_indiv, aes(x = HRD, y = RRST)) +
  geom_point(alpha = 0.7, size = 2, shape = 21, fill = "darkorange") +  # Add scatter points
  geom_smooth(method = "lm", color = "darkblue", linetype = "dashed", se = TRUE) +  # Add a trend line
  #geom_abline(intercept = intercept, slope = slope, color = "darkblue", linetype = "dashed") + # plot estimate correlation
  labs(
    title = "Scatter Plot of HRD vs. RRST",
    x = "M-Ratio HRD",
    y = "M-Ratio RRST"
  ) +
 # xlim(.4, 1.5) + 
#  ylim(.4, 1.6)+
  theme_minimal(base_size = 14) +  # Minimal theme with larger text
  theme(
    panel.grid.major = element_line(color = "gray80"),  # Light gridlines
    panel.grid.minor = element_blank()
  )

# Display the plot
print(scatter_plot)



# Arrange the two plots side by side
combined_plot <- ggarrange(
  Rho1, scatter_plot,
  ncol = 2, nrow = 1,  # Arrange in one row, two columns
  labels = c("A", "B"),  # Add labels to each plot
  common.legend = FALSE  # No common legend for this example
)



# Display the combined plot with title
print(combined_plot)

ggsave("figs/metacognition_figure_intero.png", width = 12, height = 8)