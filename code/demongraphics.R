# Leah Banellis 2025
# age & gender distribution
rm(list = ls())
library(gridExtra)
library(readr)
library(dplyr)

# load demographics tsv
alldata <- read_csv("~/Git/multi-intero/data/mergedvmpdata_all.csv") 


# 2) Subset only the RRST and HRD variables.
int_data <- alldata %>%
  dplyr::select(
    age,
    gender,
    bmi,
    rrst_estimated_mean_alpha,
    rrst_estimated_mean_beta,
    rrst_mean_confidence,
    hrd_estimated_mean_alpha,
    hrd_estimated_mean_beta,
    hrd_mean_confidence,
    extero_estimated_mean_alpha,
    extero_estimated_mean_beta,
    extero_mean_confidence
  )


# Optional: rename for clarity
int_data <- int_data %>%
  rename(
    "Respiratory Confidence" = rrst_mean_confidence,
    "Respiratory Precision" = rrst_estimated_mean_beta,
    "Respiratory Sensitivity" = rrst_estimated_mean_alpha,
    "Cardiac Confidence" = hrd_mean_confidence,
    "Cardiac Precision" = hrd_estimated_mean_beta,
    "Cardiac Sensitivity" = hrd_estimated_mean_alpha,
    "Auditory Confidence" = extero_mean_confidence,
    "Auditory Precision" = extero_estimated_mean_beta,
    "Auditory Sensitivity" = extero_estimated_mean_alpha
  )

N <- nrow(na.omit(int_data))

int_data <- na.omit(int_data)


# age
min_age = min(int_data$age)
max_age = max(int_data$age)
median_age = median(int_data$age)
#gender
Nfemales = sum(int_data$gender == 1)   
Nmales = sum(int_data$gender == 2)   
Nother = sum(int_data$gender == 3)  

  
plotage <- ggplot(int_data, aes(x = age)) +
  geom_histogram(
    binwidth = 1,                  # Adjust bin width
    fill = "skyblue",                 # Fill color
    color = "white",                  # Border color
    alpha = 0.7                       # Transparency
  ) +
  labs(
    #title = " Distribution of Age",    # Title
    x = "Age (Years)",                      # X-axis label
    y = "Frequency"                   # Y-axis label
  ) +
  theme_minimal() +                   # Use a minimal theme
  theme(
    #plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center and style title
    axis.title.x = element_text(size = 14, margin = margin(t = 7.5)),   # Space above x-axis title
    axis.title.y = element_text(size = 14, margin = margin(r = 7.5)),   # Space to the right of y-axis title
    axis.text.x = element_text(size = 14, margin = margin(t = 5)),     # Space above x-axis labels
    axis.text.y = element_text(size = 14, margin = margin(r = 5))      # Space to the right of y-axis labels
  )


# Create a bar plot for the gender variable
int_data$gender <- factor(int_data$gender, levels = c(1, 2), labels = c("Female", "Male"))
plotgender <- ggplot(int_data, aes(x = gender)) +
  geom_bar(
    fill = "skyblue",                   # Customize fill color
    color = "white",                    # Customize border color
    alpha = 0.7,                        # Set transparency
    width = 0.5                          # Set transparency
  ) +
  labs(
    #title = "Distribution of Gender",   # Add a title
    x = "Gender",                       # Label the x-axis
    y = "Frequency"                         # Label the y-axis
  ) +
  theme_minimal() +                     # Apply a clean, minimal theme
  theme(
    #plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Style the title
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),   # Space above x-axis title
    axis.title.y = element_text(size = 14, margin = margin(r = 5)),   # Space to the right of y-axis title
    axis.text.x = element_text(size = 14, margin = margin(t = 5)),     # Space above x-axis labels
    axis.text.y = element_text(size = 14, margin = margin(r = 5))      # Space to the right of y-axis labels
  )

# Arrange the plots side by side
combined_plot <- grid.arrange(plotage, plotgender, ncol = 2, widths = c(2.25, 1))
ggsave("figs/demographics.png", combined_plot, width = 12, height = 6)


# # Descriptives Table
# library(flextable)
# 
# desc_stats <- data.frame(
#   Variable = colnames(int_data[, 4:12]),
#   Mean = round(colMeans(int_data[, 4:12]), 3),
#   SD = round(apply(int_data[, 4:12], 2, sd), 3),
#   Median = round(apply(int_data[, 4:12], 2, median), 3),
#   Range = apply(int_data[, 4:12], 2, function(x) paste0(round(min(x), 3), " - ", round(max(x), 3)))
# )
# 
# # Format the table in APA style
# apa_table <- flextable(desc_stats) %>%
#   set_header_labels(Variable = "Variable", Mean = "Mean", SD = "Standard Deviation") %>%
#   autofit()
# 
# # Print the table
# apa_table
# save_as_docx(apa_table, path = "docs/APA_Descriptives.docx")
