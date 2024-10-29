load("data/merged_data.RData")

library(tidyverse)

# -----------------------------
# Load image data
image_df <- read.csv("data/Image_table.csv")
expression_df <- read.csv("data/expression_results.csv")
expression_df <- expression_df %>%
  mutate(image_file_name = str_extract(image_path, "[^/]+$"))

combined_df <- expression_df %>%
  inner_join(image_df, by = c("image_file_name" = "Image_name"))

aggregated_df <- combined_df %>%
  group_by(Genmodel_ID) %>%
  summarise(
    # Calculate the average for each probability score
    avg_prob_angry = mean(prob_angry, na.rm = TRUE),
    avg_prob_disgust = mean(prob_disgust, na.rm = TRUE),
    avg_prob_fear = mean(prob_fear, na.rm = TRUE),
    avg_prob_happy = mean(prob_happy, na.rm = TRUE),
    avg_prob_sad = mean(prob_sad, na.rm = TRUE),
    avg_prob_surprise = mean(prob_surprise, na.rm = TRUE),
    avg_prob_neutral = mean(prob_neutral, na.rm = TRUE),
    
    # Calculate the majority label
    majority_label = label[which.max(tabulate(match(label, unique(label))))],
  )

final_combined_df <- final_merged_df %>%
  left_join(aggregated_df, by = "Genmodel_ID")

# -----------------------------
# Save the data
# save(final_combined_df, file = "data/full_data.RData")
