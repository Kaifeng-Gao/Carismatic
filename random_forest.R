load("data/full_data.RData")

library(tidyverse)
library(ranger)

# -----------------------------
# Handle NAs
NAs <- final_combined_df %>% 
  summarize(across(colnames(final_combined_df), ~ sum(is.na(.)), .names = "na_{col}"))

cleaned_df <- final_combined_df %>%
  drop_na(avg_sales, avg_prob_angry, avg_prob_disgust, avg_prob_fear, avg_prob_happy, avg_prob_sad, avg_prob_surprise, avg_prob_neutral, majority_label)

cleaned_df <- cleaned_df %>%
  drop_na(avg_price, avg_gas_emission, avg_engine_size, majority_fuel_type, 
          avg_engine_power, avg_wheelbase, avg_height, avg_width, avg_length, 
          avg_mpg, avg_top_speed, majority_seat_num, majority_door_num)

# Convert Characters to Factors for Categorical Variables
cleaned_df$Maker <- as.factor(cleaned_df$Maker)
cleaned_df$majority_engine_size <- as.factor(cleaned_df$majority_engine_size)
cleaned_df$majority_bodytype <- as.factor(cleaned_df$majority_bodytype)
cleaned_df$majority_gearbox <- as.factor(cleaned_df$majority_gearbox)
cleaned_df$majority_label <- as.factor(cleaned_df$majority_label)

# -----------------------------
# Random Forest
rf_model <- ranger(avg_sales ~  Maker + avg_price + avg_gas_emission + avg_engine_size +
                           majority_fuel_type + avg_engine_power + avg_wheelbase + avg_height + avg_width + avg_length +
                           avg_mpg + avg_top_speed + majority_seat_num + majority_door_num + majority_engine_size +
                           majority_bodytype + majority_gearbox + avg_prob_angry + avg_prob_disgust + avg_prob_fear +
                           avg_prob_happy + avg_prob_sad + avg_prob_surprise + avg_prob_neutral + majority_label,
                   data = cleaned_df,
                   importance = 'permutation',
                   num.trees = 1000,
                   seed = 123)

# rf_model <- ranger(avg_sales ~  Maker + avg_price + avg_gas_emission + avg_engine_size + 
#                            majority_fuel_type + avg_engine_power + avg_wheelbase + avg_height + avg_width + avg_length + 
#                            avg_mpg + avg_top_speed + majority_seat_num + majority_door_num + majority_engine_size + 
#                            majority_bodytype + majority_gearbox + avg_prob_angry + avg_prob_disgust + avg_prob_fear + 
#                            avg_prob_happy + avg_prob_sad + avg_prob_surprise + avg_prob_neutral + majority_label, 
#                    data = cleaned_df,
#                    importance = 'permutation',
#                    num.trees = 100,        
#                    seed = 1234)

print(rf_model)

importance_sorted <- sort(rf_model$variable.importance, decreasing = TRUE)
importance_df <- data.frame(Variable = names(importance_sorted), Importance = importance_sorted)

ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Variable Importance from Random Forest", x = "Variables", y = "Importance") +
  theme_minimal()

