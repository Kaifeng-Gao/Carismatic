load("data/full_data.RData")
load("data/by_year_full_data_with_reviews.RData")

library(tidyverse)
library(ranger)

# -----------------------------
# -----------------------------
# Initial Data
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
  labs(title = "Variable Importance from Random Forest of Aggregated Data", x = "Variables", y = "Importance") +
  theme_minimal()

# -----------------------------
# -----------------------------
# By_year Data
NAs_by_year <- final_combined_with_reviews %>% 
  summarize(across(colnames(final_combined_with_reviews), ~ sum(is.na(.)), .names = "na_{col}"))

cleaned_df_by_year <- final_combined_with_reviews %>%
  drop_na(Sales, avg_prob_angry, avg_prob_disgust, avg_prob_fear, avg_prob_happy, 
          avg_prob_sad, avg_prob_surprise, avg_prob_neutral, majority_label, avg_review_rating, avg_sentiment_ating)

cleaned_df_by_year <- cleaned_df_by_year %>%
  drop_na(avg_price, avg_gas_emission, avg_engine_size, majority_fuel_type, 
          avg_engine_power, avg_wheelbase, avg_height, avg_width, avg_length, 
          avg_mpg, avg_top_speed, majority_seat_num, majority_door_num)

cleaned_df_by_year$Maker <- as.factor(cleaned_df_by_year$Maker)
cleaned_df_by_year$Year <- as.factor(cleaned_df_by_year$Year)
cleaned_df_by_year$majority_engine_size <- as.factor(cleaned_df_by_year$majority_engine_size)
cleaned_df_by_year$majority_bodytype <- as.factor(cleaned_df_by_year$majority_bodytype)
cleaned_df_by_year$majority_gearbox <- as.factor(cleaned_df_by_year$majority_gearbox)
cleaned_df_by_year$majority_label <- as.factor(cleaned_df_by_year$majority_label)

rf_model_by_year <- ranger(
  Sales ~ Maker + Year + avg_price + avg_gas_emission + avg_engine_size +
    majority_fuel_type + avg_engine_power + avg_wheelbase + avg_height + 
    avg_width + avg_length + avg_mpg + avg_top_speed + majority_seat_num +
    majority_door_num + majority_engine_size + majority_bodytype + 
    majority_gearbox + avg_prob_angry + avg_prob_disgust + avg_prob_fear +
    avg_prob_happy + avg_prob_sad + avg_prob_surprise + avg_prob_neutral + 
    majority_label,
  data = cleaned_df_by_year,
  importance = 'permutation',
  num.trees = 1000,
  seed = 123
)

print(rf_model_by_year)

importance_sorted_by_year <- sort(rf_model_by_year$variable.importance, decreasing = TRUE)
importance_df_by_year <- data.frame(Variable = names(importance_sorted_by_year), Importance = importance_sorted_by_year)

ggplot(importance_df_by_year, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Variable Importance from Random Forest for By_year Data", x = "Variables", y = "Importance") +
  theme_minimal()
