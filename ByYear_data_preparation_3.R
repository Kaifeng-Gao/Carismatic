load("data/by_year_full_data.RData")

library(tidyverse)

# -----------------------------
# Load review data
review_df <- read.csv("data/processed_review_data.csv")
aggregated_df <- review_df %>%
  group_by(Genmodel_ID, Year) %>%
  summarise(
    avg_review_rating = mean(Review_Rating, na.rm = TRUE),
    avg_sentiment_ating = mean(Sentiment.Rating, na.rm = TRUE),
  )


final_combined_with_reviews <- final_combined_df %>%
  left_join(aggregated_df, by = c("Genmodel_ID", "Year"))


# -----------------------------
# Save the data
# save(final_combined_with_reviews, file = "data/by_year_full_data_with_reviews.RData")