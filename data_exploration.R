library(ggplot2)

# Load dataset
load("data/full_data.RData")

final_combined_df %>% head()

# Expression distribution by Maker
expression_dist <- final_combined_df %>%
  drop_na(Maker, majority_label) %>% # Drop rows with missing `Maker`
  group_by(Maker, majority_label) %>%
  summarise(count = n()) %>% # Count the number of occurrences of each `majority_label` within each `Maker`
  mutate(proportion = count / sum(count)) %>% # Calculate the proportion within each `Maker`
  ungroup() # Remove the grouping

expression_dist %>% head()

# Define popular car makers
popular_makers <- c("AUDI", "BMW", "FORD", "HONDA", "MERCEDES", "TOYOTA", "VOLKSWAGEN", "TESLA")

# Using ggplot to create a stacked bar chart
expression_dist %>% 
  filter(Maker %in% popular_makers) %>% # Filter for popular car makers
  ggplot(aes(x = Maker, y = proportion, fill = majority_label)) +
  geom_bar(stat = "identity") +
  theme_minimal() + # Apply a minimal theme for cleaner look
  labs(x = "Maker", y = "Proportion", fill = "Majority Label") +
  scale_y_continuous(labels = scales::percent) + # Format y-axis as percentage
  ggtitle("Proportion of Each Expression within Maker") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability

# Expression distribution over Year
expression_dist_year <- final_combined_df %>%
  drop_na(Year, majority_label) %>% # Drop rows with missing `Year`
  group_by(Year, majority_label) %>%
  summarise(count = n()) %>% # Count the number of occurrences of each `majority_label` within each `Year`
  mutate(proportion = count / sum(count)) %>% # Calculate the proportion within each `Year`
  ungroup() # Remove the grouping
