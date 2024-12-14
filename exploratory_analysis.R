load("data/full_data.RData")

library(ggplot2)
library(pubtheme)

merged_df <-final_combined_df

# -----------------------------
# Sales
# Distribution of Average Sales
ggplot(merged_df, aes(x = avg_sales)) +
  geom_histogram(binwidth = 50, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Average Sales per Car Model",
       x = "Average Sales",
       y = "Count") +
  theme_pub()

# Average Price vs. Average Sales
ggplot(merged_df, aes(x = avg_price, y = avg_sales)) +
  geom_point(color = "blue", alpha = 0.6) +
  labs(title = "Average Price vs. Average Sales",
       x = "Average Price",
       y = "Average Sales") +
  theme_minimal()

# -----------------------------
# Engine-related
# Proportion of Fuel Types
ggplot(merged_df, aes(x = majority_fuel_type)) +
  geom_bar(fill = "orange") +
  labs(title = "Distribution of Fuel Types",
       x = "Fuel Type",
       y = "Count") +
  theme_minimal()

# Average Gas Emissions by Fuel Type
ggplot(merged_df, aes(x = majority_fuel_type, y = avg_gas_emission)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Gas Emissions by Fuel Type",
       x = "Fuel Type",
       y = "Average Gas Emissions (g/km)") +
  theme_minimal()

# Average Engine Size by Manufacturer
# Select the top 10 manufacturers based on average engine size
top_10_makers <- merged_df %>%
  group_by(Maker) %>%
  summarise(avg_engine_size = mean(avg_engine_size, na.rm = TRUE)) %>%
  arrange(desc(avg_engine_size)) %>%
  slice_head(n = 10)

# Filter the original data to include only these top 10 manufacturers
filtered_df <- merged_df %>%
  filter(Maker %in% top_10_makers$Maker)

# Plot the bar chart for these top 10 manufacturers
ggplot(filtered_df, aes(x = reorder(Maker, avg_engine_size), y = avg_engine_size)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(title = "Top 10 Car Manufacturers by Average Engine Size",
       x = "Manufacturer",
       y = "Average Engine Size (cc)") +
  theme_minimal() +
  coord_flip()


load("data/by_year_full_data_with_reviews.RData")

car_data <- final_combined_with_reviews

# Proportion of Car Expressions Across Years
expression_proportion <- car_data %>%
  drop_na(majority_label) %>%  # Drop rows with missing expression labels
  filter(Year > 2010) %>% 
  group_by(Year) %>%
  mutate(year_total = n()) %>%  # Calculate total count for each year
  group_by(Year, majority_label) %>%
  summarise(count = n(), proportion = count / unique(year_total), .groups = 'drop')

ggplot(expression_proportion, aes(x = Year, y = proportion, color = majority_label, group = majority_label)) +
  geom_line(size = 1) +  # Add lines for each expression proportion trend
  geom_point(size = 2) + # Add points for better visibility
  labs(title = "Proportion of Car Expressions Across Years",
       x = "Year",
       y = "Proportion",
       color = "Expression") +
  theme_pub() + 
  theme(legend.position = "bottom")

# Distribution of Expressions by Top Makers
top_makers <- car_data %>%
  group_by(Maker) %>%
  summarize(total_sales = sum(Sales)) %>%
  arrange(desc(total_sales)) %>%
  head(10) %>%  # You can adjust this number to show more or fewer makers
  pull(Maker)

# Create the visualization for top makers
expression_dist <- car_data %>%
  filter(Maker %in% top_makers) %>%
  group_by(Maker, majority_label) %>%
  summarize(count = n()) %>%
  group_by(Maker) %>%
  mutate(percentage = count/sum(count) * 100)

# Create stacked bar chart
ggplot(expression_dist, aes(x = Maker, y = percentage, fill = majority_label)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Distribution of Expressions by Top Makers",
       x = "Maker",
       y = "Percentage",
       fill = "Expression") +
  theme_pub() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create boxplot with points overlaid
# Calculate average sales by expression
sales_by_expression <- car_data %>%
  group_by(majority_label) %>%
  summarize(
    avg_sales = mean(Sales),
    se = sd(Sales)/sqrt(n())  # Standard error for error bars
  )

# Create bar plot with error bars
ggplot(sales_by_expression, aes(x = majority_label, y = avg_sales)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = avg_sales - se, ymax = avg_sales + se), 
                width = 0.2) +
  theme_pub() +
  labs(title = "Average Sales by Car Expression",
       x = "Expression",
       y = "Average Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# average sales by review rating bins
binned_sentiment <- car_data %>%
  filter(!is.na(avg_review_rating), 
         is.finite(avg_review_rating)) %>%
  mutate(sentiment_bin = cut(avg_review_rating, 
                             breaks = seq(min(avg_review_rating), 
                                          max(avg_review_rating), 
                                          length.out = 5))) %>%
  group_by(sentiment_bin) %>%
  summarize(
    avg_sales = mean(Sales),
    se = sd(Sales)/sqrt(n()),
    n = n()
  ) %>% 
  drop_na()

# Create the plot
ggplot(binned_sentiment, aes(x = sentiment_bin, y = avg_sales)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = avg_sales - se, ymax = avg_sales + se), 
                width = 0.2) +
  theme_minimal() +
  labs(title = "Average Sales by Review Rating Bins",
       x = "Sentiment Rating Range",
       y = "Average Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Basic boxplot
ggplot(car_data, aes(x = majority_label, y = avg_review_rating)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  geom_jitter(alpha = 0.2, width = 0.2) +  # Add individual points
  theme_pub() +
  labs(title = "Distribution of Review Ratings by Car Expression",
       x = "Expression",
       y = "Average Review Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Or a more detailed violin plot
ggplot(car_data, aes(x = majority_label, y = avg_review_rating)) +
  geom_violin(fill = "lightblue", alpha = 0.7) +
  geom_boxplot(width = 0.2, fill = "white", alpha = 0.7) +
  theme_pub() +
  labs(title = "Distribution of Review Ratings by Car Expression",
       x = "Expression",
       y = "Average Review Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


car_data %>% 
  filter(!is.na(Sales) & !is.na(majority_label)) %>%
  ggplot(aes(x = log(Sales), fill = majority_label)) +
  geom_density(alpha = 0.3) +
  theme_pub() +
  labs(title = "Distribution of Sales (Log Scale) by Car Expression",
       x = "Log(Sales)",
       y = "Density",
       fill = "Expression") +
  theme(legend.position = "right")


car_data %>% 
  filter(!is.na(Sales) & !is.na(majority_label)) %>%
  ggplot(aes(x = log(Sales), fill = majority_label)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~Year) +  # or facet_grid if you want a specific arrangement
  theme_pub() +
  labs(title = "Distribution of Sales (Log Scale) by Car Expression Across Years",
       x = "Log(Sales)",
       y = "Density",
       fill = "Expression") +
  theme(legend.position = "right")


library(ggridges)

car_data %>% 
  filter(!is.na(Sales) & !is.na(majority_label)) %>%
  filter(majority_label %in% c("Angry", "Happy", "Surprise")) %>%  # Filter for specific expressions
  filter(Year > 2015) %>%  # Filter for years of interest
  ggplot(aes(x = log(Sales), y = factor(Year), fill = majority_label)) +
  geom_density_ridges(alpha = 0.3, scale = 0.9) +
  theme_pub() +
  labs(title = "Distribution of Sales by Expression Across Years",
       x = "Log(Sales)",
       y = "Year",
       fill = "Expression") +
  theme(legend.position = "right")


car_data %>% 
  filter(!is.na(Sales) & !is.na(majority_label)) %>%
  ggplot(aes(x = log(Sales), y = factor(Year), fill = ..y..)) +
  geom_density_ridges_gradient(scale = 1.5, alpha = 0.7) +
  scale_fill_viridis_c() +
  facet_wrap(~majority_label) +
  theme_pub() +
  labs(title = "Distribution of Sales by Expression Over Years",
       x = "Log(Sales)",
       y = "Year",
       fill = "Year") +
  theme(
    legend.position = "right",
    axis.text.y = element_text(size = 7),  # Reduce text size
    strip.text = element_text(size = 10),  # Adjust facet label size
    plot.margin = margin(l = 40, r = 20, t = 20, b = 20)
  ) +
  scale_y_discrete(expand = expansion(add = c(0.2, 0.2)))  # Add more space above and below


car_data %>% 
  # First normalize sales within each year
  group_by(Year) %>%
  mutate(normalized_sales = scale(log(Sales))) %>%  # z-score standardization
  ungroup() %>%
  filter(!is.na(Sales) & !is.na(majority_label)) %>%
  ggplot(aes(x = normalized_sales, y = factor(Year), fill = ..y..)) +
  geom_density_ridges_gradient(scale = 1.5, alpha = 0.7) +
  scale_fill_viridis_c() +
  facet_wrap(~majority_label) +
  theme_pub() +
  labs(title = "Distribution of Normalized Sales by Expression Over Years",
       x = "Normalized Log(Sales)",
       y = "Year",
       fill = "Year") +
  theme(
    legend.position = "right",
    axis.text.y = element_text(size = 7),  # Reduce text size
    strip.text = element_text(size = 10),  # Adjust facet label size
    plot.margin = margin(l = 40, r = 20, t = 20, b = 20)
  )


# Basic scatter plot
ggplot(car_data, aes(x = avg_prob_happy, y = log(Sales))) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Relationship between Happiness Expression and Sales",
       x = "Average Probability of Happy Expression",
       y = "Sales Volume") +
  theme_minimal()


car_data %>% 
  filter(Maker == "PEUGEOT") %>% 
  filter(!is.na(avg_prob_happy)) %>%  # Filter out NA values
  ggplot(aes(x = avg_prob_happy, y = factor(Year), fill = ..y..)) +
  geom_density_ridges_gradient(scale = 1.5, alpha = 0.7) +
  scale_fill_viridis_c() +
  facet_wrap(~Maker) +
  theme_pub() +  # or theme_pub() if you have it defined
  labs(title = "Distribution of Happy Expression Scores Over Years for PEUGEOT",
       x = "Probability of Happy Expression",
       y = "Year",
       fill = "Year") +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.margin = margin(l = 40, r = 20, t = 20, b = 20)
  ) +
  scale_y_discrete(expand = expansion(add = c(0.2, 0.2)))



car_data %>% 
  filter(Maker == "HONDA") %>% 
  filter(!is.na(avg_prob_angry)) %>%  # Filter out NA values
  ggplot(aes(x = avg_prob_angry, y = factor(Year), fill = ..y..)) +
  geom_density_ridges_gradient(scale = 1.5, alpha = 0.7) +
  scale_fill_viridis_c() +
  facet_wrap(~Maker) +
  theme_pub() +  # or theme_pub() if you have it defined
  labs(title = "Distribution of Angry Expression Scores Over Years for Honda",
       x = "Probability of Angry Expression",
       y = "Year",
       fill = "Year") +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.margin = margin(l = 40, r = 20, t = 20, b = 20)
  ) +
  scale_y_discrete(expand = expansion(add = c(0.2, 0.2)))



car_data %>% 
  filter(Maker == "LEXUS") %>% 
  filter(!is.na(avg_prob_sad)) %>%  # Filter out NA values
  ggplot(aes(x = avg_prob_sad, y = factor(Year), fill = ..y..)) +
  geom_density_ridges_gradient(scale = 1.5, alpha = 0.7) +
  scale_fill_viridis_c() +
  facet_wrap(~Maker) +
  theme_pub() +  # or theme_pub() if you have it defined
  labs(title = "Distribution of Sad Expression Scores Over Years for Lexus",
       x = "Probability of Sad Expression",
       y = "Year",
       fill = "Year") +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.margin = margin(l = 40, r = 20, t = 20, b = 20)
  ) +
  scale_y_discrete(expand = expansion(add = c(0.2, 0.2)))



car_data %>%
  filter(Maker == "TOYOTA" | Maker == "HONDA") %>% 
  filter(!is.na(avg_prob_angry)) %>%  # Filter out NA values
  ggplot(aes(x = avg_prob_angry, y = factor(Year), fill = ..y..)) +
  geom_density_ridges_gradient(scale = 1.5, alpha = 0.7) +
  scale_fill_viridis_c() +
  theme_pub() +  # Ensure theme_pub() or use a default like theme_minimal()
  labs(title = "Distribution of Angry Expression Scores Over Years for All Makers",
       x = "Probability of Angry Expression",
       y = "Year",
       fill = "Year") +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.margin = margin(l = 40, r = 20, t = 20, b = 20)
  ) +
  scale_y_discrete(expand = expansion(add = c(0.2, 0.2)))
