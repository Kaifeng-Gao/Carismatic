load("data/merged_data.RData")

library(ggplot2)

# -----------------------------
# Sales
# Distribution of Average Sales
ggplot(merged_df, aes(x = avg_sales)) +
  geom_histogram(binwidth = 50, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Average Sales per Car Model",
       x = "Average Sales",
       y = "Count") +
  theme_minimal()

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


