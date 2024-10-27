# load("data/cleaned_data.RData")

library(tidyverse)

# -----------------------------
# Load initial data
ad_df <- read.csv("data/Ad_table.csv")
ad_extra_df <- read.csv("data/Ad_table (extra).csv")
basic_df <- read.csv("data/Basic_table.csv")
image_df <- read.csv("data/Image_table.csv")
price_df <- read.csv("data/Price_table.csv")
sales_df <- read.csv("data/Sales_table.csv")
trim_df <- read.csv("data/Trim_table.csv")

# -----------------------------
# Being processing
# Calculate average sales per Genmodel, ignoring years with zero sales
average_sales_df <- sales_df %>%
  rowwise() %>%
  mutate(avg_sales = {
    # Get the sales values across columns starting with "X"
    sales_values <- c_across(starts_with("X"))
    non_zero_sales <- sales_values[sales_values != 0]
    if (length(non_zero_sales) > 0) {
      mean(non_zero_sales)
    } else {
      NA
    }
  }) %>%
  ungroup()

# Group by Genmodel_ID and calculate the average price, gas_emission, engine_size,
# and the most common fuel type.
aggregated_trim_df <- trim_df %>%
  group_by(Genmodel_ID) %>%
  summarise(
    avg_price = mean(Price, na.rm = TRUE),
    avg_gas_emission = mean(Gas_emission, na.rm = TRUE),
    avg_engine_size = mean(Engine_size, na.rm = TRUE),
    majority_fuel_type = Fuel_type[which.max(tabulate(match(Fuel_type, unique(Fuel_type))))]
  )

# Merge the two data frames by Genmodel_ID
merged_df <- average_sales_df %>%
  select(Maker, Genmodel, Genmodel_ID, avg_sales) %>%
  inner_join(aggregated_trim_df, by = "Genmodel_ID")

# Save the data
# save(merged_df, file = "data/merged_data.RData")
