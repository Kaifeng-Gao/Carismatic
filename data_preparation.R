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
# Being processing sales_df and trim_df
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

merged_df <- merged_df %>%
  mutate(majority_fuel_type = factor(majority_fuel_type, levels = c("Petrol", "Diesel", "Other")))

# -----------------------------
# Processing ad_extra_df
colnames(ad_extra_df)[colnames(ad_extra_df) == "Engin_size"] <- "Engine_size"

aggregated_ad_extra_df <- ad_extra_df %>%
  mutate(
    # Remove units from Average_mpg and Top_speed columns and convert to numeric
    Average_mpg = as.numeric(sub(" mpg", "", Average_mpg)),
    Top_speed = as.numeric(sub(" mph", "", Top_speed))
  ) %>%
  group_by(Genmodel_ID) %>%
  summarise(
    avg_engine_power = mean(as.numeric(Engine_power), na.rm = TRUE),
    avg_wheelbase = mean(as.numeric(Wheelbase), na.rm = TRUE),
    avg_height = mean(as.numeric(Height), na.rm = TRUE),
    avg_width = mean(as.numeric(Width), na.rm = TRUE),
    avg_length = mean(as.numeric(Length), na.rm = TRUE),
    avg_mpg = mean(as.numeric(sub(" mpg", "", Average_mpg)), na.rm = TRUE),
    avg_top_speed = mean(as.numeric(sub(" mph", "", Top_speed)), na.rm = TRUE),

    # Mode for Seat_num and Door_num
    majority_seat_num = Seat_num[which.max(tabulate(match(Seat_num, unique(Seat_num))))],
    majority_door_num = Door_num[which.max(tabulate(match(Door_num, unique(Door_num))))],
    
    # Categorical columns: take the most common value
    majority_engine_size = Engine_size[which.max(tabulate(match(Engine_size, unique(Engine_size))))],
    majority_bodytype = Bodytype[which.max(tabulate(match(Bodytype, unique(Bodytype))))],
    majority_gearbox = Gearbox[which.max(tabulate(match(Gearbox, unique(Gearbox))))]
  )

final_merged_df <- merged_df %>%
  left_join(aggregated_ad_extra_df, by = "Genmodel_ID")

# -----------------------------
# Save the data
save(final_merged_df, file = "data/merged_data.RData")
