library(tidyverse)


# -----------------------------
# Load initial data
# ad_df <- read.csv("data/Ad_table.csv")
ad_extra_df <- read.csv("data/Ad_table (extra).csv")
# basic_df <- read.csv("data/Basic_table.csv")
# image_df <- read.csv("data/Image_table.csv")
# price_df <- read.csv("data/Price_table.csv")
sales_df <- read.csv("data/Sales_table.csv")
trim_df <- read.csv("data/Trim_table.csv")


# -----------------------------
# Being processing sales_df
# Reshape the data into long format
sales_long_df <- sales_df %>%
  pivot_longer(cols = starts_with("X"), 
               names_to = "Year", 
               values_to = "Sales") %>%
  mutate(
    Year = as.numeric(gsub("X", "", Year))  # Convert Year column to numeric
  )

# Filter out 2020 and zero sales
filtered_sales_df <- sales_long_df %>%
  filter(Year != 2020 & Sales != 0) %>%
  group_by(Genmodel_ID) %>%
  arrange(Year) %>%
  mutate(
    # Remove the first non-zero year
    First_NonZero = ifelse(row_number() == 1, TRUE, FALSE)
  ) %>%
  filter(!First_NonZero) %>%
  ungroup()

# Apply IQR filtering for each genmodel_ID
filtered_sales_df <- filtered_sales_df %>%
  group_by(Genmodel_ID) %>%
  mutate(
    Q1 = quantile(Sales, 0.25, na.rm = TRUE),
    Q3 = quantile(Sales, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    Lower_Threshold = Q1 - 1.5 * IQR
  ) %>%
  filter(Sales >= Lower_Threshold & Sales > 20) %>%
  select(-Q1, -Q3, -IQR, -Lower_Threshold, -First_NonZero) %>%
  ungroup()

final_sales_df <- filtered_sales_df %>%
  arrange(Maker, Genmodel, desc(Year))


# -----------------------------
# Define a reusable function to calculate the mode with tie-breaking
calculate_mode <- function(column) {
  valid_values <- column[!is.na(column) & column != ""]
  
  if (length(valid_values) == 0) {
    return(NA)
  }
  
  value_counts <- table(valid_values)
  max_count <- max(value_counts)
  tied_values <- names(value_counts[value_counts == max_count])
  
  if (length(tied_values) > 1) {
    return(sort(tied_values)[1])  
  } else {
    return(tied_values[1])
  }
}

# Add trim data
aggregated_trim_df <- trim_df %>%
  group_by(Genmodel_ID, Year) %>%
  summarise(
    avg_price = mean(Price, na.rm = TRUE),
    avg_gas_emission = mean(Gas_emission, na.rm = TRUE),
    avg_engine_size = mean(Engine_size, na.rm = TRUE),
    majority_fuel_type = calculate_mode(Fuel_type),
    .groups = "drop"
  )

merged_df <- filtered_sales_df %>%
  left_join(aggregated_trim_df, by = c("Genmodel_ID", "Year"))

merged_df <- merged_df %>%
  mutate(majority_fuel_type = factor(majority_fuel_type, levels = c("Petrol", "Diesel", "Other")))

merged_df <- merged_df %>%
  arrange(Maker, Genmodel, desc(Year))
 

# -----------------------------
# Processing ad_extra_df
colnames(ad_extra_df)[colnames(ad_extra_df) == "Engin_size"] <- "Engine_size"
colnames(ad_extra_df)[colnames(ad_extra_df) == "Reg_year"] <- "Year"

aggregated_ad_extra_df <- ad_extra_df %>%
  mutate(
    Average_mpg = as.numeric(sub(" mpg", "", Average_mpg)),
    Top_speed = as.numeric(sub(" mph", "", Top_speed))
  ) %>%
  group_by(Genmodel_ID, Year) %>%
  summarise(
    avg_engine_power = mean(as.numeric(Engine_power), na.rm = TRUE),
    avg_wheelbase = mean(as.numeric(Wheelbase), na.rm = TRUE),
    avg_height = mean(as.numeric(Height), na.rm = TRUE),
    avg_width = mean(as.numeric(Width), na.rm = TRUE),
    avg_length = mean(as.numeric(Length), na.rm = TRUE),
    avg_mpg = mean(Average_mpg, na.rm = TRUE),
    avg_top_speed = mean(Top_speed, na.rm = TRUE),
    
    # Mode for others
    majority_seat_num = calculate_mode(Seat_num),
    majority_door_num = calculate_mode(Door_num),
    majority_engine_size = calculate_mode(Engine_size),
    majority_bodytype = calculate_mode(Bodytype),
    majority_gearbox = calculate_mode(Gearbox),
    .groups = "drop"
  )

final_merged_df <- merged_df %>%
  left_join(aggregated_ad_extra_df, by = c("Genmodel_ID", "Year"))


# -----------------------------
# Save the data
# save(final_merged_df, file = "data/by_year_merged_data.RData")
