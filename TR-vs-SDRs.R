# load libraries
library(readxl)
library(readr)
library(writexl)
library(dplyr)
library(tidyr)
library(lubridate)

# 1. load SDR holdings data, its conversion table, and 
conversion_rates_data <- read_csv("~/Downloads/CCUSSP01IFM650N.csv")
SDR_holdings_data <- read_excel("~/Downloads/SDR_Allocations_Holdings.xlsx")
TR_data <- read_excel("~/Downloads/API_FI.RES.TOTL.CD_DS2_en_excel_v2_4739.xls", skip=3)

# 2. convert SDR holdings into USD -- NOT adjusted for inflation, btw 
# Clean and prepare the conversion rates
conversion_rates_data <- conversion_rates_data %>%
  mutate(DATE = as.Date(observation_date)) %>%
  filter(month(DATE) == 1 & day(DATE) == 1) %>%  # Select January 1st rates
  rename(conversion_rate = CCUSSP01IFM650N) %>%
  mutate(year = year(DATE) - 1)  # Apply to previous year's holdings

# Adjust column names  & covert SDDR Holdings column to numeric 
SDR_holdings_data <- SDR_holdings_data %>%
  mutate(year = year(as.Date(Date, format = "%d-%b-%Y")),
         SDR_Holdings = as.numeric(gsub(",", "", `SDR Holdings`))) %>%
  select(year, SDR_Holdings)  # Keep only what we need

# Join the datasets
sdr_in_usd <- SDR_holdings_data %>%
  left_join(conversion_rates_data %>% select(year, conversion_rate), by = "year") %>%
  mutate(SDR_in_USD = SDR_Holdings * conversion_rate) %>%
  select(year, SDR_in_USD)

# 3. clean the total reserves data (TR)
# Select only the year columns (years start from column 5 onward)
year_cols <- names(TR_data)[5:ncol(TR_data)]

# Sum reserves across all countries for each year
total_reserves <- TR_data %>%
  select(all_of(year_cols)) %>%
  summarise(across(everything(), ~ sum(.x, na.rm = TRUE)))

# Add identifying information for the new "Total" column

total_reserves_long <- total_reserves %>%
  pivot_longer(
    cols = everything(),
    names_to = "year_col",   # New name here to avoid conflicts
    values_to = "Total_Reserves"
  ) %>%
  mutate(year_col = as.numeric(year_col))  # Now this works because 'year_col' exists

sdr_in_usd <- sdr_in_usd %>%
  rename(year_col = year)  # Rename for clean join

# Final combined table, and see percentage of total reserves that are SDR holdings calculation -- TR are also not adjusted for inflation 
final_table <- sdr_in_usd %>%
  left_join(total_reserves_long, by = "year_col") %>%
  mutate(
    percentage = (SDR_in_USD / Total_Reserves) * 100
  ) %>%
  rename(year = year_col) %>%   # Optionally rename back to 'year' if you want
  filter(year >= 1974 & year <= 2022)

View(final_table)

# write excel 
write_xlsx(final_table, "Total_Reserves-vs-SDR.xlsx")
