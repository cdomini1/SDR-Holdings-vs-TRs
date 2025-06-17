# load libraries
library(readxl)
library(readr)
library(writexl)
library(dplyr)
library(lubridate)

# 1. load SDR holdings data, its conversion table, and 
SDR_converstion_data <- read_csv("~/Downloads/CCUSSP01IFM650N.csv")
SDR_holdings_data <- read_excel("~/Downloads/SDR_Allocations_Holdings.xlsx")

# 2. convert SDR holdings into USD -- NOT adjusted for inflation, btw 
# Clean and prepare the conversion rates
conversion_rates <- conversion_rates %>%
  mutate(DATE = as.Date(DATE)) %>%
  filter(month(DATE) == 1 & day(DATE) == 1) %>%  # Select January 1st rates
  rename(conversion_rate = CCUSSP01IFM650N) %>%
  mutate(year = year(DATE) - 1)  # Link to previous year's SDR holdings

# Adjust column names if necessary
sdr_holdings <- sdr_holdings %>%
  rename(year = Year, sdr_holdings = SDR_Holdings)

# Join the datasets
sdr_in_usd <- sdr_holdings %>%
  left_join(conversion_rates, by = "year") %>%
  mutate(usd_equivalent = sdr_holdings * conversion_rate)

# View the final dataset
View(sdr_in_usd)

# 3. percentage of total reserves that are SDR holdings calculation -- make sure TR are also not adjusted for inflation 

# 4. make a table that only shows year, % of TR that are SDR allocations for that year

# write csv 
write_csv(my_data, "~/Documents/my_output.csv")




