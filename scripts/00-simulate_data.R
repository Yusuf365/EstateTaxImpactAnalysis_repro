#### Preamble ####
# Purpose: Simulates the data for export & Bangladesh
# Author: Mohammed Yusuf Shaikh
# Date: 11 February 2024
# Contact: Mohhammedn Yusuf Shaikh
# License: MIT
# Pre-requisites:Install necessary packages 

#### Install Packages ####
install.packages("tidyverse")
install.packages("janitor")
install.packages("dplyr")

#### Workspace setup ####
library(tidyverse)
library(janitor)
library(tibble)
library(dplyr)


#### Simulate data ####

set.seed(202) # Ensure reproducibility

# Define parameters for the simulation
countries <- c("France", "Germany", "Italy", "Spain", "Poland", "Netherlands")
products <- as.character(seq(610000, 620000, by = 1000)) # Product codes
years <- 2001:2018 # Year range

# Generate simulated data
n_rows <- 1000
simulated_eurostat <- tibble(
  DECLARANT_LAB = sample(countries, n_rows, replace = TRUE),
  PARTNER_LAB = sample(countries, n_rows, replace = TRUE),
  PRODUCT = sample(products, n_rows, replace = TRUE),
  PRODUCT_LAB = "Some Product Label",
  STAT_REGIME_LAB = "Some Stat Regime",
  ELIGIBILITY_LAB = "Eligible",
  IMPORT_REGIME_LAB = sample(c("GSP ZERO", "GSP NON ZERO", "Other"), n_rows, replace = TRUE),
  year = sample(years, n_rows, replace = TRUE),
  value = sample(100000:500000, n_rows, replace = TRUE)
)

# Display the first few rows of the simulated data
head(simulated_eurostat)


#### Simulating market share data set #####

# Define parameters for the simulation
years <- 2001:2018 # Year range
entities <- c("China", "LDCs") 

# Generate simulated market share data
n_rows <- 50 # Number of rows to simulate
simulated_market_share <- tibble(
  Year = sample(years, n_rows, replace = TRUE),
  Export = sample(300000:1500000, n_rows, replace = TRUE),
  Total = sample(1600000:3000000, n_rows, replace = TRUE),
  Entity = sample(entities, n_rows, replace = TRUE)
) %>%
  mutate(Share = Export / Total) # Calculate share as Export divided by Total

# Display the first few rows of the simulated market share data
head(simulated_market_share)


# 'simulated_eurostat' 

# Filtering data
gsp_data <- simulated_eurostat %>%
  filter(PARTNER_LAB == "France", IMPORT_REGIME_LAB %in% c("GSP ZERO", "GSP NON ZERO"))


utilization_rate <- gsp_data %>%
  group_by(year) %>%
  summarise(GSP_Value = sum(value),
            Total_Value = sum(simulated_eurostat %>% 
                                filter(PARTNER_LAB == "France", year == unique(year)) %>% 
                                pull(value)),
            Utilization_Rate = GSP_Value / Total_Value)

# Make plot
ggplot(utilization_rate, aes(x = year, y = Utilization_Rate)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = seq(min(years), max(years), by = 1)) + # Adjusting X-axis
  theme_minimal() +
  labs(title = "GSP Utilization Rate Over Years",
       x = "Year",
       y = "Utilization Rate")

# Test

unique_entities <- unique(simulated_market_share$Entity)
all(unique_entities %in% c("China", "LDCs")) && length(unique_entities) == 2


all(simulated_market_share$Share >= 0 & simulated_market_share$Share <= 1)


all(simulated_eurostat$value >= 100000)


all(grepl("^61", simulated_eurostat$PRODUCT) | grepl("^62", simulated_eurostat$PRODUCT))


all(simulated_market_share$Export >= 300000 & simulated_market_share$Export <= 1500000)









