#### Preamble ####
# Purpose: Simulates the data for export & Bangladesh
# Author: Mohammed Yusuf Shaikh
# Date: 11 February 2024
# Contact: Mohhammedn Yusuf Shaikh
# License: MIT
# Pre-requisites: None


# Test

unique_entities <- unique(simulated_market_share$Entity)
all(unique_entities %in% c("China", "LDCs")) && length(unique_entities) == 2


all(simulated_market_share$Share >= 0 & simulated_market_share$Share <= 1)


all(simulated_eurostat$value >= 100000)


all(grepl("^61", simulated_eurostat$PRODUCT) | grepl("^62", simulated_eurostat$PRODUCT))


all(simulated_market_share$Export >= 300000 & simulated_market_share$Export <= 1500000)









