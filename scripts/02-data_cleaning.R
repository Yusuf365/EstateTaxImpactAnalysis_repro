library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)

# Define file paths for input and output
input_file_path <- "/cloud/project/inputs/data/Eurostat.csv"
market_share_output_path <- "/cloud/project/outputs/data/market_share_data.csv"
bgd_utilization_output_path <- "/cloud/project/outputs/data/bgd_utilization_data.csv"
figure3_output_path <- "Figure3.png"

# Load and preprocess Eurostat data with an emphasis on pipeline use
dat <- read_csv(input_file_path) %>%
  select(-1) %>%
  rename(DECLARANT_LAB = `1`, PARTNER_LAB = `2`, PRODUCT = `3`, PRODUCT_LAB = `4`, 
         STAT_REGIME_LAB = `5`, ELIGIBILITY_LAB = `6`, IMPORT_REGIME_LAB = `7`, 
         year = `8`, value = `9`) %>%
  filter(!DECLARANT_LAB %in% c("EU total", "EU MEMBER STATES- EVOLUTIVE")) %>%
  mutate(value = value / 1,
         group_id = group_indices(DECLARANT_LAB, PARTNER_LAB, PRODUCT, year))

# Create GSP filtered dataset
gsp_only <- dat %>%
  filter(IMPORT_REGIME_LAB %in% c("GSP ZERO", "GSP NON ZERO")) %>%
  mutate(gsp_value = value)

# Compute and save market share data
dat %>%
  group_by(year, PARTNER_LAB) %>%
  summarise(total_value = sum(value)) %>%
  left_join(gsp_only %>%
              group_by(year, PARTNER_LAB) %>%
              summarise(gsp_value = sum(gsp_value)), by = c("year", "PARTNER_LAB")) %>%
  mutate(share = gsp_value / total_value) %>%
  ungroup() %>%
  write_csv(market_share_output_path)

# Calculate and save utilization rate for Bangladesh
dat %>%
  left_join(gsp_only %>% select(group_id, gsp_value), by = "group_id") %>%
  replace_na(list(gsp_value = 0)) %>%
  mutate(frac = gsp_value / value,
         post = if_else(year >= 2011, 1, 0),
         woven = if_else(PRODUCT >= 620000, 1, 0)) %>%
  filter(value != 0, PARTNER_LAB == "BANGLADESH") %>%
  group_by(year, woven) %>%
  summarise(gsp_value = sum(gsp_value), total_value = sum(value), frac = gsp_value / total_value) %>%
  ungroup() %>%
  write_csv(bgd_utilization_output_path)

# Assuming market_share and bgd data frames are correctly formatted and available for plotting

# Plot Figure 3 (Market Share)
market_share <- read_csv(market_share_output_path)  # Assuming the file exists

Figure3 <- ggplot(market_share, aes(x = year, y = share, color = as.factor(PARTNER_LAB))) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(y = "Market Share", color = "Partner Lab") +
  scale_color_manual(values = c("grey50", "black")) +
  theme_minimal(base_size = 20) +
  theme(legend.position = "bottom")

# Save the plot
ggsave(figure3_output_path, Figure3, width = 8, height = 5, units = "in")