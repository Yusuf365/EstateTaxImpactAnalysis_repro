#### Preamble ####
# Purpose: Clean data
# Author: Mohammed Yusuf Shaikh
# Date: 11 February 2024
# Contact: mohammedyusuf.shaikh@mail.utoronto.ca
# License: MIT
# Pre-requisites: Change Directory for running the code


library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(janitor)

# Load and preprocess Eurostat data
dat <- read_csv("/cloud/project/inputs/data/Eurostat.csv") %>%
  select(-1)

# Rename columns for clarity
colnames(dat) <- c("DECLARANT_LAB", "PARTNER_LAB", "PRODUCT", "PRODUCT_LAB", 
                   "STAT_REGIME_LAB", "ELIGIBILITY_LAB", "IMPORT_REGIME_LAB", 
                   "year", "value")

# Remove aggregated destinations
dat <- dat %>%
  filter(!DECLARANT_LAB %in% c("EU total", "EU MEMBER STATES- EVOLUTIVE (EU15 UNTIL 30/04/2004, EU25 UNTIL 31/12/2006, EU27 UNTIL 30/06/2013, EU 28 SINCE 01/07/2013)"))

# Adjust value column
dat$value <- dat$value / 1

# Create group_id variable
dat$group_id <- dat %>% group_indices(PARTNER_LAB, PRODUCT, DECLARANT_LAB, year)

# Filter data for GSP ZERO and GSP NON ZERO
gsp_only <- dat %>%
  filter(IMPORT_REGIME_LAB %in% c("GSP ZERO", "GSP NON ZERO")) %>%
  mutate(gsp_value = value)

# Save data for Figure 3
total <- aggregate(value ~ year + PARTNER_LAB, data = dat, FUN = sum)
total_gsp <- aggregate(gsp_value ~ year + PARTNER_LAB, data = gsp_only, FUN = sum)
market_share <- merge(total, total_gsp, by = c("year", "PARTNER_LAB"), all = TRUE)
market_share$share <- market_share$gsp_value / market_share$value
write_csv(market_share, "/cloud/project/outputs/data/market_share_data.csv")

# Calculate utilization rate for Figure 4
total <- aggregate(value ~ group_id + PARTNER_LAB + PRODUCT + year + DECLARANT_LAB, data = dat, FUN = sum)
total_gsp <- aggregate(gsp_value ~ group_id, data = gsp_only, FUN = sum)
util <- merge(total, total_gsp, by = "group_id", all = FALSE, all.x = TRUE)
util$gsp_value[is.na(util$gsp_value)] <- 0
util$frac <- util$gsp_value / util$value
util <- util[util$value != 0, ]
util$post <- ifelse(util$year >= 2011, 1, 0)
util$woven <- ifelse(util$PRODUCT >= 620000, 1, 0)
bgd <- util %>%
  filter(PARTNER_LAB == "BANGLADESH") %>%
  group_by(year, woven) %>%
  summarise(gsp_value = sum(gsp_value), value = sum(value)) %>%
  mutate(frac = gsp_value / value)
write_csv(bgd, "/cloud/project/outputs/data/bgd_utilization_data.csv")



ldc_only <- util |>
  group_by(year) |>
  summarise(util = mean(frac), var = sd(frac))

write_csv(ldc_only, "/cloud/project/outputs/data/ldc_only.csv")





# Plot Figure 3 (Market Share)
# Read in the market share data (update the path as needed)
market_share <- read_csv("/cloud/project/inputs/data/market_share.csv")


write_csv(market_share, "/cloud/project/outputs/data/market_share_data.csv")


# Figure 4


# Prepare data for plotting
bgd <- util %>%
  filter(PARTNER_LAB == "BANGLADESH") %>%
  group_by(year, woven) %>%
  summarise(gsp_value = sum(gsp_value), value = sum(value)) %>%
  mutate(frac = gsp_value / value)

write_csv(bgd, "/cloud/project/outputs/data/bgd.csv")


# Define colors for the lines
cols <- c("1" = "black", "0" = "grey")

# Create Figure 4
figure4 <- ggplot(data = bgd, aes(x = year, y = frac, group = as.factor(woven))) +
  geom_line(aes(color = as.factor(woven)), size = 1) +
  geom_point(aes(color = as.factor(woven)), size = 3) +
  xlab("Year") + ylab("Utilization Rate") +
  ggtitle("Change in EBA Utilization Rate for Apparel Products") +
  labs(color = "Product Type") +
  scale_color_manual(values = cols, breaks = c("1", "0"), labels = c("Woven", "Knit")) +
  theme_bw() +
  scale_x_continuous(name = "Year", limits = c(2001, 2018), breaks = scales::pretty_breaks(n = 9)) +
  scale_y_continuous(limits = c(0.1, 1), breaks = scales::pretty_breaks(n = 5)) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        axis.text.x = element_text(color = "black", size = 20, face = "plain"),
        axis.text.y = element_text(color = "black", size = 20, face = "plain"),
        axis.title.x = element_text(color = "Black", size = 20, face = "plain"),
        axis.title.y = element_text(color = "Black", size = 20, face = "plain"),
        legend.text = element_text(size = 20),
        plot.title = element_text(size = 22, hjust = 0.5))

# Print the plot
print(figure4)

