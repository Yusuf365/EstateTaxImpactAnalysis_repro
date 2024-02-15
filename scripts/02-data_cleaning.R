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

# Aggregate data
total <- aggregate(value ~ group_id + PARTNER_LAB + PRODUCT + year + DECLARANT_LAB, data = dat, FUN = sum)
total_gsp <- aggregate(gsp_value ~ group_id, data = gsp_only, FUN = sum)
util <- merge(total, total_gsp, by = "group_id", all = FALSE, all.x = TRUE)

# Calculate fraction
util$gsp_value[is.na(util$gsp_value)] <- 0
util$frac <- util$gsp_value / util$value
util <- util[util$value != 0, ]

# Adjust post and woven columns
util$post <- ifelse(util$year >= 2011, 1, 0)
util$woven <- ifelse(util$PRODUCT >= 620000, 1, 0)

# Calculate utilization rate
ldc_only <- util %>%
  group_by(year) %>%
  summarise(util = mean(frac), var = sd(frac))

# Plot Figure 1
figure1 <- ggplot(ldc_only) +
  geom_line(aes(x = year, y = util), size = 1) +
  geom_point(aes(x = year, y = util), size = 2) +
  xlab("Year") + ylab("EBA Utilization Rate") +
  theme_bw() +
  theme(text = element_text(size = 20), axis.text.x = element_text())

print(figure1)

