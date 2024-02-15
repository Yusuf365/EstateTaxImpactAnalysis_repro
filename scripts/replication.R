#### Preamble ####
# Purpose: Replication dataset
# Author: Mohammed Yusuf Shaikh
# Date: 11 February 2024
# Contact: mohammedyusuf.shaikh@mail.utoronto.ca
# License: MIT
# Pre-requisites: Change Directory for running the code


ldc_only <- read_csv("/cloud/project/outputs/data/ldc_only.csv")

market_share_data <- read_csv("/cloud/project/outputs/data/market_share_data.csv")

bgd  <- read_csv("/cloud/project/outputs/data/bgd.csv")




ggplot(ldc_only) +
  geom_line(aes(x = year, y = util), size = 1) +
  geom_point(aes(x = year, y = util), size = 2) +
  xlab("Year") + ylab("EBA Utilization Rate") +
  theme_bw() +
  theme(text = element_text(size = 20), axis.text.x = element_text())

ggplot(market_share_data, aes(x = Year, y = share, group = type, color = type)) +
geom_line(size = 1) +
  geom_point(size = 3) +
  ylab("Market Share") +
  scale_color_manual(values = c("grey50", "black")) +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.title = element_blank(),
        legend.position = "bottom") +
  labs(color = "Type")

# Define colors for the lines
cols <- c("1" = "black", "0" = "grey")

ggplot(data = bgd, aes(x = year, y = frac, group = as.factor(woven))) +
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
        axis.text.x = element_text(color = "black", size = 10, face = "plain"),
        axis.text.y = element_text(color = "black", size = 10, face = "plain"),
        axis.title.x = element_text(color = "Black", size = 10, face = "plain"),
        axis.title.y = element_text(color = "Black", size = 10, face = "plain"),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 10, hjust = 0.9))



