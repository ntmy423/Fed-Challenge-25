# Plot: Unemployment Rate by Race

rm(list = ls())

# Load required libraries
library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)

# Load data
Labor_Market_Data <- read_excel("~/Library/CloudStorage/OneDrive-DrexelUniversity/Fed Challenge 2025/Skeleton/Labor Market - Data.xlsx", sheet = "Urate by race - reshape")

# Convert date column to Date format
Labor_Market_Data$observation_date <- as.Date(Labor_Market_Data$observation_date, format = "%m/%d/%y")

# Reshape data from wide to long
Labor_Market_Data_long <- Labor_Market_Data %>%
  pivot_longer(cols = -observation_date,
               names_to = "Race",
               values_to = "Unemployment_Rate")

# Plot
ggplot(Labor_Market_Data_long, aes(x = observation_date, y = Unemployment_Rate, color = Race)) +
  geom_line(size = 1) +
  labs(title = "Unemployment Rates by Race",
       caption = "Source: BLS",
       x = NULL,
       y = "Percent",
       color = "Race") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size=16, face = "bold"),
        plot.caption = element_text(hjust = 0, size = 10, face = "italic"),
        axis.text = element_text(size=12),
        axis.title = element_text(size=13),
        legend.title = element_blank(),
        legend.position = "top") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values = c("Black Unemployment Rate" = "blue",
                                "Hispanic Unemployment Rate" = "red",
                                "White Unemployment Rate" = "green",
                                "Asian Unemployment Rate" = "orange"))
