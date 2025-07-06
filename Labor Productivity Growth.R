# Plot: Labor Productivity Growth

rm(list = ls())

# Load the package
library(ggplot2)
library(readr)
library(lubridate)
library(dplyr)
library(readxl)

# Load data
Labor_Market_Data <- read_excel("~/Library/CloudStorage/OneDrive-DrexelUniversity/Fed Challenge 2025/Skeleton/Labor Market - Data.xlsx", sheet = "Labor Productivity Growth")

# Convert date column to Date format
Labor_Market_Data$observation_date <- as.Date(Labor_Market_Data$observation_date, format = "%m/%d/%y")

# Plot
ggplot(Labor_Market_Data, aes(x = observation_date, y =`Labor Productivity`)) +
  geom_col(fill = "blue") +
  geom_hline(yintercept = 0, color = "black") +
  labs(
    title = "Annualized Quarterly Labor Productivity Growth",
    y = "Percent Change YoY",
    x = NULL,
    caption = "Source: BLS"
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.caption = element_text(hjust = 0)
  )
