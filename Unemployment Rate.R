# Plot: Unemployment Rate

rm(list = ls())

# Load the package
library(ggplot2)
library(scales)  # for date formatting
library(readr)
library(dplyr)
library(readxl)

# Load data
Labor_Market_Data <- read_excel("~/Library/CloudStorage/OneDrive-DrexelUniversity/Fed Challenge 2025/Skeleton/Labor Market - Data.xlsx", sheet = "Unemployment Rate")

# Convert date column to Date format
Labor_Market_Data$date <- as.Date(Labor_Market_Data$`Name/Time`, format = "%m/%d/%y")

ggplot(Labor_Market_Data, aes(x = date)) +
  geom_line(aes(y = U3, color = "U-3 Unemployment"), size = 1.2) +
  geom_line(aes(y = U6, color = "U-6 Unemployment"), size = 1.2) +
  scale_color_manual(values = c("U-3 Unemployment" = "navy", "U-6 Unemployment" = "firebrick")) +
  scale_y_continuous(name = "Percent", limits = c(0, 25)) +
  scale_x_date(name = NULL, date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Unemployment Rate",
    subtitle = "Source: BLS",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(face = "italic", size = 12),
    legend.position = "bottom",
  )
