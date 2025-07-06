# Plot: Quits and Layoffs & Discharges

rm(list = ls())

# Load packages
library(readxl)
library(ggplot2)
library(scales)
library(readxl)

# Load data
Labor_Market_Data <- read_excel("~/Library/CloudStorage/OneDrive-DrexelUniversity/Fed Challenge 2025/Skeleton/Labor Market - Data.xlsx", sheet = "Quits & Layoffs", col_types = c("date", "numeric", "numeric", "skip", "skip"))

# Convert date column to Date format
Labor_Market_Data$observation_date <- as.Date(Labor_Market_Data$observation_date, format = "%m/%d/%y")

# Plot
ggplot(Labor_Market_Data, aes(x = observation_date)) +
  geom_line(aes(y = `Quits Rate`, color = "Quits Rate"), size = 1.2) +
  geom_line(aes(y = `Layoffs & Discharges Rate`, color = "Layoffs & Discharges Rate"), size = 1.2) +
  scale_color_manual(values = c("Quits Rate" = "navy", "Layoffs & Discharges Rate" = "firebrick")) +
  scale_y_continuous( name = "Percent Change YoY",
                      limits = c(-100, 100),
                      breaks = seq(-100, 100, 20)
  ) +
  scale_x_date(name = NULL, date_breaks = "6 months", date_labels = "%m/%Y") +
  labs(
    title = "Quits and Layoffs & Discharges",
    subtitle = "Source: BLS",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(face = "italic", size = 12),
    legend.position = "bottom"
  )