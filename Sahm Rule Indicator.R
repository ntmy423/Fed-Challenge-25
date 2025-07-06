# Plot: Real-time Sahm Rule Indicator

rm(list = ls())

# Load the package
library(ggplot2)
library(readr)
library(scales)
library(readxl)

# Load data
Labor_Market_Data <- read_excel("~/Library/CloudStorage/OneDrive-DrexelUniversity/Fed Challenge 2025/Skeleton/Labor Market - Data.xlsx", sheet = "Sahm Rule Indicator")

# Convert date column to Date format
Labor_Market_Data$observation_date <- as.Date(Labor_Market_Data$observation_date, format = "%m/%d/%y")

# Plot
ggplot(Labor_Market_Data, aes(x = observation_date, y = SAHMREALTIME)) +
  geom_line(color = "#0072B2", size = 1) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  scale_y_continuous(name = "Percentage Points", limits = c(-1, 10), breaks = seq(-1, 10, 1)) +
  scale_x_date(name = NULL,
               date_breaks = "6 months",
               date_labels = "%m/%Y",
               expand = c(0.01, 0.01)) +
  labs(
    title = "Real-time Sahm Rule Recession Indicator",
    caption = "Source: Sahm, Claudia via FRED®"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.caption = element_text(hjust = 0),
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    panel.grid.minor = element_blank()
  )
