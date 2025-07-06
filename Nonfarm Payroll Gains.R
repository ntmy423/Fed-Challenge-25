# Plot: Nonfarm Payroll Gains

rm(list = ls())

library(ggplot2)
library(readxl)
library(scales)
library(lubridate)

# Load data
Labor_Market_Data <- read_excel("~/Library/CloudStorage/OneDrive-DrexelUniversity/Fed Challenge 2025/Skeleton/Labor Market - Data.xlsx", 
                                sheet = "Nonfarm Payroll Gains")

# Convert observation_date to Date type
# Your format looks like m/d/yy or m/d/yyyy
Labor_Market_Data$observation_date <- as.Date(Labor_Market_Data$observation_date, format = "%m/%d/%y")
# If some dates are NA, try "%m/%d/%Y" instead

# Set axis limits (based on your screenshot and actual data)
ymin <- -12.5
ymax <- 10

# Plot
ggplot(Labor_Market_Data, aes(x = observation_date, y = `Nonfarm Payroll Gains - 12 month Net Change`)) +
  geom_line(color = "#0066CC", size = 1.2) +
  scale_y_continuous(
    limits = c(ymin, ymax),
    breaks = seq(ymin, ymax, by = 2.5)
  ) +
  scale_x_date(
    limits = c(as.Date("2017-01-01"), as.Date("2025-01-01")),
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  labs(
    title = "Nonfarm Payroll Gains",
    subtitle = "Source: BLS"
  ) +
  geom_hline(yintercept = 0, color = "black") +
  labs(
    y = "12-month Percent Change",
    x = NULL
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", size = 12),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.minor = element_blank()
  )
