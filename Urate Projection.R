# Plot: Unemployment Projection

rm(list = ls())

library(ggplot2)
library(dplyr)
library(lubridate)
library(readxl)

# Read and process your data
Labor_Market_Data <- read_excel(
  "~/Library/CloudStorage/OneDrive-DrexelUniversity/Fed Challenge 2025/Skeleton/Labor Market - Data.xlsx",
  sheet = "Urate - Forecasting",
  col_types = c("date", "numeric", "skip", "skip", "skip", "skip")
)

# Convert date, sort, and assign 'Type'
Labor_Market_Data$Date <- as.Date(sub(" UTC", "", Labor_Market_Data$`Observation date`))
Labor_Market_Data <- Labor_Market_Data %>% arrange(Date)
Labor_Market_Data$Type <- ifelse(Labor_Market_Data$Date < as.Date("2025-07-01"), "Actual", "Projection")

# Find the last actual date
last_actual_date <- max(Labor_Market_Data$Date[Labor_Market_Data$Type == "Actual"])

# For plotting: 
# - Actual includes all dates <= last_actual_date
# - Projection includes all dates >= last_actual_date
Labor_Market_Data <- Labor_Market_Data %>%
  mutate(
    Plot_Actual = Date <= last_actual_date,
    Plot_Projection = Date >= last_actual_date
  )

# Plot
ggplot() +
  geom_line(
    data = Labor_Market_Data %>% filter(Plot_Actual),
    aes(x = Date, y = `Unemployment Projection`, color = "Actual", linetype = "Actual"),
    size = 1.5
  ) +
  geom_line(
    data = Labor_Market_Data %>% filter(Plot_Projection),
    aes(x = Date, y = `Unemployment Projection`, color = "Projection", linetype = "Projection"),
    size = 1.5
  ) +
  scale_color_manual(
    name = NULL,
    values = c("Actual" = "red", "Projection" = "blue")
  ) +
  scale_linetype_manual(
    name = NULL,
    values = c("Actual" = "solid", "Projection" = "dashed")
  ) +
  scale_x_date(
    breaks = as.Date(paste0(2020:2026, "-01-01")),
    labels = 2020:2026,
    expand = c(0.01, 0.01)
  ) +
  labs(
    title = "Unemployment Projection",
    subtitle = "Source: Survey of Professional Forecasters - Philadelphia Fed",
    y = "Rate",
    x = NULL
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = element_text(face = "italic", size = 12),
    legend.position = "bottom",
    legend.text = element_text(size = 14)
  )
