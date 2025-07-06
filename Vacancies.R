# Plot: Vacancy (Job openings) & Indeed Job Postings

rm(list = ls())

library(ggplot2)
library(scales)
library(readr)
library(lubridate)
library(dplyr)
library(readxl)

# Load data
Labor_Market_Data <- read_excel("~/Library/CloudStorage/OneDrive-DrexelUniversity/Fed Challenge 2025/Skeleton/Labor Market - Data.xlsx", sheet = "Vacancy - Indeed")


# Convert date column to Date format
Labor_Market_Data$observation_date <- as.Date(Labor_Market_Data$observation_date, format = "%m/%d/%y")

# Plot
ggplot(Labor_Market_Data, aes(x = observation_date)) +
  geom_line(aes(y = `Vacancy (based 02/2020)`, color = "JOLTS (Feb 2020 = 100)"), size = 1.2) +
  geom_line(aes(y = `Indeed Job Postings (based 02/2020)`, color = "Indeed Job Postings (Feb 2020 = 100)"), size = 1.2) +
  scale_y_continuous(
    name = "Index",
    limits = c(90, 180),
    breaks = seq(90, 180, 10)
  ) +
  scale_x_date(
    name = NULL,
    date_breaks = "6 months",
    date_labels = "%m/%Y"
  ) +
  scale_color_manual(
    name = NULL,
    values = c("JOLTS (Feb 2020 = 100)" = "blue", "Indeed Job Postings (Feb 2020 = 100)" = "darkgreen")
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Vacancies",
    subtitle = "Sources: BLS, Indeed"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.subtitle = element_text(face = "italic", size = 12),
    panel.grid.minor = element_blank()
  )
