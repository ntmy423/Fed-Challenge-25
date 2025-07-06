# Plot: Jobless Claims

rm(list = ls())

library(ggplot2)
library(scales)
library(readxl)

# Load data
Labor_Market_Data <- read_excel("~/Library/CloudStorage/OneDrive-DrexelUniversity/Fed Challenge 2025/Skeleton/Labor Market - Data.xlsx", sheet = "Jobless Claims")

# Convert date column to Date format
Labor_Market_Data$observation_date <- as.Date(Labor_Market_Data$observation_date, format = "%m/%d/%y")

# Plot
ggplot(Labor_Market_Data, aes(x = observation_date, y = `Jobless Claims`)) +
  geom_line(color = "#001F5B", size = 1.2) +
  scale_y_continuous(
    name = "People",
    limits = c(190000, 260000),
    breaks = seq(190000, 260000, 10000),
    labels = comma
  ) +
  scale_x_date(
    name = NULL,
    date_breaks = "2 month",
    date_labels = "%m/%d/%y"
  ) +
  labs(
    title = "Initial Jobless Claims",
    subtitle = "Source: BLS"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    plot.subtitle = element_text(face = "italic", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )
