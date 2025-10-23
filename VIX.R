# Plot: NFCI

rm(list = ls())

# Load required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

VIXCLS <- read_excel(
  "~/Library/CloudStorage/OneDrive-DrexelUniversity/Fed Challenge 2025/VIXCLS.xlsx",
  col_types = c("date", "numeric")
) %>%
  mutate(observation_date = as.Date(observation_date)) %>%  # ensure Date
  arrange(observation_date)

ggplot(VIXCLS, aes(x = observation_date, y = VIXCLS)) +
  geom_line(linewidth = 3, color = "#E87722") +
  labs(
    title = "VIX (Weekly Average)",
    y = "Index", x = NULL,
    caption = "Source: Chicago Board Options Exchange"
  ) +
  scale_x_date(
    date_breaks = "2 months",
    date_labels = "%m/%y",
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(breaks = seq(10, 45, 5)) +
  coord_cartesian(ylim = c(10, 45)) +   # y-axis from 10 to 45
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 50),
    plot.caption = element_text(size = 25),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 30),
    axis.title.y = element_text(size = 40),
    panel.grid.minor = element_blank()
  )
