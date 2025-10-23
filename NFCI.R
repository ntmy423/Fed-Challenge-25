# Plot: NFCI (GDP-style theme)

rm(list = ls())

# Libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

# Data
nfci <- read_excel(
  "~/Library/CloudStorage/OneDrive-DrexelUniversity/Fed Challenge 2025/NFCI.xlsx",
  col_types = c("date", "numeric")
) %>%
  mutate(observation_date = as.Date(observation_date))

# COVID recession (Feb 2020–Apr 2020)
recession_start <- as.Date("2020-02-01")
recession_end   <- as.Date("2020-04-30")

# Plot
p <- ggplot(nfci, aes(x = observation_date, y = NFCI)) +
  # Shaded recession
  annotate("rect", xmin = recession_start, xmax = recession_end,
           ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.6) +
  geom_line(linewidth = 1.2, color = "#E87722", na.rm = TRUE) +
  scale_x_date(date_breaks = "6 months", date_labels = "%m/%y") +
  coord_cartesian(ylim = c(-0.8, 0.6)) +
  labs(
    title   = "Chicago Fed National Financial Conditions Index",
    y       = "Index",
    x       = NULL,
    caption = "Source: Federal Reserve Bank of Chicago"
  ) +
  theme(
    legend.position  = "bottom",
    legend.title     = element_blank(),
    legend.text      = element_text(size = 14),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray95"),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", size = 25),
    plot.caption     = element_text(size = 12),
    axis.text        = element_text(size = 16),
    axis.title.y     = element_text(size = 16)
  )

print(p)

# Save (GDP-style save pattern)
ggsave("NFCI.png", p, width = 10, height = 5, dpi = 600)
