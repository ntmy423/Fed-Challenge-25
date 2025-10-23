# Plot: Future Fed Funds Rate — GDP-style theme

rm(list = ls())

library(ggplot2)
library(scales)
library(lubridate)

# Data
fed_funds <- data.frame(
  Month = c("AUG 2025","SEP 2025","OCT 2025","NOV 2025","DEC 2025",
            "JAN 2026","FEB 2026","MAR 2026","APR 2026","MAY 2026",
            "JUN 2026","JUL 2026","AUG 2026","SEP 2026","OCT 2026",
            "NOV 2026","DEC 2026"),
  Implied_Rate = c(4.32, 4.225, 4.095, 3.915, 3.775, 3.7, 3.6, 3.545,
                   3.465, 3.385, 3.325, 3.235, 3.155, 3.115, 3.055, 3.015, 3.05)
)

# Parse to dates and make ordered x labels
fed_funds$Date <- parse_date_time(fed_funds$Month, orders = "b Y")
fed_funds$Month_fmt <- format(fed_funds$Date, "%m/%y")
fed_funds$Month_fmt <- factor(fed_funds$Month_fmt, levels = fed_funds$Month_fmt)

# Plot
p <- ggplot(fed_funds, aes(x = Month_fmt, y = Implied_Rate, group = 1)) +
  geom_line(linewidth = 1.2, color = "#E87722") +
  geom_point(size = 2.5, color = "#0C233C") +
  scale_y_continuous(name = "Rate (%)", breaks = seq(3, 4.5, by = 0.25)) +
  labs(
    title   = "30-Day Federal Funds Futures",
    x       = NULL,
    caption = "Source: CME FedWatch Tool"
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
    axis.text.x = element_text(angle = 45),
    axis.title.y     = element_text(size = 16)
  )

print(p)

# Save (GDP-style save pattern)
ggsave("30-Day Federal Funds Futures.png", p, width = 10, height = 5, dpi = 600)
