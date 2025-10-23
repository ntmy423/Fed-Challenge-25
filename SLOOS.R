# Plot: Net % Tightening C&I Loans (SLOOS)

rm(list = ls())

# Packages
library(tidyverse)
library(lubridate)
library(readxl)
library(scales)

# ---- 1) Load data -----------------------------------------------------------
SLOOS <- read_excel(
  "~/Library/CloudStorage/OneDrive-DrexelUniversity/Fed Challenge 2025/SLOOS.xlsx",
  col_types = c("date", "numeric", "numeric", "skip", "skip", "skip", "skip")
)

# Standardize names
names(SLOOS)[1:3] <- c("observation_date", "DRTSCILM", "DRTSCIS")
SLOOS <- SLOOS %>% mutate(observation_date = as.Date(observation_date))

# ---- 2) Reshape for plotting -------------------------------------------------
plot_df <- SLOOS %>%
  pivot_longer(
    c(DRTSCILM, DRTSCIS),
    names_to = "Series", values_to = "Value"
  ) %>%
  mutate(Series = recode(
    Series,
    DRTSCILM = "Net % Tightening C&I Loans: Large & Middle-Market Firms",
    DRTSCIS  = "Net % Tightening C&I Loans: Small Firms"
  ))

# ---- 3) Plot -----------------------------------------------------------------
recession_start <- as.Date("2020-02-01")
recession_end   <- as.Date("2020-04-30")

legend_cols <- c(
  "Net % Tightening C&I Loans: Large & Middle-Market Firms" = "#0C233C",
  "Net % Tightening C&I Loans: Small Firms"                  = "#E87722"
)

p <- ggplot(plot_df, aes(x = observation_date, y = Value, color = Series)) +
  # COVID recession shading
  annotate("rect", xmin = recession_start, xmax = recession_end,
           ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_line(linewidth = 1.2, na.rm = TRUE) +
  scale_color_manual(values = legend_cols, drop = FALSE) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(
    name   = "Percent",
    labels = percent_format(scale = 1),
    breaks = seq(-40, 80, 20),
    limits = c(-40, 80)
  ) +
  labs(
    title   = "Senior Loan Officer Opinion Survey on Bank Lending Practices",
    x       = NULL,
    subtitle = "Shaded area = COVID recession (NBER)",
    caption = "Source: Board of Governors of the Federal Reserve System"
  ) +
  theme(
    legend.position  = "bottom",
    legend.title     = element_blank(),
    legend.text      = element_text(size = 13),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray95"),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", size = 20),
    plot.caption     = element_text(size = 12),
    axis.text        = element_text(size = 16),
    axis.title.y     = element_text(size = 16)
  )  +
  expand_limits(x = as.Date("2026-01-01"))

print(p)

# Save (GDP-style save pattern)
ggsave("SLOOS - Net Tightening C&I Loans.png", p, width = 10, height = 6, dpi = 600)
