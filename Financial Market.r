# {r setup, include=FALSE}
# knitr::opts_chunk$set(echo = TRUE)

# install.packages(c("fredr", "tidyverse"))

# Load them into the R session (do this every time you start R):
library(fredr)     # FRED API
library(tidyverse) # ggplot2, dplyr, readr, etc.
library(lubridate)
library(scales)

fredr_set_key("aba39be8447ebff96c9230ec5ae6b007")

# Brand colors
col_orange <- "#E87722"
col_navy   <- "#0C233C"

# Reusable GDP-style theme
gdp_theme <- function() {
  theme(
    legend.position  = "bottom",
    legend.title     = element_blank(),
    legend.text      = element_text(size = 14),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray95"),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", size = 20),
    plot.caption     = element_text(size = 12),
    axis.text        = element_text(size = 16),
    axis.title.y     = element_text(size = 16)
  )
}

# Helper to build caption text
fred_caption <- function(series_id) {
  paste0("Source: FRED (Series: ", series_id, ")")
}

# Recession shading (Feb 2020–Apr 2020)
recess_shade <- tibble(
  start = as.Date("2020-02-01"),
  end   = as.Date("2020-04-30")
)

# -------- Auto-save helper (adds your size/res) --------
dir.create("Exports", showWarnings = FALSE)
auto_save <- function() {
  p  <- ggplot2::last_plot()
  nm <- p$labels$title
  if (is.null(nm) || nm == "") nm <- paste0("plot_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  nm <- gsub("[^A-Za-z0-9]+", "_", nm)
  ggplot2::ggsave(file.path("Exports", paste0(nm, ".png")), p,
                  width = 10, height = 6, dpi = 600)
}
# -------------------------------------------------------

# Pull Fed balance-sheet data (sample window)
walcl <- fredr(
  series_id         = "WALCL",
  observation_start = as.Date("2024-07-01"),
  observation_end   = as.Date("2025-12-31")
)

ggplot(walcl, aes(x = date, y = value / 1e3)) +   # scale to billions
  geom_line(color = col_orange, linewidth = 1.2) +
  labs(
    title    = "Federal Reserve Balance Sheet",
    subtitle = "Levels, 2025",
    x        = NULL,
    y        = "Billions of U.S. Dollars",
    caption  = fred_caption("WALCL")
  ) +
  gdp_theme()
auto_save()

# 2s10s spread series
t10y2y <- fredr(
  series_id         = "T10Y2Y",
  observation_start = as.Date("2017-01-01"),
  observation_end   = as.Date("2025-12-31")
)

ggplot(t10y2y, aes(date, value)) +
  # Recession band
  geom_rect(
    data = recess_shade,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
    fill = "grey80", alpha = 0.5, inherit.aes = FALSE
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(-1.5, 2), breaks = seq(-1.5, 2, by = 0.5)) +
  geom_line(color = col_orange, linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5, color = "black") +
  labs(
    title    = "Treasury Spread (T10Y2Y)",
    subtitle = "10-Year Treasury Constant Maturity minus 2-Year Treasury Constant Maturity; Shaded area = COVID recession (NBER)",
    x        = NULL,
    y        = "Percentage Points",
    caption  = "Source: Federal Reserve Bank of St. Louis"
  ) +
  gdp_theme()
auto_save()

# High-Yield option-adjusted spread
hy <- fredr(
  series_id         = "BAMLH0A0HYM2",
  observation_start = as.Date("2024-07-01"),
  observation_end   = Sys.Date()
)

ggplot(hy, aes(date, value)) +
  geom_line(color = col_orange, linewidth = 1.2) +
  scale_y_continuous(labels = percent_format(scale = 1), name = "Percent") +
  labs(
    title    = "ICE BofA US High-Yield Option-Adjusted Spread",
    subtitle = "Percent, Weekly Frequency",
    x        = NULL,
    caption  = fred_caption("BAMLH0A0HYM2")
  ) +
  gdp_theme()
auto_save()

# 30-Year Fixed-Rate Mortgage Average
mortgage30 <- fredr(
  series_id         = "MORTGAGE30US",
  observation_start = as.Date("2021-01-01"),
  observation_end   = Sys.Date()
)

ggplot(mortgage30, aes(date, value)) +
  geom_line(color = col_orange, linewidth = 1.2) +
  scale_y_continuous(labels = percent_format(scale = 1), name = "Percent") +
  labs(
    title    = "30-Year Fixed-Rate Mortgage Average in the United States",
    x        = NULL,
    caption  = fred_caption("MORTGAGE30US")
  ) +
  gdp_theme()
auto_save()

# Credit-card delinquency rate
cc_delin <- fredr(
  series_id         = "DRCCLACBS",
  observation_start = as.Date("2019-01-01"),
  observation_end   = Sys.Date()
) |>
  mutate(quarter_label = paste0("Q", quarter(date), " ", year(date)))

keep <- quarter(cc_delin$date) %in% c(1, 3)

ggplot(cc_delin, aes(x = date, y = value)) +
  geom_rect(
    data = recess_shade,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
    fill = "grey80", alpha = 0.5, inherit.aes = FALSE
  ) +
  geom_line(color = col_orange, linewidth = 1.2) +
  geom_point(color = "black", size = 2) +
  scale_x_date(breaks = cc_delin$date[keep], labels = cc_delin$quarter_label[keep]) +
  scale_y_continuous(labels = percent_format(scale = 1), name = "Percent - Quarterly Data") +
  labs(
    title    = "Delinquency Rate on Credit-Card Loans - All Commercial Banks",
    x        = NULL,
    subtitle = "Shaded area = COVID recession (NBER)",
    caption  = "Source: Board of Governors of the Federal Reserve System"
  ) +
  gdp_theme() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
auto_save()

# Business loan delinquency rate
biz_delin <- fredr(
  series_id         = "DRBLACBS",
  observation_start = as.Date("2019-10-01"),
  observation_end   = Sys.Date()
) |>
  mutate(quarter_lbl = paste0("Q", quarter(date), " ", year(date)))

ggplot(biz_delin, aes(date, value)) +
  geom_rect(
    data = recess_shade,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
    fill = "grey80", alpha = 0.5, inherit.aes = FALSE
  ) +
  geom_line(color = col_orange, linewidth = 1.2) +
  geom_point(color = col_orange, size = 2) +
  scale_x_date(breaks = biz_delin$date, labels = biz_delin$quarter_lbl) +
  scale_y_continuous(labels = percent_format(scale = 1), name = "Percent") +
  labs(
    title    = "Delinquency Rate on Business Loans",
    subtitle = "All Commercial Banks; Quarterly observations with 2020 recession shading",
    x        = NULL,
    caption  = fred_caption("DRBLACBS")
  ) +
  gdp_theme() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
auto_save()

# Active listing count
housing_inv <- fredr(
  series_id         = "ACTLISCOUUS",
  observation_start = as.Date("2016-07-01"),
  observation_end   = Sys.Date()
)

ggplot(housing_inv, aes(date, value)) +
  geom_rect(
    data = recess_shade,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
    fill = "grey80", alpha = 0.5, inherit.aes = FALSE
  ) +
  geom_line(colour = col_orange, linewidth = 1.2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = label_comma(), name = "Count") +
  labs(
    title    = "Housing Inventory: Active Listing Count",
    x        = NULL,
    caption  = fred_caption("ACTLISCOUUS")
  ) +
  gdp_theme() +
  expand_limits(x = as.Date("2026-01-01"))
auto_save()

# Case-Shiller national home-price index
cs_home <- fredr(
  series_id         = "CSUSHPISA",
  observation_start = as.Date("2017-01-01"),
  observation_end   = Sys.Date()
)

ggplot(cs_home, aes(date, value)) +
  geom_rect(
    data = recess_shade,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
    fill = "grey80", alpha = 0.5, inherit.aes = FALSE
  ) +
  geom_line(colour = col_orange, linewidth = 1.2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title    = "S&P CoreLogic Case-Shiller U.S. National Home-Price Index",
    x        = NULL,
    y        = "Index (Jan 2000 = 100)",
    caption  = fred_caption("CSUSHPISA")
  ) +
  gdp_theme()
auto_save()

# ACM decomposition series
start_date <- as.Date("2021-01-01")
end_date   <- Sys.Date()

fitted_yield <- fredr("THREEFY10",  observation_start = start_date, observation_end = end_date)
forward_rate <- fredr("THREEFF10",  observation_start = start_date, observation_end = end_date)
term_premium <- fredr("THREEFYTP10",observation_start = start_date, observation_end = end_date)

df <- tibble(
  date = fitted_yield$date,
  `10-year zero-coupon yield`           = fitted_yield$value,
  `Avg. exp. short rate next 10 years`  = forward_rate$value,
  `10-year zero-coupon term premium`    = term_premium$value
) |>
  pivot_longer(-date, names_to = "Component", values_to = "Value")

ggplot(df, aes(x = date, y = Value, color = Component)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5, color = "black") +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c(
    "10-year zero-coupon yield"           = "#1f77b4",
    "Avg. exp. short rate next 10 years"  = "#2ca02c",
    "10-year zero-coupon term premium"    = "#d62728"
  )) +
  scale_y_continuous(labels = percent_format(scale = 1), name = "Percent") +
  labs(
    title   = "Decomposition of Treasury Yields - ACM Model",
    x       = NULL,
    caption = "Source: Federal Reserve Bank of San Francisco"
  ) +
  gdp_theme()
auto_save()
