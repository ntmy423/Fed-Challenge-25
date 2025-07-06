# Plot: Colorful Nonlinear Phillips Curv Curve with slope

rm(list = ls())

# Load required libraries
library(ggplot2)
library(segmented)
library(readxl)
library(dplyr)

# Load data
Labor_Market_Data <- read_excel("~/Library/CloudStorage/OneDrive-DrexelUniversity/Fed Challenge 2025/Skeleton/Labor Market - Data.xlsx", 
                                sheet = "Nonlinear Phillips Curve", 
                                col_types = c("date", "numeric", "skip", "skip", "skip", "numeric", "skip", "skip", "skip", "skip", "skip"))

# Convert date column to Date format
Labor_Market_Data$observation_date <- as.Date(Labor_Market_Data$observation_date, format = "%m/%d/%y")

# Rename columns for convenience
colnames(Labor_Market_Data) <- c("date", "cpi", "ln_vu")

# Assign time periods (example - you can adjust the dates as needed)
Labor_Market_Data <- Labor_Market_Data %>%
  mutate(period = case_when(
    date >= as.Date("2020-04-01") & date <= as.Date("2022-04-30") ~ "Apr 2020 to Apr 2022",
    date >= as.Date("2000-12-01") & date <= as.Date("2020-03-31") ~ "Dec 2000 to Mar 2020",
    date >= as.Date("2022-05-01")                                 ~ "May 2022 to Present",
    TRUE                                                          ~ "Other"
  ))

# Order factor levels for nice legend order
Labor_Market_Data$period <- factor(Labor_Market_Data$period, levels = c(
  "Apr 2020 to Apr 2022", "Dec 2000 to Mar 2020", "May 2022 to Present", "Other"
))

# Fit piecewise linear model
Labor_Market_Data$ln_vu_pos <- pmax(0, Labor_Market_Data$ln_vu)
kink_model <- lm(cpi ~ ln_vu + ln_vu_pos, data = Labor_Market_Data)
Labor_Market_Data$predicted <- predict(kink_model)

# Most recent observation
latest_point <- Labor_Market_Data[nrow(Labor_Market_Data), ]

# Create plot
ggplot(Labor_Market_Data, aes(x = ln_vu, y = cpi, color = period)) +
  geom_point(size = 2) +
  geom_line(aes(x = ln_vu, y = predicted), color = "black", linewidth = 1, inherit.aes = FALSE, show.legend = FALSE) +
  geom_point(data = latest_point, aes(x = ln_vu, y = cpi), color = "red", fill = NA, size = 4, shape = 21, stroke = 1.5, inherit.aes = FALSE) +
  scale_color_manual(
    values = c(
      "Apr 2020 to Apr 2022" = "#E74C3C",       # Red
      "Dec 2000 to Mar 2020" = "#1F77B4",       # Blue
      "May 2022 to Present"  = "#229954",       # Green
      "Other"                = "gray"
    ),
    name = NULL
  ) +
  labs(
    title = "Nonlinear Phillips Curve",
    x = "Ln(v/u) - Percent - Monthly data",
    y = "Headline CPI (Percent Change YoY) - Monthly data",
    caption = "Source: BLS"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.caption = element_text(hjust = 0),
    legend.position = "top"
  )
