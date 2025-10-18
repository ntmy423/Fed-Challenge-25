# Plot: Core PCE Inflation

rm(list = ls())

# Load libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(readxl)

# Load your data (using your path and variable)
chart3_yoy_core_pce_contributions_06_2025 <- read_excel("~/Library/CloudStorage/OneDrive-DrexelUniversity/Fed Challenge 2025/chart3-yoy-core-pce-contributions 06 2025.xlsx")
df <- chart3_yoy_core_pce_contributions_06_2025

# Identify the time/date column (assume it's the first column)
time_column_name <- names(df)[1]

# ---- DATE FORMATTING ----
# If your date column is "YYYY-MM", use this:
df[[time_column_name]] <- as.Date(paste0(df[[time_column_name]], "-01"))
# If it's "YYYY-MM-DD", use this instead:
# df[[time_column_name]] <- as.Date(df[[time_column_name]])

# Calculate total contribution per date (exclude time column)
df$total <- rowSums(df[, setdiff(names(df), c(time_column_name))], na.rm = TRUE)

# Convert to long format for ggplot2
long_data <- df %>%
  pivot_longer(
    cols = -c(all_of(time_column_name), total),
    names_to = "Category",
    values_to = "Contribution"
  )

# Custom color palette
custom_colors <- c(
  "Housing" = "#E87722",
  "Core Services exc. Housing" = "#0C233C",
  "Core Goods" = "#009E73"
)

# Plot (no value labels, legend at top)
ggplot(long_data, aes(x = !!sym(time_column_name), y = Contribution, fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = custom_colors) +
  labs(
    title = "Core PCE Inflation",
    x = NULL,
    y = "Percent Change YoY",
    fill = "Category",
    caption = "Source: Federal Reserve Bank of San Francisco"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 40),
    plot.caption = element_text(size = 20),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    panel.grid.minor = element_blank(),
    legend.position = "top",      
    legend.title = element_blank(),
    legend.text = element_text(size = 22),
    plot.margin = margin(t = 20, r = 40, b = 10, l = 20)
  )
