rm(list = ls())

# Load required libraries
library(tidyverse)
library(readxl)

# Read the data (edit the path if needed)
Labor_Market_Data <- read_excel("~/Library/CloudStorage/OneDrive-DrexelUniversity/Fed Challenge 2025/Skeleton/Labor Market - Data.xlsx", 
                                sheet = "Immigration - USCBP", col_names = FALSE)

# Assign first row as column names
colnames(Labor_Market_Data) <- as.character(unlist(Labor_Market_Data[1, ]))

# If the first column is empty, rename it to 'FY'
colnames(Labor_Market_Data)[1] <- "FY"

# Remove first row (column names) and Total column
Labor_Market_Data <- Labor_Market_Data[-1, ]
Labor_Market_Data <- Labor_Market_Data[ , !(colnames(Labor_Market_Data) %in% "Total")]

# Reshape to long format
Labor_Market_Data_long <- Labor_Market_Data %>%
  pivot_longer(
    cols = -FY,
    names_to = "Month",
    values_to = "Count"
  )

# Convert Count to numeric, drop NAs
Labor_Market_Data_long$Count <- as.numeric(gsub(",", "", Labor_Market_Data_long$Count))
Labor_Market_Data_long <- Labor_Market_Data_long[!is.na(Labor_Market_Data_long$Count), ]

# Ensure months are ordered correctly
Labor_Market_Data_long$Month <- factor(Labor_Market_Data_long$Month, 
                                       levels = c("OCT","NOV","DEC","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP"))

# Set up colors to match your example (adjust if you want)
fy_colors <- c(
  "2022" = "#FBC984",         # light orange
  "2023" = "#43474C",         # dark gray
  "2024" = "#FF8600",         # orange
  "2025 (FYTD)" = "#9B480A"   # brown
)

# Plot
ggplot(Labor_Market_Data_long, aes(x = Month, y = Count, color = FY, group = FY)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = fy_colors) +
  labs(
    title = "FY Nationwide Encounters by Month",
    caption = "Source: USCBP",
    x = NULL,
    y = "Encounter Count",
    color = "FY"
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale_cut = cut_short_scale()),
    limits = c(0, NA) # start at 0, upper limit auto
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.caption = element_text(hjust = 0, size = 10, face = "italic"),
    axis.text.x = element_text(face = "bold"),
    legend.position = "top"
  )
