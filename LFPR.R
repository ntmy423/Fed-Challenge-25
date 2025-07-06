#Plot: LFPR (Aggregate and Prime-Age)

rm(list = ls())

# Load the package
library(ggplot2)
library(scales)
library(readxl)

# Load data
Labor_Market_Data <- read_excel("~/Library/CloudStorage/OneDrive-DrexelUniversity/Fed Challenge 2025/Skeleton/Labor Market - Data.xlsx", sheet = "LFPR", col_types = c("date", "numeric", "numeric", "skip", "skip", "skip", "skip", "skip", "skip"))

# Convert date column to Date format
Labor_Market_Data$observation_date <- as.Date(Labor_Market_Data$observation_date, format = "%m/%d/%y")                                                                                                        

# Rename for easier use in ggplot
names(Labor_Market_Data) <- c("date", "prime_lfpr", "lfpr")


ggplot(Labor_Market_Data, aes(x = date)) +
  geom_line(aes(y = prime_lfpr, color = "Prime Age LFPR"), size = 1) +
  geom_line(aes(y = lfpr, color = "Aggregate LFPR"), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    title = "Labor Force Participation Rate",
    caption = "Source: BLS",
    x = NULL,
    y = "Percent Change YoY",
    color = NULL
  ) +
  scale_color_manual(values = c("Prime Age LFPR" = "red", "Aggregate LFPR" = "navy")) +
  scale_x_date(
    date_breaks = "6 months",   
    date_labels = "%m/%Y"        
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    plot.caption = element_text(hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )



