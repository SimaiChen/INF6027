library(dplyr)
library(lubridate)

# load data
file_path <- "MN-DS-news-classification.csv"
data <- read.csv(file_path)

# Ensure the content field is a character vector
data$content <- as.character(data$content)

# Converting date fields to date format
data$date <- as.Date(data$date, format = "%Y-%m-%d")