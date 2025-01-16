library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

# Keyword frequency by month and removal of stop words
keyword_monthly <- data %>%
  unnest_tokens(word, content) %>%  
  filter(!word %in% custom_stopwords) %>%  
  mutate(month = floor_date(date, "month")) %>%  
  group_by(category_level_1, month, word) %>%
  summarise(freq = n(), .groups = "drop")  


# Extract the most frequent keywords for each month
top_keywords_monthly <- keyword_monthly %>%
  group_by(category_level_1, month) %>%
  slice_max(order_by = freq, n = 1)  


# Getting a unique classification
categories <- unique(top_keywords_monthly$category_level_1)

# Divide the classification into 3 groups in order
set.seed(42)
grouped_categories <- split(categories, ceiling(seq_along(categories) / (length(categories) / 3)))

# Splitting data by grouping
grouped_data <- lapply(grouped_categories, function(group) {
  top_keywords_monthly %>% filter(category_level_1 %in% group)
})

# Create a chart and save it
for (i in seq_along(grouped_data)) {
  p <- ggplot(grouped_data[[i]], aes(x = month, y = freq, label = word)) +
    geom_point(color = "green", size = 2) +  
    geom_text(vjust = -0.5, size = 2.5, color = "orange") +  # Showing keyword tags
    facet_wrap(~ category_level_1, scales = "free_y") +  # Pagination layout (faceted by category)
    labs(
      title = paste("Keywords trends: group", i, ""),
      x = "month",
      y = "freq"
    ) +
    theme(
      panel.background = element_rect(fill = "white"),  
      panel.grid.major = element_line(color = "black", size = 0.2),  
      panel.grid.minor = element_line(color = "black", size = 0.1),  
      strip.background = element_rect(fill = "lightgray", color = "black"),  
      strip.text = element_text(size = 10, face = "bold"),  
      axis.text.x = element_text(angle = 45, hjust = 1),  
      axis.text.y = element_text(size = 10),  
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  
      plot.background = element_rect(fill = "white") , 
      panel.spacing = unit(2, "lines")
    )
  
  # Save image
  ggsave(filename = paste0("top_keywords_scatterplot_group", i, ".png"), plot = p, width = 20, height = 10)
}
