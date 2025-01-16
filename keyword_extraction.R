library(quanteda)
library(dplyr)
library(tidyr)

# Custom Stop Words
custom_stopwords <- c("said", "people", "one", "also", "can", "new", "us", 
                      "time", "years", "like", "now", "just", "two", "even", "last", 
                      "year", "many", "first", "says", "may", "it’s", "according", 
                      "get", "told", "including", "make", "since", "day", "way", 
                      "take", "much", "made", "$","school","health","workers","per","religious","education", stopwords("en"))

# Extract top 10 keywords by category
result <- data %>%
  group_by(category_level_1) %>%
  summarise(
    keywords = list({
      content_group <- content
      corpus <- corpus(content_group)
      dfm_obj <- dfm(tokens(corpus, remove_punct = TRUE), tolower = TRUE)
      dfm_obj <- dfm_remove(dfm_obj, custom_stopwords)
      topfeatures(dfm_obj, n = 10)
    })
  )

# Convert to long format
result_long <- result %>%
  rowwise() %>%
  mutate(keywords_df = list(data.frame(
    keyword = names(keywords),
    freq = unname(keywords)
  ))) %>%
  unnest(keywords_df) %>%
  select(category_level_1, keyword, freq)

# Save as CSV
write.csv(result_long, file = "result_long.csv", row.names = FALSE)
cat("saved as result_long.csv\n")
