# EJ in NJ - rtweet R File (Version 1)

#################### Packages ####################

library(rtweet)
library(academictwitteR)
library(dplyr)
library(tidyr)
library(jsonlite)
library(tidytext)

#################### Create API Tokens #####################

create_token(
  app = "AppName",
  consumer_key = "aaaaaaaaaaaaa",
  consumer_secret = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
  access_token = "cccccccccccccccccccccccccccccccccccccccccccccccc",
  access_secret = "dddddddddddddddddddddddddddddddddddddddddddddd",
  set_renv = TRUE
)

set_bearer() 

#################### Pull Tweets from API and Save to JSONs #####################

## Confirm rate limit 
rate_limit <- rate_limit() 
rate_limit$changes <- rate_limit$limit - rate_limit$remaining

## Build request query (set to East, then pull for West)
my_query <- build_query(
  is_retweet = FALSE,
  is_reply = FALSE,
  is_quote = FALSE, 
  remove_promoted = TRUE,
  lang =  "en",
  has_geo = TRUE,
  is_verified = FALSE,
  point_radius = c(-74.1724048, 40.8154547, 25) # EAST
  #point_radius = c(-74.7984156, 40.9830035, 25) # WEST
)

## Pull Tweets from API and save to JSON files in directory 
get_all_tweets(
  query = my_query,
  start_tweets = "2019-03-30T00:00:00Z", 
  end_tweets = "2019-04-02T00:00:00Z", 
  bearer_token = get_bearer(),
  n = 1000000000, # Set high number to pull max number of Tweets for timeframe
  data_path = "NJ_Data_East_March30toApr1", # CHANGE EAST OR WEST
  bind_tweets = FALSE,
  verbose = TRUE
)

#################### Import JSONs, Process, and Clean Data #####################

## Define the path where JSON files are stored
path <- "C:/Users/cjkno/Documents/My Documents/Classes - '23 Spring/R/R Directory - Research - ACS/NJ_Data_EastWest_March30toApr1"

## Read and combine JSON files into a single dataframe
json_files <- list.files(path, pattern = "*.json")
dataframes <- lapply(json_files, function(file) {
  filepath <- file.path(path, file)
  json_content <- readLines(filepath, warn = FALSE)
  df <- fromJSON(txt = json_content, flatten = TRUE)
  select(df, text, author_id, geo.coordinates.coordinates)
})

combined_df <- do.call(rbind, dataframes)

## Export combined data to CSV
write.csv(combined_df, file = "C/.../Tweets.csv", row.names = FALSE)

#################### Data Cleaning #####################

## Remove tweets with NAs coordinates 
tweets <- na.omit(combined_df)

## Extract and clean coordinates
tweets_clean <- tweets %>%
  filter(!is.na(geo.coordinates.coordinates)) %>%
  rowwise() %>%
  mutate(
    geo_x = geo.coordinates.coordinates[1],
    geo_y = geo.coordinates.coordinates[2]
  ) %>%
  ungroup() %>%
  select(text, author_id, geo_x, geo_y)

## Remove bot accounts found through visual investigation
bot_ids <- c(123456789) # Example bot IDs
tweets_clean <- tweets_clean[!tweets_clean$author_id %in% bot_ids, ]

## Remove duplicates
tweets_clean <- tweets_clean %>% distinct()

#################### Text Processing and Analysis #####################

## Convert text to lower case
tweets_clean$text <- tolower(tweets_clean$text)

## Remove mentions, URLs, emojis, and punctuation
tweets_clean$text <- gsub("@\\w+", "", tweets_clean$text) # Remove mentions
tweets_clean$text <- gsub("https?://\\S+", "", tweets_clean$text) # Remove URLs
tweets_clean$text <- gsub("[^\x01-\x7F]", "", tweets_clean$text) # Remove emojis
tweets_clean$text <- gsub("[[:punct:]]", " ", tweets_clean$text) # Remove punctuation

## Tokenize text into unigrams
nGram_tokened <- tweets_clean %>%
  unnest_tokens(words, text, token = "ngrams", n = 1)

## Remove stopwords
stop_words <- get_stopwords(language = "en")
nGram_cleaned <- nGram_tokened %>%
  filter(!words %in% stop_words$word)

## Count the number of times each word appears at a given set of coordinates
nGram_count <- nGram_cleaned %>%
  group_by(geo_x, geo_y, words) %>%
  summarize(n = n(), .groups = 'drop')

## Calculate mean count of words for each location
nGram_mean <- nGram_count %>%
  group_by(geo_x, geo_y) %>%
  summarize(mean_words = mean(n), .groups = 'drop')

## Filter to words with above average count for the coordinates
nGram_filtered <- nGram_count %>%
  inner_join(nGram_mean, by = c("geo_x", "geo_y")) %>%
  filter(n > 1) %>%
  arrange(desc(n))

## Remove words with 2 letters or less
nGram_filtered <- nGram_filtered %>%
  filter(nchar(words) >= 3)

## Export final cleaned data
write.csv(nGram_filtered, "C:/.../Final.csv", row.names = FALSE)
