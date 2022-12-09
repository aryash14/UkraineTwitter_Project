# # Required R package installation:
# # These will install packages if they are not already installed
# # Set the correct default repository
r = getOption("repos")
r["CRAN"] = "http://cran.rstudio.com"
options(repos = r)


if (!require("knitr")) {
  install.packages("knitr")
  library(knitr)
}

if (!require("kableExtra")){
  install.packages("kableExtra")
  library(kableExtra)
}

knitr::opts_chunk$set(echo = TRUE)

source("Elasticsearch.R")
# source("plot_tweet_sentiment_timeseries.R")

getDataLoc <- function(phrase) {
  # query start date/time (inclusive)
  rangestart <- "2022-04-01 00:00:00"
  
  # query end date/time (exclusive)
  rangeend <- "2022-12-01 00:00:00"
  
  # text filter restricts results to only those containing words, phrases, or meeting a boolean condition. This query syntax is very flexible and supports a wide variety of filter scenarios:
  # words: text_filter <- "cdc nih who"  ...contains "cdc" or "nih" or "who"
  # phrase: text_filter <- '"vitamin c"' ...contains exact phrase "vitamin c"
  # boolean condition: <- '(cdc nih who) +"vitamin c"' ...contains ("cdc" or "nih" or "who") and exact phrase "vitamin c"
  #full specification here: https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-simple-query-string-query.html
  text_filter <- ""
  
  # location filter acts like text filter except applied to the location of the tweet instead of its text body.
  location_filter <- ""
  
  # if FALSE, location filter considers both user-povided and geotagged locations. If TRUE, only geotagged locations are considered.
  must_have_geo <- TRUE
  
  # query semantic similarity phrase
  semantic_phrase <- phrase
  
  # sentiment type (only 'vader' and 'roberta' are supported for now)
  # if the requested sentiment type is not available for the current index or sample, the sentiment
  # column in the result set will contain NA values.
  sentiment_type <- "roberta"
  
  # query lower bound for sentiment (inclusive). Enter a numeric value or for no lower bound set to NA.
  sentiment_lower <- NA
  
  # query upper bound for sentiment (inclusive). Enter a numeric value or for no upper bound set to NA.
  sentiment_upper <- NA
  
  # embedding type (only 'use_large' and 'sbert' are supported for now)
  embedding_type <- "sbert"
  
  # return results in chronological order or as a random sample within the range
  # (ignored if semantic_phrase is not blank)
  random_sample <- TRUE
  # if using random sampling, optionally specify a seed for reproducibility. For no seed, set to NA.
  random_seed <- NA
  # number of results to return (to return all results, set to NA)
  resultsize <- NA
  # minimum number of results to return. This should be set according to the needs of the analysis (i.e. enough samples for statistical significance)
  min_results <- 1
  results <- do_search(indexname="ukraine-data-lite-oct22",
                       rangestart=rangestart,
                       rangeend=rangeend,
                       text_filter=text_filter,
                       location_filter=location_filter,
                       semantic_phrase=semantic_phrase,
                       must_have_geo=must_have_geo,
                       embedding_type=embedding_type,
                       must_have_embedding=TRUE,
                       sentiment_type=sentiment_type,
                       sentiment_lower=sentiment_lower,
                       sentiment_upper=sentiment_upper,
                       random_sample=random_sample,
                       random_seed=random_seed,
                       resultsize=resultsize,
                       resultfields='"created_at", "user.screen_name", "user.location", "place.full_name", "place.country", "text", "full_text", "extended_tweet.full_text"',
                       elasticsearch_host="lp01.idea.rpi.edu",
                       elasticsearch_path="elasticsearch",
                       elasticsearch_port=443,
                       elasticsearch_schema="https")
  
  required_fields <- c("created_at", "user_screen_name", "user_location", "place.full_name", "place.country", "full_text", "sentiment")

  
  #Transform results for sentiment plot
  results.df <- results$df
  results.df$vector_type <- "tweet"
  return(results.df)
}

phrases <- c("Russia invades Ukraine", "NATO", "Ukraine War", "Russian War", "Attack on Ukraine")
for (phrase in phrases) {
  #getting the results
  results <- getDataLoc(phrase)
  #filtering based on cosine similarity 
  df <- results[which(results$cosine_similarity > 0.25), ]
  count <- as.data.frame(table(df$place.country))
  count <- count[order(count$Freq , decreasing = TRUE), ]
  count <- count[1:5, ]
  #getting the percenatge
  count['percentage'] <- round(count$Freq / nrow(df), 2) * 100
  #plot the barplot
  barplot(count$Freq, names = count$Var1, main = phrase);text(bp, count$Freq - 100, labels = count$percentage)
}

results <- getDataLoc("Russia invades Ukraine")
count <- as.data.frame(table(results$place.country))
count <- count[order(count$Freq , decreasing = TRUE), ]
count <- count[1:5, ]
count['percentage'] <- round(count$Freq / nrow(results), 2) * 100
View(count)
barplot(count$Freq, names = count$Var1, main = "Overall");text(bp, count$Freq - 100, labels = count$percentage)
