library(rtweet) # Usar versión 0.7.0
library(tidyverse)

tweets_df <- search_tweets(q = "rstudio",
                           n = 100,
                           type = "recent")