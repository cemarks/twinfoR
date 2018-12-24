### Basic API calls

### Set up

library(twinfoR)
library(wordcloud)

dir.create("~/twinfoR_vignette1",showWarnings=FALSE)
setwd("~/twinfoR_vignette1")


### Authenticate

auth.vector <- authorize_IT()

### Search

search.term <- "#WorstDateEver"
search.result <- search_tweets(search.term)

created.at <- sapply(
  search.result$statuses,
  function(x) return(
    x$created_at
  )
)

created.at.df <- data.frame(
  created.at = sort(as.POSIXct(created.at,format = "%a %b %d %H:%M:%S +0000 %Y")),
  cumulative.tweets = 1:length(created.at)
)

plot(
  created.at.df$created.at,
  created.at.df$cumulative.tweets,
  main = sprintf(
    "Tweet Rate for %s",
    search.term
  ),
  xlab = "Date",
  ylab = "Cumulative Tweets",
  xaxt = "n"
)
axis.POSIXct(
  1,
  at=seq(
    min(
      created.at.df$created.at
    ),
    max(
      created.at.df$created.at
    ),
    by="day"
  ),
  format = "%Y-%m-%d"
)


