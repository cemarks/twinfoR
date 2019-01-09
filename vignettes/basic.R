### Basic API calls

### Set up

library(twinfoR)
library(wordcloud)
library(tm)

dir.create("~/twinfoR_vignette1",showWarnings=FALSE)
setwd("~/twinfoR_vignette1")


## Authenticate

auth.vector <- authorize_IT()

## Search

search.term <- "#Fargo"
search.result <- search_tweets(search.term)

statuses <- search.result$statuses

created.at <- sapply(
  statuses,
  function(x) return(
    x$created_at
  )
)

### Tweet rate time plot
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

### Word Cloud

tweet.text <- paste(
  sapply(
    statuses,
    function(x) return(x$full_text)
  ),
  collapse = " "
)
tweet.text <- tolower(tweet.text)
tweet.text <- gsub('fargo','',tweet.text,i)

wc <- wordcloud(tweet.text,max.words=100)

### Users

users <- lapply(
  statuses,
  function(x) return(x$user)
)

user_ids <- sapply(
  users,
  function(x) return(x$id_str)
)

unique.user_id.indices <- which(!duplicated(user_ids))
unique.users <- users[unique.user_id.indices]

### Followers counts

followers.counts <- sapply(
  users,
  function(x) return(x$followers_count)
)

hist(
  log10(followers.counts+1),
  breaks = 0:ceiling(max(log10(followers.counts+1))),
  main = "Distribution of number of followers (log)",
  xlab = "Log number of followers (base 10)"
)


### User status counts

status.counts <- sapply(
  users,
  function(x) return(x$statuses_count)
)

hist(
  log10(status.counts+1),
  breaks = 0:ceiling(max(log10(status.counts+1))),
  main = "Distribution of number of statuses (log)",
  xlab = "Log number of statuses (base 10)"
)


## Most retweeted

not.retweets <- which(
  sapply(
    statuses,
    function(x) if("retweeted_status" %in% names(x)) return(FALSE) else return(TRUE)
  )
)


retweet.dataframe <- data.frame(
  user = sapply(
    statuses[not.retweets],
    function(x) return(x$user$screen_name)
  ),
  text = sapply(
    statuses[not.retweets],
    function(x) return(x$full_text)
  ),
  retweet.count = sapply(
    statuses[not.retweets],
    function(x) return(x$retweet_count)
  ),
  stringsAsFactors = FALSE
)


retweet.dataframe <- retweet.dataframe[order(retweet.dataframe$retweet.count,decreasing = TRUE),]
for(i in 1:5){
  if(i==1){
cat("Screen Name         Retweet Count     Text\n")
  }
  cat(
    "@",
    retweet.dataframe$user[i],
    strrep(" ",max(c(2,20-nchar(retweet.dataframe$user[i])))),
    retweet.dataframe$retweet.count[i],
    strrep(" ",max(c(2, 17-nchar(as.character(retweet.dataframe$retweet.count[i]))))),
    retweet.dataframe$text[i],
    "\n\n",
    sep=""
  )
}

## Most prolific tweeters

tweeters <- sapply(
  statuses,
  function(x) return(x$user$screen_name)
)

tweeter.table <- table(tweeters)
names(tweeter.table) <- paste("@",names(tweeter.table),sep="")
sort(tweeter.table,decreasing=TRUE)[1:3]




## User Timelines

### Get some new users.

users <- user_search("Fargo ND")

### Just use up to the first five screen names.
user.screen_names <- sapply(
  users[1:min(c(5,length(users)))],
  function(x) return(x$screen_name)
)

### Collect their timelines

user.timelines <- list()
for(i in 1:length(user.screen_names)){
  user.timelines[[user.screen_names[i]]] <- user_timeline_recursive(screen_name = user.screen_names[i])
}

### A look at the first user

screen.name <- user.screen_names[1]
timeline <- user.timelines[[screen.name]]

### Word Cloud
status.text <- paste(
  sapply(
    timeline,
    function(x) return(x$full_text)
  ),
  collapse=" "
)

wc <- wordcloud(status.text,max.words = 100)

### Most-used hashtags

hashtags <- unlist(
  lapply(
    timeline,
    function(x) {
      lapply(
        x$entities$hashtags,
        function(y) return(y$text)
      )
    }
  )
)

hashtag.table <- sort(table(tolower(hashtags)),decreasing = TRUE)
for(i in 1:10){
  if(i==1){
    cat("Hashtag             Count\n")
  }
  cat(
    names(hashtag.table)[i],
    strrep(" ",max(c(2,20-nchar(names(hashtag.table)[i])))),
    hashtag.table[i],
    "\n",
    sep=""
  )
}


### User mentions

usermentions <- unlist(
  lapply(
    timeline,
    function(x) {
      lapply(
        x$entities$user_mentions,
        function(y) return(y$screen_name)
      )
    }
  )
)

usermention.table <- sort(table(tolower(usermentions)),decreasing = TRUE)
for(i in 1:10){
  if(i==1){
    cat("User             Count\n")
  }
  cat(
    "@",
    names(usermention.table)[i],
    strrep(" ",max(c(2,20-nchar(names(usermention.table)[i])))),
    usermention.table[i],
    "\n",
    sep=""
  )
}


### All users together

status.text <- paste(
  sapply(user.timelines,
    function(timeline){
      paste(
        sapply(
          timeline,
          function(x) return(x$full_text)
        ),
        collapse=" "
      )
    }
  ),
  collapse = " "
)

wc <- wordcloud(status.text,max.words = 100)

### Most-used hashtags
hashtags <- unlist(
  lapply(
    user.timelines,
    function(timeline){
      unlist(
        lapply(
          timeline,
          function(x) {
            lapply(
              x$entities$hashtags,
              function(y) return(y$text)
            )
          }
        )
      )
    }
  )
)

hashtag.table <- sort(table(tolower(hashtags)),decreasing = TRUE)
for(i in 1:10){
  if(i==1){
    cat("Hashtag             Count\n")
  }
  cat(
    names(hashtag.table)[i],
    strrep(" ",max(c(2,20-nchar(names(hashtag.table)[i])))),
    hashtag.table[i],
    "\n",
    sep=""
  )
}

### User mentions

usermentions <- unlist(
  lapply(
    user.timelines,
    function(timeline){
      unlist(
        lapply(
          timeline,
          function(x) {
            lapply(
              x$entities$user_mentions,
              function(y) return(y$screen_name)
            )
          }
        )
      )
    }
  )
)

usermention.table <- sort(table(tolower(usermentions)),decreasing = TRUE)
for(i in 1:10){
  if(i==1){
    cat("User Count\n")
  }
  cat(
    "@",
    names(usermention.table)[i],
    strrep(" ",max(c(2,20-nchar(names(usermention.table)[i])))),
    usermention.table[i],
    "\n",
    sep=""
  )
}

