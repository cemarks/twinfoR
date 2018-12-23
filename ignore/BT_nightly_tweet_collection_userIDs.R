## script to run nightly data grabs

# lp.tmp <- .libPaths()
# .libPaths(c("F:/My Documents/R/win-library/3.3",lp.tmp,"F:/My Documents/R/win-library/3.5"))
# # 
# # library(RSQLite)
# 
# #source("C:/Users/paul.hurley1/Documents/BT_twitter/collect_functions.R")
# 
# ## Hurley: load twitter authentication information 
# ##source("C:/Users/paul.hurley1/Documents/BT_twitter/my_twitter.R")
setwd("C:/Users/paul.hurley1/Documents/BT_twitter")
# 
# 
 load("C:/Users/paul.hurley1/Documents/BT_twitter/new_master.RData")    #load new user datafile
 sampleKA <- new_master 
# 
# #### GET THE TWEETS FROM THIS SAMPLE POPULATION  
 endpoint <- 'https://api.twitter.com/1.1/statuses/user_timeline.json'
# 
# #### Load the FUNCTION to Get TWEETs  ######
 get_tweets <- function(query.params,sink.file='uncollected_users_3.csv'){
   result <- tryCatch(
     twitter_anything(auth.vector,endpoint,query.params),
     error=function(e) {
       cat(sprintf("Error collecting user %s--retrying...\n",query.params[2]));
       Sys.sleep(1)
       return(
         tryCatch(
           twitter_anything(auth.vector,endpoint,query.params),
           error = function(e) {
             cat(sprintf("Retry unsuccessful.  User %s not collected\n",query.params[2]))
             return(list(status_code=-1))
           }
         )
       )
     }
   )
   if(result$status_code > 0 && result$status_code != 200){
     cat(sprintf("User %s query returned status %i. Retrying...\n",query.params[2],result$status_code))
     result <- tryCatch(
       twitter_anything(auth.vector,endpoint,query.params),
       error=function(e) {
         cat(sprintf("Error collecting user %s--retrying...\n",query.params[2]));
         Sys.sleep(1)
         return(
           tryCatch(
             twitter_anything(auth.vector,endpoint,query.params),
             error = function(e) {
               cat(sprintf("Retry unsuccessful.  User %s not collected\n",query.params[2]))
               return(list(status_code=-1))
             }
           )
         )
       }
     )
   }
   if(result$status_code == 200){
     return(httr::content(result))
   } else {
     cat(sprintf("User %s failed with status %i.\n",query.params[2],result$status_code))
     sink(sink.file,append=TRUE,split=FALSE)
     cat(sprintf("%s,%i\n",query.params[2],result$status_code))
     sink()
     return(list())
   }
 }
 
 
 
 
 con <- dbConnect(RSQLite::SQLite(),"sampleKA.sqlite")
# 
 counter <- 0
 start.time <- Sys.time()
 error.file <- "C:/Users/paul.hurley1/Documents/BT_twitter/uncollected_users_3.csv"
 sink(error.file)
 cat(sprintf("user.id,error.code\n"))
 sink()
# 
# # query the database to identify the latest tweet id for each user
 since.id.query<-"SELECT user_id,screen_name,MAX(id) as since_id FROM status GROUP BY user_id ORDER BY since_id desc;"
 since.id.df <- dbGetQuery(con,since.id.query)
 # establish a 'no earlier date' to help limit the search beginning date.  Note that you will capture early tweets during the batch grabs.
 original.since.id <- "1029025487924932608" #13 AUG
# 
# 
 ## for every user_id in the dataset go get the tweets and insert into the DB. Limit on this query is 3200 tweets.
 for(i in as.character(sampleKA$user_id)){
   counter<-counter+1
   if(i %in% as.character(since.id.df$user_id)){
     since.id <- as.character(since.id.df$since_id[which(as.character(since.id.df$user_id)==i)])
   } else {
     since.id <- original.since.id
   }
   query.count <- 0
   query.params <- c("user_id",i,"count","200","tweet_mode","extended")
   Sys.sleep(max(1-as.numeric(difftime(Sys.time(),start.time,units="secs")),0))
   start.time <- Sys.time()
   tweets <- get_tweets(query.params,error.file)                                      # Get Tweets
   n <- length(tweets)
   if(n > 0){
     oldest.tweet.id <- as.character(tweets[[n]]$id-1)
     texts <- sapply(tweets, function(x) return(x$full_text))
     insert_statuses(tweets,con)                                                    # Insert Tweets to DB
   } else {
     oldest.tweet.id <- 0
   }
   while(query.count < 16 && as.numeric(oldest.tweet.id) > as.numeric(since.id) && n > 100){
     query.params <- c("user_id",i,"count","200","max_id",oldest.tweet.id,"tweet_mode","extended")
     Sys.sleep(max(1-as.numeric(difftime(Sys.time(),start.time,units="secs")),0))
     start.time <- Sys.time()
     tweets <- get_tweets(query.params,error.file)
     n <- length(tweets)
     if(n > 0){
       oldest.tweet.id <- as.character(tweets[[n]]$id-1)
       texts <- sapply(tweets, function(x) return(x$full_text))
       insert_statuses(tweets,con)
     } else {
       oldest.tweet.id <- 0
     }
     query.count <- query.count + 1
   }
   if(counter %% 10 == 0){
     cat(paste(as.character(counter),nrow(sampleKA),sep="/"))
     cat("\n")
     Sys.sleep(1)
   }
 }
 
# #dbDisconnect(con)
Sys.sleep(901)                                                                # Put the system to sleep prior to executing keyword queries
# 


