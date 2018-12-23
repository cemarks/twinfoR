#### THIS SCRIPT LOADS 15 FUNCTIONS THAT ESTABLISH THE QUERIES HAT ARE USED IN THE MakePlots_functions.R script


join.table <- list(
  key_media=c(status='user_id',key_media='user_id'),
  user=c(status='user_id',user='id'),
  hashtag=c(status='id',hashtag='status_id'),
  media=c(status='id',media='status_id'),
  user_mention=c(status='id',user_mention='status_id'),
  url = c(status='id',url='status_id')
)

make_join_statement <- function(sql.clause){
  output.str <- "status"
  subs <- c("+","-","=","(",")",",")
  for(s in subs){
    sql.clause <- gsub(s," ",sql.clause,fixed=TRUE)
  }
  sql.sep <- strsplit(sql.clause," ")[[1]]
  sql.tables <- grep(".",sql.sep,fixed=TRUE,value=TRUE)
  if(length(sql.tables)>0){
    sql.tables <- strsplit(sql.tables,".",fixed=TRUE)
    sql.tables <- sapply(sql.tables,function(x){
      return(x[1])
    } )
    sql.tables <- setdiff(sql.tables,"status")
    sql.tables <- unique(sql.tables)
    for(sql.table in sql.tables){
      output.str <- paste(
        output.str,
        "JOIN",
        sql.table,
        "ON",
        paste(
          "status",
          join.table[[sql.table]]['status'],
          sep="."
        ),
        "=",
        paste(
          sql.table,
          join.table[[sql.table]][sql.table],
          sep="."
        ),
        sep=" "
      )
    }
  }
  return(output.str)
}

make_where_clause <- function(where.criteria=NULL,start.date=NULL,end.date=NULL){
  if(is.null(where.criteria) && is.null(start.date) && is.null(end.date)){
    where.clause <- NULL
  } else {
    if(is.null(start.date)){
      start.date.clause <- NULL
    } else {
      start.date.clause <- paste(
        "status.created_at >= '",
        start.date,
        "'",
        sep=""
      )
    }
    if(is.null(end.date)){
      end.date.clause <- NULL
    } else {
      end.date.clause <- paste(
        "status.created_at < '",
        end.date,
        "'",
        sep=""
      )
    }
    if(is.null(where.criteria)){
      where.criteria.clause <- NULL
    } else {
      where.criteria.clause <- paste(
        "(",
        where.criteria,
        ")",
        sep=""
      )
    }
    where.clause <- paste(
      "WHERE",
      paste(c(
        start.date.clause,
        end.date.clause,
        where.criteria.clause
      ),
      collapse = " AND "
      ),
      sep= " "
    )
  }
  return(where.clause)
}

count_query <- function(count.column,
                        where.criteria=NULL,
                        start.date=NULL,
                        end.date=NULL,
                        additional.columns=NULL,
                        limit=10
){
  if(is.null(additional.columns)){
    additional.select <- NULL
  } else {
    additional.select <-paste(
      additional.columns,
      collapse=", "
    )
  }
  if(count.column=="1"){
    select.clause <- paste(
      "SELECT",
      paste(
        c(
          "COUNT(1)",
          additional.select
        ),
        collapse=", "
      ),
      sep=" "
    )
  } else {
    select.clause <- paste(
      "SELECT",
      paste(
        c(
          count.column,
          paste(
            "COUNT(LOWER(",
            count.column,
            ")) AS n",
            sep=""
          ),
          additional.select
        ),
        collapse=", "
      ),
      sep=" "
    )
  }
  where.clause <- make_where_clause(where.criteria,start.date,end.date)
  join.clause <- make_join_statement(paste(select.clause,where.clause,sep=" "))
  if(count.column=="1"){
    sql.query <- paste(
      select.clause,
      "FROM",
      join.clause,
      paste(
        where.clause,
        ";",
        sep=""
      ),
      sep= " "
    )
  } else {
    sql.query <- paste(
      select.clause,
      "FROM",
      join.clause,
      where.clause,
      "GROUP BY",
      paste(
        "LOWER(",
        count.column,
        ")",
        sep=""
      ),
      "ORDER BY n DESC",
      "LIMIT",
      paste(
        as.character(limit),
        ";",
        sep=""
      ),
      sep= " "
    )
  }
  return(sql.query)
}



#### Top 10 value query

top_value_query <- function(
  value.column,
  where.criteria=NULL,
  start.date=NULL,
  end.date=NULL,
  additional.columns=NULL,
  limit=10
){
  if(is.null(additional.columns)){
    additional.select <- NULL
  } else {
    additional.select <-paste(
      additional.columns,
      collapse=", "
    )
  }
  select.clause <- paste(
    "SELECT",
    paste(
      c(
        value.column,
        additional.select
      ),
      collapse=", "
    ),
    sep=" "
  )
  where.clause <- make_where_clause(where.criteria,start.date,end.date)
  join.clause <- make_join_statement(paste(select.clause,where.clause,sep=" "))
  sql.query <- paste(
    select.clause,
    "FROM",
    join.clause,
    where.clause,
    paste(
      "ORDER BY",
      value.column,
      "DESC",
      sep=" "
    ),
    "LIMIT",
    paste(
      as.character(limit),
      ";",
      sep=""
    ),
    sep= " "
  )
  return(sql.query)
}

##### DAILY SENTIMENT
daily_sentiment_query <- function(
  where.criteria=NULL,
  start.date=NULL,
  end.date=NULL,
  additional.columns=NULL,
  limit=NULL
){
  if(is.null(additional.columns)){
    additional.select <- NULL
  } else {
    additional.select <-paste(
      additional.columns,
      collapse=", "
    )
  }
  select.clause <- paste(
    "SELECT",
    paste(
      c(
        "text",
        "nrc_sentiment_positive",
        "nrc_sentiment_negative",
        "created_at",
        additional.select
      ),
      collapse=", "
    ),
    sep=" "
  )
  if (is.null(limit)){
    limit.clause <- ";"
  } else {
    limit.clause <-paste(
      "LIMIT",
      paste(
        as.character(limit),
        ";",
        sep=""
      ),
      sep=" "
    )
  }
  where.clause <- make_where_clause(where.criteria,start.date,end.date)
  join.clause <- make_join_statement(paste(select.clause,where.clause,sep=" "))
  sql.query <- paste(
    select.clause,
    "FROM",
    join.clause,
    where.clause,
    limit.clause,
    sep= " "
  )
  return(sql.query)
}


##### TOP 10S  ############

top_hashtags <- function(con,where.criteria=NULL,start.date=NULL,end.date=NULL,limit=10){
  query <- count_query("hashtag.hashtag_text",where.criteria,start.date,end.date,limit=limit)
  results <- dbGetQuery(con,query)
  return(results)
}
top_usermentions <- function(con,where.criteria=NULL,start.date=NULL,end.date=NULL,limit=10){
  query <- count_query("user_mention.user_mention_id",where.criteria,start.date,end.date,c("user_mention.user_mention_screen_name","user_mention.user_mention_name"),limit=limit)
  results <- dbGetQuery(con,query)
  return(results)
}
top_urls <- function(con,where.criteria=NULL,start.date=NULL,end.date=NULL,limit=10){
  query <- count_query("url.display_url",where.criteria,start.date,end.date,"url.extended_url",limit=limit)
  results <- dbGetQuery(con,query)
  return(results)
}
top_media <- function(con,where.criteria=NULL,start.date=NULL,end.date=NULL,limit=10){
  query <- count_query("media.media_id",where.criteria,start.date,end.date,"media.media_url",limit=limit)
  results <- dbGetQuery(con,query)
  return(results)
}
top_tweeters <- function(con,where.criteria=NULL,start.date=NULL,end.date=NULL,limit=10){
  query <- count_query("status.user_id",where.criteria,start.date,end.date,c("status.screen_name","user.name","user.location","user.description"),limit=limit)
  results <- dbGetQuery(con,query)
  return(results)
}
most_liked <- function(con,where.criteria=NULL,start.date=NULL,end.date=NULL,limit=10){
  query <- top_value_query("status.favorite_count",where.criteria,start.date,end.date,c("status.screen_name","status.text","status.created_at","user.name","user.location","user.description"),limit=limit)
  results <- dbGetQuery(con,query)
  return(results)
}
most_retweeted <- function(con,where.criteria=NULL,start.date=NULL,end.date=NULL,limit=10){
  query <- top_value_query("status.retweet_count",where.criteria,start.date,end.date,c("status.screen_name","status.text","status.created_at","user.name","user.location","user.description"),limit=limit)
  results <- dbGetQuery(con,query)
  return(results)
}

#### MORE FRAGILE FUNCTIONS; JOINS NOT COMPLETELY DYNAMIC

most_popular_RT_in_sample <- function(con,where.criteria=NULL,start.date=NULL,end.date=NULL,limit=10){ ## Assume we will not join to hashtags, media, usermentions, urls
  select.clause <- "SELECT status2.retweet_status_id,COUNT(status2.retweet_status_id) as n,status1.text,status1.screen_name,status1.created_at,status1.user_id,user.name,user.location,user.description FROM status as status1 JOIN status as status2 ON status1.id=status2.retweet_status_id JOIN user ON status1.user_id=user.id JOIN key_media ON status1.user_id=key_media.user_id"
  where.clause <- make_where_clause(where.criteria,NULL,NULL)
  if(is.null(start.date)){
    start.date <- "1970-01-01"
  }
  if(is.null(end.date)){
    end.date <- "2100-01-01"
  }
  query <- paste(
    select.clause,
    where.clause,
    "AND status2.created_at >=",
    paste("'",start.date,"'",sep=""),
    "AND status2.created_at <",
    paste("'",end.date,"'",sep=""),
    "GROUP BY status2.retweet_status_id",
    "ORDER BY n DESC",
    "LIMIT",
    as.character(limit),
    ";",
    sep=" "
  )
  results <- dbGetQuery(con,query)
  return(results)
}
most_reach <- function(con,where.criteria=NULL,start.date=NULL,end.date=NULL,limit=10){
  sub.query1.select <- "SELECT status.retweet_status_id as status_id,user.followers_count as followers_count"
  sub.query2.select <- "SELECT status.id as status_id,user.followers_count as followers_count"
  where.clause <- make_where_clause(where.criteria,start.date,end.date)
  join.clause <- make_join_statement(paste(sub.query1.select,where.clause,sep=" "))
  if(is.null(start.date)){
    start.date <- "1970-01-01"
  }
  if(is.null(end.date)){
    end.date <- "2100-01-01"
  }
  sub.query1 <- paste(
    sub.query1.select,
    "FROM",
    join.clause,
    where.clause,
    sep=" "
  )
  sub.query2 <- paste(
    sub.query2.select,
    "FROM",
    join.clause,
    where.clause,
    sep=" "
  )
  sub.query <- paste(
    sub.query1,
    " UNION ",
    sub.query2,
    sep=""
  )
  query <- paste(
    "SELECT status_id,SUM(followers_count) AS n FROM (",
    sub.query,
    ") WHERE status_id IS NOT NULL GROUP BY status_id ORDER BY n DESC LIMIT ",
    limit,
    ";",
    sep=""
  )
  results <- dbGetQuery(con,query)
  endpoint <- 'https://api.twitter.com/1.1/statuses/lookup.json'
  r <- twitter_anything(auth.vector,endpoint,c("id",paste(results$status_id,collapse=",")))
  cont <- httr::content(r)
  results$text <- NA
  results$screen_name <- NA
  results$name <- NA
  results$location <- NA
  results$description <- NA
  results$created_at <- NA
  for(i in cont){
    w <- which(as.character(results$status_id)==i[['id_str']])
    results$text[w] <- i[['text']]
    results$screen_name[w] <- i[['user']][['screen_name']]
    results$name[w] <- i[['user']][['name']]
    results$description[w] <- i[['user']][['description']]
    results$created_at[w] <- i[['created_at']]
    results$location[w] <- i[['user']][['location']]
  }
  return(results)
}

daily_sentiment <- function(con, where.criteria=NULL, start.date=NULL, end.date=NULL){
  query <- daily_sentiment_query(where.criteria, start.date, end.date, c("status.retweet", "status.screen_name"))
  results <- dbGetQuery(con, query)
  return(results)
}

