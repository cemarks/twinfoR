### Analysis functions

#' Open Twitter User Page
#'
#' View a Twitter user timeline in a browser
#'
#' This function opens a browser window on the specified user's Twitter page.  If only a \code{user_id}
#' is provided (i.e., no \code{screen_name}), authentication tokens must also be provided in one
#'  of two ways: either explicitly, by 
#' including them in the \code{authentication.vector}, or by storing them in a global variable
#' named \code{auth.vector}.  See \code{\link{authorize_app}} and \code{\link{authorize_IT}}.
#'
#' @param user_id character Twitter user ID. Ignored if screen_name is provided.
#' @param screen_name character Twitter user screen name.  
#' @param authentication.vector character vector containing authentication tokens and secrets.
#' See \code{\link{authorize_app}} and \code{\link{authorize_IT}}.
#' 
#' @return NULL (Invisible).
#'
#' @seealso \code{\link{user_show}}, \code{\link{authorize_app}}
#' @export
#' @examples
#'
#'
#' open_user(screen_name="realDonaldTrump")
#'
open_user <- function(
  user_id,
  screen_name,
  authentication.vector
){
  if(missing(screen_name)){
    if(missing(user_id)){
      stop("A user_id or screen_name must be provided to open_user.")
    } else {
      if(missing(authentication.vector)){
        if(!exists('auth.vector')){
          # cat(ls())
          stop("No authentication tokens found!")
        } else {
          authentication.vector <- auth.vector
        }
      }
      u <- user_show(
        user_id=user_id,
        authentication.vector = authentication.vector
      )
      screen_name <- u$screen_name
    }
  browseURL(
    paste(
      "http://twitter.com/",
      screen_name,
      sep=""
    )
  )
}


#' Access Tweet Media
#'
#' Function to access and store images attached to Tweets
#'
#' This function provides capability to download Tweet media or, if a 64-bit encoding
#' is available in the data connection, to convert it back to an image.  The image
#' can be saved to a file, encoded and written into the database, and/or rendered on
#' the screen.
#'
#' @param media.table.row data.frame row of the \code{media} table in the Twitter data connection.
#' The row must contain the \code{media_url} and \code{img_b64} columns.
#' @param conn DBI twitter database object.
#' @param screen_name save.to.file logical indicating whether to save the image to a file.
#' @param file.name character name of file to save.  Ignored if \code{save.to.file} is \code{FALSE}.
#' @param return.image logical indicating whether to return the \code{image} object to the user.
#' @param update.media.b64 logical.  If \code{TRUE}, the 64-bit encoded image and average hash value
#' will be updated in the \code{media} table in the Twitter database connection.
#' @param hash.size numeric hash table dimension for average hash algorithm.  See \code{media} table
#' description in \code{\link{twitter_database}}.
#' @param display.image logical.  If \code{TRUE} the image will be rendered on the screen.
#' @param showWarnings logical.  If \code{FALSE}, warnings will be suppressed.
#' 
#' @return NULL (Invisible).
#'
#' @seealso \code{\link{twitter_database}}, \code{\link{reconstitute_image}}
#' @export
#' @examples
#'
#' ## Not run: authenticate
#' # auth.vector <- authorize_IT()
#'
#' # con <- twitter_database("throwback.sqlite")
#' # tbt <- search_tweets_recursive(
#'   "throwbackthursday",
#'   data.connection = con
#' )
#' 
#' # query <- "SELECT *,COUNT(media_id) AS n FROM media GROUP_BY media_id ORDER BY n DESC LIMIT 1;"
#' # media.df <- DBI::dbGetQuery(con,query)
#' # status_media(
#'   media.df[1,],
#'   con,
#'   save.to.file = FALSE,
#'   file.name = NULL,
#'   return.image = NULL,
#'   update.media.b64 = TRUE,
#'   hash.size = 16
#' )
#'
status_media <- function(
  media.table.row,
  conn,
  save.to.file = FALSE,
  file.name = NULL,
  return.image = FALSE,
  update.media.b64 = FALSE,
  hash.size=16,
  display.image = FALSE,
  showWarnings = TRUE
){
  media.url <- media.table.row$media_url
  img.b64 <- media.table.row$img_b64
  if(is.na(img.b64)){
    p <- NA
    try(
      p <- retrieve_web_image(media.url)
    )
  } else {
    p <- reconstitute_image(img.b64,media.url,display.image = FALSE,showWarnings=showWarnings)
    if(!is.na(p) && save.to.file){
      if(is.null(file.name)){
        media.url.splt <- strsplit(media.url,"/",fixed=TRUE)[[1]]
        file.name <- media.url.splt[length(media.url.splt)]
      }
      OpenImageR::writeImage(p,file.name)
    }
    if(return.image){
      return(p)
    }
  }
  if(is.null(p) || is.na(p)){
    return(NA)
  } else {
    if(update.media.b64){
      a <- OpenImageR::average_hash(OpenImageR::rgb_2gray(p$image),hash_size=hash.size)
      query <- sprintf(
        "UPDATE media SET img_hash='%s',img_b64='%s' WHERE media_id='%s';",
        a,
        p$b64,
        media.table.row$media_id
      )
      if(missing(conn)){
        stop("Data connection not provided")
      }
      DBI::dbExecute(conn,query)
    }
    if(save.to.file){
      if(is.null(file.name)){
        media.url.splt <- strsplit(media.url,"/",fixed=TRUE)[[1]]
        file.name <- media.url.splt[length(media.url.splt)]
      }
      OpenImageR::writeImage(p$image,file.name)
    }
    if(return.image){
      return(p$image)
    }
  }
}


user_profile_image <- function(
  user.table.row,
  conn,
  save.to.file = FALSE,
  file.name = NULL,
  return.image = FALSE,
  update.image.b64 = FALSE,
  hash.size=16,
  ...
){
  profile.image.url <- user.table.row$profile_image_url
  img.b64 <- user.table.row$profile_image_b64
  if(is.na(img.b64)){
    p <- NA
    try(
      p <- retrieve_web_image(profile.image.url)
    )
    if(is.null(p) || is.na(p)){
      return(NA)
    } else {
      if(update.image.b64){
        a <- OpenImageR::average_hash(OpenImageR::rgb_2gray(p$image),hash_size=hash.size)
        query <- sprintf(
          "UPDATE user SET profile_image_hash='%s',profile_image_b64='%s' WHERE user_id='%s';",
          a,
          p$b64,
          user.table.row$user_id
        )
        if(missing(conn)){
          stop("Data connection not provided")
        }
        DBI::dbExecute(conn,query)
      }
      if(save.to.file){
        if(is.null(file.name)){
          profile.image.url.splt <- strsplit(profile.image.url,"/",fixed=TRUE)[[1]]
          file.name <- profile.image.url.splt[length(profile.image.url.splt)]
        }
        OpenImageR::writeImage(p$image,file.name)
      }
      if(return.image){
        return(p$image)
      }
    }
  } else {
    p <- reconstitute_image(img.b64,profile.image.url,showWarnings=showWarnings,...)
    if(!is.na(p) && save.to.file){
      if(is.null(file.name)){
        profile.image.url.splt <- strsplit(profile.image.url,"/",fixed=TRUE)[[1]]
        file.name <- profile.image.url.splt[length(profile.image.url.splt)]
      }
      OpenImageR::writeImage(p,file.name)
    }
    if(return.image){
      return(p)
    }
  }
}



# daily_sentiment_plot(con, where.criteria,start.date,end.date,"Esmeraldas")
# wordcloud_plot(con, where.criteria,start.date,end.date, "Esmeraldas")
# top_hashtags_List(con,where.criteria,start.date,end.date,11)
# top_usermentions_List(con,where.criteria,start.date,end.date,11)
# most_liked_List(con,where.criteria,start.date,end.date,11)
# top_tweeters_List(con,where.criteria,start.date,end.date,11)
# tweet rate plot


top_hashtags <- function(
  con,
  start.date=NULL,
  end.date=NULL,
  where.criteria=NULL,
  limit=10,
  excel.export.file = NULL,
  excel.file.footnote = "auto"
){
  query <- top_count_query(
    con,
    "hashtag.hashtag_text",
    start.date = start.date,
    end.date = end.date,
    where.criteria = where.criteria,
    limit=limit
  )
  results <- dbGetQuery(con,query)
  if(!is.null(excel.export.file) && is.character(excel.export.file)){
    file.name <- create_excel_filename(excel.export.file)
    footnote <- create_footnote(start.date,end.date)
    df <- results[,c("hashtag_text","n")]
    names(df)<-c("Hashtag","Count")
    df$Hashtag <- as.character(df$Hashtag)
    df$Hashtag <- paste("#",df$Hashtag,sep="")
    make_top10_excel(
      df,
      "Count",
      footnote = footnote,
      sheet.name="Sheet1",
      file.name = file.name
    )
  }
  return(results)
}


top_usermentions <- function(
  con,
  start.date=NULL,
  end.date=NULL,
  where.criteria=NULL,
  limit=10,
  excel.export.file = NULL,
  excel.file.footnote = "auto"
){
  query <-top_count_query(
    con,
    "user_mention.user_mention_id",
    start.date = start.date,
    end.date = end.date,
    where.criteria = where.criteria,
    additional.columns = c(
      "user_mention.user_mention_screen_name",
      "user_mention.user_mention_name"
    ),
    limit=limit
  )
  results <- dbGetQuery(con,query)
  if(!is.null(excel.export.file) && is.character(excel.export.file)){
    file.name <- create_excel_filename(excel.export.file)
    footnote <- create_footnote(start.date,end.date)
    df <- results[,c("user_mention_screen_name","n")]
    names(df)<-c("User","Count")
    df$User <- as.character(df$User)
    df$User <- paste("@",df$User,sep="")
    make_top10_excel(
      df,
      "Count",
      footnote = footnote,
      sheet.name="Sheet1",
      file.name = file.name
    )
  }
  return(results)
}


top_urls <- function(
  con,
  start.date=NULL,
  end.date=NULL,
  where.criteria=NULL,
  limit=10,
  excel.export.file = NULL,
  excel.file.footnote = "auto"
){
  query <-top_count_query(
    con,
    "url.extended_url",
    start.date = start.date,
    end.date = end.date,
    where.criteria = where.criteria,
    limit=limit
  )
  results <- dbGetQuery(con,query)
  if(!is.null(excel.export.file) && is.character(excel.export.file)){
    file.name <- create_excel_filename(excel.export.file)
    footnote <- create_footnote(start.date,end.date)
    df <- results[,c("extended_url","n")]
    names(df)<-c("URL","Count")
    make_top10_excel(
      df,
      "Count",
      footnote = footnote,
      sheet.name="Sheet1",
      file.name = file.name
    )
  }
  return(results)
}


top_media <- function(
  con,
  start.date=NULL,
  end.date=NULL,
  where.criteria=NULL,
  limit=10,
  excel.export.file = NULL,
  excel.file.footnote = "auto",
  get.media = TRUE,
  media.file.prefix = NULL
){
  query <-top_count_query(
    con,
    "media.media_id",
    start.date = start.date,
    end.date = end.date,
    additional.columns = c(
      "media.media_url",
      "media.img_b64",
      "media.img_hash"
    ),
    where.criteria = where.criteria,
    limit=limit
  )
  results <- dbGetQuery(con,query)
  if(!is.null(excel.export.file) && is.character(excel.export.file)){
    file.name <- create_excel_filename(excel.export.file)
    footnote <- create_footnote(start.date,end.date)
    df <- results[,c("media_url","n")]
    names(df)<-c("Media URL","Count")
    make_top10_excel(
      df,
      "Count",
      footnote = footnote,
      sheet.name="Sheet1",
      file.name = file.name
    )
  }
  if(get.media){
    for(i in 1:nrow(results)){
      status_media(
        results[i,],
        con,
        save.to.file = TRUE,
        file.name = paste(c(media.file.prefix,sprintf("TopImg-%i.png",i)),collapse=""),
        return.image = FALSE,
        update.media.b64 = FALSE,
        hash.size=16,
        display.image = FALSE
      )
    }
  }
  return(results)
}


top_tweeters <- function(
  con,
  start.date=NULL,
  end.date=NULL,
  where.criteria=NULL,
  limit=10,
  excel.export.file = NULL,
  excel.file.footnote = "auto"
){
  query <-top_count_query(
    con,
    "status.user_id",
    start.date = start.date,
    end.date = end.date,
    where.criteria = where.criteria,
    additional.columns = c(
      "status.screen_name",
      "user.name",
      "user.location",
      "user.description"
    ),
    limit=limit
  )
  results <- dbGetQuery(con,query)
  if(!is.null(excel.export.file) && is.character(excel.export.file)){
    file.name <- create_excel_filename(excel.export.file)
    footnote <- create_footnote(start.date,end.date)
    df <- results[,c("screen_name","n")]
    names(df)<-c("User","Tweets")
    df$User <- as.character(df$User)
    df$User <- paste("@",df$User,sep="")
    make_top10_excel(
      df,
      "Tweets",
      footnote = footnote,
      sheet.name="Sheet1",
      file.name = file.name
    )
  }
  return(results)
}

most_liked <- function(
  con,
  start.date=NULL,
  end.date=NULL,
  where.criteria=NULL,
  limit=10,
  excel.export.file = NULL,
  excel.file.footnote = "auto"
){
  query <- top_value_query(
    con,
    "status.favorite_count",
    start.date = start.date,
    end.date = end.date,
    where.criteria = where.criteria,
    additional.columns = c(
      "status.screen_name",
      "status.text",
      "status.created_at",
      "user.name",
      "user.location",
      "user.description"
    ),
    limit=limit
  )
  results <- dbGetQuery(con,query)
  if(!is.null(excel.export.file) && is.character(excel.export.file)){
    file.name <- create_excel_filename(excel.export.file)
    footnote <- create_footnote(start.date,end.date)
    df <- results[,c("screen_name","created_at","n","text")]
    names(df)<-c("User","Time","Likes","Text")
    df$User <- as.character(df$User)
    df$User <- paste("@",df$User,sep="")
    make_top10_excel(
      df,
      "Likes",
      footnote = footnote,
      sheet.name="Sheet1",
      file.name = file.name
    )
  }
  return(results)
}


most_retweeted <- function(
  con,
  start.date=NULL,
  end.date=NULL,
  where.criteria=NULL,
  limit=10,
  excel.export.file = NULL,
  excel.file.footnote = "auto"
){
  query <- top_value_query(
    con,
    "status.retweet_count",
    start.date = start.date,
    end.date = end.date,
    where.criteria = where.criteria,
    additional.columns = c(
      "status.screen_name",
      "status.text",
      "status.created_at",
      "user.name",
      "user.location",
      "user.description"
    ),
    limit=limit
  )
  results <- dbGetQuery(con,query)
  if(!is.null(excel.export.file) && is.character(excel.export.file)){
    file.name <- create_excel_filename(excel.export.file)
    footnote <- create_footnote(start.date,end.date)
    df <- results[,c("screen_name","created_at","n","text")]
    names(df)<-c("User","Time","Total Retweets","Text")
    df$User <- as.character(df$User)
    df$User <- paste("@",df$User,sep="")
    make_top10_excel(
      df,
      "Total Retweets",
      footnote = footnote,
      sheet.name="Sheet1",
      file.name = file.name
    )
  }
  return(results)
}

most_popular_RT_in_sample <- function(
  con,
  start.date = NULL,
  end.date = NULL,
  where.criteria = NULL,
  limit = 10,
  excel.export.file = NULL,
  excel.file.footnote = "auto"
){
  query <- top_count_query(
    con,
    "retweet_status.id",
    start.date = start.date,
    end.date = end.date,
    where.criteria = where.criteria,
    additional.columns = c( 
      "retweet_status.text",
      "retweet_status.screen_name",
      "retweet_status.created_at",
      "retweet_status.user_id",
      "retweet_user.name",
      "retweet_user.location",
      "retweet_user.description"
    ),
    limit=limit
  )
  results <- dbGetQuery(con,query)
  if(!is.null(excel.export.file) && is.character(excel.export.file)){
    file.name <- create_excel_filename(excel.export.file)
    footnote <- create_footnote(start.date,end.date)
    df <- results[,c("screen_name","created_at","n","text")]
    names(df)<-c("User","Time","Retweets","Text")
    df$User <- as.character(df$User)
    df$User <- paste("@",df$User,sep="")
    make_top10_excel(
      df,
      "Retweets",
      footnote = footnote,
      sheet.name="Sheet1",
      file.name = file.name
    )
  }
  return(results)
}


most_reach <- function(
  con,
  start.date=NULL,
  end.date=NULL,
  where.criteria=NULL,
  limit=10,
  excel.export.file = NULL,
  excel.file.footnote = "auto"
){
  sub.query1.select <- "SELECT status.retweet_status_id as status_id,user.followers_count as followers_count"
  sub.query2.select <- "SELECT status.id as status_id,user.followers_count as followers_count"
  if(is.null(where.criteria)){
    where.criteria.1 <- "status.retweet = 1"
    where.criteria.2 <- "status.retweet = 0"
  } else {
    where.criteria.1 <- paste(
      "(",
      where.criteria,
      ") AND status.retweet = 1",
      sep = ""
    )
    where.criteria.2 <- paste(
      "(",
      where.criteria,
      ") AND status.retweet = 0",
      sep = ""
    )
  }
  where.clause.1 <- make_where_clause(
    start.date,
    end.date,
    where.criteria.1
  )
  where.clause.2 <- make_where_clause(
    start.date,
    end.date,
    where.criteria.2
  )
  join.clause.1 <- make_join_statement(con,paste(sub.query1.select,where.clause.1,sep=" "))
  join.clause.2 <- make_join_statement(con,paste(sub.query2.select,where.clause.2,sep=" "))
  sub.query1 <- paste(
    sub.query1.select,
    "FROM",
    join.clause.1,
    where.clause.1,
    sep=" "
  )
  sub.query2 <- paste(
    sub.query2.select,
    "FROM",
    join.clause.2,
    where.clause.2,
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
  new.query.select <- paste(
    "SELECT status.id as status_id, user.screen_name,user.name,user.location,user.description,status.text,status.created_at"
  )
  new.query.where <- paste("status.id IN (",results$status_id,")",sep="",collapse=",")
  new.query.join <- make_join_statement(con,paste(new.query.select,new.query.where,sep=" "))
  new.query <- paste(
    new.query.select,
    "FROM",
    new.query.join,
    make_where_clause(where.criteria = new.query.where),
    ";",
    sep=""
  )
  new.result <- DBI::dbGetQuery(con,new.query)
  output <- merge(result,new.result,by="status_id")
  output <- output[order(output$n,decreasing=TRUE),]
  if(!is.null(excel.export.file) && is.character(excel.export.file)){
    file.name <- create_excel_filename(excel.export.file)
    footnote <- create_footnote(start.date,end.date)
    df <- output[,c("screen_name","created_at","n","text")]
    names(df)<-c("User","Time","Reach","Text")
    df$User <- as.character(df$User)
    df$User <- paste("@",df$User,sep="")
    make_top10_excel(
      df,
      "Retweets",
      footnote = footnote,
      sheet.name="Sheet1",
      file.name = file.name
    )
  }
  return(output)
}

text_sentiment_dataframe <- function(
  con, 
  start.date=NULL, 
  end.date=NULL, 
  where.criteria=NULL,
  additional.columns = NULL
){
  query <- text_sentiment_query(
    con,
    where.criteria = where.criteria, 
    start.date = start.date, 
    end.date = end.date, 
    where.criteria = where.criteria, 
    additional.columns = c("status.retweet", "status.screen_name",additional.columns)
  )
  results <- dbGetQuery(con, query)
  results$TimeStamp <- as.POSIXct(results$created_at) # format = "%a %b %d %H:%M:%S +0000 %Y")  #, origin = "1970-01-01")
  results$date <- as.Date(results$TimeStamp, format = '%Y-%m-%d')
  results$sent_result <- results$nrc_sentiment_positive - results$nrc_sentiment_negative
  results$sent_label <- sapply(
    results$sent_result,
    sent_label
  )
  # results$sent_label <- as.factor(results$sent_label)
  results$sent_label <- factor(results$sent_label, levels = c('positive', 'slight positive', 'neutral', 'slight negative','negative', 'unknown'))
  return(results)
}

created_at_df <- function(
  con, 
  start.date=NULL, 
  end.date=NULL, 
  where.criteria=NULL,
  additional.columns = NULL
){
  select.clause <- paste(c("status.created_at",additional.columns),collapse=",")
  where.clause <- make_where_clause(
    start.date = start.date,
    end.date = end.date,
    where.criteria = where.criteria
  )
  join.clause <- make_join_statement(paste(select.clause,where.clause,sep=" "))
  query <- paste(
    "SELECT",
    select.clause,
    "FROM",
    join.clause,
    where.clause,
    ";",
    sep=" "
  )
  results <- dbGetQuery(con, query)
  results$TimeStamp <- as.POSIXct(results$created_at) # , format = "%a %b %d %H:%M:%S +0000 %Y")#, origin = "1970-01-01")
  results$date <- as.Date(results$TimeStamp, format = '%Y-%m-%d')
  return(results)
}



create_footnote <- function(excel.file.footnote){
  if(!is.null(excel.file.footnote) && (excel.file.footnote == "auto" || excel.file.footnote)){
    footnote <- date_subtitle(
      start.date = start.date,
      end.date = end.date
    )
  } else if(is.null(excel.file.footnote) || (is.logical(excel.file.footnote) && !excel.file.footnote)) {
    footnote <- ""
  } else {
    footnote <- excel.file.footnote
  }
  return(footnote)
}

create_excel_filename <- function(excel.export.file){
  s <- strsplit(excel.export.file,".",fixed=TRUE)[[1]]
  s.ext <- s[length(s)]
  if(!(tolower(s.ext)=='xlsx')){
    file.name <- paste(s,"xlsx",sep=".")
  } else {
    file.name <- excel.export.file
  }
  return(file.name)
}

