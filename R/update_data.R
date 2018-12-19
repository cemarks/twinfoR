

update_users <- function(con,authentication.vector,user_id=NULL,screen_name=NULL){
  if(missing(authentication.vector)){
    if(!exists('auth.vector')){
      stop("No authentication tokens found!")
    } else {
      authentication.vector <- auth.vector
    }
  }
  count <- DBI::dbGetQuery(con,"SELECT COUNT(1) FROM sqlite_master WHERE type='table' AND name = 'query_users';")
  count <- count[1,1]
  if(is.null(user_id) && is.null(screen_name)){
    if(count==0){
      stop("Table query_users does not exist, and no user_id or screen_name parameters supplied.")
    }
    d <- DBI::dbGetQuery(con,"PRAGMA table_info(query_users);")
    n <- d$name
    if('user_id' %in% n){
      user.df <- DBI::dbGetQuery(con,"SELECT user_id FROM query_users;")
      already.collected <- DBI::dbGetQuery(
        con,
        paste(
          "SELECT id as user_id FROM user WHERE id IN (",
          paste("'",user.df$user_id,"'",sep="",collapse=","),
          ");",
          sep=""
        )
      )
      user.df <- user.df[which(!(user.df$user_id) %in% already.collected$user_id),]
      if(!is.null(nrow(user.df)) && nrow(user.df) > 0){
        try(
          user_lookup_recursive(
            user_id = user.df$user_id,
            data.connection = con,
            authentication.vector = authentication.vector
          )
        )
      }
    } else {
      user.df <- DBI::dbGetQuery(con,"SELECT screen_name FROM query_users;")
      already.collected <- DBI::dbGetQuery(
        con,
        paste(
          "SELECT screen_name FROM user WHERE LOWER(screen_name) IN (",
          paste("'",tolower(user.df$screen_name),"'",sep="",collapse=","),
          ");",
          sep=""
        )
      )
      user.df <- user.df[which(!(tolower(user.df$screen_name)) %in% tolower(already.collected$screen_name)),]
      if(!is.null(nrow(user.df)) && nrow(user.df) > 0){
        try(
          user_lookup_recursive(
            screen_name = user.df$screen_name,
            data.connection = con,
            authentication.vector = authentication.vector
          )
        )
      }
    }
  } else {
    if(is.null(user_id)){
      try(
        user_lookup_recursive(
          screen_name = screen_name,
          data.connection = con,
          authentication.vector = authentication.vector
        )
      )
      user.df <- DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT id as user_id,screen_name FROM user WHERE LOWER(screen_name) IN (%s);",
          paste("'",tolower(screen_name),"'",collapse = ",",sep="")
        )
      )
    } else {
      try(
        user_lookup_recursive(
          user_id = user_id,
          data.connection = con,
          authentication.vector = authentication.vector
        )
      )
      user.df <- DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT id as user_id,screen_name FROM user WHERE id IN (%s);",
          paste("'",user_id,"'",collapse = ",",sep="")
        )
      )
    }
    if(count == 1){
      d <- DBI::dbGetQuery(con,"PRAGMA table_info(query_users);")
      n <- d$name
      if('user_id' %in% n){
        user.df.2 <- DBI::dbGetQuery(con,"SELECT id,user_id FROM query_users;")
        row.id <- max(user.df.2$id)+1
        user.df$in_db <- user.df$user_id %in% user.df.2$user_id
        for(i in which(!user.df$in_db)){
          if('screen_name' %in% n){
            query <- paste(
              "INSERT OR IGNORE INTO query_users(id,user_id,screen_name,since_id) VALUES (",
              write_int(row.id),
              ",",
              write_text(user.df$user_id[i]),
              ",",
              write_text(user.df$screen_name[i]),
              ",NULL);",
              sep=""
            )
          } else {
            query <- paste(
              "INSERT OR IGNORE INTO query_users(id,user_id,since_id) VALUES (",
              write_int(row.id),
              ",",
              write_text(user.df$user_id[i]),
              ",NULL);",
              sep=""
            )
          }
          DBI::dbExecute(con,query)
          row.id <- row.id + 1
        }
      } else {
        user.df.2 <- DBI::dbGetQuery(con,"SELECT id,screen_name,since_id FROM query_users;")
        row.id <- max(user.df.2$id)+1
        user.df$in_db <- tolower(user.df$screen_name) %in% tolower(user.df.2$screen_name)
        for(i in which(!(user.df$in_db))){
          query <- paste(
            "INSERT OR IGNORE INTO query_users(id,screen_name,since_id) VALUES (",
            write_int(row.id),
            ",",
            write_text(user.df$screen_name[i]),
            ",NULL);",
            sep=""
          )
          DBI::dbExecute(con,query)
          row.id <- row.id + 1
        }
      }
    } else {
      if(!is.null(nrow(user.df)) && nrow(user.df) > 0){
        d <- data.frame(id=1:nrow(user.df),user_id=user.df$user_id,screen_name=user.df$screen_name,since_id=rep(NA,nrow(user.df)),followers_collected=rep(0,nrow(user.df)),friends_collected=rep(0,nrow(user.df)),stringsAsFactors = FALSE)
        upload_query_users(con,d)
      }
    }
  }
}

update_user_timelines <- function(con,authentication.vector,user_id=NULL,screen_name=NULL){
  if(missing(authentication.vector)){
    if(!exists('auth.vector')){
      stop("No authentication tokens found!")
    } else {
      authentication.vector <- auth.vector
    }
  }
  update_users(
    con,
    authentication.vector = authentication.vector,
    user_id = user_id,
    screen_name=screen_name
  )
  d <- DBI::dbGetQuery(con,"PRAGMA table_info(query_users);")
  n <- d$name
  if('user_id' %in% n){
    if(is.null(user_id) && is.null(screen_name)){
      query <- "SELECT query_users.id as row_id,query_users.user_id as user_id,query_users.since_id as since_id,user.protected as protected from query_users LEFT JOIN user ON query_users.user_id = user.id;"
    } else if(is.null(user_id)){
      query <- sprintf(
        "SELECT query_users.id as row_id,query_users.user_id as user_id,query_users.since_id as since_id,user.protected as protected from query_users LEFT JOIN user ON query_users.user_id = user.id WHERE LOWER(user.screen_name) IN (%s);",
        paste("'",screen_name,"'",sep="",collapse=",")
      )
    } else {
      query <- sprintf(
        "SELECT query_users.id as row_id,query_users.user_id as user_id,query_users.since_id as since_id,user.protected as protected from query_users LEFT JOIN user ON query_users.user_id = user.id WHERE LOWER(user.id) IN (%s);",
        paste("'",user_id,"'",sep="",collapse=",")
      )
    }
  } else {
    if(is.null(user_id) && is.null(screen_name)){
      query <- "SELECT query_users.id as row_id,query_users.user_id as user_id,query_users.since_id as since_id,user.protected as protected from query_users LEFT JOIN user ON LOWER(query_users.screen_name) = LOWER(user.screen_name);"
    } else if(is.null(user_id)){
      query <- sprintf(
        "SELECT query_users.id as row_id,query_users.user_id as user_id,query_users.since_id as since_id,user.protected as protected from query_users LEFT JOIN user ON LOWER(query_users.screen_name) = LOWER(user.screen_name) WHERE LOWER(user.screen_name) IN (%s);",
        paste("'",screen_name,"'",sep="",collapse=",")
      )
    } else {
      query <- sprintf(
        "SELECT query_users.id as row_id,query_users.user_id as user_id,query_users.since_id as since_id,user.protected as protected from query_users LEFT JOIN user ON LOWER(query_users.screen_name) = LOWER(user.screen_name) WHERE LOWER(user.id) IN (%s);",
        paste("'",user_id,"'",sep="",collapse=",")
      )
    }
  }
  user.df <- DBI::dbGetQuery(con,query)
  if(!is.null(nrow(user.df)) && nrow(user.df)> 0){
    w <- which(!as.logical(user.df$protected))
    for(i in w){
      try({
        if(is.na(user.df$since_id[i])){
          s <- user_timeline_recursive(user_id = user.df$user_id[i],data.connection = con, authentication.vector = authentication.vector)
        } else {
          s <- user_timeline_recursive(user_id = user.df$user_id[i],data.connection = con, authentication.vector = authentication.vector, since_id = user.df$since_id[i])
        }
        if(!is.na(s)){
          query <- sprintf("UPDATE query_users SET since_id = '%s' WHERE id = %i;",s,user.df$row_id[i])
          DBI::dbExecute(con,query)
        }
      })
    }
  }
}


update_search <- function(con,authentication.vector,query.text=NULL){
  if(missing(authentication.vector)){
    if(!exists('auth.vector')){
      stop("No authentication tokens found!")
    } else {
      authentication.vector <- auth.vector
    }
  }
  count <- DBI::dbGetQuery(con,"SELECT COUNT(1) FROM sqlite_master WHERE type='table' AND name = 'query_text';")
  count <- count[1,1]
  if(is.null(query.text) && count==0){
    stop("Table query_text does not exist, and no query.text parameter supplied.")
  } else if(count==0){
    d <- data.frame(id=1:length(query.text),query_text=query.text,since_id=rep(NA,length(query.text)),stringsAsFactors = FALSE)
    upload_query_text(con,d)
    query.text <- NULL
  }
  query <- "SELECT query_text.id as row_id, query_text.query_text as query_text, query_text.since_id as since_id FROM query_text;"
  query.df <- DBI::dbGetQuery(con,query)
  if(!is.null(query.text)){
    row.ids <- rep(0,length(query.text))
    current.row.id <- max(query.df$row_id)+1
    for(i in 1:length(query.text)){
      w <- which(lower(query.df$query_text)==lower(query.text[i]))
      if(length(w)>0){
        row.ids[i] <- query.df$row_id[w[1]]
      } else {
        new.query <- sprintf(
          "INSERT INTO query_text(id,query_text,since_id) VALUES (%i,'%s',NULL);",
          current.row.id,
          query.text[i]
        )
        DBI::dbExecute(con,new.query)
        row.ids[i] <- current.row.id
        current.row.id <- current.row.id + 1
      }
    }
    query.df <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT query_text.id as row_id, query_text.query_text as query_text, query_text.since_id as since_id FROM query_text WHERE query_text.id IN (%s);",
        paste(unique(row.ids),collapse=",")
      )
    )
  }
  if(!is.null(nrow(query.df)) && nrow(query.df) > 0){
    for(i in 1:nrow(query.df)){
      if(is.na(query.df$since_id[i])){
        s <- search_tweets_recursive(q = query.df$query_text[i],data.connection = con, authentication.vector = authentication.vector, query.row.id = query.df$row_id[i])
      } else {
        s <- search_tweets_recursive(q = query.df$query_text[i],data.connection = con, authentication.vector = authentication.vector, since_id = query.df$since_id[i], query.row.id = query.df$row_id[i])
      }
      if(!is.na(s)){
        query <- sprintf("UPDATE query_text SET since_id = '%s' WHERE id = '%s';",s,query.df$row_id[i])
        DBI::dbExecute(con,query)
      }
    }
  }
}


get_all_friends <- function(con,authentication.vector,user_id=NULL,screen_name=NULL){
  if(missing(authentication.vector)){
    if(!exists('auth.vector')){
      stop("No authentication tokens found!")
    } else {
      authentication.vector <- auth.vector
    }
  }
  count <- DBI::dbGetQuery(con,"SELECT COUNT(1) FROM sqlite_master WHERE type='table' AND name = 'query_users';")
  count <- count[1,1]
  if(count == 0 && is.null(user_id) && is.null(screen_name)){
    stop("Table query_users does not exist, and no user_id or screen_name parameters supplied.")
  }
  update_users(
    con=con,
    authentication.vector=authentication.vector,
    user_id=user_id,
    screen_name=screen_name
  )
  d <- DBI::dbGetQuery(con,"PRAGMA table_info(query_users);")
  n <- d$name
  if(is.null(user_id) && is.null(screen_name)){
    if('user_id' %in% n){
      user.df <- DBI::dbGetQuery(con,"SELECT query_users.id as row_id, query_users.user_id as user_id,user.friends_count as friends_count,user.protected as protected FROM query_users JOIN user ON query_users.user_id = user.id WHERE query_users.friends_collected = 0;")
    } else {
      user.df <- DBI::dbGetQuery(con,"SELECT query_users.id as row_id, user.id as user_id,user.friends_count as friends_count,user.protected as protected FROM query_users JOIN user ON query_users.screen_name = user.screen_name WHERE query_users.friends_collected = 0;")
    }
  } else if(is.null(user_id)){
    if('user_id' %in% n){
      user.df <- DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT query_users.id as row_id,user.id as user_id,user.screen_name,user.friends_count as friends_count, user.protected as protected FROM query_users JOIN user ON query_users.user_id=user.id WHERE LOWER(user.screen_name) IN (%s);",
          paste(
            "'",
            tolower(screen_name),
            "'",
            collapse=",",
            sep=""
          )
        )
      )
    } else {
      user.df <- DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT query_users.id as row_id,user.id as user_id,user.screen_name,user.friends_count as friends_count, user.protected as protected FROM query_users JOIN user ON LOWER(query_users.screen_name) = LOWER(user.screen_name) WHERE LOWER(user.screen_name) IN (%s);",
          paste(
            "'",
            tolower(screen_name),
            "'",
            collapse=",",
            sep=""
          )
        )
      )
    }
  } else {
    if('user_id' %in% n){
      user.df <- DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT query_users.id as row_id,user.id as user_id,user.screen_name,user.friends_count as friends_count, user.protected as protected FROM query_users JOIN user ON query_users.user_id=user.id WHERE user.user_id IN (%s);",
          paste(
            "'",
            user_id,
            "'",
            collapse=",",
            sep=""
          )
        )
      )
    } else {
      user.df <- DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT query_users.id as row_id,user.id as user_id,user.screen_name,user.friends_count as friends_count, user.protected as protected FROM query_users JOIN user ON LOWER(query_users.screen_name) = LOWER(user.screen_name) WHERE user.user_id IN (%s);",
          paste(
            "'",
            user_id,
            "'",
            collapse=",",
            sep=""
          )
        )
      )
    }
  }
  if(!is.null(nrow(user.df)) && nrow(user.df)> 0){
    for(i in 1:nrow(user.df)){
      query <- sprintf(
        "UPDATE query_users SET friends_collected = 1 WHERE id = %i",
        user.df$row_id
      )
      if(!as.logical(user.df$protected[i])){
        if(user.df$friends_count[i] < 200){
          try({
            uid <- friends_list_recursive(
              user_id = user.df$user_id[i],
              data.connection = con,
              authentication.vector = authentication.vector
            )
            DBI::dbExecute(
              con,
              query
              )
            })
        } else {
          try({
            uid <- friends_ids_recursive(
              user_id = user.df$user_id[i],
              data.connection = con,
              authentication.vector = authentication.vector
            )
            DBI::dbExecute(
              con,
              query
            )
          })
          user.ids <- DBI::dbGetQuery(
            con,
            sprintf(
              "SELECT friend_id FROM followers WHERE follower_id = '%s';",
              user.df$user_id[i]
            )
          )
          if(length(user.ids$friend_id)>0){
            user_lookup_recursive(
              user_id = user.ids$friend_id,
              data.connection = con,
              authentication.vector = authentication.vector
            )
          }
        }
      } else {
        DBI::dbExecute(
          con,
          query
        )
      }
    }
  }
}

get_all_followers <- function(con,authentication.vector,user_id=NULL,screen_name=NULL){
  if(missing(authentication.vector)){
    if(!exists('auth.vector')){
      stop("No authentication tokens found!")
    } else {
      authentication.vector <- auth.vector
    }
  }
  count <- DBI::dbGetQuery(con,"SELECT COUNT(1) FROM sqlite_master WHERE type='table' AND name = 'query_users';")
  count <- count[1,1]
  if(count == 0 && is.null(user_id) && is.null(screen_name)){
    stop("Table query_users does not exist, and no user_id or screen_name parameters supplied.")
  }
  update_users(
    con=con,
    authentication.vector=authentication.vector,
    user_id=user_id,
    screen_name=screen_name
  )
  d <- DBI::dbGetQuery(con,"PRAGMA table_info(query_users);")
  n <- d$name
  if(is.null(user_id) && is.null(screen_name)){
    if('user_id' %in% n){
      user.df <- DBI::dbGetQuery(con,"SELECT query_users.user_id as user_id,user.followers_count as followers_count,user.protected as protected FROM query_users JOIN user ON query_users.user_id = user.id WHERE query_users.followers_collected = 0;")
    } else {
      user.df <- DBI::dbGetQuery(con,"SELECT user.id as user_id,user.followers_count as followers_count,user.protected as protected FROM query_users JOIN user ON query_users.screen_name = user.screen_name WHERE query_users.followers_collected = 0;")
    }
  } else if(is.null(user_id)){
    if('user_id' %in% n){
      user.df <- DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT query_users.id as row_id,user.id as user_id,user.screen_name,user.followers_count as followers_count, user.protected as protected FROM query_users JOIN user ON query_users.user_id=user.id WHERE LOWER(user.screen_name) IN (%s);",
          paste(
            "'",
            tolower(screen_name),
            "'",
            collapse=",",
            sep=""
          )
        )
      )
    } else {
      user.df <- DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT query_users.id as row_id,user.id as user_id,user.screen_name,user.followers_count as followers_count, user.protected as protected FROM query_users JOIN user ON LOWER(query_users.screen_name) = LOWER(user.screen_name) WHERE LOWER(user.screen_name) IN (%s);",
          paste(
            "'",
            tolower(screen_name),
            "'",
            collapse=",",
            sep=""
          )
        )
      )
    }
  } else {
    if('user_id' %in% n){
      user.df <- DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT query_users.id as row_id,user.id as user_id,user.screen_name,user.followers_count as followers_count, user.protected as protected FROM query_users JOIN user ON query_users.user_id=user.id WHERE user.user_id IN (%s);",
          paste(
            "'",
            user_id,
            "'",
            collapse=",",
            sep=""
          )
        )
      )
    } else {
      user.df <- DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT query_users.id as row_id,user.id as user_id,user.screen_name,user.followers_count as followers_count, user.protected as protected FROM query_users JOIN user ON LOWER(query_users.screen_name) = LOWER(user.screen_name) WHERE user.user_id IN (%s);",
          paste(
            "'",
            user_id,
            "'",
            collapse=",",
            sep=""
          )
        )
      )
    }
  }
  if(!is.null(nrow(user.df)) && nrow(user.df)> 0){
    for(i in 1:nrow(user.df)){
      query <- sprintf(
        "UPDATE query_users SET followers_collected = 1 WHERE id = %i",
        user.df$row_id
      )
      if(as.logical(user.df$protected[i])){
        DBI::dbExecute(
          con,
          query
        )
      } else {
        if(user.df$followers_count[i] < 200){
          try({
            uid <- followers_list_recursive(
              user_id = user.df$user_id[i],
              data.connection = con,
              authentication.vector = authentication.vector
            )
            DBI::dbExecute(
              con,
              query
            )
          })
        } else {
          try({
            uid <- followers_ids_recursive(
              user_id = user.df$user_id[i],
              data.connection = con,
              authentication.vector = authentication.vector
            )
            DBI::dbExecute(
              con,
              query
            )
          })
          user.ids <- DBI::dbGetQuery(
            con,
            sprintf(
              "SELECT follower_id FROM followers WHERE friend_id = '%s';",
              user.df$user_id[i]
            )
          )
          if(length(user.ids$follower_id)>0){
            user_lookup_recursive(
              user_id = user.ids$follower_id,
              data.connection = con,
              authentication.vector = authentication.vector
            )
          }
        }
      }
    }
  }
}


# Update search (needs to get most recent tweet from each search)  Include option to supply queries.  If queries are supplied, add them to the query table.
### Also needs to update the status_search table.
# Collect friends and followers.

### Summarize functions


summarize_database <- function(con){
  count <- DBI::dbGetQuery(con,"SELECT COUNT(1) FROM sqlite_master WHERE type='table' AND name = 'query_users';")
  count <- count[1,1]
  if(count == 1){
    query.user.count <- DBI::dbGetQuery(con,"SELECT COUNT(1) as n FROM query_users;")
    cat("Query users: ",as.character(query.user.count$n),"\n",sep="")
    cat("User categories:\n",paste(get_query_user_categories(con),collapse = "\n"),"\n",sep="")
  } else {
    cat("No query user table.\n")
  }
  count <- DBI::dbGetQuery(con,"SELECT COUNT(1) FROM sqlite_master WHERE type='table' AND name = 'query_text';")
  count <- count[1,1]
  if(count == 1){
    query.text.count <- DBI::dbGetQuery(con,"SELECT COUNT(1) as n FROM query_text;")
    cat("Search queries: ",as.character(query.text.count$n),"\n",sep="")
    cat("Search categories:\n",paste(get_query_text_categories(con),collapse = "\n"),"\n",sep="")
  } else {
    cat("No query text table.\n")
  }
  cat("Users: ",as.character(user_count(con)),"\n",sep="")
  cat("Statuses: ",as.character(status_count(con)),"\n",sep="")
  um.count <- usermention_count(con)
  ht.count <- hashtag_count(con)
  url.count <- url_count(con)
  media.count <- media_count(con)
  cat("Unique user mentions: ",as.character(um.count$unique),"\n",sep="")
  cat("Unique hashtags: ",as.character(ht.count$unique),"\n",sep="")
  cat("Unique URLs: ",as.character(url.count$unique),"\n",sep="")
  cat("Total Images: ",as.character(media.count$total),"\n",sep="")
  cat("Unique Images: ",as.character(media.count$unique),"\n",sep="")
  cat("Total relationships: ",as.character(relationship_count(con)),"\n",sep="")
  filesize.str <- db_size_str(con)
  if(is.na(filesize.str)){
    cat("Cannot get file size.  Did you change directory?\n")
  } else {
    cat("Database file size: ",filesize.str,"\n",sep="")
  }
}

db_size_str <- function(con){
  filesize <- NA
  try(filesize <- file.size(con@dbname))
  if(is.na(filesize)){
    filesize.str <- NA
  } else if(filesize < 1000000){
    filesize.str <- paste(round(filesize/1000),"kB",sep="")
  } else if(filesize < 10000000){
    filesize.str <- paste(sprintf("%1.1f",filesize/1000000),"MB",sep="")
  } else {
    filesize.str <- paste(round(filesize/1000000),"MB",sep="")
  }
  return(filesize.str)
}
user_count <- function(con){
  query <- "SELECT COUNT(1) as n FROM user;"
  result <- DBI::dbGetQuery(con,query)
  return(result$n)
}
status_count <- function(con){
  query <- "SELECT COUNT(1) as n FROM status;"
  result <- DBI::dbGetQuery(con,query)
  return(result$n)
}
usermention_count <- function(con){
  query <- "SELECT user_mention_id,COUNT(user_mention_id) as n FROM user_mention GROUP BY user_mention_id;"
  result <- DBI::dbGetQuery(con,query)
  return(list('total'=sum(result$n),'unique'=nrow(result)))
}
hashtag_count <- function(con){
  query <- "SELECT LOWER(hashtag_text),COUNT(LOWER(hashtag_text)) as n FROM hashtag GROUP BY LOWER(hashtag_text);"
  result <- DBI::dbGetQuery(con,query)
  return(list('total'=sum(result$n),'unique'=nrow(result)))
}
url_count <- function(con){
  query <- "SELECT LOWER(extended_url),COUNT(LOWER(extended_url)) as n FROM url GROUP BY LOWER(extended_url);"
  result <- DBI::dbGetQuery(con,query)
  return(list('total'=sum(result$n),'unique'=nrow(result)))
}
media_count <- function(con){
  query <- "SELECT media_id,COUNT(media_id) as n FROM media GROUP BY media_id;"
  result <- DBI::dbGetQuery(con,query)
  return(list('total'=sum(result$n),'unique'=nrow(result)))
}
relationship_count <- function(con){
  query <- "SELECT COUNT(1) as n FROM followers;"
  result <- DBI::dbGetQuery(con,query)
  return(result$n)
}

get_query_users <- function(con){
  count <- DBI::dbGetQuery(con,"SELECT COUNT(1) FROM sqlite_master WHERE type='table' AND name = 'query_users';")
  if(count == 1){
    return(DBI::dbGetQuery(con,"SELECT * FROM query_users;"))
  } else {
    stop("No query user table exists in this database.")
  }
}
get_query_text <- function(con){
  count <- DBI::dbGetQuery(con,"SELECT COUNT(1) FROM sqlite_master WHERE type='table' AND name = 'query_text';")
  if(count == 1){
    return(DBI::dbGetQuery(con,"SELECT * FROM query_text;"))
  } else {
    stop("No query text table exists in this database.")
  }
}

get_query_user_categories <- function(con){
  count <- DBI::dbGetQuery(con,"SELECT COUNT(1) FROM sqlite_master WHERE type='table' AND name = 'query_users';")
  count <- count[1,1]
  if(count == 1){
    d <- DBI::dbGetQuery(con,"PRAGMA table_info(query_users);")
    n <- setdiff(d$name,c("id","user_id","screen_name"))
    return(n)
  } else {
    stop("No query user table exists in this database.")
  }
}
get_query_text_categories <- function(con){
  count <- DBI::dbGetQuery(con,"SELECT COUNT(1) FROM sqlite_master WHERE type='table' AND name = 'query_text';")
  count <- count[1,1]
  if(count == 1){
    d <- DBI::dbGetQuery(con,"PRAGMA table_info(query_text);")
    n <- setdiff(d$name,c("id","query_text"))
    return(n)
  } else {
    stop("No query text table exists in this database.")
  }
}

