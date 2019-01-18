join_table <- function(con){
  d <- DBI::dbGetQuery(con,"PRAGMA table_info(query_users);")
  n <- d$name
  if('user_id' %in% n){
    user.join <- 'user_id'
  } else {
    user.join <- 'screen_name'
  }
  join.table <- list(
    query_users=c(status = 'user_id',query_users = user.join),
    query_text = c(search_status = 'query_id',query_text = 'id'),
    search_status = c(status = 'id', search_status = 'status_id'),
    user = c(status = 'user_id',user = 'id'),
    hashtag = c(status = 'id',hashtag = 'status_id'),
    media = c(status = 'id',media = 'status_id'),
    user_mention = c(status = 'id',user_mention = 'status_id'),
    url = c(status = 'id',url = 'status_id')
  )
  return(join.table)
}

make_join_statement <- function(con,sql.clause){
  join.table <- join_table(con)
  output.str <- "status"
  subs <- c("+","-","=","(",")",",")
  for(s in subs){
    sql.clause <- gsub(s," ",sql.clause,fixed=TRUE)
  }
  sql.sep <- strsplit(sql.clause," ")[[1]]
  sql.tables <- grep(".",sql.sep,fixed=TRUE,value=TRUE)
  if(length(sql.tables)>0){
    sql.tables <- strsplit(sql.tables,".",fixed=TRUE)
    sql.tables <- sapply(
      sql.tables,
      function(x){
        return(x[1])
      }
    )
    sql.tables <- setdiff(sql.tables,"status")
    sql.tables <- unique(sql.tables)
    for(sql.table in sql.tables){
      if(sql.table == 'query_text'){
        int.table <- "search_status"
        output.str <- paste(
          output.str,
          "JOIN",
          int.table,
          "ON",
          paste(
            "status",
            join.table[[int.table]]['status'],
            sep="."
          ),
          "=",
          paste(
            int.table,
            join.table[[int.table]][int.table],
            sep="."
          ),
          sep=" "
        )
        output.str <- paste(
          output.str,
          "JOIN",
          sql.table,
          "ON",
          paste(
            int.table,
            join.table[[sql.table]][int.table],
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
      } else if(sql.table == 'retweet_status'){
        output.str <- paste(
          output.str,
          "JOIN status AS retweet_status ON status.retweet_status_id = retweet_status.id JOIN user AS retweet_user ON retweet_status.user_id = retweet_user.id",
          sep=" "
        )
      } else if (sql.table != 'retweet_user') {
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
  }
  return(output.str)
}

make_where_clause <- function(start.date=NULL,end.date=NULL,where.criteria=NULL){
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


top_count_query <- function(
  con,
  count.column,
  start.date=NULL,
  end.date=NULL,
  where.criteria=NULL,
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
  where.clause <- make_where_clause(start.date,end.date,where.criteria)
  join.clause <- make_join_statement(con,paste(select.clause,where.clause,sep=" "))
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


top_value_query <- function(
  con,
  value.column,
  start.date=NULL,
  end.date=NULL,
  where.criteria=NULL,
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
  where.clause <- make_where_clause(start.date,end.date,where.criteria)
  join.clause <- make_join_statement(con,paste(select.clause,where.clause,sep=" "))
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
text_sentiment_query <- function(
  con,
  start.date=NULL,
  end.date=NULL,
  where.criteria=NULL,
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
  where.clause <- make_where_clause(start.date,end.date,where.criteria)
  join.clause <- make_join_statement(con,paste(select.clause,where.clause,sep=" "))
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

