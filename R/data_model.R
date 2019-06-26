## Twitter Data Functions
## To do: review @seealso's.  These should probably reference recursive calls rather than the updates.

#' Create Twitter SQLite Connection
#'
#' Create RSQLite connection and Twitter tables
#'
#' Opens a connection to an SQLite database on the path provided.
#' The database file is created if it does not exist.  Tables 'user',
#' 'status', 'user_mention', 'hashtag', 'url', and 'media' are created
#' if the do not exist.  The data connection has a special structure to
#' store Twitter data.
#' Optional tables 'query_users' and 'query_text' are not created unless these
#' tables are provided as data frames.  The search_status table is created
#' if a 'query_text' table is created.  Each of tables are described below.
#'
#' @section \code{user} table:
#' This table is created automatically to contain all of the user objects
#' collected, including user objects embedded in tweets collected using
#' a method that returns statuses, e.g., the search API.  It consists of
#' the following columns.
#' \tabular{lll}{
#' COLUMN \tab TYPE \tab DESCRIPTION \cr
#' id \tab TEXT \tab Twitter user_id \cr
#' screen_name \tab TEXT \tab Twitter user screen_name \cr
#' name \tab TEXT \tab Twitter user name \cr
#' location \tab TEXT \tab User self-identified location \cr
#' description \tab TEXT \tab User self-description \cr
#' url \tab TEXT \tab User self-identified URL \cr
#' protected \tab TINYINT(1) \tab Account protected TINYINT(1)ean \cr
#' followers_count \tab INT \tab User follower count \cr
#' friends_count \tab INT \tab User friend count \cr
#' statuses_count \tab INT \tab User's number of tweets \cr
#' created_at \tab DATETIME \tab Account creation date-time \cr
#' favourites_count \tab INT \tab Number of tweets 'liked' by user \cr
#' geo_enabled \tab TINYINT(1) \tab Account geo-enabled TINYINT(1)ean \cr
#' verified \tab TINYINT(1) \tab Account owner verified TINYINT(1)ean \cr
#' lang \tab TEXT \tab Account langauage abbreviation \cr
#' status_id \tab TEXT \tab Account recent/pinned status \cr
#' profile_image_url \tab TEXT \tab URL to profile image \cr
#' profile_banner_url \tab TEXT \tab URL to profile banner image \cr
#' profile_image_hash \tab TEXT \tab profile image average hash (optional, requires OpenImageR) \cr
#' profile_banner_hash \tab TEXT \tab profile banner average hash (optional, requires OpenImageR) \cr
#' profile_image_b64 \tab TEXT \tab profile image 64-bit encoding (optional, requires base64enc) \cr
#' profile_banner_b64 \tab TEXT \tab profile banner 64-bit encoding (optional, requires base64enc)
#' }
#'
#' @section \code{status} table:
#' This table contains the status or tweet objects collected.  These
#' statuses can include retweets embedded in collected tweets or
#' statuses embedded in user objects.  The table consists of
#' the following columns.
#' \tabular{lll}{
#' COLUMN \tab TYPE \tab DESCRIPTION \cr
#' id \tab TEXT \tab Twitter status_id \cr
#' created_at \tab DATETIME \tab Status creation date-time \cr
#' user_id \tab TEXT \tab Twitter status user_id \cr
#' screen_name \tab TEXT \tab Twitter status screen_name \cr
#' text \tab TEXT \tab Status text \cr
#' in_reply_to_status_id \tab TEXT \tab Twitter status_id of replied-to tweet \cr
#' in_reply_to_user_id \tab TEXT \tab Twitter user_id of replied-to user \cr
#' retweet_count \tab INT \tab Number of times original status was retweeted \cr
#' favorite_count \tab INT \tab Number of times original status was 'liked' \cr
#' lang \tab TEXT \tab Status language abbreviation \cr
#' geo_lat \tab FLOAT \tab Geo-location latitude, if available \cr
#' geo_long \tab FLOAT \tab Geo-location longitude, if available \cr
#' source \tab TEXT \tab Application used to post the tweet \cr
#' retweet \tab TINYINT(1) \tab TINYINT(1)ean indiating whether status is a retweet \cr
#' retweet_status_id \tab TEXT \tab Retweeted status status_id \cr
#' sentiment_score \tab INT \tab Rsentiment sentiment score \cr
#' nrc_sentiment_anger \tab INT \tab NRC 'anger' score using syuzhet package \cr
#' nrc_sentiment_anticipation  \tab INT \tab NRC 'anticipation' score using syuzhet package \cr
#' nrc_sentiment_disgust  \tab INT \tab NRC 'disgust' score using syuzhet package \cr
#' nrc_sentiment_fear \tab INT \tab NRC 'fear' score using syuzhet package \cr
#' nrc_sentiment_joy \tab INT \tab NRC 'joy' score using syuzhet package \cr
#' nrc_sentiment_sadness \tab INT \tab NRC 'sadness' score using syuzhet package \cr
#' nrc_sentiment_surprise \tab INT \tab NRC 'surprise' score using syuzhet package \cr
#' nrc_sentiment_trust \tab INT \tab NRC 'trust' score using syuzhet package \cr
#' nrc_sentiment_negative \tab INT \tab NRC 'negative' score using syuzhet package \cr
#' nrc_sentiment_positive \tab INT \tab NRC 'positive' score using syuzhet package \cr
#' sentiment_collected \tab TINYINT(1) \tab TINYINT(1)ean (0/1) indicating whether sentiment analysis has been run\cr
#' }
#'
#' @section \code{user_mention} table:
#' This table captures the one to many relationship between tweets (i.e., statuses)
#' and user-mentions.  If a status mentions a user, that mention is captured in this
#' table.
#' \tabular{lll}{
#' COLUMN \tab TYPE \tab DESCRIPTION \cr
#' status_id \tab TEXT \tab The status_id of the tweet \cr
#' user_mention_id \tab TEXT \tab The mentioned user's user_id \cr
#' user_mention_screen_name \tab TEXT \tab the mentioned user's screen_name \cr
#' user_mention_name \tab TEXT \tab the mentioned user's name
#' }
#'
#' @section \code{hashtag} table:
#' This table captures the one to many relationship between tweets (i.e., statuses)
#' and hashtags.  If a status includes a hashtag, that hashtag is captured in this
#' table.
#' \tabular{lll}{
#' COLUMN \tab TYPE \tab DESCRIPTION \cr
#' status_id \tab TEXT \tab The status_id of the tweet \cr
#' hashtag_text \tab TEXT \tab The hashtag text (omitting the '#')
#' }
#'
#' @section \code{url} table:
#' This table captures the one to many relationship between tweets (i.e., statuses)
#' and urls.  If a status includes a url, that url is captured in this
#' table.
#' \tabular{lll}{
#' COLUMN \tab TYPE \tab DESCRIPTION \cr
#' status_id \tab TEXT \tab The status_id of the tweet \cr
#' display_url \tab TEXT \tab The display text for the URL in the status \cr
#' extended_url \tab TEXT \tab The full URL
#' }
#'
#' @section \code{media} table:
#' This table captures the one to many relationship between tweets (i.e., statuses)
#' and attached media (images).  If a status includes an image, information about that
#' image is captured in this table.
#' \tabular{lll}{
#' COLUMN \tab TYPE \tab DESCRIPTION \cr
#' status_id \tab TEXT \tab The status_id of the tweet \cr
#' media_id \tab TEXT \tab The twitter media_id assigned to the image \cr
#' expanded_url \tab TEXT \tab The twitter URL to the image \cr
#' media_url \tab TEXT \tab The twitter URL to the image page \cr
#' media_type \tab TEXT \tab The twitter media_type category \cr
#' source_user_id \tab TEXT \tab The user_id for the user that uploaded the image \cr
#' source_status_id \tab TEXT \tab the status_id for the original status attaching the image \cr
#' img_hash \tab TEXT \tab average hash string for image (optional, requires OpenImageR) \cr
#' img_b64 \tab TEXT \tab 64-bit image encoding (optional, requires base64enc)
#' }
#'
#' @section \code{query_users} table:
#' This optional table allows analysts to categorize Twitter users for
#' collection and analysis.  It *must* include users' screen_names or
#' user_ids.  It can include as many columns as the analyst requires.
#' \tabular{lll}{
#' COLUMN \tab TYPE \tab DESCRIPTION \cr
#' id \tab TEXT \tab Row ID for reference and indexing \cr
#' user_id \tab TEXT or TEXT \tab Twitter user_id \cr
#' screen_name \tab TEXT \tab Twitter screen_name \cr
#' since_id \tab TEXT \tab Most recent status from user in database \cr
#' friends_collected \tab TINYINT \tab Indicates whether user's friends have been collected \cr
#' followers_collected \tab TINYINT \tab Indicates whether user's followers have been collected \cr
#' [CATEGORY1] \tab [TYPE] \tab Analyst-defined user categorization \cr
#' [CATEGORY2] \tab [TYPE] \tab Analyst-defined user categorization \cr
#' ... \tab ... \tab Analyst-defined user categorization \cr
#' [CATEGORYN] \tab [TYPE] \tab Analyst-defined user categorization \cr
#' }
#' @section \code{followers} table:
#' This table is created to capture virtual relationships in Twitter.
#' \tabular{lll}{
#' COLUMN \tab TYPE \tab DESCRIPTION \cr
#' follower_id \tab TEXT \tab The user_id of the follower in the relationship \cr
#' friend_id \tab TEXT \tab The user_id of the friend in the relationship.
#' }
#' 
#' @section \code{query_text} table:
#' This optional table allows analysts to categorize search queries for
#' collection and analysis.  It *must* include a query_text column.
#' It can include as many columns as the analyst requires.
#' \tabular{lll}{
#' COLUMN \tab TYPE \tab DESCRIPTION \cr
#' id \tab INT \tab Row ID for indexing and referencing. \cr
#' query_text \tab TEXT \tab Character string to be used in search queries \cr
#' since_id \tab TEXT \tab Most recent status from query in database \cr
#' [CATEGORY1] \tab [TYPE] \tab Analyst-defined user categorization \cr
#' [CATEGORY2] \tab [TYPE] \tab Analyst-defined user categorization \cr
#' ... \tab ... \tab Analyst-defined user categorization \cr
#' [CATEGORYN] \tab [TYPE] \tab Analyst-defined user categorization \cr
#' }
#'
#' @section \code{search_status} table:
#' This table is created to map statuses to queries, effectively recording
#' which search queries produced which tweets.
#' \tabular{lll}{
#' COLUMN \tab TYPE \tab DESCRIPTION \cr
#' query_id \tab INT \tab The row ID for the query in the query_text table \cr
#' status_id \tab TEXT \tab The status_id that resulted from the query.
#' }
#'
#' @param db.name character name of database.  If SQLite connection, this is the
#' file name.
#' @param query.users.df data.frame to be uploaded as the query.users table
#' @param query.text.df data.frame to be uploaded as the query.text table
#' @param query.tables.overwrite logical indicating whether the query.users
#' and query.text tables should be overwritten with tables provided (does not
#' apply if tables are not provided).
#' @param driver DBI database driver object (experimental).  Default is RSQLite::SQLite().
#' Other drivers are in development.
#' @param ... Other named arguments passed to the DBI::dbConnect function.
#'
#' @seealso \code{\link{search_tweets}},\code{\link{user_timeline}}, \code{\link[DBI]{dbConnect}}
#' @return An S4 connection object that inherits from \code{DBI::DBIConnection} class.
#' @export
#' @examples
#'
#' \dontrun{
#' con <- twitter_database("tweet_collection.sqlite")
#' }
#'
twitter_database <- function(
  db.name,
  query.users.df = NULL,
  query.text.df = NULL,
  query.tables.overwrite=FALSE,
  driver = RSQLite::SQLite(),
  ...
){
  if(
    (
      class(driver) == 'function' &&
      any(grepl("SQLite",as.character(body(driver))))
    ) ||
    (
      grepl("SQLite",class(driver))
    ) ||
    (
      class(driver) == 'character' &&
      grepl("sqlite",tolower(driver))
    )
  ) {
    con <- DBI::dbConnect(driver,db.name,...)
    DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS user (id TEXT PRIMARY KEY, screen_name TEXT, name TEXT, location TEXT, description TEXT, url TEXT, protected TINYINT(1), followers_count INT, friends_count INT, statuses_count INT, created_at DATETIME, favourites_count INT, geo_enabled TINYINT(1), verified TINYINT(1), lang TEXT, status_id TEXT, profile_image_url TEXT, profile_banner_url TEXT, profile_image_hash TEXT, profile_banner_hash TEXT, profile_image_b64 TEXT, profile_banner_b64 TEXT);")
    DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS status (id TEXT PRIMARY KEY, created_at DATETIME, user_id TEXT, screen_name TEXT, text TEXT, in_reply_to_status_id TEXT, in_reply_to_user_id TEXT, retweet_count INT, favorite_count INT, lang TEXT, geo_lat FLOAT, geo_long FLOAT, source TEXT, retweet TINYINT(1), retweet_status_id TEXT, sentiment_score INT, nrc_sentiment_anger INT, nrc_sentiment_anticipation INT, nrc_sentiment_disgust INT, nrc_sentiment_fear INT, nrc_sentiment_joy INT, nrc_sentiment_sadness INT, nrc_sentiment_surprise INT, nrc_sentiment_trust INT, nrc_sentiment_negative INT, nrc_sentiment_positive INT, sentiment_collected TINYINT(1));")
    DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS user_mention (status_id TEXT, user_mention_id TEXT, user_mention_screen_name TEXT, user_mention_name TEXT, PRIMARY KEY (status_id,user_mention_id), FOREIGN KEY (status_id) REFERENCES status(id));")
    DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS hashtag (status_id TEXT, hashtag_text TEXT, PRIMARY KEY (status_id,hashtag_text), FOREIGN KEY (status_id) REFERENCES status(id));")
    DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS url (status_id TEXT, url TEXT, extended_url TEXT, PRIMARY KEY (status_id,url), FOREIGN KEY (status_id) REFERENCES status(id));")
    DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS media (status_id TEXT, media_id TEXT, expanded_url TEXT, media_url TEXT, media_type TEXT, source_user_id TEXT, source_status_id TEXT, img_hash TEXT, img_b64 TEXT, PRIMARY KEY (status_id,media_id), FOREIGN KEY (status_id) REFERENCES status(id));")
    DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS followers (follower_id TEXT, friend_id TEXT, UNIQUE(follower_id,friend_id));")
    # DBI::dbCommit(con)
  } else {
    con <- DBI::dbConnect(driver,...)
    tabs <- DBI::dbGetQuery(con,"SHOW DATABASES;")
    if(db.name %in% as.character(tabs[,1])){
      DBI::dbExecute(con, sprintf("USE %s",db.name))
    } else {
      DBI::dbExecute(con, sprintf("CREATE DATABASE %s;",db.name))
      DBI::dbExecute(con, sprintf("USE %s",db.name))
    }
    DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS user (id VARCHAR(40) PRIMARY KEY, screen_name VARCHAR(40), name VARCHAR(60), location TINYTEXT, description TINYTEXT, url TINYTEXT, protected TINYINT(1), followers_count INT, friends_count INT, statuses_count INT, created_at DATETIME, favourites_count INT, geo_enabled TINYINT, verified TINYINT, lang VARCHAR(5), status_id VARCHAR(40), profile_image_url TINYTEXT, profile_banner_url TINYTEXT, profile_image_hash TINYTEXT, profile_banner_hash TINYTEXT, profile_image_b64 MEDIUMTEXT, profile_banner_b64 MEDIUMTEXT);")
    DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS status (id VARCHAR(40) PRIMARY KEY, created_at DATETIME, user_id VARCHAR(40), screen_name VARCHAR(40), text TEXT, in_reply_to_status_id VARCHAR(40), in_reply_to_user_id VARCHAR(40), retweet_count INT, favorite_count INT, lang VARCHAR(5), geo_lat FLOAT, geo_long FLOAT, source TINYTEXT, retweet TINYINT, retweet_status_id VARCHAR(40), sentiment_score INT, nrc_sentiment_anger INT, nrc_sentiment_anticipation INT, nrc_sentiment_disgust INT, nrc_sentiment_fear INT, nrc_sentiment_joy INT, nrc_sentiment_sadness INT, nrc_sentiment_surprise INT, nrc_sentiment_trust INT, nrc_sentiment_negative INT, nrc_sentiment_positive INT, sentiment_collected TINYINT);")
    DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS user_mention (status_id VARCHAR(40), user_mention_id VARCHAR(40), user_mention_screen_name VARCHAR(40), user_mention_name VARCHAR(60), PRIMARY KEY (status_id,user_mention_id), FOREIGN KEY (status_id) REFERENCES status(id));")
    DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS hashtag (status_id VARCHAR(40), hashtag_text VARCHAR(40), PRIMARY KEY (status_id,hashtag_text), FOREIGN KEY (status_id) REFERENCES status(id));")
    DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS url (status_id VARCHAR(40), url varchar(100), extended_url TINYTEXT, PRIMARY KEY (status_id,url), FOREIGN KEY (status_id) REFERENCES status(id));")
    DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS media (status_id VARCHAR(40), media_id VARCHAR(40), expanded_url TINYTEXT, media_url TINYTEXT, media_type TINYTEXT, source_user_id VARCHAR(40), source_status_id VARCHAR(40), img_hash TINYTEXT, img_b64 MEDIUMTEXT, PRIMARY KEY (status_id,media_id), FOREIGN KEY (status_id) REFERENCES status(id));")
    DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS followers (follower_id VARCHAR(40), friend_id VARCHAR(40), UNIQUE(follower_id,friend_id));")
    # DBI::dbCommit(con)
  }
  if(!(is.null(query.users.df))){
    upload_query_users(con,query.users.df,query.tables.overwrite)
  }
  if(!(is.null(query.text.df))){
    upload_query_text(con,query.text.df,query.tables.overwrite)
  }
  return(con)
}

#' Upload User Query Table
#'
#' Upload a table of users to a Twitter database connection
#'
#' The \code{query.users.df} must be a data frame with either a \code{screen_name} column
#' or a \code{user_id} column containing Twitter screen names or user_ids for collection.
#' Additional user category columns are optional.  A unique 'id' column will be added if
#' it is not included.  Additional mandatory columns are \code{friends_collected}, 
#' \code{followers_collected}, and {since_id}.  These will all be automatically added and
#' populated if they are not included.
#' See \code{\link{twitter_database}}.  
#'
#' @param con a Twitter database connection.  See \code{twitter_database}.
#' @param query.users.df data frame.  See details and \code{\link{twitter_database}} documentation.
#' @param overwrite logical indicating whether to overwrite an existing \code{query_user} table in
#' the \code{conn} data connection.
#'
#' @return \code{NULL} (Invisible).
#'
#' @seealso \code{\link{twitter_database}}, \code{\link{update_users}}, \code{\link{update_user_timelines}}
#' @export
#' @examples
#' \dontrun{
#' users <- data.frame(
#'   screen_name=c(
#'     "nytimes",
#'     "latimes",
#'     "bostonglobe",
#'     "realDonaldTrump"
#'   ),
#'   category = c(
#'     "media",
#'     "media",
#'     "media",
#'     "politician"
#'   )
#' )
#' 
#' conn <- twitter_database("tweetanalysis.sqlite")
#' upload_query_users(conn,users)
#' }
upload_query_users <- function(con,query.users.df,overwrite = FALSE){
  if(grepl("SQLite",class(con))){
    count <- DBI::dbGetQuery(con,"SELECT COUNT(1) FROM sqlite_master WHERE type='table' AND name = 'query_users';")
    count <- count[1,1]
  } else {
    result <- DBI::dbGetQuery(con,"SHOW TABLES;")
    if('query_users' %in% as.character(result[,1])){
      count <- 1
    } else {
      count <- 0
    }
  } ##############
  if(count==1 && !overwrite){
    stop("Table query_users exists!  Use overwrite = TRUE to overwrite")
  }
  if(!('id' %in% names(query.users.df))){
    query.users.df <- cbind(data.frame(id=1:nrow(query.users.df)),query.users.df)
  }
  if((nrow(query.users.df) != length(unique(query.users.df$id))) || !(class(query.users.df$id) %in% c('integer','numeric'))){
    query.users.df$id <- 1:nrow(query.users.df)
  }
  names(query.users.df) <- gsub(".","_",names(query.users.df),fixed = TRUE)
  if(!('user_id' %in% names(query.users.df)) && !('screen_name' %in% names(query.users.df))){
    stop("query.users.df dataframe does not contain a 'user_id' or 'screen_name' field.")
  } 
  if(grepl("SQLite",class(con))){
    RSQLite::dbWriteTable(con,"query_users",query.users.df,overwrite=overwrite)
    d <- DBI::dbGetQuery(con,"PRAGMA table_info(query_users);")
    n <- d$name
    if(!('since_id' %in% n)){
      DBI::dbExecute(con,"ALTER TABLE query_users ADD COLUMN since_id TEXT;")
      DBI::dbExecute(con,"UPDATE query_users SET since_id=NULL;")
    } else {
      w <- which(d$name == "since_id")
      tp <- d$type[w]
      if(tolower(tp) != "text"){
        DBI::dbExecute(con,"ALTER TABLE query_users MODIFY since_id TEXT;")
      }
    }
  } else {
    RMySQL::dbWriteTable(con,"query_users",query.users.df,overwrite=overwrite)
    d <- DBI::dbGetQuery(con,"EXPLAIN query_users;")
    n <- as.character(d[,1])
    if(!('since_id' %in% n)){
      DBI::dbExecute(con,"ALTER TABLE query_users ADD COLUMN since_id VARCHAR(40);")
      DBI::dbExecute(con,"UPDATE query_users SET since_id=NULL;")
    } else {
      w <- which(d$Field == "since_id")
      tp <- d$Type[w]
      if(!grepl("varchar",tolower(tp))){
        DBI::dbExecute(con,"ALTER TABLE query_users MODIFY since_id VARCHAR(40);")
      }
    }
  }
  DBI::dbExecute(con,"CREATE UNIQUE INDEX row_id_unique ON query_users(id);")
  if(!('friends_collected' %in% n)){
    DBI::dbExecute(con,"ALTER TABLE query_users ADD COLUMN friends_collected TINYINT(1);")
    DBI::dbExecute(con,"UPDATE query_users SET friends_collected = 0;")
  }
  if(!('followers_collected' %in% n)){
    DBI::dbExecute(con,"ALTER TABLE query_users ADD COLUMN followers_collected TINYINT(1);")
    DBI::dbExecute(con,"UPDATE query_users SET followers_collected = 0;")
  }
  # DBI::dbCommit(con)
}


#' Upload Search Text Query Table
#'
#' Upload a table of search text to a Twitter database connection
#'
#' The \code{query.text.df} must be a data frame with a \code{query_text} column
#' containing character strings to execute in search queries.
#' Additional query category columns are optional.  A unique 'id' column will be added if
#' it is not included, as well as an additional mandatory {since_id} column.
#' See \code{\link{twitter_database}}.  
#'
#' @param con a Twitter database connection.  See \code{twitter_database}.
#' @param query.text.df data frame.  See details and \code{\link{twitter_database}} documentation.
#' @param overwrite logical indicating whether to overwrite an existing \code{query_text} table in
#' the \code{conn} data connection.
#'
#' @return \code{NULL} (Invisible).
#'
#' @seealso \code{\link{twitter_database}}, \code{\link{update_search}}
#' @export
#' @examples
#' \dontrun{
#' users <- data.frame(
#'   screen_name=c(
#'     "nytimes",
#'     "latimes",
#'     "bostonglobe",
#'     "realDonaldTrump"
#'   ),
#'   category = c(
#'     "media",
#'     "media",
#'     "media",
#'     "politician"
#'   )
#' )
#' conn <- twitter_database("tweetanalysis.sqlite")
#' upload_query_users(conn,users)
#' }
upload_query_text <- function(con,query.text.df,overwrite = FALSE){
  if(grepl("SQLite",class(con))){
    count <- DBI::dbGetQuery(con,"SELECT COUNT(1) FROM sqlite_master WHERE type='table' AND name = 'query_text';")
    count <- count[1,1]
  } else {
    result <- DBI::dbGetQuery(con,"SHOW TABLES;")
    if('query_text' %in% as.character(result[,1])){
      count <- 1
    } else {
      count <- 0
    }
  } ##############
  if(count==1 && !overwrite){
    stop("Table query_text exists!  Use overwrite = TRUE to overwrite")
  }
  if(!('id' %in% names(query.text.df))){
    query.text.df <- cbind(data.frame(id=1:nrow(query.text.df)),query.text.df)
  }
  if(nrow(query.text.df) != length(unique(query.text.df$id)) || !(class(query.text.df$id) %in% c("numeric","integer"))){
    query.text.df$id <- 1:nrow(query.text.df)
  }
  if(!('query_text' %in% names(query.text.df))){
    stop("query.text.df dataframe does not contain a 'query_text' field.")
  }
  if(grepl("SQLite",class(con))){
    RSQLite::dbWriteTable(con,"query_text",query.text.df,overwrite=overwrite)
    DBI::dbExecute(con,"CREATE TABLE IF NOT EXISTS search_status (query_id INT,status_id TEXT, UNIQUE(query_id,status_id));")
    DBI::dbExecute(con,"CREATE UNIQUE INDEX IF NOT EXISTS row_id_unique ON query_text(id);")
    d <- DBI::dbGetQuery(con,"PRAGMA table_info(query_text);")
    n <- d$name
    if(!('since_id' %in% n)){
      DBI::dbExecute(con,"ALTER TABLE query_text ADD COLUMN since_id TEXT;")
      DBI::dbExecute(con,"UPDATE query_text SET since_id=NULL;")
    } else {
      w <- which(d$name == "since_id")
      tp <- d$type[w]
      if(tolower(tp) != "text"){
        DBI::dbExecute(con,"ALTER TABLE query_users MODIFY since_id TEXT;")
      }
    }
  } else {
    RMySQL::dbWriteTable(con,"query_text",query.text.df,overwrite=overwrite)
    DBI::dbExecute(con,"CREATE TABLE IF NOT EXISTS search_status (query_id INT,status_id VARCHAR(40), UNIQUE(query_id,status_id));")
    DBI::dbExecute(con,"CREATE UNIQUE INDEX IF NOT EXISTS row_id_unique ON query_text(id);")
    d <- DBI::dbGetQuery(con,"EXPLAIN query_text;")
    n <- as.character(d[,1])
    if(!('since_id' %in% n)){
      DBI::dbExecute(con,"ALTER TABLE query_text ADD COLUMN since_id VARCHAR(40);")
      DBI::dbExecute(con,"UPDATE query_text SET since_id=NULL;")
    } else {
      w <- which(d$Field == "since_id")
      tp <- d$Type[w]
      if(!grepl("varchar",tolower(tp))){
        DBI::dbExecute(con,"ALTER TABLE query_users MODIFY since_id VARCHAR(40);")
      }
    }
  } ##############
  # DBI::dbCommit(con)
}


#' Insert Twitter User Objects
#'
#' Insert Twitter user objects into Twitter database connection
#'
#' This function does *not* add users to the \code{query_users} table.  Rather, it inserts
#' the Twitter user objects in the the \code{user} table in the data connection.  These 
#' users will appear in analyses of the data, but will not be automatically updated or 
#' included in queries of only.  To add users to the query table, see \code{\link{update_users}}.
#' This method is called by recursive methods that return Twitter user objects, if a
#' data connection is supplied.
#'
#' @param conn a Twitter database connection.  See \code{twitter_database}.
#' @param user.list list of Twitter user objects,
#' @param authentication.vector character vector containing authentication tokens and secrets.
#' See \code{\link{authorize_app}} and \code{\link{authorize_IT}}.
#' @param calc.Rsentiment logical.  If \code{TRUE}, the status text sentiment will be calculated using
#' the \code{RSentiment} library.
#' @param calc.syu logical.  If \code{TRUE}, the status text sentiment will be calculated using
#' the \code{syuzhet} library.
#' @param status.media.hash logical.  If \code{TRUE}, status image average hash values will be
#' calculated and inserted into the \code{media} data table.
#' @param status.media.64bit logical.  If \code{TRUE}, status images will be 64-bit encoded and inserted
#' into the \code{media} table.  Note: this will significantly increase processing time and sqlite
#' database file size.
#' @param profile.img.hash logical.  If \code{TRUE}, the function will attempt to calculate profile 
#' and profile banner image average hash values and insert them into the \code{user} data table.
#' @param profile.img.64bit logical.  If \code{TRUE}, the function will attempt to compute profile 
#' and profile banner image 64-bit encodings and insert them into the \code{user} data table. Note: this
#' will significantly increase processing time and sqlite database file size.
#' @param users.per.insert integer.  The maximum number of users per insert statement.
#' @param insert.statuses logical.  If \code{TRUE}, status objects attached to the user objects
#' will be inserted into the \code{status} table.
#'
#' @return \code{NULL} (Invisible).
#'
#' @seealso \code{\link{twitter_database}}, \code{\link{user_lookup_recursive}},
#' \code{\link{user_search_recursive}}, \code{\link{update_users}}
#' @export
#' @examples
#' 
#' \dontrun{
#' auth.vector <- authorize_IT()
#' 
#' user.objs <- user_lookup(c("nytimes","bostonglobe"))
#' 
#' conn <- twitter_database("tweetanalysis.sqlite")
#' 
#' insert_users(conn,user.objs)
#' }
insert_users <- function(
  conn,
  user.list,
  authentication.vector,
  calc.Rsentiment = FALSE,
  calc.syu = FALSE,
  status.media.hash=FALSE,
  status.media.64bit = FALSE,
  profile.img.hash = FALSE,
  profile.img.64bit = FALSE,
  users.per.insert=50,
  insert.statuses = FALSE
){
  if(missing(authentication.vector)){
    if(!exists('auth.vector')){
      # cat(ls())
      stop("No authentication tokens found!")
    } else {
      authentication.vector <- auth.vector
    }
  }
  if(profile.img.64bit){
    users.per.insert=1
  }
  if(grepl("SQLite",class(con))){
    ins.ig <- "INSERT OR IGNORE "
  } else {
    ins.ig <- "INSERT IGNORE "
  } ##############
  if('screen_name' %in% names(user.list)){
    u <- user.list
    if(('status' %in% names(u)) && insert.statuses){
      u$status[['user']] <- u
      insert_statuses(
        conn,
        u$status,
        calc.Rsentiment,
        calc.syu,
        status.media.hash,
        status.media.64bit,
        insert.users=FALSE
      )
    }
    query.values <- user_values(u,profile.img.hash,profile.img.64bit)
    query <- paste(
      ins.ig,
      "INTO user VALUES ",
      query.values,
      ";",
      sep=""
    )
    DBI::dbExecute(conn,query)
    # DBI::dbCommit(conn)
  } else {
    w <- which(sapply(user.list,function(x) return("status" %in% names(x))))
    if((length(w) > 0) && insert.statuses){
      status.ids <- unique(sapply(user.list[w],function(x) return(x$status$id_str)))
      if(length(status.ids)>0){
        search_tweets_recursive(
          status.ids,
          conn,
          authentication.vector,
          insert.users = FALSE
        )
      }
    }
    index <- 0
    while(index < length(user.list)){
      new.index <- min(c(length(user.list),index + users.per.insert))
      u <- user.list[(index+1):new.index]
      values <- paste(
        sapply(
          u,
          function(x) return(user_values(x,profile.img.hash,profile.img.64bit))
        ),
        collapse=","
      )
      query <- paste(
        ins.ig,
        "INTO user VALUES ",
        values,
        ";",
        sep=""
      )
      DBI::dbExecute(conn,query)
      # DBI::dbCommit(conn)
      index <- new.index
    }
  }
}

#' Insert Twitter Status Objects
#'
#' Insert Twitter Status (Tweet) objects into Twitter database connection
#'
#' This function inserts
#' Twitter status objects in the the \code{status} table in the data connection.  
#' It also inserts applicable records into the \code{media}, \code{hashtag},
#' \code{user_mention}, and \code{url} tables.  This method is called by recursive
#' functions that obtain status objects through the Twitter API, if a data
#' connection is supplied.
#'
#' @param conn a Twitter database connection.  See \code{twitter_database}.
#' @param status.list list of Twitter status objects,
#' @param calc.Rsentiment logical.  If \code{TRUE}, the status text sentiment will be calculated using
#' the \code{RSentiment} library.
#' @param calc.syu logical.  If \code{TRUE}, the status text sentiment will be calculated using
#' the \code{syuzhet} library.
#' @param status.media.hash logical.  If \code{TRUE}, status image average hash values will be
#' calculated and inserted into the \code{media} data table.
#' @param status.media.64bit logical.  If \code{TRUE}, status images will be 64-bit encoded and inserted
#' into the \code{media} table.  Note: this will significantly increase processing time and sqlite
#' database file size.
#' @param profile.img.hash logical.  If \code{TRUE}, the function will attempt to calculate profile 
#' and profile banner image average hash values and insert them into the \code{user} data table.
#' @param profile.img.64bit logical.  If \code{TRUE}, the function will attempt to compute profile 
#' and profile banner image 64-bit encodings and insert them into the \code{user} data table. Note: this
#' will significantly increase processing time and sqlite database file size.
#' @param statuses.per.insert integer.  The maximum number of statuses per insert statement.
#' @param insert.users logical.  If \code{TRUE}, user objects attached to the input status objects
#' will be inserted into the \code{user} table.
#'
#' @return \code{NULL} (Invisible).
#'
#' @seealso \code{\link{twitter_database}}, \code{\link{user_timeline_recursive}},
#' \code{\link{status_lookup_recursive}}, \code{\link{search_tweets_recursive}},
#' \code{\link{update_user_timelines}}, \code{\link{update_search}}
#' @export
#' @examples
#' 
#' \dontrun{
#' auth.vector <- authorize_IT()
#' 
#' statuses <- user_timeline_recursive("nytimes")
#' 
#' conn <- twitter_database("tweetanalysis.sqlite")
#' 
#' insert_statuses(conn,statuses)
#' }
insert_statuses <- function(
  conn,
  status.list,
  calc.Rsentiment = FALSE,
  calc.syu = FALSE,
  status.media.hash=FALSE,
  status.media.64bit = FALSE,
  profile.img.hash = FALSE,
  profile.img.64bit = FALSE,
  statuses.per.insert = 50,
  insert.users = TRUE
){
  if(grepl("SQLite",class(con))){
    ins.ig <- "INSERT OR IGNORE "
  } else {
    ins.ig <- "INSERT IGNORE "
  } ##############
  if('id_str' %in% names(status.list)){
    if(('retweeted_status' %in% names(status.list)) && (!is.null(status.list$retweeted_status$id_str))){
      insert_statuses(
        conn,
        status.list$retweeted_status,
        calc.Rsentiment,
        calc.syu,
        status.media.hash,
        status.media.64bit,
        insert.users = TRUE
      )
    }
    if(('user' %in% names(status.list)) && (!is.null(status.list$user$id_str)) && insert.users){
      insert_users(conn,
        status.list$user,
        calc.Rsentiment,
        calc.syu,
        profile.img.hash,
        profile.img.64bit,
        insert.statuses=FALSE
      )
    }
    s <- status.list
    query.values <- status_values(s,calc.Rsentiment,calc.syu)
    query <- paste(
      ins.ig,
      "INTO status VALUES ",
      query.values,
      ";",
      sep=""
    )
    DBI::dbExecute(conn,query)
    # DBI::dbCommit(conn)
    query.values <- usermention_values(s)
    if((!is.null(query.values)) && (nchar(query.values) > 2)){
      query <- paste(
        ins.ig,
        "INTO user_mention VALUES ",
        query.values,
        ";",
        sep=""
      )
      DBI::dbExecute(conn,query)
      # DBI::dbCommit(conn)
    }
    query.values <- hashtag_values(s)
    if((!is.null(query.values)) && (nchar(query.values) > 2)){
      query <- paste(
        ins.ig,
        "INTO hashtag VALUES ",
        query.values,
        ";",
        sep=""
      )
      DBI::dbExecute(conn,query)
      # DBI::dbCommit(conn)
    }
    query.values <- url_values(s)
    if((!is.null(query.values)) && (nchar(query.values) > 2)){
      query <- paste(
        ins.ig,
        "INTO url VALUES ",
        query.values,
        ";",
        sep=""
      )
      DBI::dbExecute(conn,query)
      # DBI::dbCommit(conn)
    }
    query.values <- media_values(s,status.media.hash,status.media.64bit)
    if((!is.null(query.values)) && (nchar(query.values) > 2)){
      query <- paste(
        ins.ig,
        "INTO media VALUES ",
        query.values,
        ";",
        sep=""
      )
      DBI::dbExecute(conn,query)
      # DBI::dbCommit(conn)
    }
  } else {
    w <- which(sapply(status.list,function(x) return(("retweeted_status" %in% names(x)) && ("id_str" %in% names(x$retweeted_status)))))
    if(length(w) > 0){
      retweeted.statuses <- lapply(status.list[w],function(x) return(x$retweeted_status))
      unique.retweeted.statuses <- get_unique(retweeted.statuses)
      insert_statuses(
        conn,
        unique.retweeted.statuses,
        calc.Rsentiment,
        calc.syu,
        status.media.hash,
        status.media.64bit,
        insert.users = TRUE
      )
    }
    if(insert.users){
      w <- which(sapply(status.list,function(x) return(("user" %in% names(x)) && ("id_str" %in% names(x$user)))))
      if(length(w) > 0){
        users <- lapply(status.list[w],function(x) return(x$user))
        unique.users <- get_unique(users)
        insert_users(
          conn,
          unique.users,
          calc.Rsentiment,
          calc.syu,
          profile.img.hash,
          profile.img.64bit,
          insert.statuses = FALSE
        )
      }
    }
    index <- 0
    while(index < length(status.list)){
      new.index <- min(c(length(status.list),index + statuses.per.insert))
      s <- status.list[(index+1):new.index]
      values <- paste(
        sapply(
          s,
          function(x) return(status_values(x,calc.Rsentiment,calc.syu))
        ),
        collapse=","
      )
      query <- paste(
        ins.ig,
        "INTO status VALUES ",
        values,
        ";",
        sep=""
      )
      DBI::dbExecute(conn,query)
      # DBI::dbCommit(conn)
      values <- paste(
        unlist(
          sapply(
            s,
            function(x) return(usermention_values(x))
          )
        ),
        collapse=","
      )
      if((!is.null(values)) && (nchar(values) > 2)){
        query <- paste(
          ins.ig,
          "INTO user_mention VALUES ",
          values,
          ";",
          sep=""
        )
          DBI::dbExecute(conn,query)
          # DBI::dbCommit(conn)
      }
      values <- paste(
        unlist(
          sapply(
            s,
            function(x) return(hashtag_values(x))
          )
        ),
        collapse=","
      )
      if((!is.null(values)) && (nchar(values) > 2)){
      query <- paste(
        ins.ig,
        "INTO hashtag VALUES ",
        values,
        ";",
        sep=""
      )
      DBI::dbExecute(conn,query)
      # DBI::dbCommit(conn)
      }
      values <- paste(
        unlist(
          sapply(
            s,
            function(x) return(url_values(x))
          )
        ),
        collapse=","
      )
      if((!is.null(values)) && (nchar(values) > 2)){
      query <- paste(
        ins.ig,
        "INTO url VALUES ",
        values,
        ";",
        sep=""
      )
      DBI::dbExecute(conn,query)
      # DBI::dbCommit(conn)
     }
      if(status.media.64bit){
        for(j in 1:length(s)){
          v <- s[[j]]
          query.values <- media_values(v,status.media.hash,status.media.64bit)
          if((!is.null(query.values)) && (nchar(query.values) > 2)){
            query <- paste(
              ins.ig,
              "INTO media VALUES ",
              query.values,
              ";",
              sep=""
            )
            DBI::dbExecute(conn,query)
            # DBI::dbCommit(conn)
          }
        }
      } else {
        values <- paste(
          unlist(
            sapply(
              s,
              function(x) return(media_values(x,status.media.hash,status.media.64bit))
            )
          ),
          collapse=","
        )
        if((!is.null(values)) && (nchar(values) > 2)){
          query <- paste(
            ins.ig,
            "INTO media VALUES ",
            values,
            ";",
            sep=""
          )
            DBI::dbExecute(conn,query)
            # DBI::dbCommit(conn)
        }
      }
      index <- new.index
    }
  }
}

#' Insert Follower Relationships
#'
#' Insert Twitter Follower relationships into Twitter database connection
#'
#' This function inserts 'follower' relationship information into the
#' \code{followers} table in the data connection.  It takes a single
#' friend user_id and multiple follower user_ids.  For each follower user_id,
#' a follower-friend relationship is entered into the table.  This method
#' is called by the \code{\link{followers_ids_recursive}} and 
#' \code{\link{followers_list_recursive}} functions.
#'
#' @param conn a Twitter database connection.  See \code{twitter_database}.
#' @param friend.id character user_id of Twitter friend.
#' @param follower.ids character user_ids of Twitter followers.
#' @param ids.per.insert integer maximum number of relationships per insert query.
#'
#' @return \code{NULL} (Invisible).
#'
#' @seealso \code{\link{twitter_database}}, \code{\link{followers_list_recursive}}
#' \code{\link{followers_ids_recursive}}, \code{\link{get_all_followers}}
#' @export
#' @examples
#' 
#' \dontrun{
#' auth.vector <- authorize_IT()
#' 
#' nyt <- user_show("nytimes")
#' nyt_followers <- followers_ids(user_id=nyt$id_str)
#' 
#' conn <- twitter_database("tweetanalysis.sqlite")
#' 
#' insert_followers(
#'   conn,
#'   nyt$id_str,
#'   sapply(
#'     nyt_followers,
#'     function(x) return(x$id_str)
#'   )
#' )
#' }
insert_followers <- function(
  conn,
  friend.id,
  follower.ids,
  ids.per.insert = 1000
){
  if(grepl("SQLite",class(con))){
    ins.ig <- "INSERT OR IGNORE "
  } else {
    ins.ig <- "INSERT IGNORE "
  } ##############
  if(is.list(follower.ids)){
    follower.ids <- unlist(follower.ids)
  }
  index <- 0
  n.followers <- length(follower.ids)
  while(index < n.followers){
    new.index <- min(c(index+ids.per.insert,n.followers))
    query <- paste(
      ins.ig,
      "INTO followers VALUES ",
      paste(
        "(",
        sapply(
          follower.ids,
          function(x) return(write_text(x))
        ),
        ",",
        write_text(friend.id),
        ")",
        sep="",
        collapse=","
      ),
      ";",
      sep=""
    )
    DBI::dbExecute(conn,query)
    # DBI::dbCommit(conn)
    index <- new.index
  }
}


#' Insert Friend Relationships
#'
#' Insert Twitter Friend relationships into Twitter database connection
#'
#' This function inserts 'friend' relationship information into the
#' \code{followers} table in the data connection.  It takes a single
#' follower user_id and multiple friend user_ids.  For each friend user_id,
#' a follower-friend relationship is entered into the table.  This method
#' is called by the \code{\link{friends_ids_recursive}} and 
#' \code{\link{friends_list_recursive}} functions.
#'
#' @param conn a Twitter database connection.  See \code{twitter_database}.
#' @param follower.id character user_id of Twitter follower.
#' @param friend.ids character user_ids of Twitter friends.
#' @param ids.per.insert integer maximum number of relationships per insert query.
#'
#' @return \code{NULL} (Invisible).
#'
#' @seealso \code{\link{twitter_database}}, \code{\link{friends_list_recursive}}
#' \code{\link{friends_ids_recursive}}, \code{\link{get_all_friends}}
#' @export
#' @examples
#' 
#' \dontrun{
#' auth.vector <- authorize_IT()
#' 
#' nyt <- user_show("nytimes")
#' nyt_friends <- friends_ids(user_id=nyt$id_str)
#' 
#' conn <- twitter_database("tweetanalysis.sqlite")
#' 
#' insert_friends(
#'   conn,
#'   nyt$id_str,
#'   sapply(
#'     nyt_friends,
#'     function(x) return(x$id_str)
#'   )
#' )
#' }
insert_friends <- function(
  conn,
  follower.id,
  friend.ids,
  ids.per.insert = 1000
){
  if(grepl("SQLite",class(con))){
    ins.ig <- "INSERT OR IGNORE "
  } else {
    ins.ig <- "INSERT IGNORE "
  } ##############
  if(is.list(friend.ids)){
    friend.ids <- unlist(friend.ids)
  }
  index <- 0
  n.friends <- length(friend.ids)
  while(index < n.friends){
    new.index <- min(c(index+ids.per.insert,n.friends))
    query <- paste(
      ins.ig,
      "INTO followers VALUES ",
      paste(
        "(",
        write_text(follower.id),
        ",",
        sapply(
          friend.ids,
          function(x) return(write_text(x))
        ),
        ")",
        sep="",
        collapse=","
      ),
      ";",
      sep=""
    )
    DBI::dbExecute(conn,query)
    # DBI::dbCommit(conn)
    index <- new.index
  }
}


write_int <- function(val){
  if(is.null(val)){
    output <- "NULL"
  } else {
    output <- as.character(val)
  }
  return(output)
}

write_text <- function(val){
  if(is.null(val)){
    output <- "NULL"
  } else {
    output <- as.character(val)
    output <- gsub("'","''",output)
    output <- gsub("\\","\\\\",output,fixed = TRUE)
    output <- paste("'",output,"'",sep="")
  }
  return(output)
}


write_geo_long <- function(input.geo){
  if(is.null(input.geo)){
    return("NULL")
  } else {
    return(sprintf("%1.6f",input.geo$coordinates[1]))
  }
}

write_geo_lat <- function(input.geo){
  if(is.null(input.geo)){
    return("NULL")
  } else {
    return(sprintf("%1.6f",input.geo$coordinates[2]))
  }
}


write_bool <- function(val){
  if(tolower(val)=="true"){
    return("1")
  } else if(tolower(val)=="false"){
    return("0")
  } else {
    return("NULL")
  }
}

write_retweet_bool <- function(input.status){
  if(is.null(input.status$retweeted_status)){
    return('0')
  } else {
    return('1')
  }
}


write_datetime <- function(val){
  px <- as.POSIXct(val,format="%a %b %d %H:%M:%S +0000 %Y",tz="GMT")
  output <- format(px,format="%Y-%m-%d %H:%M:%S")
  output <- paste("'",px,"'",sep="")
  return(output)
}



write_avg_hash <- function(media.url,compute.avg.hash = TRUE,hash.size=16){
  if(compute.avg.hash){
    u.ext <- strsplit(media.url,".",fixed=TRUE)[[1]]
    u.ext <- tolower(u.ext[length(u.ext)])
    if(u.ext %in% c("png","jpg","jpeg","tiff","tif")){
      success <- -1
      try({
        z.rezponse <- httr::GET(media.url)
        z.raw <- httr::content(z.rezponse,type='raw')
        if(u.ext %in% c("jpg","jpeg")){
          im <- jpeg::readJPEG(z.raw)
        } else if(u.ext == "png"){
          im <- png::readPNG(z.raw)
        } else if(u.ext %in% c("tif","tiff")){
          im <- tiff::readTIFF(z.raw)
        }
        a <- OpenImageR::average_hash(OpenImageR::rgb_2gray(im),hash_size=hash.size)
        success <- 1
      })
      if(success > 0){
        return(paste(
          "'",
          a,
          "'",
          sep=""
        ))
      } else {
        return("NULL")
      }
    } else if(!grepl(".",media.url,fixed=TRUE)){
      z <- paste(tempfile(),"png",sep="")
      success <- -1
      try({
        success <- utils::download.file(media.url,z,mode="wb",quiet=TRUE)
        im <- OpenImageR::readImage(z)
        file.remove(z)
        a <- OpenImageR::average_hash(OpenImageR::rgb_2gray(im),hash_size=hash.size)
        success <- success + 1
      })
      if(success > 0){
        return(paste(
          "'",
          a,
          "'",
          sep=""
        ))
      } else {
        return("NULL")
      }
    } else {
      return("NULL")
    }
  } else {
    return("NULL")
  }
}

write_media_64 <- function(media.url,base64.encode = FALSE){
  if(base64.encode){
    b <- NULL
    try(
      b <- base64enc::base64encode(media.url)
    )
    if(is.null(b)){
      return("NULL")
    } else {
      return(paste(
        "'",
        b,
        "'",
        sep=""
      ))
    }
  } else {
    return("NULL")
  }
}


get_avg_hash <- function(media.entity,compute.avg.hash = TRUE,hash.size=16){
  if(media.entity$type=='photo'){
    return(write_avg_hash(media.entity$media_url,compute.avg.hash,hash.size))
  } else {
    return("NULL")
  }
}

get_media_64 <- function(media.entity,base64.encode = FALSE){
  if(media.entity$type=="photo"){
    return(write_media_64(media.entity$media_url,base64.encode))
  } else {
    return("NULL")
  }
}

get_user_avg_hash <- function(user.obj,compute.avg.hash = TRUE,hash.size=16){
  if(!user.obj$default_profile_image){
    return(write_avg_hash(user.obj$profile_image_url,compute.avg.hash,hash.size))
  } else {
    return("NULL")
  }
}

get_user_media_64 <- function(user.obj,base64.encode = FALSE){
  if(!user.obj$default_profile_image){
    return(write_media_64(user.obj$profile_image_url,base64.encode))
  } else {
    return("NULL")
  }
}

get_banner_avg_hash <- function(user.obj,compute.avg.hash = TRUE,hash.size=16){
  if(!user.obj$default_profile){
    return(write_avg_hash(user.obj$profile_banner_url,compute.avg.hash,hash.size))
  } else {
    return("NULL")
  }
}

get_banner_media_64 <- function(user.obj,base64.encode = FALSE){
  if(!user.obj$default_profile){
    return(write_media_64(user.obj$profile_banner_url,base64.encode))
  } else {
    return("NULL")
  }
}


user_values <- function(user,avg.hash=TRUE,store.64encode = FALSE){
  if('status' %in% names(user)){
    status_id <- user$status$id_str
  } else {
    status_id <- NULL
  }
  values <- paste(
    write_text(user$id_str),
    write_text(user$screen_name),
    write_text(user$name),
    write_text(user$location),
    write_text(user$description),
    write_text(user$url),
    write_bool(user$protected),
    write_int(user$followers_count),
    write_int(user$friends_count),
    write_int(user$statuses_count),
    write_datetime(user$created_at),
    write_int(user$favourites_count),
    write_bool(user$geo_enabled),
    write_bool(user$verified),
    write_text(user$lang),
    write_text(status_id),
    write_text(user$profile_image_url),
    write_text(user$profile_banner_url),
    get_user_avg_hash(user,avg.hash),
    get_banner_avg_hash(user,avg.hash),
    get_user_media_64(user,store.64encode),
    get_banner_media_64(user,store.64encode),
    sep=","
  )
  return(
    paste(
      "(",
      values,
      ")",
      sep=""
    )
  )
}

status_values <- function(status,calc.Rsentiment,calc.syu){
  if('full_text' %in% names(status)){
    txt <- status$full_text
  } else if('text' %in% names(status)){
    txt <- status$text
  } else {
    txt <- NULL
    calc.Rsentiment <- FALSE
    calc.syu <- FALSE
    warning("No text found in status")
  }
  txt <- gsub("\u201C","\"",gsub("\u201D","\"",txt))
  sent.list <- calculate_sentiment(txt,lang=status$lang,calc.RSentiment=calc.Rsentiment,calc.syuzhet=calc.syu)
  if((calc.Rsentiment || calc.syu)){
    calc.sent <- 1
  } else {
    calc.sent <- 0
  }
  values <- paste(
    write_text(status$id_str),
    write_datetime(status$created_at),
    write_text(status$user$id_str),
    write_text(status$user$screen_name),
    write_text(txt),
    write_text(status$in_reply_to_status_id_str),
    write_text(status$in_reply_to_user_id_str),
    write_int(status$retweet_count),
    write_int(status$favorite_count),
    write_text(status$lang),
    write_geo_lat(status$geo),
    write_geo_long(status$geo),
    write_text(status$source),
    write_retweet_bool(status),
    write_text(status$retweeted_status$id_str),
    write_int(sent.list[["RSent"]]),
    write_int(sent.list[["syu"]]$anger),
    write_int(sent.list[["syu"]]$anticipation),
    write_int(sent.list[["syu"]]$disgust),
    write_int(sent.list[["syu"]]$fear),
    write_int(sent.list[["syu"]]$joy),
    write_int(sent.list[["syu"]]$sadness),
    write_int(sent.list[["syu"]]$surprise),
    write_int(sent.list[["syu"]]$trust),
    write_int(sent.list[["syu"]]$negative),
    write_int(sent.list[["syu"]]$positive),
    write_int(calc.sent),
    sep=","
  )
  return(
    paste(
      "(",
      values,
      ")",
      sep=""
    )
  )
}

usermention_values <- function(status){
  if(('user_mentions' %in% names(status$entities)) && (length(status$entities$user_mentions) > 0)){
    values <- paste(
      "(",
      rep(write_text(status$id_str),length(status$entities$user_mentions)),
      ",",
      sapply(
        status$entities$user_mentions,
        function(x) return(write_text(x$id_str))
      ),
      ",",
      sapply(
        status$entities$user_mentions,
        function(x) return(write_text(x$screen_name))
      ),
      ",",
      sapply(
        status$entities$user_mentions,
        function(x) return(write_text(x$name))
      ),
      ")",
      sep="",
      collapse=","
    )
  } else {
    values <- NULL
  }
  return(values)
}

hashtag_values <- function(status){
  if(('hashtags' %in% names(status$entities)) && (length(status$entities$hashtags) > 0)){
    values <- paste(
      "(",
      rep(write_text(status$id_str),length(status$entities$hashtags)),
      ",",
      sapply(
        status$entities$hashtags,
        function(x) return(write_text(x$text))
      ),
      ")",
      sep="",
      collapse=","
    )
  } else {
    values <- NULL
  }
  return(values)
}

url_values <- function(status){
  if(('urls' %in% names(status$entities)) && (length(status$entities$urls) > 0)){
    values <- paste(
      "(",
      rep(write_text(status$id_str),length(status$entities$urls)),
      ",",
      sapply(
        status$entities$urls,
        function(x) return(write_text(x$url))
      ),
      ",",
      sapply(
        status$entities$urls,
        function(x) return(write_text(x$expanded_url))
      ),
      ")",
      sep="",
      collapse=","
    )
  } else {
    values <- NULL
  }
  return(values)
}

media_values <- function(status,avg.hash=TRUE,store.64encode = FALSE){
  if(('extended_entities' %in% names(status)) && ('media' %in% names(status$extended_entities)) && (length(status$extended_entities$media)>0)){
    values <- paste(
      "(",
      rep(write_text(status$id_str),length(status$extended_entities$media)),
      ",",
      sapply(
        status$extended_entities$media,
        function(x) return(write_text(x$id_str))
      ),
      ",",
      sapply(
        status$extended_entities$media,
        function(x) return(write_text(x$expanded_url))
      ),
      ",",
      sapply(
        status$extended_entities$media,
        function(x) return(write_text(x$media_url))
      ),
      ",",
      sapply(
        status$extended_entities$media,
        function(x) return(write_text(x$type))
      ),
      ",",
      sapply(
        status$extended_entities$media,
        function(x) if(is.null(x$source_user_id_str)) return(write_text(status$user$id_str)) else return(write_text(x$source_user_id_str))
      ),
      ",",
      sapply(
        status$extended_entities$media,
        function(x) if(is.null(x$source_status_id_str)) return(write_text(status$id_str)) else return(write_text(x$source_status_id_str))
      ),
      ",",
      sapply(
        status$extended_entities$media,
        function(x) return(get_avg_hash(x,compute.avg.hash = avg.hash))
      ),
      ",",
      sapply(
        status$extended_entities$media,
        function(x) return(get_media_64(x,base64.encode = store.64encode))
      ),
      ")",
      sep="",
      collapse=","
    )
  } else {
    if(('media' %in% names(status$entities)) && (length(status$entities$media) > 0)){
      values <- paste(
        "(",
        rep(write_text(status$id_str),length(status$entities$media)),
        ",",
        sapply(
          status$entities$media,
          function(x) return(write_text(x$id_str))
        ),
        ",",
        sapply(
          status$entities$media,
          function(x) return(write_text(x$expanded_url))
        ),
        ",",
        sapply(
          status$entities$media,
          function(x) return(write_text(x$media_url))
        ),
        ",",
        sapply(
          status$entities$media,
          function(x) return(write_text(x$type))
        ),
        ",",
        sapply(
          status$entities$media,
          function(x) if(is.null(x$source_user_id_str)) return(write_text(status$user$id_str)) else return(write_text(x$source_user_id_str))
        ),
        ",",
        sapply(
          status$entities$media,
          function(x) if(is.null(x$source_status_id_str)) return(write_text(status$id_str)) else return(write_text(x$source_status_id_str))
        ),
        ",",
        sapply(
          status$entities$media,
          function(x) return(get_avg_hash(x,compute.avg.hash = avg.hash))
        ),
        ",",
        sapply(
          status$entities$media,
          function(x) return(get_media_64(x,base64.encode = store.64encode))
        ),
        ")",
        sep="",
        collapse=","
      )
    } else {
      values <- NULL
    }
  }
  return(values)
}

