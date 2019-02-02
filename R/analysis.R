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
#' @seealso \code{\link{twitter_database}}, \code{\link{authorize_app}}, \code{\link{open_status}}
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
  }
  utils::browseURL(
    paste(
      "http://twitter.com/",
      screen_name,
      sep=""
    )
  )
}

#' Open a Tweet
#'
#' View a Twitter status in a browser
#'
#' This function opens a browser window to the Tweet corresponding to the \code{status_id} provided.
#' Authentication tokens are not required.
#'
#' @param status_id character Twitter status ID. 
#' 
#' @return NULL (Invisible).
#'
#' @seealso \code{\link{open_user}}
#' @export
#' @examples
#'
#'
#' open_status("1009830068624019457")
#'
open_status <- function(
  status_id
){
  utils::browseURL(
    paste(
      "http://twitter.com/i/status/",
      as.character(status_id),
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
#' @param save.to.file logical indicating whether to save the image to a file.
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
#' @seealso \code{\link{twitter_database}}
#' @export
#' @examples
#' \dontrun{
#' auth.vector <- authorize_IT()
#'
#' con <- twitter_database("throwback.sqlite")
#' tbt <- search_tweets_recursive(
#'   "throwbackthursday",
#'   data.connection = con
#' )
#' 
#' query <- "SELECT *,COUNT(media_id) AS n FROM media GROUP_BY media_id ORDER BY n DESC LIMIT 1;"
#' media.df <- DBI::dbGetQuery(con,query)
#' status_media(
#'   media.df[1,],
#'   con,
#'   save.to.file = FALSE,
#'   file.name = NULL,
#'   return.image = NULL,
#'   update.media.b64 = TRUE,
#'   hash.size = 16
#' )
#' }
status_media <- function(
  media.table.row,
  conn,
  save.to.file = FALSE,
  file.name = NULL,
  return.image = FALSE,
  update.media.b64 = TRUE,
  hash.size=16,
  display.image = FALSE,
  showWarnings = TRUE
){
  media.url <- media.table.row$media_url
  img.b64 <- media.table.row$img_b64
  if(is.null(img.b64) || is.na(img.b64)){
    p <- NA
    try(
      p <- retrieve_web_image(media.url,display.image=display.image)
    )
    need.b64 <- TRUE
  } else {
    p <- reconstitute_image(img.b64,media.url,display.image = FALSE,showWarnings=showWarnings)
    if(!is.na(p) && save.to.file){
      if(is.null(file.name)){
        media.url.splt <- strsplit(media.url,"/",fixed=TRUE)[[1]]
        file.name <- media.url.splt[length(media.url.splt)]
      }
      OpenImageR::writeImage(p,file.name)
      p <- list(image=p,b64=img.b64)
      need.b64 <- FALSE
    } else {
      p <- NA
      try(
        p <- retrieve_web_image(media.url,display.image=display.image)
      )
      need.b64 <- TRUE
    }
  }
  if(is.null(p) || is.na(p)){
    return(NA)
  } else {
    if(update.media.b64 && need.b64){
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
  if(is.null(img.b64) || is.na(img.b64)){
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
    p <- reconstitute_image(img.b64,profile.image.url,...)
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



#' Top Hashtags
#'
#' Get top hashtags from a Twitter database
#'
#' Provide filter criteria in the \code{start.date}, \code{end.date}, and 
#' \code{where.criteria} parameters.  The results will include statuses created
#' at times greater than or equal to the \code{start.date}, but strictly less
#' than the \code{end.date}.  The \code{where.criteria} must be in SQLite syntax and
#' reference tables and columns in the database (see \code{\link{twitter_database}}).
#' Be sure to include table names, e.g., \code{user.location LIKE '\%fargo\%'}.
#' Returns the most frequent hashtags 
#' in the tweets meeting the filter criteria.
#'
#' @param con SQLite data connection to Twitter database (see \code{\link{twitter_database}}).
#' @param start.date character date (or datetime) of earliest statuses queried.
#' @param end.date character date.  Results will include statuses 
#' with creation times or dates strictly less than this parameter.
#' @param where.criteria character additional criteria to filter the status results, in 
#' SQLite syntax.  See details.
#' @param limit integer number of results to return.
#' @param excel.export.file character name of file to write results in Excel format.  If 
#' \code{NULL} no file is written.
#' @param excel.file.footnote character table footnote to include in Excel file.  if 
#' \code{"auto"} the date range will be written.
#' 
#' @return data frame containing most frequent hashtags.
#'
#' @seealso \code{\link{twitter_database}}, \code{\link{authorize_app}}
#' @export
#' @examples
#' \dontrun{
#' auth.vector <- authorize_IT()
#'
#' con <- twitter_database(
#'   "test.sqlite",
#'   query.users.df = data.frame(
#'     screen_name = c(
#'       "pb2pv",
#'       "zlisto"
#'     )
#'   ),
#'   query.text.df = data.frame(
#'     query_text = c(
#'       "#throwbackthursday",
#'       "#worstfirstdate"
#'     )
#'   )
#' )
#' 
#' update_user_timelines(con)
#' update_search(con)
#' 
#' top.hashtags <- top_hashtags(
#'   con,
#'   start.date = as.Date(Sys.time())-7,
#'   where.criteria = "user.screen_name = 'zlisto'"
#' )
#' 
#' 
#' DBI::dbDisonnect(con)
#' }
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
    "LOWER(hashtag.hashtag_text)",
    start.date = start.date,
    end.date = end.date,
    where.criteria = where.criteria,
    additional.columns = "hashtag.hashtag_text as HT",
    limit=limit
  )
  results <- DBI::dbGetQuery(con,query)
  if(!is.null(excel.export.file) && is.character(excel.export.file)){
    file.name <- create_excel_filename(excel.export.file)
    footnote <- create_footnote(excel.file.footnote,start.date,end.date)
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



#' Top User Mentions
#'
#' Get the most frequent user mentions from a Twitter database
#'
#' Provide filter criteria in the \code{start.date}, \code{end.date}, and 
#' \code{where.criteria} parameters.  The results will include statuses created
#' at times greater than or equal to the \code{start.date}, but strictly less
#' than the \code{end.date}.  The \code{where.criteria} must be in SQLite syntax and
#' reference tables and columns in the database (see \code{\link{twitter_database}}).
#' Be sure to include table names, e.g., \code{user.location LIKE '\%fargo\%'}.
#' Returns the most frequent hashtags 
#' in the tweets meeting the filter criteria.
#'
#' @param con SQLite data connection to Twitter database (see \code{\link{twitter_database}}).
#' @param start.date character date (or datetime) of earliest statuses queried.
#' @param end.date character date.  Results will include statuses 
#' with creation times or dates strictly less than this parameter.
#' @param where.criteria character additional criteria to filter the status results, in 
#' SQLite syntax.  See details.
#' @param limit integer number of results to return.
#' @param excel.export.file character name of file to write results in Excel format.  If 
#' \code{NULL} no file is written.
#' @param excel.file.footnote character table footnote to include in Excel file.  if 
#' \code{"auto"} the date range will be written.
#' 
#' @return data frame containing most frequent user mentions.
#'
#' @seealso \code{\link{twitter_database}}, \code{\link{authorize_app}}
#' @export
#' @examples
#'
#' \dontrun{
#' auth.vector <- authorize_IT()
#'
#' con <- twitter_database(
#'   "test.sqlite",
#'   query.users.df = data.frame(
#'     screen_name = c(
#'       "pb2pv",
#'       "zlisto"
#'     )
#'   ),
#'   query.text.df = data.frame(
#'     query_text = c(
#'       "#throwbackthursday",
#'       "#worstfirstdate"
#'     )
#'   )
#' )
#' 
#' update_user_timelines(con)
#' update_search(con)
#' 
#' top.hashtags <- top_usermentions(
#'   con,
#'   start.date = as.Date(Sys.time())-7,
#'   where.criteria = "user.screen_name = 'zlisto'"
#' )
#' 
#' 
#' DBI::dbDisonnect(con)
#' }
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
  results <- DBI::dbGetQuery(con,query)
  if(!is.null(excel.export.file) && is.character(excel.export.file)){
    file.name <- create_excel_filename(excel.export.file)
    footnote <- create_footnote(excel.file.footnote,start.date,end.date)
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


#' Top URLs
#'
#' Get most frequent URLs from a Twitter database
#'
#' Provide filter criteria in the \code{start.date}, \code{end.date}, and 
#' \code{where.criteria} parameters.  The results will include statuses created
#' at times greater than or equal to the \code{start.date}, but strictly less
#' than the \code{end.date}.  The \code{where.criteria} must be in SQLite syntax and
#' reference tables and columns in the database (see \code{\link{twitter_database}}).
#' Be sure to include table names, e.g., \code{user.location LIKE '\%fargo\%'}.
#' Returns the most frequent hashtags 
#' in the tweets meeting the filter criteria.
#'
#' @param con SQLite data connection to Twitter database (see \code{\link{twitter_database}}).
#' @param start.date character date (or datetime) of earliest statuses queried.
#' @param end.date character date.  Results will include statuses 
#' with creation times or dates strictly less than this parameter.
#' @param where.criteria character additional criteria to filter the status results, in 
#' SQLite syntax.  See details.
#' @param limit integer number of results to return.
#' @param excel.export.file character name of file to write results in Excel format.  If 
#' \code{NULL} no file is written.
#' @param excel.file.footnote character table footnote to include in Excel file.  if 
#' \code{"auto"} the date range will be written.
#' 
#' @return data frame containing most frequently posted URLs.
#'
#' @seealso \code{\link{twitter_database}}, \code{\link{authorize_app}}
#' @export
#' @examples
#'
#' \dontrun{
#' auth.vector <- authorize_IT()
#'
#' con <- twitter_database(
#'   "test.sqlite",
#'   query.users.df = data.frame(
#'     screen_name = c(
#'       "pb2pv",
#'       "zlisto"
#'     )
#'   ),
#'   query.text.df = data.frame(
#'     query_text = c(
#'       "#throwbackthursday",
#'       "#worstfirstdate"
#'     )
#'   )
#' )
#' 
#' update_user_timelines(con)
#' update_search(con)
#' 
#' top.urls <- top_urls(
#'   con,
#'   start.date = as.Date(Sys.time())-7,
#'   where.criteria = "user.screen_name = 'zlisto'"
#' )
#' 
#' 
#' DBI::dbDisonnect(con)
#'}
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
  results <- DBI::dbGetQuery(con,query)
  if(!is.null(excel.export.file) && is.character(excel.export.file)){
    file.name <- create_excel_filename(excel.export.file)
    footnote <- create_footnote(excel.file.footnote,start.date,end.date)
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


#' Top Images
#'
#' Get most frequently appearing images from a Twitter database
#'
#' Provide filter criteria in the \code{start.date}, \code{end.date}, and 
#' \code{where.criteria} parameters.  The results will include statuses created
#' at times greater than or equal to the \code{start.date}, but strictly less
#' than the \code{end.date}.  The \code{where.criteria} must be in SQLite syntax and
#' reference tables and columns in the database (see \code{\link{twitter_database}}).
#' Be sure to include table names, e.g., \code{user.location LIKE '\%fargo\%'}.
#' Returns the most frequent hashtags 
#' in the tweets meeting the filter criteria.
#'
#' @param con SQLite data connection to Twitter database (see \code{\link{twitter_database}}).
#' @param start.date character date (or datetime) of earliest statuses queried.
#' @param end.date character date.  Results will include statuses 
#' with creation times or dates strictly less than this parameter.
#' @param where.criteria character additional criteria to filter the status results, in 
#' SQLite syntax.  See details.
#' @param limit integer number of results to return.
#' @param excel.export.file character name of file to write results in Excel format.  If 
#' \code{NULL} no file is written.
#' @param excel.file.footnote character table footnote to include in Excel file.  if 
#' \code{"auto"} the date range will be written.
#' @param get.media logical indicating whether to download the image.  If the image is already
#' in the database (64-bit encoded), it will be reconstructed.
#' @param save.to.file logical indicating whether to save the image to a file.
#' @param media.file.prefix character prefix that will be prepended to the file name of image
#' when it is saved.
#' @param ... additional parameters passed to status_media.
#' 
#' @return data frame containing most frequent media.
#'
#' @seealso \code{\link{twitter_database}}, \code{\link{authorize_app}},
#' \code{\link{status_media}}
#' @export
#' @examples
#'
#' \dontrun{
#' auth.vector <- authorize_IT()
#'
#' con <- twitter_database(
#'   "test.sqlite",
#'   query.users.df = data.frame(
#'     screen_name = c(
#'       "pb2pv",
#'       "zlisto"
#'     )
#'   ),
#'   query.text.df = data.frame(
#'     query_text = c(
#'       "#throwbackthursday",
#'       "#worstfirstdate"
#'     )
#'   )
#' )
#' 
#' update_user_timelines(con)
#' update_search(con)
#' 
#' top.media <- top_media(
#'   con,
#'   start.date = as.Date(Sys.time())-7,
#'   where.criteria = "user.screen_name = 'zlisto'"
#' )
#' 
#' DBI::dbDisonnect(con)
#' }
top_media <- function(
  con,
  start.date=NULL,
  end.date=NULL,
  where.criteria=NULL,
  limit=10,
  excel.export.file = NULL,
  excel.file.footnote = "auto",
  get.media = TRUE,
  save.to.file=TRUE,
  media.file.prefix = NULL,
  ...
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
  results <- DBI::dbGetQuery(con,query)
  if(!is.null(excel.export.file) && is.character(excel.export.file)){
    file.name <- create_excel_filename(excel.export.file)
    footnote <- create_footnote(excel.file.footnote,start.date,end.date)
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
        save.to.file = save.to.file,
        file.name = paste(c(media.file.prefix,sprintf("TopImg-%i.png",i)),collapse=""),
        ...
      )
    }
  }
  return(results)
}


#' Top Tweeters
#'
#' Get most frequent Tweeters from a Twitter database
#'
#' Provide filter criteria in the \code{start.date}, \code{end.date}, and 
#' \code{where.criteria} parameters.  The results will include statuses created
#' at times greater than or equal to the \code{start.date}, but strictly less
#' than the \code{end.date}.  The \code{where.criteria} must be in SQLite syntax and
#' reference tables and columns in the database (see \code{\link{twitter_database}}).
#' Be sure to include table names, e.g., \code{user.location LIKE '\%fargo\%'}.
#' Returns the most frequent hashtags 
#' in the tweets meeting the filter criteria.
#'
#' @param con SQLite data connection to Twitter database (see \code{\link{twitter_database}}).
#' @param start.date character date (or datetime) of earliest statuses queried.
#' @param end.date character date.  Results will include statuses 
#' with creation times or dates strictly less than this parameter.
#' @param where.criteria character additional criteria to filter the status results, in 
#' SQLite syntax.  See details.
#' @param limit integer number of results to return.
#' @param excel.export.file character name of file to write results in Excel format.  If 
#' \code{NULL} no file is written.
#' @param excel.file.footnote character table footnote to include in Excel file.  if 
#' \code{"auto"} the date range will be written.
#' 
#' @return data frame containing the users with the most tweets that meet the filter criteria.
#'
#' @seealso \code{\link{twitter_database}}, \code{\link{authorize_app}}
#' @export
#' @examples
#'
#' \dontrun{
#' auth.vector <- authorize_IT()
#'
#' con <- twitter_database(
#'   "test.sqlite",
#'   query.users.df = data.frame(
#'     screen_name = c(
#'       "pb2pv",
#'       "zlisto"
#'     )
#'   ),
#'   query.text.df = data.frame(
#'     query_text = c(
#'       "#throwbackthursday",
#'       "#worstfirstdate"
#'     )
#'   )
#' )
#' 
#' update_user_timelines(con)
#' update_search(con)
#' 
#' top.tweeters <- top_tweeters(
#'   con,
#'   start.date = as.Date(Sys.time())-7,
#'   where.criteria = "user.screen_name = 'zlisto'"
#' )
#' 
#' 
#' DBI::dbDisonnect(con)
#' }
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
  results <- DBI::dbGetQuery(con,query)
  if(!is.null(excel.export.file) && is.character(excel.export.file)){
    file.name <- create_excel_filename(excel.export.file)
    footnote <- create_footnote(excel.file.footnote,start.date,end.date)
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

#' Most Popular Tweets
#'
#' Get the most 'liked' Tweets from a Twitter database
#'
#' Provide filter criteria in the \code{start.date}, \code{end.date}, and 
#' \code{where.criteria} parameters.  The results will include statuses created
#' at times greater than or equal to the \code{start.date}, but strictly less
#' than the \code{end.date}.  The \code{where.criteria} must be in SQLite syntax and
#' reference tables and columns in the database (see \code{\link{twitter_database}}).
#' Be sure to include table names, e.g., \code{user.location LIKE '\%fargo\%'}.
#' Returns the most frequent hashtags 
#' in the tweets meeting the filter criteria.
#'
#' @param con SQLite data connection to Twitter database (see \code{\link{twitter_database}}).
#' @param start.date character date (or datetime) of earliest statuses queried.
#' @param end.date character date.  Results will include statuses 
#' with creation times or dates strictly less than this parameter.
#' @param where.criteria character additional criteria to filter the status results, in 
#' SQLite syntax.  See details.
#' @param limit integer number of results to return.
#' @param excel.export.file character name of file to write results in Excel format.  If 
#' \code{NULL} no file is written.
#' @param excel.file.footnote character table footnote to include in Excel file.  if 
#' \code{"auto"} the date range will be written.
#' 
#' @return data frame containing Tweets with the highest numbers of 'likes' meeting the filtering
#' criteria.
#'
#' @seealso \code{\link{twitter_database}}, \code{\link{authorize_app}}
#' @export
#' @examples
#' 
#' \dontrun{
#' auth.vector <- authorize_IT()
#'
#' con <- twitter_database(
#'   "test.sqlite",
#'   query.users.df = data.frame(
#'     screen_name = c(
#'       "pb2pv",
#'       "zlisto"
#'     )
#'   ),
#'   query.text.df = data.frame(
#'     query_text = c(
#'       "#throwbackthursday",
#'       "#worstfirstdate"
#'     )
#'   )
#' )
#' 
#' update_user_timelines(con)
#' update_search(con)
#' 
#' most.liked <- most_liked(
#'   con,
#'   start.date = as.Date(Sys.time())-7,
#'   where.criteria = "user.screen_name = 'zlisto'"
#' )
#' 
#' 
#' DBI::dbDisonnect(con)
#' }
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
  results <- DBI::dbGetQuery(con,query)
  if(!is.null(excel.export.file) && is.character(excel.export.file)){
    file.name <- create_excel_filename(excel.export.file)
    footnote <- create_footnote(excel.file.footnote,start.date,end.date)
    df <- results[,c("screen_name","created_at","favorite_count","text")]
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



#' Most Retweeted
#'
#' Get the most retweeted Tweets from a Twitter database
#'
#' Provide filter criteria in the \code{start.date}, \code{end.date}, and 
#' \code{where.criteria} parameters.  The results will include statuses created
#' at times greater than or equal to the \code{start.date}, but strictly less
#' than the \code{end.date}.  The \code{where.criteria} must be in SQLite syntax and
#' reference tables and columns in the database (see \code{\link{twitter_database}}).
#' Be sure to include table names, e.g., \code{user.location LIKE '\%fargo\%'}.
#' Returns the most frequent hashtags 
#' in the tweets meeting the filter criteria.
#'
#' This method is different from \code{\link{most_popular_RT_in_sample}} because it uses
#' the total number of retweets, which is attached as meta-data to the Tweet
#' when it is collected.  The \code{\link{most_popular_RT_in_sample}}
#' method only counts retweets in the database meeting the supplied filter criteria.
#' 
#' Note that retweet counts are obtained at the time of collection, and are not maintained
#' in real time.  Therefore, this metric is prone to time-of-collection bias and should
#' be used with care.
#' 
#' @param con SQLite data connection to Twitter database (see \code{\link{twitter_database}}).
#' @param start.date character date (or datetime) of earliest statuses queried.
#' @param end.date character date.  Results will include statuses 
#' with creation times or dates strictly less than this parameter.
#' @param where.criteria character additional criteria to filter the status results, in 
#' SQLite syntax.  See details.
#' @param limit integer number of results to return.
#' @param excel.export.file character name of file to write results in Excel format.  If 
#' \code{NULL} no file is written.
#' @param excel.file.footnote character table footnote to include in Excel file.  if 
#' \code{"auto"} the date range will be written.
#' 
#' @return data frame containing most 'retweeted' Tweets meeting the filter criteria.
#'
#' @seealso \code{\link{twitter_database}}, \code{\link{authorize_app}}, 
#' \code{\link{most_popular_RT_in_sample}}
#' @export
#' @examples
#'
#' \dontrun{
#' auth.vector <- authorize_IT()
#'
#' con <- twitter_database(
#'   "test.sqlite",
#'   query.users.df = data.frame(
#'     screen_name = c(
#'       "pb2pv",
#'       "zlisto"
#'     )
#'   ),
#'   query.text.df = data.frame(
#'     query_text = c(
#'       "#throwbackthursday",
#'       "#worstfirstdate"
#'     )
#'   )
#' )
#' 
#' update_user_timelines(con)
#' update_search(con)
#' 
#' most.retweeted <- most_retweeted(
#'   con,
#'   start.date = as.Date(Sys.time())-7,
#'   where.criteria = "user.screen_name = 'zlisto'"
#' )
#' 
#' 
#' DBI::dbDisonnect(con)
#' }
most_retweeted <- function(
  con,
  start.date=NULL,
  end.date=NULL,
  where.criteria=NULL,
  limit=10,
  excel.export.file = NULL,
  excel.file.footnote = "auto"
){
  if(is.null(where.criteria) || where.criteria == ""){
    where.criteria <- "status.retweet = 0"
  } else {
    where.criteria <- paste(paste("(",where.criteria,")",sep=""),"status.retweet = 0",sep = " AND ")
  }
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
  results <- DBI::dbGetQuery(con,query)
  if(!is.null(excel.export.file) && is.character(excel.export.file)){
    file.name <- create_excel_filename(excel.export.file)
    footnote <- create_footnote(excel.file.footnote,start.date,end.date)
    df <- results[,c("screen_name","created_at","retweet_count","text")]
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

#' Most Retweeted Within Sample
#'
#' Get the most frequently occurring Retweet
#'
#' Provide filter criteria in the \code{start.date}, \code{end.date}, and 
#' \code{where.criteria} parameters.  The results will include statuses created
#' at times greater than or equal to the \code{start.date}, but strictly less
#' than the \code{end.date}.  The \code{where.criteria} must be in SQLite syntax and
#' reference tables and columns in the database (see \code{\link{twitter_database}}).
#' Be sure to include table names, e.g., \code{user.location LIKE '\%fargo\%'}.
#' Returns the most frequent hashtags 
#' in the tweets meeting the filter criteria.
#' 
#' This method is different from \code{\link{most_retweeted}} because it only counts
#' retweets in the database meeting the supplied filter criteria.  The \code{\link{most_retweeted}}
#' method uses the total number of retweets, which is attached as meta-data to the Tweet
#' when it is collected.
#'
#' @param con SQLite data connection to Twitter database (see \code{\link{twitter_database}}).
#' @param start.date character date (or datetime) of earliest statuses queried.
#' @param end.date character date.  Results will include statuses 
#' with creation times or dates strictly less than this parameter.
#' @param where.criteria character additional criteria to filter the status results, in 
#' SQLite syntax.  See details.
#' @param limit integer number of results to return.
#' @param excel.export.file character name of file to write results in Excel format.  If 
#' \code{NULL} no file is written.
#' @param excel.file.footnote character table footnote to include in Excel file.  if 
#' \code{"auto"} the date range will be written.
#' 
#' @return data frame containing most frequently occuring Retweet.
#'
#' @seealso \code{\link{twitter_database}}, \code{\link{authorize_app}}
#' @export
#' @examples
#' 
#' \dontrun{
#' auth.vector <- authorize_IT()
#'
#' con <- twitter_database(
#'   "test.sqlite",
#'   query.users.df = data.frame(
#'     screen_name = c(
#'       "pb2pv",
#'       "zlisto"
#'     )
#'   ),
#'   query.text.df = data.frame(
#'     query_text = c(
#'       "#throwbackthursday",
#'       "#worstfirstdate"
#'     )
#'   )
#' )
#' 
#' update_user_timelines(con)
#' update_search(con)
#' 
#' most.popular.retweets <- most_popular_RT_in_sample(
#'   con,
#'   start.date = as.Date(Sys.time())-7,
#'   where.criteria = "user.screen_name = 'zlisto'"
#' )
#' 
#' 
#' DBI::dbDisonnect(con)
#' }
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
  results <- DBI::dbGetQuery(con,query)
  if(!is.null(excel.export.file) && is.character(excel.export.file)){
    file.name <- create_excel_filename(excel.export.file)
    footnote <- create_footnote(excel.file.footnote,start.date,end.date)
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


#' Most Reach
#'
#' Get Tweets that have reached the most people from a Twitter database
#'
#' Provide filter criteria in the \code{start.date}, \code{end.date}, and 
#' \code{where.criteria} parameters.  The results will include statuses created
#' at times greater than or equal to the \code{start.date}, but strictly less
#' than the \code{end.date}.  The \code{where.criteria} must be in SQLite syntax and
#' reference tables and columns in the database (see \code{\link{twitter_database}}).
#' Be sure to include table names, e.g., \code{user.location LIKE '\%fargo\%'}.
#' Returns the most frequent hashtags 
#' in the tweets meeting the filter criteria.
#' 
#' Note that this method only includes reach that is measurable within the database.  If
#' retweets that have not been collected will not counted.  Therefore, this method
#' is most accurate when run on results that have been obtained from search queries, and
#' less useful on results obtained from user timeline queries.
#'
#' @param con SQLite data connection to Twitter database (see \code{\link{twitter_database}}).
#' @param start.date character date (or datetime) of earliest statuses queried.
#' @param end.date character date.  Results will include statuses 
#' with creation times or dates strictly less than this parameter.
#' @param where.criteria character additional criteria to filter the status results, in 
#' SQLite syntax.  See details.
#' @param limit integer number of results to return.
#' @param excel.export.file character name of file to write results in Excel format.  If 
#' \code{NULL} no file is written.
#' @param excel.file.footnote character table footnote to include in Excel file.  if 
#' \code{"auto"} the date range will be written.
#' 
#' @return data frame containing Tweets that have been tweeted or retweeted to the
#' most followers in the Twitter database.
#'
#' @seealso \code{\link{twitter_database}}, \code{\link{authorize_app}}
#' @export
#' @examples
#'
#' \dontrun{
#' auth.vector <- authorize_IT()
#'
#' con <- twitter_database(
#'   "test.sqlite",
#'   query.users.df = data.frame(
#'     screen_name = c(
#'       "pb2pv",
#'       "zlisto"
#'     )
#'   ),
#'   query.text.df = data.frame(
#'     query_text = c(
#'       "#throwbackthursday",
#'       "#worstfirstdate"
#'     )
#'   )
#' )
#' 
#' update_user_timelines(con)
#' update_search(con)
#' 
#' most.reach <- most_reach(
#'   con,
#'   start.date = as.Date(Sys.time())-7,
#'   where.criteria = "query_text.id = 1"
#' )
#' 
#' 
#' DBI::dbDisonnect(con)
#' }
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
    "SELECT super.status_id,SUM(super.followers_count) AS n FROM (",
    sub.query,
    ") as super WHERE super.status_id IS NOT NULL GROUP BY super.status_id ORDER BY n DESC LIMIT ",
    limit,
    ";",
    sep=""
  )
  results <- DBI::dbGetQuery(con,query)
  new.query.select <- paste(
    "SELECT status.id as status_id, user.screen_name,user.name,user.location,user.description,status.text,status.created_at"
  )
  new.query.where <- paste("status.id IN (",paste(results$status_id,collapse=","),")",sep="")
  new.query.join <- make_join_statement(con,paste(new.query.select,new.query.where,sep=" "))
  new.query <- paste(
    new.query.select,
    " FROM ",
    new.query.join,
    " ",
    make_where_clause(where.criteria = new.query.where),
    ";",
    sep=""
  )
  new.result <- DBI::dbGetQuery(con,new.query)
  output <- merge(results,new.result,by="status_id")
  output <- output[order(output$n,decreasing=TRUE),]
  if(!is.null(excel.export.file) && is.character(excel.export.file)){
    file.name <- create_excel_filename(excel.export.file)
    footnote <- create_footnote(excel.file.footnote,start.date,end.date)
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
    start.date = start.date, 
    end.date = end.date, 
    where.criteria = where.criteria, 
    additional.columns = c("status.retweet", "status.screen_name",additional.columns)
  )
  results <- DBI::dbGetQuery(con, query)
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
  join.clause <- make_join_statement(con,paste(select.clause,where.clause,sep=" "))
  query <- paste(
    "SELECT",
    select.clause,
    "FROM",
    join.clause,
    where.clause,
    "ORDER BY status.created_at",
    ";",
    sep=" "
  )
  results <- DBI::dbGetQuery(con, query)
  results$TimeStamp <- as.POSIXct(results$created_at, format = "%Y-%m-%d %H:%M:%S")#, origin = "1970-01-01")
  results$date <- as.Date(results$TimeStamp, format = '%Y-%m-%d')
  return(results)
}



create_footnote <- function(excel.file.footnote,start.date,end.date){
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

