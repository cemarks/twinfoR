#### Recursives

#' Recursive Twitter Search
#'
#' Call the Twitter Search API recursively
#'
#' This function recursively searches the Twitter API from most recent results to less
#' recent results until no tweets are returned.  Each query includes a five-second
#' delay in order to prevent exceeding the API rate limit.  Optionally, this function
#' inputs a function to call that takes the resulting status list as an argument.  This
#' function can be anything, but is intended to be a function that writes the statuses to
#' a data connection.
#'
#' If the \code{authentication.vector} parameter is missing, this method checks the global
#' environment for \code{auth.vector}.  If this variable exists, the function will use
#' this variable as the \code{authentication.vector}.  If this variable does not exist, the
#' function will throw an error.
#'
#' @param q character search query string.
#' @param data.connection a DBI connection to an RSQLite \code{\link{twitter_database}}.
#' @param authentication.vector character vector containing authentication tokens and secrets.
#' See \code{\link{authorize_app}} and \code{\link{authorize_IT}}.
#' @param geocode character string of the form "latitude,longitude,radius".  Note that this functionality
#' does not always work well and can severely limit the number of tweets returned.
#' @param lang character language abbreviation to limit results.  The correct abbreviation must be used, e.g.,
#' 'en' for English or 'es' for Spanish.  Not all langauges are supported, and some Tweets are not correctly
#' labeled.
#' @param locale character specification of the language of the query.  The default (\code{NULL}) should work
#' in most cases.
#' @param result_type character from \code{c("mixed","recent","popular")}, indicating which algorithm
#' Twitter will use to determine which statuses to return.
#' @param until character date string formated as 'YYYY-MM-DD'.  If supplied, only tweets *before* this date
#' will be returned.
#' @param since_id numeric or character status_id.  If supplied, only tweets with status_ids greater
#' than this value (and therefore more recent) will be returned.
#' @param max_id numeric or character status_id.  If suppled, only tweets with status_ids less than or
#' equal to this value (and therefore no more recent) will be returned.
#' @param include_entities logical indicating whether the status entities
#' (e.g., urls, hashtags, usermentions, media) will be included in the results.
#' @param tweet_mode character either 'extended' for full_text statuses or 'compat' for
#' 140 character compatability.
#' @param query.row.id integer row \code{id} of the query in the \code{query_text} table.  This
#' value is inserted into the \code{search_status} table.  Ignored if no \code{data.connection}
#' provided.  See \code{\link{twitter_database}}.
#' @param ... other named parameters passed to \code{\link{insert_statuses}}.
#'
#' @return character most recent status id collected if \code{data.connection} is supplied,
#' otherwise a list of status objects (lists).
#'
#' @seealso \code{\link{search_tweets}}, \code{\link{insert_statuses}}, \code{\link{twitter_database}}
#' @export
#' @examples
#' 
#' \dontrun{
#' auth.vector <- load("auth-vector.RData")
#' query <- "Miami"
#' statuses <- search_tweets_recursive(
#'   query,
#'   authentication.vector = auth.vector
#' )
#' }
search_tweets_recursive <- function(
  q,
  data.connection,
  authentication.vector,
  geocode = NULL,
  lang = NULL,
  locale = NULL,
  result_type = 'recent',
  until = NULL,
  since_id = NULL,
  max_id = NULL,
  include_entities = TRUE,
  tweet_mode = 'extended',
  query.row.id = NULL,
  ...
){
  if(missing(authentication.vector)){
    if(!exists('auth.vector')){
      stop("No authentication tokens found!")
    } else {
      authentication.vector <- auth.vector
    }
  }
  last.query.time <- as.POSIXct(Sys.time())
  fails <- 0
  if(missing(data.connection)){
    statuses <- list()
  }
  first.query <- TRUE
  repeat{
    Sys.sleep(
      max(
        c(
          0,
          5-as.numeric(
            difftime(
              as.POSIXct(
                Sys.time()
              ),
              last.query.time,
              units = 'secs'
            )
          )
        )
      )
    )
    request.result <- search_tweets(
      q,
      authentication.vector,
      geocode,
      lang,
      locale,
      result_type,
      100,
      until,
      since_id,
      max_id,
      include_entities,
      tweet_mode
    )
    last.query.time <- as.POSIXct(Sys.time())
    if(class(request.result) == "response"){
      fails <- fails + 1
      if(fails < 3){
        warning(sprintf("HTTP Request returned %i status (%s).",request.result$status,httr::content(request.result)))
      } else {
        stop(sprintf("Three consecutive HTTP Errors!  HTTP Request returned %i status (%s).",request.result$status,httr::content(request.result)))
      }
    } else {
      fails <- 0
      if(length(request.result$statuses)==0){
        if(first.query){
          since.id <- NA
        }
        break
      }
      if(first.query){
        since.id <- as.character(
          max(
            bit64::as.integer64(
              sapply(
                request.result$statuses,
                function(x) return(x$id_str)
              )
            )
          )
        )
        first.query <- FALSE
      }
      if(missing(data.connection)){
        statuses <- c(statuses,request.result$statuses)
      } else {
        insert_statuses(
          data.connection,
          request.result$statuses,
          ...
        )
        if(!is.null(query.row.id)){
          status.ids <- sapply(request.result$statuses,function(x) return(x$id_str))
          query <- paste(
            "INSERT OR IGNORE INTO search_status VALUES ",
            paste("(",query.row.id,",'",status.ids,"')",sep="",collapse=","),
            ";"
          )
          DBI::dbExecute(data.connection,query)
        }
      }
      max_id <- as.character(
        min(
          bit64::as.integer64(
            sapply(
              request.result$statuses,
              function(x) return(x$id_str)
            )
          )
        )-1
      )
    }
  }
  if(missing(data.connection)){
    return(statuses)
  } else {
    return(since.id)
  }
}



#' Recursive Timeline Query
#'
#' Call the get/user_timeline API recursively
#'
#' This function recursively calls the get/user_timeline API to obtain all available statuses in
#' a user timeline that meet the conditions specified by the input parameters.  This method
#' includes a one second delay for each query in order to prevent exceeding the rate limit.
#' Twitter limits the number of tweets available through this API to the most recent 3200
#' posts for each user.  See
#' \href{https://developer.twitter.com/en/docs/tweets/search/api-reference/get-statuses-user_timeline}{user_timeline API Documentation}
#' for more information.
#'
#' If the \code{authentication.vector} parameter is missing, this method checks the global
#' environment for \code{auth.vector}.  If this variable exists, the function will use
#' this variable as the \code{authentication.vector}.  If this variable does not exist, the
#' function will throw an error.
#'
#' @param screen_name character screen_name.  This is only used is \code{user_id} is missing.
#' @param user_id numeric or character user_id.
#' @param data.connection a DBI connection to an RSQLite \code{\link{twitter_database}}.
#' @param authentication.vector character vector containing authentication tokens and secrets.
#' See \code{\link{authorize_app}} and \code{\link{authorize_IT}}.
#' @param since_id numeric or character status_id.  If supplied, only tweets with status_ids greater
#' than this value (and therefore more recent) will be returned.
#' @param max_id numeric or character status_id.  If suppled, only tweets with status_ids less than or
#' equal to this value (and therefore no more recent) will be returned.
#' @param trim_user logical indicating whether to remove the user object from each status.
#' @param exclude_replies logical indicating whether to filter user replies to other statuses
#' from the results.
#' @param include_rts logical indicating whether to include retweets in the results.
#' @param tweet_mode character either 'extended' for full_text statuses or 'compat' for
#' 140 character compatability.
#' @param ... other named parameters passed to \code{\link{insert_statuses}}.
#'
#' @return character most recent status id if \code{data.connection} is supplied, 
#' otherwise a list of status objects (lists).
#'
#' @seealso \code{\link{user_timeline}}, \code{\link{insert_statuses}}, \code{\link{twitter_database}}
#' @export
#' @examples
#'
#' \dontrun{
#' auth.vector <- authorize_IT()
#'
#' user.statuses <- user_timeline_recursive(
#'   "realDonaldTrump",
#'   authentication.vector = auth.vector
#' )
#' }
user_timeline_recursive <- function(
  screen_name,
  user_id,
  data.connection,
  authentication.vector,
  since_id = NULL,
  max_id = NULL,
  trim_user = FALSE,
  exclude_replies = FALSE,
  include_rts = TRUE,
  tweet_mode = 'extended',
  ...
){
  if(missing(authentication.vector)){
    if(!exists('auth.vector')){
      stop("No authentication tokens found!")
    } else {
      authentication.vector <- auth.vector
    }
  }
  last.query.time <- as.POSIXct(Sys.time())
  fails <- 0
  if(missing(data.connection)){
    statuses <- list()
  }
  status.count <- 0
  first.query <- TRUE
  since.id <- NA
  repeat{
    Sys.sleep(
      max(
        c(
          0,
          1-as.numeric(
            difftime(
              as.POSIXct(
                Sys.time()
              ),
              last.query.time,
              units = 'secs'
            )
          )
        )
      )
    )
    if(missing(user_id)){
      request.result <- user_timeline(
        screen_name,
        authentication.vector = authentication.vector,
        count = 200,
        since_id = since_id,
        max_id = max_id,
        trim_user = trim_user,
        exclude_replies = exclude_replies,
        include_rts = include_rts,
        tweet_mode = tweet_mode
      )
    } else {
      request.result <- user_timeline(
        user_id = user_id,
        authentication.vector = authentication.vector,
        count = 200,
        since_id = since_id,
        max_id = max_id,
        trim_user = trim_user,
        exclude_replies = exclude_replies,
        include_rts = include_rts,
        tweet_mode = tweet_mode
      )
    }
    last.query.time <- as.POSIXct(Sys.time())
    if(class(request.result) == "response"){
      if(request.result$status == 401){
        # warning("HTTP Request returned status 401 (Not Authorized).")
        statuses <- NULL
        since_id <- NA
        break
      }
      fails <- fails + 1
      if(fails >= 3){
        stop(sprintf("Three consecutive HTTP Errors!  HTTP Request returned %i status (%s).",request.result$status,httr::content(request.result)))
      } else {
        # warning(sprintf("HTTP Request returned %i status (%s).",request.result$status,httr::content(request.result)))
      }
    } else {
      fails <- 0
      if(length(request.result)>0){
        if(first.query){
          first.query <- FALSE
          since.id <- as.character(
            max(
              bit64::as.integer64(
                sapply(
                  request.result,
                  function(x) return(x$id_str)
                )
              )
            )
          )
        }
        if(missing(data.connection)){
          statuses <- c(statuses,request.result)
        } else {
          insert_statuses(
            data.connection,
            request.result,
            ...
          )
        }
        status.count <- status.count + length(request.result)
      }
      if((length(request.result) < 100) || ((length(request.result)+status.count) > 3200)){
        break
      }
      max_id <- as.character(
        min(
          bit64::as.integer64(
            sapply(
              request.result,
              function(x) return(x$id_str)
            )
          )
        )-1
      )
    }
  }
  if(missing(data.connection)){
    return(statuses)
  } else {
    return(since.id)
  }
}



#' Recursive User Lookup Query
#'
#' Call the users/lookup API recursively
#'
#' This function recursively calls the users/lookup endpoint (see
#' \href{https://developer.twitter.com/en/docs/accounts-and-users/follow-search-get-users/api-reference/get-users-lookup}{User Lookup API Documentation}).
#' This is useful for lists of more than 100 users.  For no more than 100 users, the
#' \code{\link{user_lookup}} function is preferred.  This method
#' includes a one second delay for each query in order to prevent exceeding the rate limit.
#'
#' If the \code{authentication.vector} parameter is missing, this method checks the global
#' environment for \code{auth.vector}.  If this variable exists, the function will use
#' this variable as the \code{authentication.vector}.  If this variable does not exist, the
#' function will throw an error.
#'
#' @param screen_name character vector of screen_names.  This is only used is \code{user_id} is missing.
#' @param user_id numeric or character vector of user_ids.
#' @param data.connection a DBI connection to an RSQLite \code{\link{twitter_database}}.
#' @param authentication.vector character vector containing authentication tokens and secrets.
#' See \code{\link{authorize_app}} and \code{\link{authorize_IT}}.
#' @param include_entities logical indicating whether to include profile entities (e.g., urls)
#' as separate nodes in the returned json object.
#' @param tweet_mode character either 'extended' for full_text statuses or 'compat' for
#' 140 character compatability.
#' @param ... other named parameters passed to \code{\link{insert_users}}.
#'
#' @return \code{NULL} if \code{data.connection} is supplied, otherwise a list of user objects (lists).
#'
#' @seealso \code{\link{user_lookup}}, \code{\link{insert_users}}, \code{\link{twitter_database}}
#' @export
#' @examples
#'
#' \dontrun{
#' auth.vector <- authorize_IT()
#' user.ids.response <- get_followers_ids("realDonaldTrump")
#' user.ids <- user.ids.response$ids
#'
#' users <- user_lookup_recursive(user_id=user.ids)
#' }
user_lookup_recursive <- function(
  screen_name,
  user_id,
  data.connection,
  authentication.vector,
  include_entities = TRUE,
  tweet_mode = 'extended',
  ...
){
  if(missing(authentication.vector)){
    if(!exists('auth.vector')){
      stop("No authentication tokens found!")
    } else {
      authentication.vector <- auth.vector
    }
  }
  if(missing(user_id)){
    total.users <- length(screen_name)
  } else {
    total.users <- length(user_id)
  }
  last.query.time <- as.POSIXct(Sys.time())
  fails <- 0
  if(missing(data.connection)){
    users <- list()
  }
  current_index <- 0
  repeat{
    Sys.sleep(
      max(
        c(
          0,
          1-as.numeric(
            difftime(
              as.POSIXct(
                Sys.time()
              ),
              last.query.time,
              units = 'secs'
            )
          )
        )
      )
    )
    if(missing(user_id)){
      request.result <- user_lookup(
        screen_name[(current_index+1):min(c(current_index+100,total.users))],
        authentication.vector = authentication.vector,
        include_entities = include_entities,
        tweet_mode = tweet_mode
      )
    } else {
      request.result <- user_lookup(
        user_id = user_id[(current_index+1):min(c(current_index+100,total.users))],
        authentication.vector = authentication.vector,
        include_entities = include_entities,
        tweet_mode = tweet_mode
      )
    }
    last.query.time <- as.POSIXct(Sys.time())
    if(class(request.result) == "response"){
      fails <- fails + 1
      if(fails < 3){
        warning(sprintf("HTTP Request returned %i status (%s).",request.result$status,httr::content(request.result)))
      } else {
        stop(sprintf("Three consecutive HTTP Errors!  HTTP Request returned %i status (%s).",request.result$status,httr::content(request.result)))
      }
    } else {
      fails <- 0
      if(missing(data.connection)){
        users <- c(users,request.result)
      } else {
        insert_users(
          data.connection,
          request.result,
          ...
        )
      }
      current_index <- current_index + 100
      if(current_index >= total.users){
        break
      }
    }
  }
  if(missing(data.connection)){
    return(users)
  }
}


#' Recursive Status Lookup
#'
#' Recursively call the status lookup API
#'
#' This function recursively calls the statuses/lookup endpoint (see
#' \href{https://developer.twitter.com/en/docs/tweets/post-and-engage/api-reference/get-statuses-lookup}{Status Lookup API Documentation}).
#' This is useful for lists of more than 100 status_ids.  For no more than 100 statuses, the
#' \code{\link{status_lookup}} function is preferred.  This method
#' includes a one second delay for each query in order to prevent exceeding the rate limit.
#'
#' If the \code{authentication.vector} parameter is missing, this method checks the global
#' environment for \code{auth.vector}.  If this variable exists, the function will use
#' this variable as the \code{authentication.vector}.  If this variable does not exist, the
#' function will throw an error.
#'
#' @param status_id numeric or character vector of up to 100 status_ids.
#' @param data.connection a DBI connection to an RSQLite \code{\link{twitter_database}}.
#' @param authentication.vector character vector containing authentication tokens and secrets.
#' See \code{\link{authorize_app}} and \code{\link{authorize_IT}}.
#' @param include_entities logical indicating whether to include profile entities (e.g., urls)
#' as separate nodes in the returned json object.
#' @param trim_user logical indicating whether to remove the user object from each status.
#' @param map logical indicating whether to return status_ids with NULL values if they do not
#' exist or cannot be viewed by the authenticated user.
#' @param include_ext_alt_text logical indicating whether to return "alt" text assigned to any
#' attached media entities.
#' @param include_card_uri logical indicating whether to include a \code{card_uni} attribute with each
#' tweet when there is an ad attached to the tweet using the \code{card_uni} value.
#' @param tweet_mode character either 'extended' for full_text statuses or 'compat' for
#' 140 character compatability.
#' @param ... other named parameters passed to \code{\link{insert_statuses}}.
#'
#' @return \code{NULL} if \code{data.connection} is supplied, otherwise a list of status objects.
#'
#' @seealso \code{\link{status_lookup}}, \code{\link{insert_statuses}}, \code{\link{twitter_database}}
#' @export
#' @examples
#'
#' \dontrun{
#' auth.vector <- authorize_IT()
#'
#' original.statuses <- user_timeline_recursive("realDonaldTrump")
#' status.ids <- sapply(original.statuses,function(x) return(x$id_str))
#'
#' new.statuses <- status_lookup_recursive(
#'   status.ids,
#'   authentication.vector <- auth.vector
#' )
#'
#' cat(new.statuses[[1]]$full_text)
#' }
status_lookup_recursive <- function(
  status_id,
  data.connection,
  authentication.vector,
  include_entities = TRUE,
  trim_user = FALSE,
  map = NULL,
  include_ext_alt_text = NULL,
  include_card_uri = NULL,
  tweet_mode = 'extended',
  ...
){
  if(missing(authentication.vector)){
    if(!exists('auth.vector')){
      stop("No authentication tokens found!")
    } else {
      authentication.vector <- auth.vector
    }
  }
  total.statuses <- length(status_id)
  last.query.time <- as.POSIXct(Sys.time())
  fails <- 0
  if(missing(data.connection)){
    statuses <- list()
  }
  current_index <- 0
  repeat{
    Sys.sleep(
      max(
        c(
          0,
          1-as.numeric(
            difftime(
              as.POSIXct(
                Sys.time()
              ),
              last.query.time,
              units = 'secs'
            )
          )
        )
      )
    )
    request.result <- status_lookup(
      status_id = status_id[(current_index+1):min(c(current_index+100,total.statuses))],
      authentication.vector = authentication.vector,
      include_entities = include_entities,
      trim_user = trim_user,
      map = map,
      include_ext_alt_text = include_ext_alt_text,
      include_card_uri = include_card_uri,
      tweet_mode = tweet_mode
    )
    last.query.time <- as.POSIXct(Sys.time())
    if(class(request.result) == "response"){
      fails <- fails + 1
      if(fails < 3){
        warning(sprintf("HTTP Request returned %i status (%s).",request.result$status,httr::content(request.result)))
      } else {
        stop(sprintf("Three consecutive HTTP Errors!  HTTP Request returned %i status (%s).",request.result$status,httr::content(request.result)))
      }
    } else {
      fails <- 0
      if(missing(data.connection)){
        statuses <- c(statuses,request.result)
      } else {
        insert_statuses(
          data.connection,
          request.result,
          ...
        )
      }
      current_index <- current_index + 100
      if(current_index >= total.statuses){
        break
      }
    }
  }
  if(missing(data.connection)){
    return(statuses)
  }
}



#' Recursive User Search
#'
#' Recursively query the User Search API
#'
#' This function recursively queries the
#' users/search endpoint (see
#' \href{https://developer.twitter.com/en/docs/accounts-and-users/follow-search-get-users/api-reference/get-users-search}{User Search API Documentation}).
#' A query (\code{q}) is required.  This method includes a one second delay per query
#' to avoid exceeding the API rate limit.  User search queries are performed until
#' either \code{max_queries} is reached or results are repeated.
#'
#' If the \code{authentication.vector} parameter is missing, this method checks the global
#' environment for \code{auth.vector}.  If this variable exists, the function will use
#' this variable as the \code{authentication.vector}.  If this variable does not exist, the
#' function will throw an error.
#'
#' @param q character search query.
#' @param data.connection a DBI connection to an RSQLite \code{\link{twitter_database}}.
#' @param authentication.vector character vector containing authentication tokens and secrets.
#' See \code{\link{authorize_app}} and \code{\link{authorize_IT}}.
#' @param include_entities logical indicating whether to include profile entities (e.g., urls)
#' as separate nodes in the returned json object.
#' @param tweet_mode character either 'extended' for full_text statuses or 'compat' for
#' 140 character compatability.
#' @param max_queries numeric maximum number of user search queries to perform.
#' @param ... other named parameters passed to \code{\link{insert_users}}.
#'
#' @return \code{NULL} if \code{data.connection} is supplied, otherwise a list of user objects.
#'
#' @seealso \code{\link{user_search}}, \code{\link{insert_users}}, \code{\link{twitter_database}}
#' @export
#' @examples
#'
#' \dontrun{
#' auth.vector <- authorize_IT()
#'
#' searched.users <- user_search_recursive(
#'   "Trump",
#'   authentication.vector = auth.vector,
#'   max_queries = 5
#' )
#'
#' cat(searched.users[[3]]$description)
#' }
user_search_recursive <- function(
  q,
  data.connection,
  authentication.vector,
  include_entities = TRUE,
  tweet_mode = 'extended',
  max_queries = 5,
  ...
){
  if(missing(authentication.vector)){
    if(!exists('auth.vector')){
      stop("No authentication tokens found!")
    } else {
      authentication.vector <- auth.vector
    }
  }
  last.query.time <- as.POSIXct(Sys.time())
  fails <- 0
  if(missing(data.connection)){
    users <- list()
  }
  user.ids <- NULL
  page <- 1
  while(page <= max_queries){
    Sys.sleep(
      max(
        c(
          0,
          1-as.numeric(
            difftime(
              as.POSIXct(
                Sys.time()
              ),
              last.query.time,
              units = 'secs'
            )
          )
        )
      )
    )
    request.result <- user_search(
      q,
      authentication.vector = authentication.vector,
      page=page,
      20,
      include_entities = include_entities,
      tweet_mode = tweet_mode
    )
    last.query.time <- as.POSIXct(Sys.time())
    if(class(request.result) == "response"){
      fails <- fails + 1
      if(fails < 3){
        warning(sprintf("HTTP Request returned %i status (%s).",request.result$status,httr::content(request.result)))
      } else {
        stop(sprintf("Three consecutive HTTP Errors!  HTTP Request returned %i status (%s).",request.result$status,httr::content(request.result)))
      }
    } else {
      fails <- 0
      new.user.ids <- sapply(request.result,function(x) return(x$id_str))
      w <- which(!(new.user.ids %in% user.ids))
      if(missing(data.connection)){
        users <- c(users,request.result[w])
      } else {
        insert_users(
          data.connection,
          request.result[w],
          ...
        )
      }
      user.ids <- c(user.ids,new.user.ids[w])
      page <- page + 1
      if(length(w) < length(request.result)){
        break
      }
    }
  }
  if(missing(data.connection)){
    return(users)
  }
}


#' Recursively Get Followers List
#'
#' Recursively call the Followers List API to get all followers
#'
#' This function calls recursively calls the
#' followers/list endpoint (see
#' \href{https://developer.twitter.com/en/docs/accounts-and-users/follow-search-get-users/api-reference/get-followers-list}{Followers List API Documentation}).
#' A user_id or screen_name is required.  If an authentication vector is not
#' provided and there is no globally defined \code{auth.vector}, an error is returned.  A *one minute* delay
#' is included for each query in order to avoid exceeding the rate limit.
#'
#' @param screen_name character single screen_name.  This is only used is \code{user_id} is missing.
#' @param user_id numeric or character single user_id.
#' @param data.connection a DBI connection to an RSQLite \code{\link{twitter_database}}.
#' @param authentication.vector character vector containing authentication tokens and secrets.
#' @param skip_status logical indicating whether to exclude the user status from the returned
#' follower user objects.
#' @param include_user_entities logical indicating whether to include profile entities (e.g. profile url)
#' in the returned follower user objects.
#' @param tweet_mode character either 'extended' for full_text statuses or 'compat' for
#' 140 character compatability.
#' @param last.query.time POSIXct time of last query executed.  If not provided, a one minute delay will 
#' be added before the first query to prevent exceeding the Twitter rate limit.
#' @param ... other named parameters passed to \code{\link{insert_users}}.
#'
#' @return character collected \code{user_id} if \code{data.connection} is supplied, otherwise a list of user objects.
#'
#' @seealso \code{\link{followers_list}}, \code{\link{insert_followers}}, \code{\link{insert_users}}, \code{\link{twitter_database}}
#' @export
#' @examples
#'
#' \dontrun{
#' auth.vector <- authorize_IT()
#'
#' dma.relief.followers <- followers_list_recursive(
#'   "dominica_relief",
#'   authentication.vector = auth.vector
#' )
#'
#' cat(
#'   paste(
#'     "@",
#'     sapply(dma.relief.followers[1:20],function(x) return(x$screen_name)),
#'     ": ",
#'     sapply(dma.relief.followers[1:20],function(x) return(x$description)),
#'     sep="",
#'     collapse="\n"
#'   )
#' )
#' }
followers_list_recursive <- function(
  screen_name,
  user_id,
  data.connection,
  authentication.vector,
  skip_status = NULL,
  include_user_entities = NULL,
  tweet_mode = 'extended',
  last.query.time = NULL,
  ...
){
  if(missing(authentication.vector)){
    if(!exists('auth.vector')){
      stop("No authentication tokens found!")
    } else {
      authentication.vector <- auth.vector
    }
  }
  if(is.null(last.query.time)){
    last.query.time <- as.POSIXct(Sys.time())
  }
  fails <- 0
  if(missing(data.connection)){
    users <- list()
  }
  cursor <- NULL
  if(missing(user_id)){
    Sys.sleep(1)
    user.request <- user_show(
      screen_name = screen_name[1],
      authentication.vector = authentication.vector
    )
    user_id <- user.request$id_str
  } else {
    user.request <- user_show(
      user_id = user_id,
      authentication.vector = authentication.vector
    )
  }
  if(!user.request$protected && (user.request$followers_count > 0)){
    repeat{
      Sys.sleep(
        max(
          c(
            0,
            60-as.numeric(
              difftime(
                as.POSIXct(
                  Sys.time()
                ),
                last.query.time,
                units = 'secs'
              )
            )
          )
        )
      )
      request.result <- followers_list(
        user_id = user_id[1],
        authentication.vector = authentication.vector,
        cursor=cursor,
        count=200,
        skip_status = NULL,
        include_user_entities = NULL,
        tweet_mode = tweet_mode
      )
      last.query.time <- as.POSIXct(Sys.time())
      if(class(request.result) == "response"){
        fails <- fails + 1
        if(fails < 3){
          warning(sprintf("HTTP Request returned %i status (%s).",request.result$status,httr::content(request.result)))
        } else {
          stop(sprintf("Three consecutive HTTP Errors!  HTTP Request returned %i status (%s).",request.result$status,httr::content(request.result)))
        }
      } else {
        fails <- 0
    if(missing(data.connection)){
          users <- c(users,request.result$users)
        } else {
          if(length(request.result$users)>0){
            insert_users(
              data.connection,
              request.result$users,
              ...
            )
            followers.ids <- sapply(request.result$users,function(x) return(x$id_str))
            insert_followers(
              data.connection,
              user_id,
              followers.ids
            )
          }
        }
        cursor <- request.result$next_cursor_str
        if(cursor=="0"){
          break
        }
      }
    }
    if(missing(data.connection)){
      return(users)
    } else {
      return(last.query.time)
    }
  } else {
    if(user.request$protected){
      cat(sprintf("@%s account is protected.\n",user.request$screen_name))
    }
    if(user.request$followers_count==0){
      cat(sprintf("@%s has no followers", user.request$screen_name))
    }
    if(missing(data.connection)){
      return(NULL)
    } else {
      return(last.query.time)
    }
  }
}


#' Recursively Get Friends List
#'
#' Recursively call the Friends List API to get all friends
#'
#' This function calls recursively calls the
#' friends/list endpoint (see
#' \href{https://developer.twitter.com/en/docs/accounts-and-users/follow-search-get-users/api-reference/get-friends-list}{Friends List API Documentation}).
#' A user_id or screen_name is required.  If an authentication vector is not
#' provided and there is no globally defined \code{auth.vector}, an error is returned.  A *one minute* delay
#' is included for each query in order to avoid exceeding the rate limit.
#'
#' @param screen_name character single screen_name.  This is only used is \code{user_id} is missing.
#' @param user_id numeric or character single user_id.
#' @param data.connection a DBI connection to an RSQLite \code{\link{twitter_database}}.
#' @param authentication.vector character vector containing authentication tokens and secrets.
#' @param skip_status logical indicating whether to exclude the user status from the returned
#' follower user objects.
#' @param include_user_entities logical indicating whether to include profile entities (e.g. profile url)
#' in the returned follower user objects.
#' @param tweet_mode character either 'extended' for full_text statuses or 'compat' for
#' 140 character compatability.
#' @param last.query.time POSIXct time of last query executed.  If not provided, a one minute delay will 
#' be added before the first query to prevent exceeding the Twitter rate limit.
#' @param ... other named parameters passed to \code{\link{insert_users}}.
#'
#' @return character collected \code{user_id} if \code{data.connection} is supplied, otherwise a list of user objects.
#'
#' @seealso \code{\link{friends_list}}, \code{\link{insert_friends}}, \code{\link{insert_users}}, \code{\link{twitter_database}}
#' @export
#' @examples
#'
#' \dontrun{
#' auth.vector <- authorize_IT()
#'
#' dma.relief.friends <- followers_list_recursive(
#'   "dominica_relief",
#'   authentication.vector = auth.vector
#' )
#'
#' cat(
#'   paste(
#'     "@",
#'     sapply(dma.relief.friends[1:20],function(x) return(x$screen_name)),
#'     ": ",
#'     sapply(dma.relief.friends[1:20],function(x) return(x$description)),
#'     s# ep="",
#'     collapse="\n"
#'   )
#' )
#' }
friends_list_recursive <- function(
  screen_name,
  user_id,
  data.connection,
  authentication.vector,
  skip_status = NULL,
  include_user_entities = NULL,
  tweet_mode = 'extended',
  last.query.time = NULL,
  ...
){
  if(missing(authentication.vector)){
    if(!exists('auth.vector')){
      stop("No authentication tokens found!")
    } else {
      authentication.vector <- auth.vector
    }
  }
  if(is.null(last.query.time)){
    last.query.time <- as.POSIXct(Sys.time())
  }
  fails <- 0
  if(missing(data.connection)){
    users <- list()
  }
  cursor <- NULL
  if(missing(user_id)){
    Sys.sleep(1)
    user.request <- user_show(
      screen_name = screen_name[1],
      authentication.vector = authentication.vector
    )
    user_id <- user.request$id_str
  } else {
    user.request <- user_show(
      user_id = user_id,
      authentication.vector = authentication.vector
    )
  }
  if(!user.request$protected && (user.request$friends_count > 0)){
    repeat{
      Sys.sleep(
        max(
          c(
            0,
            60-as.numeric(
              difftime(
                as.POSIXct(
                  Sys.time()
                ),
                last.query.time,
                units = 'secs'
              )
            )
          )
        )
      )
      request.result <- friends_list(
        user_id = user_id[1],
        authentication.vector = authentication.vector,
        cursor=cursor,
        count=200,
        skip_status = NULL,
        include_user_entities = NULL,
        tweet_mode = tweet_mode
      )
      last.query.time <- as.POSIXct(Sys.time())
      if(class(request.result) == "response"){
        fails <- fails + 1
        if(fails < 3){
          warning(sprintf("HTTP Request returned %i status (%s).",request.result$status,httr::content(request.result)))
        } else {
          stop(sprintf("Three consecutive HTTP Errors!  HTTP Request returned %i status (%s).",request.result$status,httr::content(request.result)))
        }
      } else {
        fails <- 0
        if(missing(data.connection)){
          users <- c(users,request.result$users)
        } else {
          if(length(request.result$users)>0){
            insert_users(
              data.connection,
              request.result$users,
              ...
            )
            friends.ids <- sapply(request.result$users,function(x) return(x$id_str))
            insert_friends(
              data.connection,
              user_id,
              friends.ids
            )
          }
        }
        cursor <- request.result$next_cursor_str
        if(cursor=="0"){
          break
        }
      }
    }
    if(missing(data.connection)){
      return(users)
    } else {
      return(last.query.time)
    }
  } else {
    if(user.request$protected){
      cat(sprintf("@%s account is protected.\n",user.request$screen_name))
    }
    if(user.request$friends_count==0){
      cat(sprintf("@%s has no friends",user.request$screen_name))
    }
    if(missing(data.connection)){
      return(NULL)
    } else {
      return(last.query.time)
    }
  }
}


#' Recursively Get Followers IDs
#'
#' Recursively call the Followers IDs API to get all followers IDs
#'
#' This function calls recursively calls the
#' followers/list endpoint (see
#' \href{https://developer.twitter.com/en/docs/accounts-and-users/follow-search-get-users/api-reference/get-followers-ids}{Followers IDs API Documentation}).
#' A user_id or screen_name is required.  If an authentication vector is not
#' provided and there is no globally defined \code{auth.vector}, an error is returned.  A *one minute* delay
#' is included for each query in order to avoid exceeding the rate limit.
#' If a \code{data.connection} is provided, user objects will also be collected and inserted.
#' 
#' @param screen_name character single screen_name.  This is only used is \code{user_id} is missing.
#' @param user_id numeric or character single user_id.
#' @param data.connection a DBI connection to an RSQLite \code{\link{twitter_database}}.
#' @param authentication.vector character vector containing authentication tokens and secrets.
#' @param stringify_ids logical indicating whether to return user_ids as strings.
#' @param tweet_mode character either 'extended' for full_text statuses or 'compat' for
#' 140 character compatability.  This parameter has no effect if no \code{data.connection} is provided.
#' @param last.query.time POSIXct time of last query executed.  If not provided, a one minute delay will 
#' be added before the first query to prevent exceeding the Twitter rate limit.
#' @param ... other named parameters passed to \code{\link{insert_users}}.
#'
#' @return character collected \code{user_id} if \code{data.connection} is supplied, otherwise a character or numeric vector of user_ids.
#'
#' @seealso \code{\link{followers_ids}}, \code{\link{insert_followers}}, \code{\link{twitter_database}}
#' @export
#' @examples
#'
#' \dontrun{
#' auth.vector <- authorize_IT()
#'
#' dma.followers <- followers_ids_recursive(
#'   "Nature_Island",
#'   authentication.vector = auth.vector
#' )
#'
#' cat(
#'   dma.followers[1:20]
#' )
#' }
followers_ids_recursive <- function(
  screen_name,
  user_id,
  data.connection,
  authentication.vector,
  stringify_ids = TRUE,
  tweet_mode = 'extended',
  last.query.time = NULL,
  ...
){
  if(missing(authentication.vector)){
    if(!exists('auth.vector')){
      stop("No authentication tokens found!")
    } else {
      authentication.vector <- auth.vector
    }
  }
  if(is.null(last.query.time)){
    last.query.time <- as.POSIXct(Sys.time())
  }
  fails <- 0
  if(missing(data.connection)){
    users <- NULL
  }
  cursor <- NULL
  if(missing(user_id)){
    Sys.sleep(1)
    user.request <- user_show(
      screen_name = screen_name[1],
      authentication.vector = authentication.vector
    )
    user_id <- user.request$id_str
  } else {
    user.request <- user_show(
      user_id = user_id,
      authentication.vector = authentication.vector
    )
  }
  if(!user.request$protected && (user.request$followers_count > 0)){
    repeat{
      Sys.sleep(
        max(
          c(
            0,
            60-as.numeric(
              difftime(
                as.POSIXct(
                  Sys.time()
                ),
                last.query.time,
                units = 'secs'
              )
            )
          )
        )
      )
      request.result <- followers_ids(
        user_id = user_id[1],
        authentication.vector = authentication.vector,
        cursor = cursor,
        stringify_ids = stringify_ids,
        count = 5000
      )
      last.query.time <- as.POSIXct(Sys.time())
      if(class(request.result) == "response"){
        fails <- fails + 1
        if(fails < 3){
          warning(sprintf("HTTP Request returned %i status (%s).",request.result$status,httr::content(request.result)))
        } else {
          stop(sprintf("Three consecutive HTTP Errors!  HTTP Request returned %i status (%s).",request.result$status,httr::content(request.result)))
        }
      } else {
        fails <- 0
        if(missing(data.connection)){
          users <- c(users,as.character(request.result$ids))
        } else {
          if(length(request.result$ids)>0){
            user_lookup_recursive(
              user_id = as.character(request.result$ids),
              data.connection = data.connection,
              authentication.vector = authentication.vector,
              tweet_mode = tweet_mode,
              ...
            )
            insert_followers(
              data.connection,
              user_id,
              request.result$ids
            )
          }
        }
        cursor <- request.result$next_cursor_str
        if(cursor=="0"){
          break
        }
      }
    }
    if(missing(data.connection)){
      return(users)
    } else {
      return(last.query.time)
    }
  } else {
    if(user.request$protected){
      cat(sprintf("@%s account is protected.\n",user.request$screen_name))
    }
    if(user.request$followers_count==0){
      cat(sprintf("@%s has no followers",user.request$screen_name))
    }
    if(missing(data.connection)){
      return(NULL)
    } else {
      return(last.query.time)
    }
  }
}



#' Recursively Get Friends IDs
#'
#' Recursively call the Friends IDs API to get all friends IDs
#'
#' This function calls recursively calls the
#' friends/list endpoint (see
#' \href{https://developer.twitter.com/en/docs/accounts-and-users/follow-search-get-users/api-reference/get-friends-ids}{Friends IDs API Documentation}).
#' A user_id or screen_name is required.  If an authentication vector is not
#' provided and there is no globally defined \code{auth.vector}, an error is returned.  A *one minute* delay
#' is included for each query in order to avoid exceeding the rate limit.
#' If a \code{data.connection} is provided, user objects will also be collected and inserted.
#'
#' @param screen_name character single screen_name.  This is only used is \code{user_id} is missing.
#' @param user_id numeric or character single user_id.
#' @param data.connection a DBI connection to an RSQLite \code{\link{twitter_database}}.
#' @param authentication.vector character vector containing authentication tokens and secrets.
#' @param stringify_ids logical indicating whether to return user_ids as strings.
#' @param tweet_mode character either 'extended' for full_text statuses or 'compat' for
#' 140 character compatability. This parameter has no effect if no \code{data.connection} is provided.
#' @param last.query.time POSIXct time of last query executed.  If not provided, a one minute delay will 
#' be added before the first query to prevent exceeding the Twitter rate limit.
#' @param ... other named parameters passed to \code{\link{insert_users}}.
#'
#' @return character collected \code{user_id} if \code{data.connection} is supplied, otherwise a character or numeric vector of user_ids.
#'
#' @seealso \code{\link{friends_ids}}, \code{\link{insert_friends}}, \code{\link{twitter_database}}
#' @export
#' @examples
#'
#' \dontrun{
#' auth.vector <- authorize_IT()
#'
#' dma.friends <- friends_ids_recursive(
#'   "Nature_Island",
#'   authentication.vector = auth.vector
#' )
#'
#' cat(
#'   dma.friends[1:20]
#' )
#' }
friends_ids_recursive <- function(
  screen_name,
  user_id,
  data.connection,
  authentication.vector,
  stringify_ids = TRUE,
  tweet_mode = 'extended',
  last.query.time = NULL,
  ...
){
  if(missing(authentication.vector)){
    if(!exists('auth.vector')){
      stop("No authentication tokens found!")
    } else {
      authentication.vector <- auth.vector
    }
  }
  if(is.null(last.query.time)){
    last.query.time <- as.POSIXct(Sys.time())
  }
  fails <- 0
  if(missing(data.connection)){
    users <- NULL
  }
  cursor <- NULL
  if(missing(user_id)){
    Sys.sleep(1)
    user.request <- user_show(
      screen_name = screen_name[1],
      authentication.vector = authentication.vector
    )
    user_id <- user.request$id_str
  } else {
    user.request <- user_show(
      user_id = user_id,
      authentication.vector = authentication.vector
    )
  }
  if(!user.request$protected && (user.request$friends_count > 0)){
    repeat{
      Sys.sleep(
        max(
          c(
            0,
            60-as.numeric(
              difftime(
                as.POSIXct(
                  Sys.time()
                ),
                last.query.time,
                units = 'secs'
              )
            )
          )
        )
      )
      request.result <- friends_ids(
        user_id = user_id[1],
        authentication.vector = authentication.vector,
        cursor = cursor,
        stringify_ids = stringify_ids,
        count = 5000
      )
      last.query.time <- as.POSIXct(Sys.time())
      if(class(request.result) == "response"){
        fails <- fails + 1
        if(fails < 3){
          warning(sprintf("HTTP Request returned %i status (%s).",request.result$status,httr::content(request.result)))
        } else {
          stop(sprintf("Three consecutive HTTP Errors!  HTTP Request returned %i status (%s).",request.result$status,httr::content(request.result)))
        }
      } else {
        fails <- 0
    if(missing(data.connection)){
          users <- c(users,as.character(request.result$ids))
        } else {
          if(length(request.result$ids) > 0){
            user_lookup_recursive(
              user_id = as.character(request.result$ids),
              data.connection = data.connection,
              authentication.vector = authentication.vector,
              tweet_mode = tweet_mode,
              ...
            )
            insert_friends(
              data.connection,
              user_id,
              request.result$ids
            )
          }
        }
        cursor <- request.result$next_cursor_str
        if(cursor=="0"){
          break
        }
      }
    }
    if(missing(data.connection)){
      return(users)
    } else {
      return(last.query.time)
    }
  } else {
    if(user.request$protected){
      cat(sprintf("@%s account is protected.\n",user.request$screen_name))
    }
    if(user.request$friends_count==0){
      cat(sprintf("@%s has no friends",user.request$screen_name))
    }
    if(missing(data.connection)){
      return(NULL)
    } else {
      return(last.query.time)
    }
  }
}
