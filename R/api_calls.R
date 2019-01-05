#' Access Twitter REST Endpoint
#'
#' Send an authenticated HTTPS request to a Twitter REST API endpoint.
#'
#' This is the workhorse function of this package, as it is called by every 
#' function that queries the Twitter API.  It is useful for accessing API endpoints
#' that are not provided in this package.
#' The authentication vector must be provided.  It can be created using the
#' \code{\link{authorize_app}} or \code{\link{authorize_IT}} functions, or can be read from
#' a locally saved location if one has already been created.  This function
#' formulates an authenticated and signed HTTPS request and sends it to the
#' Twitter API endpoint specified.  Parameters vary among the endpoints; user
#' familiarity with the endpoint is assumed.
#'
#' @param auth.vector character authentication vector (see Details).
#' @param endpoint.url character Twitter REST API endpoint URL.  See
#' \href{https://developer.twitter.com/en/docs/api-reference-index}{Twitter Documentation} for details, and
#' \href{https://developer.twitter.com/en/docs/tweets/search/api-reference/get-search-tweets.html}{Twitter Search API} for an example of where to find these URLs.
#' @param query.param.list named list giving the query parameters.  Names should
#' only correspond to parameters provided in the
#' \href{https://developer.twitter.com/en/docs/api-reference-index}{Twitter API Documentation}
#' specific to the endpoint being called.  Any parameter values with length greater than
#' one will be collapsed into a single, comma-separated string.
#'
#' @return httr response object.
#'
#' @seealso \code{\link{authorize_app}},\code{\link{authorize_IT}}
#' @export
#' @examples
#' ## Not run: read authentication vector from file.
#' # auth.vector <- load("auth-vector.RData")
#'
#' # resource.url <- "https://api.twitter.com/1.1/statuses/user_timeline.json"
#' # params <- list(screen_name="realDonaldTrump",count="200",tweet_mode="extended")
#' # httr.response <- twitter_request(auth.vector,resource.url,params)
#'
twitter_request<-function(auth.vector,base.url,query.param.list=list()){
  consumer.token<-as.character(auth.vector['consumer.token'])
  consumer.secret<-as.character(auth.vector['consumer.secret'])
  access.token<-as.character(auth.vector['access.token'])
  access.secret<-as.character(auth.vector['access.secret'])
  query.params<-matrix(nrow=0,ncol=2)
  if(length(query.param.list) > 0){
    for(i in 1:length(query.param.list)){
      query.params <- rbind(
        query.params,
        matrix(
          c(
            names(query.param.list)[i],
            paste(query.param.list[[i]],collapse=",")
          ),
          nrow=1
        )
      )
    }
  }
  oauth.params<-matrix(nrow=7,ncol=2)
  colnames(query.params)<-c("key","value")
  colnames(oauth.params)<-c("key","value")
  oauth.params[1,]<-c('oauth_consumer_key',consumer.token)
  oauth.params[2,]<-c('oauth_nonce',gen_oauth_nonce())
  oauth.params[3,]<-c('oauth_signature',NA)
  oauth.params[4,]<-c('oauth_signature_method','HMAC-SHA1')
  oauth.params[5,]<-c('oauth_timestamp',as.character(as.integer(as.POSIXct(Sys.time()))))
  oauth.params[6,]<-c('oauth_token',access.token)
  oauth.params[7,]<-c('oauth_version','1.0')
  all.params<-rbind(query.params,oauth.params[c(1:2,4:7),])
  oauth.sign<-gen_oauth_signature('GET',base.url,all.params,consumer.secret,access.secret)
  oauth.params[3,2]<-oauth.sign
  oauth.str<-"OAuth "
  for(i in 1:dim(oauth.params)[1]){
    if(i>1){
      oauth.str<-paste(oauth.str,", ",sep="")
    }
    oauth.str<-paste(oauth.str,utils::URLencode(oauth.params[i,1],reserved=TRUE),sep="")
    oauth.str<-paste(oauth.str,'="',sep='')
    oauth.str<-paste(oauth.str,utils::URLencode(oauth.params[i,2],reserved=TRUE),sep="")
    oauth.str<-paste(oauth.str,'"',sep="")
  }
  mod<-''
  h<-httr::add_headers(Authorization=oauth.str)
  ## cat(h$headers)
  if(dim(query.params)[1]>0){
    for(i in 1:dim(query.params)[1]){
      if (i > 1){
        mod<-paste(mod,'&',sep="")
      }
      mod<-paste(mod,utils::URLencode(query.params[i,1],reserved=TRUE),sep="")
      mod<-paste(mod,'=',sep='')
      mod<-paste(mod,utils::URLencode(query.params[i,2],reserved=TRUE),sep="")
    }
    x<-httr::GET(base.url,
                 query=mod,
                 httr::add_headers(Authorization=oauth.str)
    )
  } else {
    x<-httr::GET(base.url,
                 httr::add_headers(Authorization=oauth.str)
    )
  }
  return(x)
}



#' Search Twitter
#'
#' Access Twitter Statuses Using the Search API
#'
#' This function calls \code{\link{twitter_request}} using the Twitter
#' standard search endpoint (see
#' \href{https://developer.twitter.com/en/docs/tweets/search/api-reference/get-search-tweets}{Search API Documentation}).
#' A query string (\code{q}) is required.  If an authentication vector is not
#' provided and there is no globally defined \code{auth.vector}, an error is returned.
#'
#' @param q character search query string.
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
#' @param count numeric number of tweets to return per page, up to a maximum of 100.
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
#'
#' @return On success, an R list representation of the returned JSON object.  On failure, a warning
#' is thrown and the http response is returned, if any.
#'
#' @seealso \code{\link{twitter_request}}, \code{\link{authorize_app}}
#' @export
#' @examples
#'
#' ## Not run: authenticate
#' # auth.vector <- authorize_IT()
#'
#' # search.json <- search_tweets(
#' #   "#myfirsttweet",
#' #   auth.vector
#' # )
#'
#' cat(json.list$statuses[[1]]$full_text)
#'
search_tweets <- function(
  q,
  authentication.vector,
  geocode = NULL,
  lang = NULL,
  locale = NULL,
  result_type = 'recent',
  count = 100,
  until = NULL,
  since_id = NULL,
  max_id = NULL,
  include_entities = TRUE,
  tweet_mode = 'extended'
){
  if(missing(authentication.vector)){
    if(!exists('auth.vector')){
      # cat(ls())
      stop("No authentication tokens found!")
    } else {
      authentication.vector <- auth.vector
    }
  }
  params <- list(
    geocode=geocode,
    lang=lang,
    locale=locale,
    result_type=result_type,
    count=count,
    until=until,
    since_id=since_id,
    max_id=max_id,
    include_entities=include_entities,
    tweet_mode = tweet_mode
  )
  query.param.list <- list(q=q)
  for(param.index in 1:length(params)){
    if(!is.null(params[[param.index]])){
      query.param.list[[names(params)[param.index]]] <- tolower(as.character(params[[param.index]]))
    }
  }
  result <- twitter_request(
    authentication.vector,
    "https://api.twitter.com/1.1/search/tweets.json",
    query.param.list
  )
  if(result$status == 200){
    return(httr::content(result))
  } else {
    warning(sprintf("HTTP request returned %i status\n",result$status))
    return(result)
  }
}


#' Get User Timeline
#'
#' Access Twitter Statuses Using the User Timeline API
#'
#' This function calls \code{\link{twitter_request}} using the Twitter
#' statuses/user_timeline endpoint (see
#' \href{https://developer.twitter.com/en/docs/tweets/search/api-reference/get-statuses-user_timeline}{user_timeline API Documentation}).
#' A user_id or screen_name is required.  If an authentication vector is not
#' provided and there is no globally defined \code{auth.vector}, an error is returned.
#'
#' @param screen_name character screen_name.  This is only used is \code{user_id} is missing.
#' @param user_id numeric or character user_id.
#' @param authentication.vector character vector containing authentication tokens and secrets.
#' See \code{\link{authorize_app}} and \code{\link{authorize_IT}}.
#' @param count numeric number of tweets to return per page, up to a maximum of 100.
#' @param since_id numeric or character status_id.  If supplied, only tweets with status_ids greater
#' than this value (and therefore more recent) will be returned.
#' @param max_id numeric or character status_id.  If suppled, only tweets with status_ids less than or
#' equal to this value (and therefore no more recent) will be returned.
#' @param trim_user logical indicating whether to remove the user object from each status.
#' @param exlude_replies logical indicating whether to filter user replies to other statuses
#' from the results.
#' @param include_rts logical indicating whether to include retweets in the results.
#' @param tweet_mode character either 'extended' for full_text statuses or 'compat' for
#' 140 character compatability.
#'
#' @return On success, an R list representation of the returned JSON object.  On failure, a warning
#' is thrown and the http response is returned, if any.
#'
#' @seealso \code{\link{twitter_request}}, \code{\link{authorize_app}}
#' @export
#' @examples
#'
#' ## Not run: authenticate
#' # auth.vector <- authorize_IT()
#'
#' # user.statuses <- user_timeline(
#' #   "realDonaldTrump",
#' #   authentication.vector = auth.vector
#' # )
#'
#' # cat(user.statuses[[1]]$full_text)
#'
user_timeline <- function(
  screen_name,
  user_id,
  authentication.vector,
  count = 200,
  since_id = NULL,
  max_id = NULL,
  trim_user = FALSE,
  exclude_replies = FALSE,
  include_rts = TRUE,
  tweet_mode = 'extended'
){
  if(missing(authentication.vector)){
    if(!exists('auth.vector')){
      # cat(ls())
      stop("No authentication tokens found!")
    } else {
      authentication.vector <- auth.vector
    }
  }
  params <- list(
    count=count,
    since_id=since_id,
    max_id=max_id,
    trim_user = trim_user,
    exclude_replies = exclude_replies,
    include_rts = include_rts,
    tweet_mode = tweet_mode
  )
  if(missing(user_id)){
    if(missing(screen_name)){
      stop("user_id or screen_name must be provided.")
    } else {
      query.param.list <- list(
        screen_name = screen_name[1]
      )
    }
  } else {
    query.param.list <- list(
      user_id = as.character(user_id[1])
    )
  }
  for(param.index in 1:length(params)){
    if(!is.null(params[[param.index]])){
      query.param.list[[names(params)[param.index]]] <- tolower(as.character(params[[param.index]]))
    }
  }
  result <- twitter_request(
    authentication.vector,
    "https://api.twitter.com/1.1/statuses/user_timeline.json",
    query.param.list
  )
  if(result$status == 200){
    return(httr::content(result))
  } else {
    warning(sprintf("HTTP request returned %i status\n",result$status))
    return(result)
  }
}


#' Lookup Users
#'
#' Get Twitter User Objects from the User Lookup API
#'
#' This function calls \code{\link{twitter_request}} using the Twitter
#' users/lookup endpoint (see
#' \href{https://developer.twitter.com/en/docs/accounts-and-users/follow-search-get-users/api-reference/get-users-lookup}{User Lookup API Documentation}).
#' A user_id or screen_name vector is required.  If an authentication vector is not
#' provided and there is no globally defined \code{auth.vector}, an error is returned.
#'
#' @param screen_name character vector of up to 100 screen_names.  This is only used is \code{user_id} is missing.
#' @param user_id numeric or character vector of up to 100 user_ids.
#' @param authentication.vector character vector containing authentication tokens and secrets.
#' See \code{\link{authorize_app}} and \code{\link{authorize_IT}}.
#' @param include_entities logical indicating whether to include profile entities (e.g., urls)
#' as separate nodes in the returned json object.
#' @param tweet_mode character either 'extended' for full_text statuses or 'compat' for
#' 140 character compatability.
#'
#' @return On success, an R list representation of the returned JSON object.  On failure, a warning
#' is thrown and the http response is returned, if any.
#'
#' @seealso \code{\link{twitter_request}}, \code{\link{authorize_app}}, \code{\link{user_show}}
#' @export
#' @examples
#'
#' ## Not run: authenticate
#' # auth.vector <- authorize_IT()
#'
#' # user.list <- user_lookup(
#' #   c(
#' #   "realDonaldTrump",
#' #   "SouthComWatch",
#' #   "Stephenfaller"
#' #   ),
#' #   authentication.vector = auth.vector
#' # )
#'
#' # cat(user.list[[1]]$description)
#'
user_lookup <- function(
  screen_name,
  user_id,
  authentication.vector,
  include_entities = TRUE,
  tweet_mode = 'extended'
){
  if(missing(authentication.vector)){
    if(!exists('auth.vector')){
      # cat(ls())
      stop("No authentication tokens found!")
    } else {
      authentication.vector <- auth.vector
    }
  }
  params <- list(
    include_entities = include_entities,
    tweet_mode = tweet_mode
  )
  if(missing(user_id)){
    if(missing(screen_name)){
      stop("user_id or screen_name must be provided.")
    } else {
      query.param.list <- list(
        screen_name = screen_name[1:min(c(length(screen_name),100))]
      )
    }
  } else {
    query.param.list <- list(
      user_id = as.character(user_id[1:min(c(length(user_id),100))])
    )
  }
  for(param.index in 1:length(params)){
    if(!is.null(params[[param.index]])){
      query.param.list[[names(params)[param.index]]] <- tolower(as.character(params[[param.index]]))
    }
  }
  result <- twitter_request(
    authentication.vector,
    "https://api.twitter.com/1.1/users/lookup.json",
    query.param.list
  )
  if(result$status == 200){
    return(httr::content(result))
  } else {
    warning(sprintf("HTTP request returned %i status\n",result$status))
    return(result)
  }
}


#' Show User
#'
#' Get a Single Twitter User Object from the Show User API
#'
#' This function calls \code{\link{twitter_request}} using the Twitter
#' users/show endpoint (see
#' \href{https://developer.twitter.com/en/docs/accounts-and-users/follow-search-get-users/api-reference/get-users-show}{User/Show API Documentation}).
#' A user_id or screen_name is required.  If an authentication vector is not
#' provided and there is no globally defined \code{auth.vector}, an error is returned.
#' This function is similar to the \code{\link{user_lookup}} function, except that it only takes and
#' returns a single user object.
#'
#' @param screen_name character single screen_name.  This is only used is \code{user_id} is missing.
#' @param user_id numeric or character single user_id.
#' @param authentication.vector character vector containing authentication tokens and secrets.
#' See \code{\link{authorize_app}} and \code{\link{authorize_IT}}.
#' @param include_entities logical indicating whether to include profile entities (e.g., urls)
#' as separate nodes in the returned json object.
#' @param tweet_mode character either 'extended' for full_text statuses or 'compat' for
#' 140 character compatability.
#'
#' @return On success, an R list representation of the returned JSON object.  On failure, a warning
#' is thrown and the http response is returned, if any.
#'
#' @seealso \code{\link{twitter_request}}, \code{\link{authorize_app}}, \code{\link{user_lookup}}
#' @export
#' @examples
#'
#' ## Not run: authenticate
#' # auth.vector <- authorize_IT()
#'
#' # potus.obj <- user_show(
#' #   "realDonaldTrump",
#' #   authentication.vector = auth.vector
#' # )
#'
#' # cat(potus$description)
#'
user_show <- function(
  screen_name,
  user_id,
  authentication.vector,
  include_entities = TRUE,
  tweet_mode = 'extended'
){
  if(missing(authentication.vector)){
    if(!exists('auth.vector')){
      # cat(ls())
      stop("No authentication tokens found!")
    } else {
      authentication.vector <- auth.vector
    }
  }
  params <- list(
    include_entities = include_entities,
    tweet_mode = tweet_mode
  )
  if(missing(user_id)){
    if(missing(screen_name)){
      stop("user_id or screen_name must be provided.")
    } else {
      query.param.list <- list(
        screen_name = screen_name[1]
      )
    }
  } else {
    query.param.list <- list(
      user_id = as.character(user_id[1])
    )
  }
  for(param.index in 1:length(params)){
    if(!is.null(params[[param.index]])){
      query.param.list[[names(params)[param.index]]] <- tolower(as.character(params[[param.index]]))
    }
  }
  result <- twitter_request(
    authentication.vector,
    "https://api.twitter.com/1.1/users/show.json",
    query.param.list
  )
  if(result$status == 200){
    return(httr::content(result))
  } else {
    warning(sprintf("HTTP request returned %i status\n",result$status))
    return(result)
  }
}


#' Lookup Statuses
#'
#' Get Twitter Status Objects from the Status Lookup API
#'
#' This function calls \code{\link{twitter_request}} using the Twitter
#' statuses/lookup endpoint (see
#' \href{https://developer.twitter.com/en/docs/tweets/post-and-engage/api-reference/get-statuses-lookup}{Status Lookup API Documentation}).
#' A status_id vector is required.  If an authentication vector is not
#' provided and there is no globally defined \code{auth.vector}, an error is returned.
#'
#' @param status_id numeric or character vector of up to 100 status_ids.
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
#'
#' @return On success, an R list representation of the returned JSON object.  On failure, a warning
#' is thrown and the http response is returned, if any.
#'
#' @seealso \code{\link{twitter_request}}, \code{\link{authorize_app}}, \code{\link{status_show}}
#' @export
#' @examples
#'
#' ## Not run: authenticate
#' # auth.vector <- authorize_IT()
#'
#' # status.list <- status_lookup(
#' #   c(
#' #    "1059843699298000897",
#' #    "1059843147906322432",
#' #    "1059842917769068544",
#' #    "1059842858797223937"
#' #   ),
#' #   authentication.vector = auth.vector
#' # )
#'
#' # cat(status.list[[1]]$full_text)
#'
status_lookup <- function(
  status_id,
  authentication.vector,
  include_entities = TRUE,
  trim_user = FALSE,
  map = NULL,
  include_ext_alt_text = NULL,
  include_card_uri = NULL,
  tweet_mode = 'extended'
){
  if(missing(authentication.vector)){
    if(!exists('auth.vector')){
      # cat(ls())
      stop("No authentication tokens found!")
    } else {
      authentication.vector <- auth.vector
    }
  }
  params <- list(
    include_entities = include_entities,
    trim_user = trim_user,
    map = map,
    include_ext_alt_text = include_ext_alt_text,
    include_card_uri = include_card_uri,
    tweet_mode = tweet_mode
  )
  if(missing(status_id)){
    stop("status_id must be provided.")
  } else {
    query.param.list <- list(
      id = as.character(status_id[1:min(c(length(status_id),100))])
    )
  }
  for(param.index in 1:length(params)){
    if(!is.null(params[[param.index]])){
      query.param.list[[names(params)[param.index]]] <- tolower(as.character(params[[param.index]]))
    }
  }
  result <- twitter_request(
    authentication.vector,
    "https://api.twitter.com/1.1/statuses/lookup.json",
    query.param.list
  )
  if(result$status == 200){
    return(httr::content(result))
  } else {
    warning(sprintf("HTTP request returned %i status\n",result$status))
    return(result)
  }
}


#' Show Status
#'
#' Get a Single Twitter Status Object from the Show Status API
#'
#' This function calls \code{\link{twitter_request}} using the Twitter
#' statuses/show endpoint (see
#' \href{https://developer.twitter.com/en/docs/tweets/post-and-engage/api-reference/get-statuses-show-id}{Show Status API Documentation}).
#' A user_id or screen_name is required.  If an authentication vector is not
#' provided and there is no globally defined \code{auth.vector}, an error is returned.
#' This function is similar to the \code{\link{status_lookup}} function, except that it only takes and
#' returns a single status object.
#'
#' @param status_id numeric or character single status_id.
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
#'
#' @return On success, an R list representation of the returned JSON object.  On failure, a warning
#' is thrown and the http response is returned, if any.
#'
#' @seealso \code{\link{twitter_request}}, \code{\link{authorize_app}}, \code{\link{status_lookup}}
#' @export
#' @examples
#'
#' ## Not run: authenticate
#' # auth.vector <- authorize_IT()
#'
#' # status.obj <- status_show(
#' #   "1059843699298000897",
#' #   authentication.vector = auth.vector
#' # )
#'
#' # cat(status.obj$full_text)
#'
status_show <- function(
  status_id,
  authentication.vector,
  include_entities = TRUE,
  trim_user = FALSE,
  map = NULL,
  include_ext_alt_text = NULL,
  include_card_uri = NULL,
  tweet_mode = 'extended'
){
  if(missing(authentication.vector)){
    if(!exists('auth.vector')){
      # cat(ls())
      stop("No authentication tokens found!")
    } else {
      authentication.vector <- auth.vector
    }
  }
  params <- list(
    include_entities = include_entities,
    trim_user = trim_user,
    map = map,
    include_ext_alt_text = include_ext_alt_text,
    include_card_uri = include_card_uri,
    tweet_mode = tweet_mode
  )
  if(missing(status_id)){
    stop("status_id must be provided.")
  } else {
    query.param.list <- list(
      id = as.character(status_id[1])
    )
  }
  for(param.index in 1:length(params)){
    if(!is.null(params[[param.index]])){
      query.param.list[[names(params)[param.index]]] <- tolower(as.character(params[[param.index]]))
    }
  }
  result <- twitter_request(
    authentication.vector,
    "https://api.twitter.com/1.1/statuses/show.json",
    query.param.list
  )
  if(result$status == 200){
    return(httr::content(result))
  } else {
    warning(sprintf("HTTP request returned %i status\n",result$status))
    return(result)
  }
}


#' User Search
#'
#' Get a Twitter User Objects from the User Search API
#'
#' This function calls \code{\link{twitter_request}} using the Twitter
#' users/search endpoint (see
#' \href{https://developer.twitter.com/en/docs/accounts-and-users/follow-search-get-users/api-reference/get-users-search}{User Search API Documentation}).
#' A query (\code{q}) is required.  If an authentication vector is not
#' provided and there is no globally defined \code{auth.vector}, an error is returned.
#'
#' @param q character search query.
#' @param authentication.vector character vector containing authentication tokens and secrets.
#' See \code{\link{authorize_app}} and \code{\link{authorize_IT}}.
#' @param page numeric page of results to retrieve.
#' @param count numeric number of results per page, up to a maximum of 20.
#' @param include_entities logical indicating whether to include profile entities (e.g., urls)
#' as separate nodes in the returned json object.
#' @param tweet_mode character either 'extended' for full_text statuses or 'compat' for
#' 140 character compatability.
#'
#' @return On success, an R list representation of the returned JSON object.  On failure, a warning
#' is thrown and the http response is returned, if any.
#'
#' @seealso \code{\link{twitter_request}}, \code{\link{authorize_app}}
#' @export
#' @examples
#'
#' ## Not run: authenticate
#' # auth.vector <- authorize_IT()
#'
#' # searched.users <- user_search(
#' #   "Donald Trump",
#' #   authentication.vector = auth.vector
#' # )
#'
#' # cat(searched.users[[3]]$description)
#'
user_search <- function(
  q,
  authentication.vector,
  page = NULL,
  count = 20,
  include_entities = TRUE,
  tweet_mode = 'extended'
){
  if(missing(authentication.vector)){
    if(!exists('auth.vector')){
      # cat(ls())
      stop("No authentication tokens found!")
    } else {
      authentication.vector <- auth.vector
    }
  }
  params <- list(
    page = page,
    count = count,
    include_entities = include_entities,
    tweet_mode = tweet_mode
  )
  if(missing(q)){
      stop("Search query criteria must be provided.")
  } else {
    query.param.list <- list(
      q = as.character(q[1])
    )
  }
  for(param.index in 1:length(params)){
    if(!is.null(params[[param.index]])){
      query.param.list[[names(params)[param.index]]] <- tolower(as.character(params[[param.index]]))
    }
  }
  result <- twitter_request(
    authentication.vector,
    "https://api.twitter.com/1.1/users/search.json",
    query.param.list
  )
  if(result$status == 200){
    return(httr::content(result))
  } else {
    warning(sprintf("HTTP request returned %i status\n",result$status))
    return(result)
  }
}


#' Get Followers IDs
#'
#' Get a User's Followers' IDs using the Followers IDs API
#'
#' This function calls \code{\link{twitter_request}} using the Twitter
#' followers/ids endpoint (see
#' \href{https://developer.twitter.com/en/docs/accounts-and-users/follow-search-get-users/api-reference/get-followers-ids}{Followers IDs API Documentation}).
#' A user_id or screen_name is required.  If an authentication vector is not
#' provided and there is no globally defined \code{auth.vector}, an error is returned.
#'
#' @param screen_name character single screen_name.  This is only used is \code{user_id} is missing.
#' @param user_id numeric or character single user_id.
#' @param cursor numeric or character cursor value for controlling pagination.
#' @param stringify_ids logical indicating whether to return user_ids as strings.
#' @param count numeric number of user_ids per page, up to 5000.
#'
#' @return On success, an R list representation of the returned JSON object.  On failure, a warning
#' is thrown and the http response is returned, if any.
#'
#' @seealso \code{\link{twitter_request}}, \code{\link{authorize_app}}, \code{\link{followers_list}},
#' \code{\link{friends_ids}}
#' @export
#' @examples
#'
#' ## Not run: authenticate
#' # auth.vector <- authorize_IT()
#'
#' # potus.followers <- followers_ids(
#' #   "realDonaldTrump",
#' #   authentication.vector = auth.vector
#' # )
#'
#' # cat(paste(potus.followers$ids[1:20],collapse=","))
#'
followers_ids <- function(
  screen_name,
  user_id,
  authentication.vector,
  cursor = NULL,
  stringify_ids = TRUE,
  count = 5000
){
  if(missing(authentication.vector)){
    if(!exists('auth.vector')){
      # cat(ls())
      stop("No authentication tokens found!")
    } else {
      authentication.vector <- auth.vector
    }
  }
  params <- list(
    cursor = cursor,
    stringify_ids = stringify_ids,
    count = count
  )
  if(missing(user_id)){
    if(missing(screen_name)){
      stop("user_id or screen_name must be provided.")
    } else {
      query.param.list <- list(
        screen_name = screen_name[1]
      )
    }
  } else {
    query.param.list <- list(
      user_id = as.character(user_id[1])
    )
  }
  for(param.index in 1:length(params)){
    if(!is.null(params[[param.index]])){
      query.param.list[[names(params)[param.index]]] <- tolower(as.character(params[[param.index]]))
    }
  }
  result <- twitter_request(
    authentication.vector,
    "https://api.twitter.com/1.1/followers/ids.json",
    query.param.list
  )
  if(result$status == 200){
    return(httr::content(result))
  } else {
    warning(sprintf("HTTP request returned %i status\n",result$status))
    return(result)
  }
}


#' Get Friends IDs
#'
#' Get a User's Friends' IDs using the Friends IDs API
#'
#' This function calls \code{\link{twitter_request}} using the Twitter
#' friends/ids endpoint (see
#' \href{https://developer.twitter.com/en/docs/accounts-and-users/follow-search-get-users/api-reference/get-friends-ids}{Friends IDs API Documentation}).
#' A user_id or screen_name is required.  If an authentication vector is not
#' provided and there is no globally defined \code{auth.vector}, an error is returned.
#'
#' @param screen_name character single screen_name.  This is only used is \code{user_id} is missing.
#' @param user_id numeric or character single user_id.
#' @param cursor numeric or character cursor value for controlling pagination.
#' @param stringify_ids logical indicating whether to return user_ids as strings.
#' @param count numeric number of user_ids per page, up to 5000.
#'
#' @return On success, an R list representation of the returned JSON object.  On failure, a warning
#' is thrown and the http response is returned, if any.
#'
#' @seealso \code{\link{twitter_request}}, \code{\link{authorize_app}}, \code{\link{friends_list}},
#' \code{\link{followers_ids}}
#' @export
#' @examples
#'
#' ## Not run: authenticate
#' # auth.vector <- authorize_IT()
#'
#' # potus.friends <- friends_ids(
#' #   "realDonaldTrump",
#' #   authentication.vector = auth.vector
#' # )
#'
#' # cat(paste(potus.friends$ids[1:20],collapse=","))
#'
friends_ids <- function(
  screen_name,
  user_id,
  authentication.vector,
  cursor = NULL,
  stringify_ids = TRUE,
  count = 5000
){
  if(missing(authentication.vector)){
    if(!exists('auth.vector')){
      # cat(ls())
      stop("No authentication tokens found!")
    } else {
      authentication.vector <- auth.vector
    }
  }
  params <- list(
    cursor = cursor,
    stringify_ids = stringify_ids,
    count = count
  )
  if(missing(user_id)){
    if(missing(screen_name)){
      stop("user_id or screen_name must be provided.")
    } else {
      query.param.list <- list(
        screen_name = screen_name[1]
      )
    }
  } else {
    query.param.list <- list(
      user_id = as.character(user_id[1])
    )
  }
  for(param.index in 1:length(params)){
    if(!is.null(params[[param.index]])){
      query.param.list[[names(params)[param.index]]] <- tolower(as.character(params[[param.index]]))
    }
  }
  result <- twitter_request(
    authentication.vector,
    "https://api.twitter.com/1.1/friends/ids.json",
    query.param.list
  )
  if(result$status == 200){
    return(httr::content(result))
  } else {
    warning(sprintf("HTTP request returned %i status\n",result$status))
    return(result)
  }
}


#' Get Followers List
#'
#' Get a User's Followers' User Objects Using the Followers List API
#'
#' This function calls \code{\link{twitter_request}} using the Twitter
#' followers/list endpoint (see
#' \href{https://developer.twitter.com/en/docs/accounts-and-users/follow-search-get-users/api-reference/get-followers-list}{Followers List API Documentation}).
#' A user_id or screen_name is required.  If an authentication vector is not
#' provided and there is no globally defined \code{auth.vector}, an error is returned.
#'
#' @param screen_name character single screen_name.  This is only used is \code{user_id} is missing.
#' @param user_id numeric or character single user_id.
#' @param cursor numeric or character cursor value for controlling pagination.
#' @param count numeric number of user_ids per page, up to 200.
#' @param skip_status logical indicating whether to exclude the user status from the returned
#' follower user objects.
#' @param include_user_entities logical indicating whether to include profile entities (e.g. profile url)
#' in the returned follower user objects.
#' @param tweet_mode character either 'extended' for full_text statuses or 'compat' for
#' 140 character compatability.
#'
#' @return On success, an R list representation of the returned JSON object.  On failure, a warning
#' is thrown and the http response is returned, if any.
#'
#' @seealso \code{\link{twitter_request}}, \code{\link{authorize_app}}, \code{\link{followers_ids}},
#' \code{\link{friends_list}}
#' @export
#' @examples
#'
#' ## Not run: authenticate
#' # auth.vector <- authorize_IT()
#'
#' # potus.followers <- followers_list(
#' #   "realDonaldTrump",
#' #   authentication.vector = auth.vector
#' # )
#'
#' # cat(
#' #   paste(
#' #     "@",
#' #     sapply(potus.followers$users[1:20],function(x) return(x$screen_name)),
#' #     ": ",
#' #     sapply(potus.followers$users[1:20],function(x) return(x$description)),
#' #     sep="",
#' #     collapse="\n"
#' #   )
#' # )
#'
followers_list <- function(
  screen_name,
  user_id,
  authentication.vector,
  cursor = NULL,
  count = 200,
  skip_status = NULL,
  include_user_entities = NULL,
  tweet_mode = 'extended'
){
  if(missing(authentication.vector)){
    if(!exists('auth.vector')){
      # cat(ls())
      stop("No authentication tokens found!")
    } else {
      authentication.vector <- auth.vector
    }
  }
  params <- list(
    cursor = cursor,
    count = count,
    skip_status = skip_status,
    include_user_entities = include_user_entities,
    tweet_mode = tweet_mode
  )
  if(missing(user_id)){
    if(missing(screen_name)){
      stop("user_id or screen_name must be provided.")
    } else {
      query.param.list <- list(
        screen_name = screen_name[1]
      )
    }
  } else {
    query.param.list <- list(
      user_id = as.character(user_id[1])
    )
  }
  for(param.index in 1:length(params)){
    if(!is.null(params[[param.index]])){
      query.param.list[[names(params)[param.index]]] <- tolower(as.character(params[[param.index]]))
    }
  }
  result <- twitter_request(
    authentication.vector,
    "https://api.twitter.com/1.1/followers/list.json",
    query.param.list
  )
  if(result$status == 200){
    return(httr::content(result))
  } else {
    warning(sprintf("HTTP request returned %i status\n",result$status))
    return(result)
  }
}


#' Get Friends List
#'
#' Get a User's Friends' User Objects Using the Friends List API
#'
#' This function calls \code{\link{twitter_request}} using the Twitter
#' friends/list endpoint (see
#' \href{https://developer.twitter.com/en/docs/accounts-and-users/follow-search-get-users/api-reference/get-friends-list}{Friends List API Documentation}).
#' A user_id or screen_name is required.  If an authentication vector is not
#' provided and there is no globally defined \code{auth.vector}, an error is returned.
#'
#' @param screen_name character single screen_name.  This is only used is \code{user_id} is missing.
#' @param user_id numeric or character single user_id.
#' @param cursor numeric or character cursor value for controlling pagination.
#' @param count numeric number of user_ids per page, up to 200.
#' @param skip_status logical indicating whether to exclude the user status from the returned
#' follower user objects.
#' @param include_user_entities logical indicating whether to include profile entities (e.g. profile url)
#' in the returned follower user objects.
#' @param tweet_mode character either 'extended' for full_text statuses or 'compat' for
#' 140 character compatability.
#'
#' @return On success, an R list representation of the returned JSON object.  On failure, a warning
#' is thrown and the http response is returned, if any.
#'
#' @seealso \code{\link{twitter_request}}, \code{\link{authorize_app}}, \code{\link{friends_ids}},
#' \code{\link{followers_list}}
#' @export
#' @examples
#'
#' ## Not run: authenticate
#' # auth.vector <- authorize_IT()
#'
#' # potus.friends <- friends_list(
#' #   "realDonaldTrump",
#' #   authentication.vector = auth.vector
#' # )
#'
#' # cat(
#' #   paste(
#' #     "@",
#' #     sapply(potus.friends$users[1:20],function(x) return(x$screen_name)),
#' #     ": ",
#' #     sapply(potus.friends$users[1:20],function(x) return(x$description)),
#' #     sep="",
#' #     collapse="\n"
#' #   )
#' # )
#'
friends_list <- function(
  screen_name,
  user_id,
  authentication.vector,
  cursor = NULL,
  count = 200,
  skip_status = NULL,
  include_user_entities = NULL,
  tweet_mode = 'extended'
){
  if(missing(authentication.vector)){
    if(!exists('auth.vector')){
      # cat(ls())
      stop("No authentication tokens found!")
    } else {
      authentication.vector <- auth.vector
    }
  }
  params <- list(
    cursor = cursor,
    count = count,
    skip_status = skip_status,
    include_user_entities = include_user_entities,
    tweet_mode = tweet_mode
  )
  if(missing(user_id)){
    if(missing(screen_name)){
      stop("user_id or screen_name must be provided.")
    } else {
      query.param.list <- list(
        screen_name = screen_name[1]
      )
    }
  } else {
    query.param.list <- list(
      user_id = as.character(user_id[1])
    )
  }
  for(param.index in 1:length(params)){
    if(!is.null(params[[param.index]])){
      query.param.list[[names(params)[param.index]]] <- tolower(as.character(params[[param.index]]))
    }
  }
  result <- twitter_request(
    authentication.vector,
    "https://api.twitter.com/1.1/friends/list.json",
    query.param.list
  )
  if(result$status == 200){
    return(httr::content(result))
  } else {
    warning(sprintf("HTTP request returned %i status\n",result$status))
    return(result)
  }
}



