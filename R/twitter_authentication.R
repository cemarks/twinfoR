## Twitter authentication functions

#' Authorize Twitter Application
#'
#' Authorize a Twitter application on a user account
#'
#' The application consumer token and consumer secret must be provided.
#' These can be obtained by the application owner from the
#' \href{https://developer.twitter.com/en/apps}{Twitter Developer} site.
#' The user must log-in to Twitter (as directed) and authorize the
#' application.  The Twitter website will return a PIN, which the user
#' must enter in R to complete the authentication.
#'
#' @section Authentication Keys:
#' The application consumer secret and user token secret are authentication passcodes
#' that should not be shared.  The authentication vector returned by this function
#' can be saved locally and reloaded for use at a later time, enabling the user
#' to skip the authentication step.
#'
#' @section Note:
#' It is easy to hit <ENTER> in the console after calling this function before
#' entering your PIN, especially when copying and pasting code; this will
#' result in an authentication failure. To avoid this 
#' mistake, enter the \code{authorize_app} line manually into the console.  Then,
#' log on to Twitter and browse to the URL provided (if this does not happen automatically),
#' authorize the app in the browser, retrieve the PIN, and enter the PIN back into the
#' R Console. 
#' 
#' @param consumer.token character Twitter application consumer token
#' (see \href{https://developer.twitter.com/en/docs/basics/authentication/overview/3-legged-oauth}{Twitter Oauth}).
#' @param consumer.secret character Twitter application consumer secret
#' (see \href{https://developer.twitter.com/en/docs/basics/authentication/overview/3-legged-oauth}{Twitter Oauth}).
#'
#' @return On successful authentication, a character vector containing the following named character strings:
#' \tabular{ll}{
#' \code{consumer.token} \tab The Twitter Application Consumer Token \cr
#' \code{consumer.secret} \tab The Twitter Application Consumer Secret \cr
#' \code{access.token} \tab The User's Access Token for the Twitter Application \cr
#' \code{access.secret} \tab The User's Access Secret for the Twitter Application.
#' }
#' On authentication failure, a warning is thrown and the failed http response object
#' is returned.
#'
#' @seealso \code{\link{twitter_request}}, \code{\link{authorize_IT}}
#' @export
#' @examples
#' \dontrun{
#' ## Consumer token and consumer secret are fake.
#' consumer.token <- "ArP09ecJ8ismgszHGuxis5p2tie6zLiLiWhgsWPbZxw3qpLSds"
#' consumer.secret <- "7N2e5hh1fYzFYfYib4OvRGv1zYAOpBGg2d2SK81qygnIYdtjnohehg2gx11HTInmCFF4uC9puLl"
#' 
#' # Be careful not to hit <ENTER> before entering your PIN following the next line.
#' 
#' auth.vector <- authorize_app(consumer.token,consumer.secret)
#' 
#' }
authorize_app<-function(consumer.token,consumer.secret){
  consumer.token <- as.character(consumer.token)
  consumer.secret <- as.character(consumer.secret)
  tokens<-request_token(consumer.token,consumer.secret)
  cnt<-httr::content(tokens,as='text')
  cnt.splt<-strsplit(cnt,'&')
  tmp.token<-strsplit(cnt.splt[[1]][1],'=')[[1]][2]
  tmp.secret<-strsplit(cnt.splt[[1]][[2]],'=')[[1]][2]
  cat("Log in to Twitter and navigate to the following URL in your browser\n\n")
  cat(sprintf("https://api.twitter.com/oauth/authorize?%s\n\n",cnt.splt[[1]][1]))
  utils::browseURL(sprintf("https://api.twitter.com/oauth/authorize?%s",cnt.splt[[1]][1]))
  pin.code<-readline("After authorizing the app enter your PIN:  \n")
  valid.tokens<-post_token(c(consumer.token=as.character(consumer.token),
                             consumer.secret=as.character(consumer.secret),
                             access.token=as.character(tmp.token),
                             access.secret=as.character(tmp.secret)),
                           pin.code
  )
  if(valid.tokens$status_code==200){
    vt.splt<-strsplit(httr::content(valid.tokens,as='text'),'&')
    new.token<-strsplit(vt.splt[[1]][1],'=')[[1]][2]
    new.secret<-strsplit(vt.splt[[1]][2],'=')[[1]][[2]]
    v<-verify_acct(c(consumer.token=as.character(consumer.token),
                     consumer.secret=as.character(consumer.secret),
                     access.token=as.character(new.token),
                     access.secret=as.character(new.secret))
    )
    v.cnt<-httr::content(v)
    auth.vector<-c(
      consumer.token=as.character(consumer.token),
      consumer.secret=as.character(consumer.secret),
      access.token=as.character(new.token),
      access.secret=as.character(new.secret)
    )
    return(auth.vector)
  } else {
    warning("Authorization Failed.\n")
    return(valid.tokens)
  }
}

#' Authorize INFORMS-Tutorial Twitter Application
#'
#' Authorize the INFORMS-Tutorial Twitter application for a user.
#'
#' This is the easiest way to get started collecting and analyzing Twitter data.
#' All that is needed is a Twitter user account.
#' The application consumer token and consumer secret for the
#' INFORMS-Tutorial Application are provided in this package.
#' This function supplies these to the Twitter API to authorize this
#' *read only* application to query the Twitter API on the user's behalf.
#' See \href{http://zlisto.scripts.mit.edu/informs_tutorial/tutorial.html}{2015 INFORMS Twitter Mining Tutorial}
#' for more information about the origin of this Application.  This function
#' will require the user to log in to Twitter and authorize this application.
#' Once authorized, Twitter will provide the User with a PIN that must be entered
#' into R.  Using this PIN, R will complete the authentication process and return
#' the user's authentication credentials (access token and secret), which the user
#' can save locally for future use.
#'
#' @section Authentication Keys:
#' The application consumer secret and user token secret are authentication passcodes
#' that should not be shared.  In particular, the INFORMS-Tutorial consumer secret, while
#' provided as a part of this package, should not be shared or used outside of
#' the scope of mining Twitter data for the purposes of research and analyses that
#' conform to applicable laws and policies, including Twitter's terms of use.
#'
#' @section Note:
#' It is easy to hit <ENTER> in the console after calling this function before
#' entering your PIN, especially when copying and pasting code; this will
#' result in an authentication failure. To avoid this 
#' mistake, enter the \code{authorize_app} line manually into the console.  Then,
#' log on to Twitter and browse to the URL provided (if this does not happen automatically),
#' authorize the app in the browser, retrieve the PIN, and enter the PIN back into the
#' R Console. 
#'
#' @return On successful authentication, a character vector containing the following named character strings:
#' \tabular{ll}{
#' \code{consumer.token} \tab The Twitter Application Consumer Token \cr
#' \code{consumer.secret} \tab The Twitter Application Consumer Secret \cr
#' \code{access.token} \tab The User's Access Token for the Twitter Application \cr
#' \code{access.secret} \tab The User's Access Secret for the Twitter Application.
#' }
#' On authentication failure, a warning is thrown and the failed http response object
#' is returned.
#'
#' @seealso \code{\link{twitter_request}}, \code{\link{authorize_app}}
#' @export
#' @examples
#' \dontrun{
#' auth.vector <- authorize_IT()
#' }
authorize_IT <- function(){
  consumer.token <- as.character(cons.token)
  consumer.secret <- as.character(cons.secret)
  tokens<-request_token(consumer.token,consumer.secret)
  cnt<-httr::content(tokens,as='text')
  cnt.splt<-strsplit(cnt,'&')
  tmp.token<-strsplit(cnt.splt[[1]][1],'=')[[1]][2]
  tmp.secret<-strsplit(cnt.splt[[1]][[2]],'=')[[1]][2]
  cat("Log in to Twitter and navigate to the following URL in your browser\n\n")
  cat(sprintf("https://api.twitter.com/oauth/authorize?%s\n\n",cnt.splt[[1]][1]))
  utils::browseURL(sprintf("https://api.twitter.com/oauth/authorize?%s",cnt.splt[[1]][1]))
  pin.code<-readline("After authorizing the app enter your PIN:  \n")
  valid.tokens<-post_token(c(consumer.token=as.character(consumer.token),
                             consumer.secret=as.character(consumer.secret),
                             access.token=as.character(tmp.token),
                             access.secret=as.character(tmp.secret)),
                           pin.code
  )
  if(valid.tokens$status_code==200){
    vt.splt<-strsplit(httr::content(valid.tokens,as='text'),'&')
    new.token<-strsplit(vt.splt[[1]][1],'=')[[1]][2]
    new.secret<-strsplit(vt.splt[[1]][2],'=')[[1]][[2]]
    v<-verify_acct(c(consumer.token=as.character(consumer.token),
                     consumer.secret=as.character(consumer.secret),
                     access.token=as.character(new.token),
                     access.secret=as.character(new.secret))
    )
    v.cnt<-httr::content(v)
    auth.vector<-c(
      consumer.token=as.character(consumer.token),
      consumer.secret=as.character(consumer.secret),
      access.token=as.character(new.token),
      access.secret=as.character(new.secret)
    )
    return(auth.vector)
  } else {
    warning("Authorization Failed.\n")
    return(valid.tokens)
  }
}


### Request token -- Get oauth/authorize --

request_token<-function(consumer.token,consumer.secret){
  base.url<-'https://api.twitter.com/oauth/request_token'
  oauth.params<-matrix(nrow=7,ncol=2)
  # query.params<-matrix(c("oauth_callback","oob"),nrow=1,ncol=2)
  query.params<-matrix(nrow=0,ncol=2)
  colnames(query.params)<-c("key","value")
  colnames(oauth.params)<-c("key","value")
  oauth.params[5,]<-c('oauth_consumer_key',consumer.token)
  oauth.params[1,]<-c('oauth_nonce',gen_oauth_nonce())
  oauth.params[6,]<-c('oauth_signature',NA)
  oauth.params[3,]<-c('oauth_signature_method','HMAC-SHA1')
  oauth.params[4,]<-c('oauth_timestamp',as.character(as.integer(as.POSIXct(Sys.time()))))
  oauth.params[2,]<-c('oauth_callback','oob')
  oauth.params[7,]<-c('oauth_version','1.0')
  all.params<-rbind(query.params,oauth.params[c(1:5,7),])
  oauth.sign<-gen_oauth_signature('POST',base.url,all.params,consumer.secret,0,include_as=FALSE)
  oauth.params[6,2]<-oauth.sign
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
  if (dim(query.params)[1]>0){
    for(i in 1:dim(query.params)[1]){
      if (i > 1){
        mod<-paste(mod,'&',sep="")
      }
      mod<-paste(mod,utils::URLencode(query.params[i,1]),sep="")
      mod<-paste(mod,'=',sep='')
      mod<-paste(mod,utils::URLencode(query.params[i,2]),sep="")
    }
  }
  x<-httr::POST(base.url,
         #query=mod,
         httr::add_headers(Authorization=oauth.str)
  )
  return(x)
}

### Post token

post_token<-function(auth.vector,pin.code){
  consumer.token<-as.character(auth.vector['consumer.token'])
  consumer.secret<-as.character(auth.vector['consumer.secret'])
  access.token<-as.character(auth.vector['access.token'])
  access.secret<-as.character(auth.vector['access.secret'])
  base.url<-'https://api.twitter.com/oauth/access_token'
  oauth.params<-matrix(nrow=7,ncol=2)
  query.params<-matrix(c("oauth_verifier",as.character(pin.code)),nrow=1,ncol=2)
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
  oauth.sign<-gen_oauth_signature('POST',base.url,all.params,consumer.secret,access.secret)
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
  if (dim(query.params)[1]>0){
    for(i in 1:dim(query.params)[1]){
      if (i > 1){
        mod<-paste(mod,'&',sep="")
      }
      mod<-paste(mod,utils::URLencode(query.params[i,1]),sep="")
      mod<-paste(mod,'=',sep='')
      mod<-paste(mod,utils::URLencode(query.params[i,2]),sep="")
    }
  }
  x<-httr::POST(base.url,
          query=mod,
          httr::add_headers(Authorization=oauth.str)
  )
  return(x)
}

### verify account

verify_acct<-function(auth.vector){
  base.url<-'https://api.twitter.com/1.1/account/verify_credentials.json'
  return(
    twitter_request(
      auth.vector,
      base.url
    )
  )
}

### Functions to enable authentication

gen_oauth_nonce<-function(){
  s<-c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','1','2','3','4','5','6','7','8','9','0')
  output<-sample(s,sample(40:50,1),replace=TRUE)
  return(paste(output,collapse=""))
}

gen_oauth_signature<-function(request.method,base.url,params.matrix,cs,as,include_as=TRUE){
  for(i in 1:dim(params.matrix)[1]){
    for(j in 1:dim(params.matrix)[2]){
      params.matrix[i,j]<-utils::URLencode(params.matrix[i,j])
    }
  }
  params.matrix<-params.matrix[order(params.matrix[,1]),]
  pstr<-''
  if(dim(params.matrix)[1]>0){
    for(i in 1:dim(params.matrix)[1]){
      if(i > 1){
        pstr<-paste(pstr,'&',sep='')
      }
      pstr<-paste(pstr,utils::URLencode(params.matrix[i,1],reserved=TRUE),sep='')
      pstr<-paste(pstr,'=',sep='')
      pstr<-paste(pstr,utils::URLencode(params.matrix[i,2],reserved=TRUE),sep='')
    }
  }
  out<-toupper(request.method)
  out<-paste(out,"&",sep="")
  out<-paste(out,utils::URLencode(base.url,reserved=TRUE),sep="")
  out<-paste(out,"&",sep='')
  out<-paste(out,utils::URLencode(pstr,reserved=TRUE,repeated=TRUE),sep='')
  if(include_as){
    signing.key<-paste(utils::URLencode(cs,reserved=TRUE),utils::URLencode(as,reserved=TRUE),sep="&")
  } else {
    signing.key<-paste(utils::URLencode(cs,reserved=TRUE),"&",sep="")
  }
  final.out<-httr::hmac_sha1(signing.key,out)
  return(final.out)
}

