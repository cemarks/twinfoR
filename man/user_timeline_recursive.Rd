% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_recursives.R
\name{user_timeline_recursive}
\alias{user_timeline_recursive}
\title{Recursive Timeline Query}
\usage{
user_timeline_recursive(screen_name, user_id, data.connection,
  authentication.vector, since_id = NULL, max_id = NULL,
  trim_user = FALSE, exclude_replies = FALSE, include_rts = TRUE,
  tweet_mode = "extended", ...)
}
\arguments{
\item{screen_name}{character screen_name.  This is only used is \code{user_id} is missing.}

\item{user_id}{numeric or character user_id.}

\item{data.connection}{a DBI connection to an RSQLite \code{\link{twitter_database}}.}

\item{authentication.vector}{character vector containing authentication tokens and secrets.
See \code{\link{authorize_app}} and \code{\link{authorize_IT}}.}

\item{since_id}{numeric or character status_id.  If supplied, only tweets with status_ids greater
than this value (and therefore more recent) will be returned.}

\item{max_id}{numeric or character status_id.  If suppled, only tweets with status_ids less than or
equal to this value (and therefore no more recent) will be returned.}

\item{trim_user}{logical indicating whether to remove the user object from each status.}

\item{exclude_replies}{logical indicating whether to filter user replies to other statuses
from the results.}

\item{include_rts}{logical indicating whether to include retweets in the results.}

\item{tweet_mode}{character either 'extended' for full_text statuses or 'compat' for
140 character compatability.}

\item{...}{other named parameters passed to \code{\link{insert_statuses}}.}
}
\value{
character most recent status id if \code{data.connection} is supplied, 
otherwise a list of status objects (lists).
}
\description{
Call the get/user_timeline API recursively
}
\details{
This function recursively calls the get/user_timeline API to obtain all available statuses in
a user timeline that meet the conditions specified by the input parameters.  This method
includes a one second delay for each query in order to prevent exceeding the rate limit.
Twitter limits the number of tweets available through this API to the most recent 3200
posts for each user.  See
\href{https://developer.twitter.com/en/docs/tweets/search/api-reference/get-statuses-user_timeline}{user_timeline API Documentation}
for more information.

If the \code{authentication.vector} parameter is missing, this method checks the global
environment for \code{auth.vector}.  If this variable exists, the function will use
this variable as the \code{authentication.vector}.  If this variable does not exist, the
function will throw an error.
}
\examples{

\dontrun{
auth.vector <- authorize_IT()

user.statuses <- user_timeline_recursive(
  "realDonaldTrump",
  authentication.vector = auth.vector
)
}
}
\seealso{
\code{\link{user_timeline}}, \code{\link{insert_statuses}}, \code{\link{twitter_database}}
}
