% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_calls.R
\name{friends_ids}
\alias{friends_ids}
\title{Get Friends IDs}
\usage{
friends_ids(screen_name, user_id, authentication.vector, cursor = NULL,
  stringify_ids = TRUE, count = 5000)
}
\arguments{
\item{screen_name}{character single screen_name.  This is only used is \code{user_id} is missing.}

\item{user_id}{numeric or character single user_id.}

\item{authentication.vector}{character vector containing authentication tokens and secrets.
See \code{\link{authorize_app}} and \code{\link{authorize_IT}}.}

\item{cursor}{numeric or character cursor value for controlling pagination.}

\item{stringify_ids}{logical indicating whether to return user_ids as strings.}

\item{count}{numeric number of user_ids per page, up to 5000.}
}
\value{
On success, an R list representation of the returned JSON object.  On failure, a warning
is thrown and the http response is returned, if any.
}
\description{
Get a User's Friends' IDs using the Friends IDs API
}
\details{
This function calls \code{\link{twitter_request}} using the Twitter
friends/ids endpoint (see
\href{https://developer.twitter.com/en/docs/accounts-and-users/follow-search-get-users/api-reference/get-friends-ids}{Friends IDs API Documentation}).
A user_id or screen_name is required.  If an authentication vector is not
provided and there is no globally defined \code{auth.vector}, an error is returned.
}
\examples{

\dontrun{
auth.vector <- authorize_IT()

potus.friends <- friends_ids(
  "realDonaldTrump",
  authentication.vector = auth.vector
)

cat(paste(potus.friends$ids[1:20],collapse=","))
}
}
\seealso{
\code{\link{twitter_request}}, \code{\link{authorize_app}}, \code{\link{friends_list}},
\code{\link{followers_ids}}
}
