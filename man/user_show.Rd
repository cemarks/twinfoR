% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_calls.R
\name{user_show}
\alias{user_show}
\title{Show User}
\usage{
user_show(screen_name, user_id, authentication.vector,
  include_entities = TRUE, tweet_mode = "extended")
}
\arguments{
\item{screen_name}{character single screen_name.  This is only used is \code{user_id} is missing.}

\item{user_id}{numeric or character single user_id.}

\item{authentication.vector}{character vector containing authentication tokens and secrets.
See \code{\link{authorize_app}} and \code{\link{authorize_IT}}.}

\item{include_entities}{logical indicating whether to include profile entities (e.g., urls)
as separate nodes in the returned json object.}

\item{tweet_mode}{character either 'extended' for full_text statuses or 'compat' for
140 character compatability.}
}
\value{
On success, an R list representation of the returned JSON object.  On failure, a warning
is thrown and the http response is returned, if any.
}
\description{
Get a Single Twitter User Object from the Show User API
}
\details{
This function calls \code{\link{twitter_request}} using the Twitter
users/show endpoint (see
\href{https://developer.twitter.com/en/docs/accounts-and-users/follow-search-get-users/api-reference/get-users-show}{User/Show API Documentation}).
A user_id or screen_name is required.  If an authentication vector is not
provided and there is no globally defined \code{auth.vector}, an error is returned.
This function is similar to the \code{\link{user_lookup}} function, except that it only takes and
returns a single user object.
}
\examples{

\dontrun{
auth.vector <- authorize_IT()

potus.obj <- user_show(
  "realDonaldTrump",
  authentication.vector = auth.vector
)

cat(potus$description)
}
}
\seealso{
\code{\link{twitter_request}}, \code{\link{authorize_app}}, \code{\link{user_lookup}}
}
