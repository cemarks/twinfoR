% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis.R
\name{open_user}
\alias{open_user}
\title{Open Twitter User Page}
\usage{
open_user(user_id, screen_name, authentication.vector)
}
\arguments{
\item{user_id}{character Twitter user ID. Ignored if screen_name is provided.}

\item{screen_name}{character Twitter user screen name.}

\item{authentication.vector}{character vector containing authentication tokens and secrets.
See \code{\link{authorize_app}} and \code{\link{authorize_IT}}.}
}
\value{
NULL (Invisible).
}
\description{
View a Twitter user timeline in a browser
}
\details{
This function opens a browser window on the specified user's Twitter page.  If only a \code{user_id}
is provided (i.e., no \code{screen_name}), authentication tokens must also be provided in one
 of two ways: either explicitly, by 
including them in the \code{authentication.vector}, or by storing them in a global variable
named \code{auth.vector}.  See \code{\link{authorize_app}} and \code{\link{authorize_IT}}.
}
\examples{


open_user(screen_name="realDonaldTrump")

}
\seealso{
\code{\link{twitter_database}}, \code{\link{authorize_app}}, \code{\link{open_status}}
}
