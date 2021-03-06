% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_model.R
\name{insert_followers}
\alias{insert_followers}
\title{Insert Follower Relationships}
\usage{
insert_followers(conn, friend.id, follower.ids, ids.per.insert = 1000)
}
\arguments{
\item{conn}{a Twitter database connection.  See \code{twitter_database}.}

\item{friend.id}{character user_id of Twitter friend.}

\item{follower.ids}{character user_ids of Twitter followers.}

\item{ids.per.insert}{integer maximum number of relationships per insert query.}
}
\value{
\code{NULL} (Invisible).
}
\description{
Insert Twitter Follower relationships into Twitter database connection
}
\details{
This function inserts 'follower' relationship information into the
\code{followers} table in the data connection.  It takes a single
friend user_id and multiple follower user_ids.  For each follower user_id,
a follower-friend relationship is entered into the table.  This method
is called by the \code{\link{followers_ids_recursive}} and 
\code{\link{followers_list_recursive}} functions.
}
\examples{

\dontrun{
auth.vector <- authorize_IT()

nyt <- user_show("nytimes")
nyt_followers <- followers_ids(user_id=nyt$id_str)

conn <- twitter_database("tweetanalysis.sqlite")

insert_followers(
  conn,
  nyt$id_str,
  sapply(
    nyt_followers,
    function(x) return(x$id_str)
  )
)
}
}
\seealso{
\code{\link{twitter_database}}, \code{\link{followers_list_recursive}}
\code{\link{followers_ids_recursive}}, \code{\link{get_all_followers}}
}
