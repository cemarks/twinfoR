# twinfoR: A package for mining Twitter.

This package provides methods for collecting and analyzing data from the Twitter REST APIs.

## What this package does
The goal of this package is to make it very easy to sent user authenticated
queries to the [Twitter REST APIs](https://developer.twitter.com/en/docs/api-reference-index),
and collect, store, and analyze the response data.


## Installation and Getting Started
Install from the compiled .zip file provided, as shown below from the R Console.
After installing, check out the documentation.

```{r}
> install.packages("twinfoR.zip",repos=NULL,type="binary")}
> ?twinfoR::twinfoR
```

If you do not have a copy of this package and would like one, send a request to `cemarks` at `alum.mit.edu`.

## Authentication
The `authorize_app` and `authorize_IT` provide
methods to obtain access tokens needed for most API methods.  These methods
require the user to log on to Twitter and authorize a Twitter Application
on the user's account.  **You must have a Twitter account to authenticate.**  
Without a [Twitter account](https://www.twitter.com), this package will
be of very limited use.  When calling either of the authentication functions, 
use care not to hit <ENTER> following the call to the function until you have
authorized the App in your browser, retrieved the PIN, and entered the PIN into
the R console.  This mistake is easy to make if you are copying and pasting
the authentication function into the console. 

All methods in this package that send requests to the Twitter REST APIs, with 
the single exception of `twitter_request`, look for user 
authentication credentials in a variable named `auth.vector` if
credentials are not supplied to the function.

## Twitter Requsts
The workhorse function for this package is `twitter_request`.
It can be used to send a user authenticated request to any of the 
[REST API endpoints](https://developer.twitter.com/en/docs/api-reference-index).
See `twitter_request` documentation for details.  All functions
in this package that send requests to the Twitter REST APIs call this function.

## Basic API Methods

This package offers several functions that access common Twitter REST API endpoints
and return the response.  These functions are:

* `search_tweets` (see [Twitter Search API](https://developer.twitter.com/en/docs/tweets/search/api-reference/get-search-tweets))
* `user_timeline` (see [User Timeline API](https://developer.twitter.com/en/docs/tweets/search/api-reference/get-statuses-user_timeline))
* `user_lookup` (see [User Lookup API](https://developer.twitter.com/en/docs/accounts-and-users/follow-search-get-users/api-reference/get-users-lookup))
* `user_show` (see [Show User API](https://developer.twitter.com/en/docs/accounts-and-users/follow-search-get-users/api-reference/get-users-show))
* `status_lookup` (see [Status Lookup API](https://developer.twitter.com/en/docs/tweets/post-and-engage/api-reference/get-statuses-lookup))
* `status_show` (see [Show Status API](https://developer.twitter.com/en/docs/tweets/post-and-engage/api-reference/get-statuses-show-id))
* `user_search` (see [User Search API](https://developer.twitter.com/en/docs/accounts-and-users/follow-search-get-users/api-reference/get-users-search))
* `followers_ids` (see [Followers IDs API](https://developer.twitter.com/en/docs/accounts-and-users/follow-search-get-users/api-reference/get-followers-ids))
* `friends_ids` (see [Friends IDs API](https://developer.twitter.com/en/docs/accounts-and-users/follow-search-get-users/api-reference/get-friends-ids))
* `followers_list` (see [Followers List API](https://developer.twitter.com/en/docs/accounts-and-users/follow-search-get-users/api-reference/get-followers-list))
* `friends_list` (see [Friends List API](https://developer.twitter.com/en/docs/accounts-and-users/follow-search-get-users/api-reference/get-friends-list))

## Recursive API Methods

Often it is useful to call the methods listed in the preceding section recursively, 
in order to continue to collect relevant data from the Twitter REST APIs.  Twitter
imposes rate limits on all of these API end points.  The following functions
recursively query the end points, but include a delay for each query in order
to build a large data set without exceeding the rate limits.  The recursive nature
of these functions varies.  Some functions are designed to iterate through more
inputs than a single query can handle, e.g., `status_lookup_recursive`;
others page through more results than can be returned by a single query, e.g.,
`search_tweets_recursive`.  The recursive API request functions are

* `search_tweets_recursive`
* `user_timeline_recursive`
* `user_lookup_recursive`
* `status_lookup_recursive`
* `user_search_recursive`
* `followers_list_recursive`
* `friends_list_recursive`
* `followers_ids_recursive`
* `friends_ids_recursive`

These recursive functions can return the results to the user, or, if a 
`twitter_database` connection is supplied, they will insert
the results directly into the database.  This specific database structure
is provided in the next section.

## Data
This package offers a useful SQLite data structure that can be initialized
or re-established useing the `twitter_database` function.  
Understanding this data structure and familiarity with SQLite syntax will
enable a user to carry out a wide array of analyses that are tailored for
specific scenarios using this package.  Of  particular note, the 
`query_users} and `query_text} tables enable a user to specify
and categorize query criteria that can be executed and analyzed over
a time horizon.  The following functions act on a `twitter_database`
connection, and interact with the `query_text} and `query_users} tables,
enabling a wide range of collection and analysis capabilities.

* `update_users`
* `update_user_timelines`
* `update_search`
* `get_all_friends`
* `get_all_followers`
* `summarize_database`

Additionally, the following functions enable direct updates to a 
`twitter_database`, and are called by some of the functions
listed above.

* `upload_query_users`
* `upload_query_text`
* `insert_users`
* `insert_statuses`
* `insert_followers`
* `insert_friends`

## Analysis
While many analyses are situation dependent, this package provides several
useful, basic analysis and plotting functions that have applicability in many
use cases.  These functions include the following:

* `open_user` (open a user profile in a browser)
* `status_media` (general function to manage images attached to Tweets)
* `top_hashtags`
* `top_usermentions`
* `top_urls`
* `top_media`
* `top_tweeters`
* `most_liked`
* `most_retweeted`
* `most_popular_RT_in_sample`
* `most_reach`

The following functions produce plots of different aspects of the Twitter data.

* `wordcloud_plot`
* `sentiment_plots`
* `timeplot`

## Vignettes
The vignettes for this package demonstrate many of features described above.  They are:

* Basic.  This vignette uses the API calls to collect data and provides some simple analyses.
* US Politics (Parts I and II).  This vignette walks through the process of collecting 
and analyzing Tweets from US politicians.  It demonstrates the
`twitter_database` functionality.
* Locations Search (Parts I and II).  This vignette walks through the process of collecting 
and analyzing Tweets using different search criteria.  It demonstrates the
`twitter_database` functionality.


