# Vignette to analyze US politics.  

## Somebody already keeps lists of republican and democrat politician
## Twitter accounts.  We take advantage of those lists in this vignette.

library(twinfoR)
library(httr)
# devtools::load_all("/home/cemarks/Projects/twinfoR")
# devtools::load_all("/Users/cemarks/Projects/twinfoR")

dir.create("~/twitter_politics",showWarnings=FALSE)
setwd("~/twitter_politics")

### Authenticate!

auth.vector <- authorize_IT()

### Collect list members.  See https://developer.twitter.com/en/docs/accounts-and-users/create-manage-lists/api-reference/get-lists-members

url <- 'https://api.twitter.com/1.1/lists/members.json'

### Republicans

#### House
slug <- 'house-republicans'
owner_screen_name <- 'houseGOP'
count <- 5000

response <- twitter_request(
  auth.vector,
  url,
  query.param.list = list(
    slug = slug,
    owner_screen_name = owner_screen_name,
    count=count
  )
)

### Check for 200 status
cat(response$status,"\n",sep="")

house.reps.obj <- content(response)
house.reps <- house.reps.obj$users

### Check number of reps.  Should be close to number of house republicans
cat(length(house.reps),"\n",sep="")


#### Senate

slug <- 'SenateRepublicans'
owner_screen_name <- 'SenateGOP'
count <- 5000

response <- twitter_request(
  auth.vector,
  url,
  query.param.list = list(
    slug = slug,
    owner_screen_name = owner_screen_name,
    count=count
  )
)

### Check for 200 status
cat(response$status,"\n",sep="")

senate.reps.obj <- content(response)
senate.reps <- senate.reps.obj$users


### Check number of reps.  Should be close to number of Senate republicans
cat(length(senate.reps),"\n",sep="")


### Now let's get the democrats.

slug <- 'house-democrats'
owner_screen_name <- 'HouseDemocrats'
count <- 5000

response <- twitter_request(
  auth.vector,
  url,
  query.param.list = list(
    slug = slug,
    owner_screen_name = owner_screen_name,
    count=count
  )
)

### Check for 200 status
cat(response$status,"\n",sep="")

house.dems.obj <- content(response)
house.dems <- house.dems.obj$users

### Check number of reps.  Should be close to number of house republicans
cat(length(house.dems),"\n",sep="")

#### Senate

slug <- 'SenateDemocrats'
owner_screen_name <- 'SenateDems'
count <- 5000

response <- twitter_request(
  auth.vector,
  url,
  query.param.list = list(
    slug = slug,
    owner_screen_name = owner_screen_name,
    count=count
  )
)

### Check for 200 status
cat(response$status,"\n",sep="")

senate.dems.obj <- content(response)
senate.dems <- senate.dems.obj$users


### Check number of dems.  Should be close to number of Senate republicans
cat(length(senate.dems),"\n",sep="")  

## Something's up here.  Too many Senators!!!  
## We're going to roll with it anyway.


### President

pres <- user_show('potus')
cat(pres$screen_name,"\n",sep="")

### Create user dataframe and initialize database.
### See documentation for twitter_database

house.reps.df <- data.frame(
  user_id = sapply(
    house.reps,
    function(x) return(x$id_str)
  ),
  screen_name = sapply(
    house.reps,
    function(x) return(x$screen_name)
  ),
  party = rep('republican',length(house.reps)),
  body = rep('house',length(house.reps)),
  stringsAsFactors = FALSE
)
senate.reps.df <- data.frame(
  user_id = sapply(
    senate.reps,
    function(x) return(x$id_str)
  ),
  screen_name = sapply(
    senate.reps,
    function(x) return(x$screen_name)
  ),
  party = rep('republican',length(senate.reps)),
  body = rep('senate',length(senate.reps)),
  stringsAsFactors = FALSE
)
house.dems.df <- data.frame(
  user_id = sapply(
    house.dems,
    function(x) return(x$id_str)
  ),
  screen_name = sapply(
    house.dems,
    function(x) return(x$screen_name)
  ),
  party = rep('democrat',length(house.dems)),
  body = rep('house',length(house.dems)),
  stringsAsFactors = FALSE
)
senate.dems.df <- data.frame(
  user_id = sapply(
    senate.dems,
    function(x) return(x$id_str)
  ),
  screen_name = sapply(
    senate.dems,
    function(x) return(x$screen_name)
  ),
  party = rep('democrat',length(senate.dems)),
  body = rep('senate',length(senate.dems)),
  stringsAsFactors = FALSE
)

pres.df <- data.frame(
  user_id = pres$id_str,
  screen_name = pres$screen_name,
  party = 'republican', # For now!!!
  body = 'whitehouse',
  stringsAsFactors = FALSE
)

user.df <- rbind(
  house.reps.df,
  senate.reps.df,
  house.dems.df,
  senate.dems.df,
  pres.df
)


### Initialize the database

t.con <- twitter_database(
  "pol.sqlite",
  # query.users.df = user.df
)

insert_users(
  t.con,
  house.reps,
  calc.Rsentiment = TRUE,
  calc.syu = TRUE
)
insert_users(
  t.con,
  house.dems,
  calc.Rsentiment = TRUE,
  calc.syu = TRUE
)
insert_users(
  t.con,
  senate.reps,
  calc.Rsentiment = TRUE,
  calc.syu = TRUE
)
insert_users(
  t.con,
  senate.dems,
  calc.Rsentiment = TRUE,
  calc.syu = TRUE
)
insert_users(
  t.con,
  pres,
  calc.Rsentiment = TRUE,
  calc.syu = TRUE
)

##### Now let's get all of the timelines.
##### This could take a while--as in days in order to do all of the sentiment processing.

sink("/dev/null") # Good idea to supress the RSentiment output
update_user_timelines(
  t.con,
  calc.Rsentiment = TRUE,
  calc.syu = TRUE
)
sink()



### Summary of what we have so far.

summarize_database(t.con)

### Top hashtags from democrats and republicans in the last week

tophashtags.dem <- top_hashtags(
  t.con,
  start.date = as.Date(Sys.time())-7,
  where.criteria = "query_users.party = 'democrat'"
)

tophashtags.rep <- top_hashtags(
  t.con,
  start.date = as.Date(Sys.time())-7,
  where.criteria = "query_users.party = 'republican'"
)


### Top usermentions from democrats and republicans in the last week

topusermentions.dem <- top_usermentions(
  t.con,
  start.date = as.Date(Sys.time())-7,
  where.criteria = "query_users.party = 'democrat'"
)

topusermentions.rep <- top_usermentions(
  t.con,
  start.date = as.Date(Sys.time())-7,
  where.criteria = "query_users.party = 'republican'"
)

### Top media from democrats and republicans in the last week

topmedia.dem <- top_media(
  t.con,
  start.date = as.Date(Sys.time())-7,
  where.criteria = "query_users.party = 'democrat'",
  media.file.prefix = "DEM"
)



topmedia.rep <- top_media(
  t.con,
  start.date = as.Date(Sys.time())-7,
  where.criteria = "query_users.party = 'republican'",
  media.file.prefix = "REP"
)


### Top tweeters democrats and republicans in the last week

toptweeters.dem <- top_tweeters(
  t.con,
  start.date = as.Date(Sys.time())-7,
  where.criteria = "query_users.party = 'democrat'"
)

toptweeters.rep <- top_tweeters(
  t.con,
  start.date = as.Date(Sys.time())-7,
  where.criteria = "query_users.party = 'republican'"
)


### Most liked tweets democrats and republicans in the last week

mostliked.dem <- most_liked(
  t.con,
  start.date = as.Date(Sys.time())-7,
  where.criteria = "query_users.party = 'democrat'"
)

mostliked.rep <- most_liked(
  t.con,
  start.date = as.Date(Sys.time())-7,
  where.criteria = "query_users.party = 'republican'"
)



### Most retweeted tweets democrats and republicans in the last week

mostretweeted.dem <- most_retweeted(
  t.con,
  start.date = as.Date(Sys.time())-7,
  where.criteria = "query_users.party = 'democrat'"
)

mostretweeted.rep <- most_retweeted(
  t.con,
  start.date = as.Date(Sys.time())-7,
  where.criteria = "query_users.party = 'republican'"
)
