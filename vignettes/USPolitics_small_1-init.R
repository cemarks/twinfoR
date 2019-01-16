# Vignette to analyze US politics Part I of III (SMALL).  

## Somebody already keeps lists of republican and democrat politician
## Twitter accounts.  We take advantage of those lists in this vignette.

## This vignette samples from the lists of democrats and republicans in order
## to keep the size of the database, and the processing times small

## The USPolitics_large vignette runs on the entire lists of republicans
## and democrats.

## This is part 1 of a three-part vignette.  This script initializes
## the database with the query_users.  Unlike the other two parts of this
## vignette, the part of the process represented by this script would only
## occur at the outset of a project, but could be udpated later on.

library(twinfoR)
# devtools::load_all("/home/cemarks/Projects/twinfoR")
# devtools::load_all("/Users/cemarks/Projects/twinfoR")

dir.create("~/small_politics",showWarnings=FALSE)
setwd("~/small_politics")

### Authenticate!

auth.vector <- authorize_IT()
save(auth.vector,file="auth_vector.RData")
# load("auth_vector.RData")

### Collect list members.  See https://developer.twitter.com/en/docs/accounts-and-users/create-manage-lists/api-reference/get-lists-members
### These sections make use of the general "twitter_request" function

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

house.reps.obj <- httr::content(response)
house.reps <- sample(house.reps.obj$users,3)

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

senate.reps.obj <- httr::content(response)
senate.reps <- sample(senate.reps.obj$users,3)


## Check number of reps.  Should be close to number of Senate republicans
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

house.dems.obj <- httr::content(response)
house.dems <- sample(house.dems.obj$users,3)

### Check number of reps.  Should be close to number of house republicans
cat(length(house.dems),"\n",sep="")

### Senate

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

senate.dems.obj <- httr::content(response)
senate.dems <- sample(senate.dems.obj$users,3)


### Check number of dems.  Should be close to number of Senate republicans
cat(length(senate.dems),"\n",sep="")  

# Something's up here.  Too many Senators!!!  
# We're going to roll with it anyway.


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
  "pol-small.sqlite",
  query.users.df = user.df
)

update_users(t.con)

### Get friends of each user
### NOTE: IN GENERAL, THIS PROCESS WILL TAKE A ***LONG*** TIME.  
### The Twitter API limits friends and followers requests to essentially one per minute.
### Because there are only 13 users in this set, and because we are only going to get *friends*,
### which usually don't tend to be too many per user, we can anticipate that this function will terminate
### in ***roughly*** 15 minutes.  This approximation assumes one query (<= 5000 friends) per user, with
### very few exceptions.

get_all_friends(t.con)


### Database summary

summarize_database(t.con)

### Disconnect from database

DBI::dbDisconnect(t.con)