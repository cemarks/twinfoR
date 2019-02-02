options(java.parameters = "-Xmx1024m")
#library(twinfoR)
devtools::load_all("/home/cemarks/mnt/Chris/Projects/twinfoR-all/twinfoR")
setwd("/home/cemarks/mnt/Chris/Projects/twinfoR-all/twinfoR/ignore/tests")
# Some important initializations

## Initialize database

con <- twitter_database(
  "twitter_USPol2",
  driver=RMySQL::MySQL(),
  user="modu",
  password="!gO98aRMY&",
  host="10.0.0.2"
)

# con <- twitter_database(
#   "USPol.sqlite"
# )

load("auth_vector.RData")

url <- 'https://api.twitter.com/1.1/lists/members.json'
count <- 5000


slug <- 'house-democrats'
owner_screen_name <- 'HouseDemocrats'
response <- twitter_request(
  auth.vector,
  url,
  query.param.list = list(
    slug = slug,
    owner_screen_name = owner_screen_name,
    count=count
  )
)
# Check for 200 status
if(response$status==200){
  house.dems.obj <- httr::content(response)
  house.dems <- house.dems.obj$users
} else {
  cat(paste("API request failed; status ",response$status,"\n",sep=""))
}


# Senate Democrats
slug <- 'SenateDemocrats'
owner_screen_name <- 'SenateDems'
response <- twitter_request(
  auth.vector,
  url,
  query.param.list = list(
    slug = slug,
    owner_screen_name = owner_screen_name,
    count=count
  )
)
if(response$status==200){
  senate.dems.obj <- httr::content(response)
  senate.dems <- senate.dems.obj$users
} else {
  cat(paste("API request failed; status ",response$status,"\n",sep=""))
}


# House Republicans
slug <- 'house-republicans'
owner_screen_name <- 'houseGOP'
response <- twitter_request(
  auth.vector,
  url,
  query.param.list = list(
    slug = slug,
    owner_screen_name = owner_screen_name,
    count=count
  )
)
if(response$status==200){
  house.reps.obj <- httr::content(response)
  house.reps <- house.reps.obj$users
} else {
  cat(paste("API request failed; status ",response$status,"\n",sep=""))
}


# Senate Republicans
slug <- 'SenateRepublicans'
owner_screen_name <- 'SenateGOP'
response <- twitter_request(
  auth.vector,
  url,
  query.param.list = list(
    slug = slug,
    owner_screen_name = owner_screen_name,
    count=count
  )
)

if(response$status==200){
  senate.reps.obj <- httr::content(response)
  senate.reps <- senate.reps.obj$users
} else {
  cat(paste("API request failed; status ",response$status,"\n",sep=""))
}


# President
pres <- user_show('potus')
# Not run
sample.size <- 5
house.dems <- sample(house.dems,sample.size)
senate.dems <- sample(senate.dems,sample.size)
house.reps <- sample(house.reps,sample.size)
senate.reps <- sample(senate.reps,sample.size)
house.reps.df <- data.frame(
  user_id = sapply(
    house.reps,
    function(x) return (x$id_str)
  ),
  screen_name = sapply(
    house.reps,
    function(x) return (x$screen_name)
  ),
  party = rep('republican',length(house.reps)),
  body = rep('house',length(house.reps)),
  since_id = '1087541961295314945',
  stringsAsFactors = FALSE
)
senate.reps.df <- data.frame(
  user_id = sapply(
    senate.reps,
    function(x) return (x$id_str)
  ),
  screen_name = sapply(
    senate.reps,
    function(x) return (x$screen_name)
  ),
  party = rep('republican',length(senate.reps)),
  body = rep('senate',length(senate.reps)),
  since_id = '1087541961295314945',
  stringsAsFactors = FALSE
)
house.dems.df <- data.frame(
  user_id = sapply(
    house.dems,
    function(x) return (x$id_str)
  ),
  screen_name = sapply(
    house.dems,
    function(x) return (x$screen_name)
  ),
  party = rep('democrat',length(house.dems)),
  body = rep('house',length(house.dems)),
  since_id = '1087541961295314945',
  stringsAsFactors = FALSE
)
senate.dems.df <- data.frame(
  user_id = sapply(
    senate.dems,
    function(x) return (x$id_str)
  ),
  screen_name = sapply(
    senate.dems,
    function(x) return (x$screen_name)
  ),
  party = rep('democrat',length(senate.dems)),
  body = rep('senate',length(senate.dems)),
  since_id = '1087541961295314945',
  stringsAsFactors = FALSE
)
pres.df <- data.frame(
  user_id = pres$id_str,
  screen_name = pres$screen_name,
  party = 'republican', # For now!!!
  body = 'whitehouse',
  since_id = '1087541961295314945',
  stringsAsFactors = FALSE
)

user.df <- rbind(
  house.reps.df,
  senate.reps.df,
  house.dems.df,
  senate.dems.df,
  pres.df
)

# Upload this user data.frame to the Twitter Database

upload_query_users(con,user.df)


update_users(con)
# THIS TAKES A LONG TIME (15-20 hours) ON THE FULL SET.  You have been warned.
get_all_friends(con)

# THIS TAKES A LONG TIME (2-3 days) ON THE FULL SET.  You have been warned.

# sink("/dev/null") # Recommended to suppress output, but might not work
## on all platforms
update_user_timelines(
  con,
  calc.Rsentiment = TRUE,
  calc.syu = TRUE
)

summarize_database(con)

DBI::dbDisconnect(con)

