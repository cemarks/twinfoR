#### GET TWEETS FOR KEY WORD COLLECT FUNCTIONS ############
con <- dbConnect(RSQLite::SQLite(),"C:\\Users\\paul.hurley1\\Documents\\BT_twitter\\sampleKA.sqlite")

#### QUERY 1: #ENDURING PROMISE  ######

d.url<-'https://api.twitter.com/1.1/search/tweets.json'
query.str<-'#EnduringPromise'
collect <- TRUE
n.calls<-0
while(n.calls < 180 && collect){
  n.calls<-n.calls+1
  if(n.calls==1){
    x<-twitter_anything(auth.vector,d.url,c('q',query.str,'count','100','result_type','recent',"tweet_mode","extended"))
    ep.tweets<- httr::content(x)[['statuses']]
  } else {
    max.id<-min(sapply(ep.tweets,function(z) return(as.double(z$id_str))))-1
    x<-twitter_anything(auth.vector,d.url,c('q',query.str,'count','100','result_type','recent','max_id',"tweet_mode","extended",sprintf("%0.0f",max.id)))
    ep.tweets<-c(ep.tweets,httr::content(x)[['statuses']])
  }
  if(length(httr::content(x))< 2){
    collect <- FALSE
  }
}

## insert into the dbase
insert_statuses(ep.tweets,con) 

Sys.sleep(901)


### load into dataframe

# enduringpromise.df<- data.frame(user.id=sapply(ep.tweets,function(x) return(x$id_str)),
#                                 screen.name=sapply(ep.tweets,function(x) return(x$user$screen_name)),
#                                # text = sapply(ep.tweets,function(x) return(x$full_text)),
#                                 created_at = sapply(ep.tweets,function(x) return(x$created_at)),
#                                 lang = sapply(ep.tweets,function(x) return(x$lang))
# )
# 
# write.csv(enduringpromise.df,"L:/Org_Folders/SCJ8/Restricted/SCJ86/SCJ86 Files/BT_twitter\\#EnduringPromise_tweets.csv",row.names=FALSE)


#### QUERY 2: HASHTAG USNSComfort  #########
d.url<-'https://api.twitter.com/1.1/search/tweets.json'

query.str<-'#USNSComfort'
collect <- TRUE
n.calls<-0
while(n.calls < 180 && collect){
  n.calls<-n.calls+1
  if(n.calls==1){
    x<-twitter_anything(auth.vector,d.url,c('q',query.str,'count','100','result_type','recent',"tweet_mode","extended"))
    comfort.tweets<- httr::content(x)[['statuses']]
  } else {
    max.id<-min(sapply(comfort.tweets,function(z) return(as.double(z$id_str))))-1
    x<-twitter_anything(auth.vector,d.url,c('q',query.str,'count','100','result_type','recent','max_id',"tweet_mode","extended",sprintf("%0.0f",max.id)))
    comfort.tweets<-c(comfort.tweets,httr::content(x)[['statuses']])
  }
  if(length(httr::content(x))< 2){
    collect <- FALSE
  }
}

#### load into dbase
insert_statuses(comfort.tweets,con) 

Sys.sleep(901)

#### QUERY 3:  KEYWORD: HOSPITAL SHIP   #####
d.url<-'https://api.twitter.com/1.1/search/tweets.json'

query.str<-'hospital ship'
collect <- TRUE
n.calls<-0
while(n.calls < 180 && collect){
  n.calls<-n.calls+1
  if(n.calls==1){
    x<-twitter_anything(auth.vector,d.url,c('q',query.str,'count','100','result_type','recent',"tweet_mode","extended"))
    hosp.ship.tweets<- httr::content(x)[['statuses']]
  } else {
    max.id<-min(sapply(hosp.ship.tweets,function(z) return(as.double(z$id_str))))-1
    x<-twitter_anything(auth.vector,d.url,c('q',query.str,'count','100','result_type','recent','max_id',"tweet_mode","extended",sprintf("%0.0f",max.id)))
    hosp.ship.tweets<-c(hosp.ship.tweets,httr::content(x)[['statuses']])
  }
  if(length(httr::content(x))< 2){
    collect <- FALSE
  }
}

#### load into dbase
insert_statuses(hosp.ship.tweets,con) 
Sys.sleep(901)

#### QUERY 4:  KEYWORD: "chinese hospital ship"  #################
d.url<-'https://api.twitter.com/1.1/search/tweets.json'

query.str<-'peace ark'
collect <- TRUE
n.calls<-0
while(n.calls < 180 && collect){
  n.calls<-n.calls+1
  if(n.calls==1){
    x<-twitter_anything(auth.vector,d.url,c('q',query.str,'count','100','result_type','recent',"tweet_mode","extended"))
    peaceark.tweets<- httr::content(x)[['statuses']]
  } else {
    max.id<-min(sapply(peaceark.tweets,function(z) return(as.double(z$id_str))))-1
    x<-twitter_anything(auth.vector,d.url,c('q',query.str,'count','100','result_type','recent','max_id',"tweet_mode","extended",sprintf("%0.0f",max.id)))
    peaceark.tweets<-c(peaceark.tweets,httr::content(x)[['statuses']])
  }
  if(length(httr::content(x))< 2){
    collect <- FALSE
  }
}

#### load into dbase
insert_statuses(peaceark.tweets,con) 
Sys.sleep(901)

#### QUERY 5:  KEYWORD: "arca del paz"  #################
d.url<-'https://api.twitter.com/1.1/search/tweets.json'

query.str<-'arca de paz'
collect <- TRUE
n.calls<-0
while(n.calls < 180 && collect){
  n.calls<-n.calls+1
  if(n.calls==1){
    x<-twitter_anything(auth.vector,d.url,c('q',query.str,'count','100','result_type','recent',"tweet_mode","extended"))
    arcadepaz.tweets<- httr::content(x)[['statuses']]
  } else {
    max.id<-min(sapply(arcadepaz.tweets,function(z) return(as.double(z$id_str))))-1
    x<-twitter_anything(auth.vector,d.url,c('q',query.str,'count','100','result_type','recent','max_id',"tweet_mode","extended",sprintf("%0.0f",max.id)))
    arcadepaz.tweets<-c(arcadepaz.tweets,httr::content(x)[['statuses']])
  }
  if(length(httr::content(x))< 2){
    collect <- FALSE
  }
}

#### load into dbase
insert_statuses(arcadepaz.tweets,con) 
Sys.sleep(901)

#### QUERY 5b:  KEYWORD: "arca de la paz"  #################
d.url<-'https://api.twitter.com/1.1/search/tweets.json'

query.str<-'arca de la paz'
collect <- TRUE
n.calls<-0
while(n.calls < 180 && collect){
  n.calls<-n.calls+1
  if(n.calls==1){
    x<-twitter_anything(auth.vector,d.url,c('q',query.str,'count','100','result_type','recent',"tweet_mode","extended"))
    arcadelapaz.tweets<- httr::content(x)[['statuses']]
  } else {
    max.id<-min(sapply(arcadelapaz.tweets,function(z) return(as.double(z$id_str))))-1
    x<-twitter_anything(auth.vector,d.url,c('q',query.str,'count','100','result_type','recent','max_id',"tweet_mode","extended",sprintf("%0.0f",max.id)))
    arcadelapaz.tweets<-c(arcadelapaz.tweets,httr::content(x)[['statuses']])
  }
  if(length(httr::content(x))< 2){
    collect <- FALSE
  }
}

#### load into dbase
insert_statuses(arcadelapaz.tweets,con) 
Sys.sleep(901)


#### QUERY 6:  KEYWORD: "Promesa Duradera"  #################
d.url<-'https://api.twitter.com/1.1/search/tweets.json'

query.str<-'Promesa Duradera'
collect <- TRUE
n.calls<-0
while(n.calls < 180 && collect){
  n.calls<-n.calls+1
  if(n.calls==1){
    x<-twitter_anything(auth.vector,d.url,c('q',query.str,'count','100','result_type','recent',"tweet_mode","extended"))
    promesa.duradera.tweets<-httr::content(x)[['statuses']]
  } else {
    max.id<-min(sapply(promesa.duradera.tweets,function(z) return(as.double(z$id_str))))-1
    x<-twitter_anything(auth.vector,d.url,c('q',query.str,'count','100','result_type','recent','max_id',"tweet_mode","extended",sprintf("%0.0f",max.id)))
    promesa.duradera.tweets<-c(promesa.duradera.tweets,httr::content(x)[['statuses']])
  }
  if(length(httr::content(x))< 2){
    collect <- FALSE
  }
}

#### load into dbase
insert_statuses(promesa.duradera.tweets,con) 
Sys.sleep(901)


#### QUERY 7:  KEYWORD: "Buque Hospital"  #################
d.url<-'https://api.twitter.com/1.1/search/tweets.json'

query.str<-'buque hospital'
collect <- TRUE
n.calls<-0
while(n.calls < 180 && collect){
  n.calls<-n.calls+1
  if(n.calls==1){
    x<-twitter_anything(auth.vector,d.url,c('q',query.str,'count','100','result_type','recent',"tweet_mode","extended"))
    buque.hospital.tweets<-httr::content(x)[['statuses']]
  } else {
    max.id<-min(sapply(buque.hospital.tweets,function(z) return(as.double(z$id_str))))-1
    x<-twitter_anything(auth.vector,d.url,c('q',query.str,'count','100','result_type','recent','max_id',"tweet_mode","extended",sprintf("%0.0f",max.id)))
    buque.hospital.tweets<-c(buque.hospital.tweets,httr::content(x)[['statuses']])
  }
  if(length(httr::content(x))< 2){
    collect <- FALSE
  }
}

#### load into dbase
insert_statuses(buque.hospital.tweets,con) 

Sys.sleep(901)

##### QUERY 8  KEYWORD Buque de Paz  #############
# d.url<-'https://api.twitter.com/1.1/search/tweets.json'
# 
# query.str<-'#BuquedePaz'
# collect <- TRUE
# n.calls<-0
# while(n.calls < 180 && collect){
#   n.calls<-n.calls+1
#   if(n.calls==1){
#     x<-twitter_anything(auth.vector,d.url,c('q',query.str,'count','100','result_type','recent',"tweet_mode","extended"))
#     buque.de.paz.tweets<-httr::content(x)[['statuses']]
#   } else {
#     max.id<-min(sapply(buque.de.paz.tweets,function(z) return(as.double(z$id_str))))-1
#     x<-twitter_anything(auth.vector,d.url,c('q',query.str,'count','100','result_type','recent','max_id',sprintf("%0.0f",max.id)))
#     buque.de.paz.tweets<-c(buque.de.paz.tweets,httr::content(x)[['statuses']])
#   }
#   if(length(httr::content(x))< 2){
#     collect <- FALSE
#   }
# }

# #### load into dbase
# insert_statuses(buque.de.paz.tweets,con) 
# Sys.sleep(901)
# 
# getwd()
# setwd("C:\\Users\\paul.hurley1\\Documents\\BT_twitter\\25Nov")
# #dbDisconnect(con)
# arcadepaz_tweets <- readRDS("arcadepaz_tweets.rds")
# insert_statuses(arcadepaz_tweets,con)
# 
# arca_de_la_paz_tweets <- readRDS("arca_de_la_paz_tweets.rds")
# insert_statuses(arca_de_la_paz_tweets,con)
# 
# buque_hospital_tweets <- readRDS("buque_hospital_tweets.rds")
# insert_statuses(buque_hospital_tweets,con)
# 
# comfort_tweets <- readRDS("comfort_tweets.rds")
# insert_statuses(comfort_tweets,con)
# 
# ep_tweets <- readRDS("ep_tweets.rds")
# insert_statuses(ep_tweets,con)
# 
# hospital_ship_tweets <- readRDS("hosp_ship_tweets.rds")
# insert_statuses(hosp.ship.tweets,con)
# 
# peaceark_tweets <- readRDS("peaceark_tweets.rds")
# insert_statuses(peaceark_tweets,con)
# 
# promesa_duradera_tweets <- readRDS("promesa_duradera_tweets.rds")
# insert_statuses(promesa_duradera_tweets,con)
# 
# count.df <- dbGetQuery(con,"SELECT COUNT(1) FROM status;")
# n <- count.df[1,1]