lp.tmp <- .libPaths()
.libPaths(c("C:/Users/paul.hurley1/Documents/R/win-library/3.5","F:/My Documents/R/win-library/3.5","C:/Program Files/R/R-3.5.1/library",lp.tmp))

library(rJava)
library(httr)
library(rjson)
library(jsonlite)
library(RSentiment)
library(RSQLite)
library(ggplot2)
library(wordcloud2)
library(webshot)

source("C:\\Users\\paul.hurley1\\Documents\\BT_twitter\\MakeQueries_functions.R")
source("C:\\Users\\paul.hurley1\\Documents\\BT_twitter\\MakePlots_functions.R")


## setup data
setwd("C:\\Users\\paul.hurley1\\Documents\\BT_twitter\\")
record.dir <-"C:\\Users\\paul.hurley1\\Documents\\BT_twitter\\3Dec/"
samp <-"KA" 
con <- dbConnect(SQLite(),"sampleKA.sqlite")                    #establish dbase connection

#### Esmeraldas  ###########
where.criteria <- ("key_media.sublocation == 'Esmeraldas'")
start.date = "2018-10-18"
end.date = "2018-10-30"
daily_sentiment_plot(con, where.criteria,start.date,end.date,"Esmeraldas")
wordcloud_plot(con, where.criteria,start.date,end.date, "Esmeraldas")
top_hashtags_List(con,where.criteria,start.date,end.date,11)
top_usermentions_List(con,where.criteria,start.date,end.date,11)
most_liked_List(con,where.criteria,start.date,end.date,11)
top_tweeters_List(con,where.criteria,start.date,end.date,11)

#### Paita  ###########
where.criteria <- ("key_media.sublocation == 'Paita'")
start.date = "2018-10-28"
end.date = "2018-11-11"
daily_sentiment_plot(con, where.criteria,start.date,end.date,"Paita")
wordcloud_plot(con, where.criteria,start.date,end.date, "Paita")
top_hashtags_List(con,where.criteria,start.date,end.date,11)
top_usermentions_List(con,where.criteria,start.date,end.date,11)
most_liked_List(con,where.criteria,start.date,end.date,11)
top_tweeters_List(con,where.criteria,start.date,end.date,11)

#### TURBO  ###########
where.criteria <- ("key_media.sublocation == 'Turbo'")
start.date = "2018-11-12"
end.date = "2018-11-23"
daily_sentiment_plot(con, where.criteria,start.date,end.date,"Turbo")
wordcloud_plot(con, where.criteria,start.date,end.date, "Turbo")
top_hashtags_List(con,where.criteria,start.date,end.date,11)
top_usermentions_List(con,where.criteria,start.date,end.date,11)
most_liked_List(con,where.criteria,start.date,end.date,11)
top_tweeters_List(con,where.criteria,start.date,end.date,11)

#### RIOHACHA  ###########
where.criteria <- ("key_media.sublocation == 'Riohacha'")
start.date = "2018-11-22"
end.date = "2018-12-02"
daily_sentiment_plot(con, where.criteria,start.date,end.date,"Riohacha")
wordcloud_plot(con, where.criteria,start.date,end.date, "Riohacha")
top_hashtags_List(con,where.criteria,start.date,end.date,11)
top_usermentions_List(con,where.criteria,start.date,end.date,11)
most_liked_List(con,where.criteria,start.date,end.date,11)
top_tweeters_List(con,where.criteria,start.date,end.date,11)

#### KEYWORDS  ###############

#### COMFORT ####
where.criteria <- ("text LIKE '%%enduring promise%%' or text LIKE '%%#EnduringPromise%%'or text LIKE '%%promesa duradera%%' or text LIKE '%%#PromesaDuradera%%' or text LIKE '%%usns comfort%%' or text LIKE '%%a#USNSCOMFORT%%' or text LIKE '%%USNSComfort%%' or text LIKE '%%buque Comfort%%' or text LIKE '%%hospital ship%%' or text LIKE '%%Hospital Ship%%' or text LIKE '%%buque hospital%%') AND (NOT text LIKE '%Guayaquil%' AND NOT text LIKE '%#Guayaquil%' AND NOT text LIKE '%Chino%' AND NOT text LIKE '%chino%' AND NOT text LIKE '%China%'")   
GROUP_BY <- NULL
start.date = "2018-11-22"
end.date = "2018-12-02"

daily_sentiment_plot(con, where.criteria,start.date,end.date,"COMFORT")
wordcloud_plot(con, where.criteria,start.date,end.date, "Keyword ~ COMFORT")

top_hashtags_List(con,where.criteria,start.date,end.date,11)
top_usermentions_List(con,where.criteria,start.date,end.date,11)
most_liked_List(con,where.criteria,start.date,end.date,11)
top_tweeters_List(con,where.criteria,start.date,end.date,11)
most_retweeted_List(con,where.criteria,start.date,end.date,11)
top_media_List(con,where.criteria,start.date,end.date,11)
most_popular_RT_in_sample_List(con,where.criteria,start.date,end.date,11)
most_liked_List(con,where.criteria,start.date,end.date,11)
most_reach_List(con,where.criteria,start.date,end.date,11)

#### ANWEI ####
where.criteria <- ("text LIKE '%%#BuqueHospitalChino%%' OR text LIKE '%%china peace ark%%' OR text LIKE '%%arca de paz%%' OR text LIKE '%%#ArcaDePaz%%' OR text LIKE '%%#BuquedePaz%%' or text LIKE '%%Peace Ark%%' or text LIKE '%%#PeaceArk%%'")
start.date = "2018-11-12"
end.date = "2018-11-19"
daily_sentiment_plot(con, where.criteria, start.date, end.date,"ANWEI Terms (e.g., Peace Ark, Arca de Paz, Buque Hospital Chino)")
wordcloud_plot(con, where.criteria,start.date,end.date, "Keyword ~ Peace Ark")

top_hashtags_List(con,where.criteria,start.date,end.date,11)
top_usermentions_List(con,where.criteria,start.date,end.date,11)
most_liked_List(con,where.criteria,start.date,end.date,11)
top_tweeters_List(con,where.criteria,start.date,end.date,11)
most_retweeted_List(con,where.criteria,start.date,end.date,11)
top_media_List(con,where.criteria,start.date,end.date,11)                



### COMFORT ANALYSIS. ID TWEETS AND BUILD 
# Q5a  COMFORT related Terms from any Global Twitter Account
SELECT <- "SELECT status.screen_name, status.text, status.created_at, status.retweet, status.lang, status.sentiment_score, status.nrc_sentiment_positive, status.nrc_sentiment_negative, key_media.sublocation, key_media.subgroup, key_media.country, key_media.category"
FROM <- "FROM status LEFT JOIN key_media ON key_media.user_id=status.user_id"
WHERE <- "WHERE status.created_at >= '2018-09-12' AND status.created_at <= '2018-12-12' AND (text LIKE '%%enduring promise%%' or text LIKE '%%buque hospital Comfort%%' or text LIKE '%%#EnduringPromise%%'or text LIKE '%%promesa duradera%%' or text LIKE '%%#PromesaDuradera%%' or text LIKE '%%usns comfort%%' or text LIKE '%%a#USNSCOMFORT%%' or text LIKE '%%USNSComfort%%' or text LIKE '%%buque Comfort%%' or text LIKE '%%hospital ship%%' or text LIKE '%%Hospital Ship%%' or text LIKE '%%buque hospital%%') AND (NOT text LIKE '%Guayaquil%' AND NOT text LIKE '%#Guayaquil%' AND NOT text LIKE '%Chino%' AND NOT text LIKE '%chino%' AND NOT text LIKE '%China%');" 
GROUP_BY <- NULL
ORDER_BY <- "ORDER BY status.created_at"
LIMIT <- NULL #"LIMIT 500"
OFFSET <- NULL      # Allows you to ignore the first n results

QUERY <- paste(SELECT,FROM,WHERE,GROUP_BY,ORDER_BY,LIMIT,OFFSET,";",sep=" ")
r5.1 <- dbGetQuery(con,QUERY)
r5.1$created_at <- as.POSIXct(r5.1$created_at) # , format = "%a %b %d %H:%M:%S +0000 %Y")#, origin = "1970-01-01")
r5.1$TimeStamp <- as.Date(r5.1$created_at, format = '%Y-%m-%d')

## what number of COMFORT Tweets are attributable to xxxx population?  Compare larger xxxx population to the COMFORT results
sample.full <- read.csv("L:\\Org_Folders\\SCJ8\\Restricted\\SCJ86\\SCJ86 Files\\BT_twitter\\KeyAudience_Lists\\Comfort_Stop_locations_full_users_list.csv")
riohacha.full <- sample.full[sample.full$location =="Riohacha",]
index <- r5.1$screen_name %in% riohacha.full$screen.name
riohacha.sample.matches.comfort <- r5.1[index, ]

write.csv(r5.1, "C:\\Users\\paul.hurley1\\Documents\\BT_twitter\\OBT_Tweets_sep2dec.csv")




#### Q5b  ANWEI related Terms from any Global Twitter Account ####
SELECT <- "SELECT status.screen_name, status.text, status.created_at, status.retweet, status.lang, status.sentiment_score, status.nrc_sentiment_positive, status.nrc_sentiment_negative, key_media.sublocation, key_media.subgroup, key_media.country, key_media.category"
FROM <- "FROM status LEFT JOIN key_media ON key_media.user_id=status.user_id"
WHERE <- "WHERE status.created_at >= '2018-09-15' AND status.created_at < '2018-12-12' AND (text LIKE '%%buque hospital%%' or text LIKE '%%#BuquedePaz%%' or text LIKE '%%arca de la paz%%' or text LIKE '%%arcadepaz%%' or text LIKE '%%peace ark%%' or text LIKE '%%ANWEI%%') AND (NOT text LIKE '%%enduring promise%%' AND NOT text LIKE '%%#EnduringPromise%%' AND NOT text LIKE '%%Buque Hospital USNS%%' AND NOT text LIKE '%%EEUU%%' AND NOT text LIKE '%%EE.UU.%%' AND NOT text LIKE '%%norteamericano%%' AND NOT text LIKE '%%Riohacha%%' AND NOT text LIKE '%%#UCSG%%' AND NOT text LIKE '%%Buque hospital Confort%%' AND NOT text LIKE '%%USN Comfort%%' AND NOT text LIKE '%%Estados Unidos%%' AND NOT text like '%%EE. UU%%' AND NOT text LIKE '%%Colombia%%' AND NOT text LIKE '%%U.S. Navy%%' AND NOT text LIKE '%%buque hospital Comfort%%' AND NOT text LIKE '%%USNS%%' AND NOT text LIKE '%%@USEmbassyBogota%%' AND NOT text LIKE '%%promesa duradera%%' AND NOT text LIKE '%%#PromesaDuradera%%' AND NOT text LIKE '%%#USNS COMFORT%%' AND NOT text LIKE '%%usns comfort%%' AND NOT text LIKE '%%a#USNSCOMFORT%%' AND NOT text LIKE '%%USNSComfort%%' AND NOT text LIKE '%%buque Comfort%%');"   
GROUP_BY <- NULL
ORDER_BY <- "ORDER BY status.created_at"
LIMIT <- NULL #"LIMIT 500"
OFFSET <- NULL      # Allows you to ignore the first n results

QUERY <- paste(SELECT,FROM,WHERE,GROUP_BY,ORDER_BY,LIMIT,OFFSET,";",sep=" ")

r5.2 <- dbGetQuery(con,QUERY)
r5.2$created_at <- as.POSIXct(r5.2$created_at) # , format = "%a %b %d %H:%M:%S +0000 %Y")#, origin = "1970-01-01")
r5.2$TimeStamp <- as.Date(r5.2$created_at, format = '%Y-%m-%d')
r5.2$sent_result <- r5.2$nrc_sentiment_positive - r5.2$nrc_sentiment_negative


write.csv(r5.2, "C:\\Users\\paul.hurley1\\Documents\\BT_twitter\\ANWEI_Tweets_sep2Dec2018.csv")


#############################################################################################################################################################
#### WORKING SECTION BELOW...NOT USED #################################################################################
#################################################################################################################################################################



### COMFORT ANALYSIS. ID TWEETS AND BUILD 
####Q1d: Summary Count of Tweets from our population sample by sublocation
SELECT <- "SELECT key_media.sublocation, COUNT(key_media.sublocation) AS n"
FROM <- "FROM status JOIN key_media ON status.user_id = key_media.user_id"
WHERE <- "WHERE status.created_at >= '2018-10-01' AND status.created_at < '2018-12-02' AND (text LIKE '%%enduring promise%%' or text LIKE '%%a#EnduringPromise%%'or text LIKE '%%promesa duradera%%' or text LIKE '%%#PromesaDuradera%%' or text LIKE '%%usns comfort%%' or text LIKE '%%a#USNSCOMFORT%%' or text LIKE '%%USNSComfort%%' or text LIKE '%%buque Comfort%%')"   
GROUP_BY <- "GROUP BY key_media.sublocation"
ORDER_BY <- "ORDER BY n DESC"                              # cite how you want to see results a-z, z-a
LIMIT <- NULL                                         #number of results you want to see or NULL
OFFSET <- NULL                                       #allows you to ignore first x entries

QUERY <- paste(SELECT,FROM,WHERE,GROUP_BY,ORDER_BY,LIMIT,OFFSET,";",sep=" ")

r5.4 <- dbGetQuery(con,QUERY)


####
SELECT <- "SELECT status.text, status.created_at, status.screen_name"
FROM <- "FROM status JOIN key_media ON status.user_id = key_media.user_id"
WHERE <- "WHERE status.created_at >= '2018-10-31' AND status.created_at < '2018-11-08' AND key_media.category == 'ADV'AND (text LIKE '%%enduring promise%%' or text LIKE '%%a#EnduringPromise%%'or text LIKE '%%promesa duradera%%' or text LIKE '%%#PromesaDuradera%%' or text LIKE '%%usns comfort%%' or text LIKE '%%a#USNSCOMFORT%%' or text LIKE '%%USNSComfort%%' or text LIKE '%%buque Comfort%%')"   
GROUP_BY <- NULL
ORDER_BY <- NULL                             # cite how you want to see results a-z, z-a
LIMIT <- NULL                                         #number of results you want to see or NULL
OFFSET <- NULL                                       #allows you to ignore first x entries

QUERY <- paste(SELECT,FROM,WHERE,GROUP_BY,ORDER_BY,LIMIT,OFFSET,";",sep=" ")

r5.7 <- dbGetQuery(con,QUERY)

r5.3[r5.3$screen_name == "contrapuntovzla",]

write.csv(r5.3,"C:\\Users\\paul.hurley1\\Documents\\BT_twitter\\stratcomms_6nov.csv")

####
SELECT <- "SELECT status.text, status.created_at, status.screen_name"
FROM <- "FROM status"
WHERE <- "WHERE status.created_at >= '2018-10-31' AND status.created_at <= '2018-11-30' AND (text LIKE '%%navy hospital ship%%' or text LIKE '%%US hospital ship%%' or text LIKE '%%US buque hospital%%' and text LIKE '%%enduring promise%%' or text LIKE '%%a#EnduringPromise%%'or text LIKE '%%promesa duradera%%' or text LIKE '%%#PromesaDuradera%%' or text LIKE '%%usns comfort%%' or text LIKE '%%a#USNSCOMFORT%%' or text LIKE '%%USNSComfort%%' or text LIKE '%%buque Comfort%%')"   
GROUP_BY <- NULL
ORDER_BY <- NULL                             # cite how you want to see results a-z, z-a
LIMIT <- NULL                                         #number of results you want to see or NULL
OFFSET <- NULL                                       #allows you to ignore first x entries

QUERY <- paste(SELECT,FROM,WHERE,GROUP_BY,ORDER_BY,LIMIT,OFFSET,";",sep=" ")

r5.81 <- dbGetQuery(con,QUERY)
write.csv(r5.81,"31OCT_7NOV_Keywords")
u <-unique(r5.81$text)


### special queries looking for tweets with a specific usermention

#  #US
SELECT <- "SELECT status.text, status.created_at, status.screen_name"
FROM <- "FROM status"
WHERE <- "WHERE status.created_at >= '2018-10-31' AND status.created_at < '2018-11-08' AND (text LIKE '%%navy hospital ship%%' or text LIKE '%%US hospital ship%%' or text LIKE '%%US buque hospital%%' and text LIKE '%%enduring promise%%' or text LIKE '%%a#EnduringPromise%%'or text LIKE '%%promesa duradera%%' or text LIKE '%%#PromesaDuradera%%' or text LIKE '%%usns comfort%%' or text LIKE '%%a#USNSCOMFORT%%' or text LIKE '%%USNSComfort%%' or text LIKE '%%buque Comfort%%')"   
GROUP_BY <- NULL
ORDER_BY <- NULL                             # cite how you want to see results a-z, z-a
LIMIT <- NULL                                         #number of results you want to see or NULL
OFFSET <- NULL                                       #allows you to ignore first x entries

QUERY <- paste(SELECT,FROM,WHERE,GROUP_BY,ORDER_BY,LIMIT,OFFSET,";",sep=" ")

r5.xx <- dbGetQuery(con,QUERY)

####
SELECT <- "SELECT status.text, status.created_at, status.screen_name"
FROM <- "FROM status JOIN key_media ON status.user_id = key_media.user_id"
WHERE <- "WHERE status.created_at >= '2018-10-30' AND status.created_at < '2018-11-08' AND key_media.sublocation == 'Paita' AND text LIKE '%%#US%%'"   
GROUP_BY <- NULL
ORDER_BY <- NULL                             # cite how you want to see results a-z, z-a
LIMIT <- NULL                                         #number of results you want to see or NULL
OFFSET <- NULL                                       #allows you to ignore first x entries

QUERY <- paste(SELECT,FROM,WHERE,GROUP_BY,ORDER_BY,LIMIT,OFFSET,";",sep=" ")

r5.9 <- dbGetQuery(con,QUERY)
r5.9unique <- unique(r5.9$text)

SELECT <- "SELECT status.text, status.created_at, status.screen_name"
FROM <- "FROM status JOIN key_media ON status.user_id = key_media.user_id"
WHERE <- "WHERE status.created_at >= '2018-11-21' AND status.created_at < '2018-11-28' AND key_media.sublocation == 'Turbo'"  #AND (text LIKE '%%navy hospital ship%%' or text LIKE '%%US hospital ship%%' or text LIKE '%%US buque hospital%%' and text LIKE '%%enduring promise%%' or text LIKE '%%a#EnduringPromise%%'or text LIKE '%%promesa duradera%%' or text LIKE '%%#PromesaDuradera%%' or text LIKE '%%usns comfort%%' or text LIKE '%%a#USNSCOMFORT%%' or text LIKE '%%USNSComfort%%' or text LIKE '%%buque Comfort%%')"  
GROUP_BY <- NULL
ORDER_BY <- NULL                                      # cite how you want to see results a-z, z-a
LIMIT <- NULL                                         # number of results you want to see or NULL
OFFSET <- NULL                                        # allows you to ignore first x entries

QUERY <- paste(SELECT,FROM,WHERE,GROUP_BY,ORDER_BY,LIMIT,OFFSET,";",sep=" ")

r5.91 <- dbGetQuery(con,QUERY)
r5.91unique <- unique(r5.9$text)




where.criteria <- ("text LIKE '%%#BuqueHospitalChino%%' OR text LIKE '%%china peace ark%%' OR text LIKE '%%arca de paz%%' OR text LIKE '%%#ArcaDePaz%%' OR text LIKE '%%#BuquedePaz%%' or text LIKE 'Peace Ark'")
daily_sentiment_plot(con, where.criteria, start.date='2018-09-15', end.date='2018-10-01',"ANWEI Terms (e.g., Peace Ark, Arca de Paz, Buque Hospital Chino)")


SELECT <- "SELECT status.text, status.created_at, status.screen_name, status.id, status.nrc_sentiment_positive, status.nrc_sentiment_negative"
FROM <- "FROM status" #JOIN key_media ON status.user_id = key_media.user_id"
WHERE <- "WHERE status.created_at >= '2018-09-01' AND status.created_at < '2018-10-15' AND (text LIKE '%%#BuqueHospitalChino%%' OR text LIKE '%%china peace ark%%' OR text LIKE '%%arca de paz%%' OR text LIKE '%%#ArcaDePaz%%' OR text LIKE '%%#BuquedePaz%%' or text LIKE '%%Peace Ark%%' or TEXT LIKE '%%buque hospital china%%')"  
GROUP_BY <- NULL
ORDER_BY <- NULL                                      # cite how you want to see results a-z, z-a
LIMIT <- NULL                                         # number of results you want to see or NULL
OFFSET <- NULL                                        # allows you to ignore first x entries

QUERY <- paste(SELECT,FROM,WHERE,GROUP_BY,ORDER_BY,LIMIT,OFFSET,";",sep=" ")

r5.95 <- dbGetQuery(con,QUERY)
r5.95unique <- unique(r5.95$text)
write.csv(r5.95,"peace_ark_tweet_sentiment.csv")



### KEY MEDIA TABLE CONTENTS
SELECT <- "SELECT *"
FROM <- "FROM key_media"
WHERE <- NULL
ORDER_BY <- NULL
LIMIT <- NULL #"LIMIT 500"
OFFSET <- NULL      # Allows you to ignore the first n results

QUERY <- paste(SELECT,FROM,WHERE,GROUP_BY,ORDER_BY,LIMIT,OFFSET,";",sep=" ")
r11.1 <- dbGetQuery(con,QUERY)

str(r11.1)
r11.1$category <- as.factor(r11.1$category)
levels(r11.1$category)
