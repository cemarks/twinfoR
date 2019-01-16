
auth.vector <- authorize_IT()


con <- twitter_database("/home/cemarks/Projects/twinfoR/ignore/test1.sqlite")

# search_tweets_recursive("realDonaldTrump",data.connection=con)
user_timeline_recursive(
  "realDonaldTrump",
  data.connection=con,
  calc.Rsentiment=TRUE,
  calc.syu=TRUE,
  status.media.hash=TRUE,
  status.media.64bit=FALSE,
  profile.img.hash=TRUE,
  profile.img.64bit=FALSE
)

DBI::dbGetQuery(con,"SELECT COUNT(1) FROM status;")
DBI::dbGetQuery(con,"SELECT COUNT(1) FROM user;")


z <- DBI::dbGetQuery(con,"SELECT *,COUNT(img_hash) AS n FROM media GROUP BY img_hash ORDER BY n DESC LIMIT 3;")
setwd("/home/cemarks/Projects/twinfoR/ignore")
p<-status_media(z[1,],conn=con,update.media.b64 = TRUE,save.to.file = TRUE)

insert_users(con,user_show("zlisto"))
followers_ids_recursive("zlisto",data.connection=con)

insert_users(con,user_show("pb2pv"))
followers_list_recursive("pb2pv",data.connection=con)
friends_list_recursive("pb2pv",data.connection=con)

DBI::dbGetQuery(con,"SELECT COUNT(1) FROM followers;")
zlisto<-DBI::dbGetQuery(con,"SELECT * FROM user WHERE screen_name='zlisto';")

DBI::dbGetQuery(con,sprintf("SELECT COUNT(1) FROM followers WHERE follower_id = '%s';",zlisto$id))
DBI::dbGetQuery(con,sprintf("SELECT COUNT(1) FROM followers WHERE friend_id = '%s';",zlisto$id))

summarize_database(con)

DBI::dbDisconnect(con)
file.remove("/home/cemarks/Projects/twinfoR/ignore/test1.sqlite")


#########################


con <- twitter_database("/home/cemarks/Projects/twinfoR/ignore/test1.sqlite")

update_users(con,screen_name=c("pb2pv","zlisto","pop_of_lexi"))
d<-DBI::dbGetQuery(con,"SELECT * FROM user;")
d2 <- DBI::dbGetQuery(con,"SELECT * FROM query_users;")
update_users(con)

update_user_timelines(con)

DBI::dbGetQuery(con,"SELECT COUNT(1) FROM status;")
DBI::dbGetQuery(con,"SELECT COUNT(1) FROM user;")

update_search(con,query.text = "#WorstFirstDate")

DBI::dbGetQuery(con,"SELECT * FROM query_text;")

update_search(con)

DBI::dbGetQuery(con,"SELECT * FROM search_status;")

get_all_friends(con,screen_name="pb2pv")
get_all_friends(con,screen_name="zlisto")
get_all_followers(con,screen_name="pb2pv")
get_all_followers(con,screen_name="zlisto")


get_all_friends(con)

DBI::dbGetQuery(con,"SELECT COUNT(1) FROM followers;")
DBI::dbGetQuery(con,sprintf("SELECT COUNT(1) FROM followers WHERE follower_id = '%s';",zlisto$id))
DBI::dbGetQuery(con,sprintf("SELECT COUNT(1) FROM followers WHERE friend_id = '%s';",zlisto$id))

summarize_database(con)

DBI::dbDisconnect(con)
file.remove("/home/cemarks/Projects/twinfoR/ignore/test1.sqlite")









