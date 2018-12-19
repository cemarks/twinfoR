devtools::load_all("~/Projects/twinfoR")
DBI::dbDisconnect(con)
file.remove('test1.sqlite')

s <- search_tweets("#Snowmageddon2018")
s <- s$statuses
con<-twitter_database("test1.sqlite")
insert_statuses(con,s,store.64encode = TRUE)

DBI::dbGetQuery(con,"SELECT COUNT(1) FROM user WHERE profile_img_hash IS NOT NULL;")
DBI::dbGetQuery(con,"SELECT COUNT(1) FROM user WHERE profile_banner_hash IS NOT NULL;")
DBI::dbGetQuery(con,"SELECT COUNT(1) FROM user;")
DBI::dbGetQuery(con,"SELECT COUNT(1) FROM media WHERE media_type='photo';")
DBI::dbGetQuery(con,"SELECT COUNT(1) FROM media WHERE img_hash IS NOT NULL;")

DBI::dbGetQuery(con,"SELECT COUNT(1) FROM user WHERE profile_img_b64 IS NOT NULL;")
DBI::dbGetQuery(con,"SELECT COUNT(1) FROM user WHERE profile_banner_b64 IS NOT NULL;")
DBI::dbGetQuery(con,"SELECT COUNT(1) FROM user;")
DBI::dbGetQuery(con,"SELECT COUNT(1) FROM media WHERE media_type='photo';")
DBI::dbGetQuery(con,"SELECT COUNT(1) FROM media WHERE img_b64 IS NOT NULL;")
