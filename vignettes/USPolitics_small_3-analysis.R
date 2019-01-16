# Vignette to analyze US politics Part III of III (SMALL).  

### This is part 3 of the three-part USPolitics_small vignette.

### This script shows some of the analysis capabilities of the twinfoR package.
### These are farily simple functions to quickly look at features of the 
### collected data.  The real power is in tailored analysis to match the
### specific problem being addressed.  A user that can write SQLite queries
### and is familiar with R can reference the twitter_database documentation
### for database structure and produce any analysis supported by the data.


library(twinfoR)
# devtools::load_all("/home/cemarks/Projects/twinfoR")

# Initialize some things
setwd("~/small_politics")
load("auth_vector.RData")

# Connect to database created in part 1.
t.con <- twitter_database(
  "pol-small.sqlite",
)


### Top hashtags from democrats and republicans in the last week

tophashtags.dem <- top_hashtags(
  t.con,
  start.date = Sys.Date()-7,
  where.criteria = "query_users.party = 'democrat'",
  excel.export.file = "thtd.xlsx"
)

tophashtags.rep <- top_hashtags(
  t.con,
  start.date = Sys.Date()-7,
  where.criteria = "query_users.party = 'republican'",
  excel.export.file = "thtr.xlsx"
)

tophashtags.dem
tophashtags.rep

### Top usermentions from democrats and republicans in the last week

topusermentions.dem <- top_usermentions(
  t.con,
  start.date = Sys.Date()-7,
  where.criteria = "query_users.party = 'democrat'",
  excel.export.file = "tumd.xlsx"
)


topusermentions.rep <- top_usermentions(
  t.con,
  start.date = Sys.Date()-7,
  where.criteria = "query_users.party = 'republican'",
  excel.export.file = "tumr.xlsx"
)

topusermentions.dem
topusermentions.rep

### Top media from democrats and republicans in the last week

topmedia.dem <- top_media(
  t.con,
  start.date = Sys.Date()-7,
  where.criteria = "query_users.party = 'democrat'",
  media.file.prefix = "DEM",
  excel.export.file = "tmd.xlsx"
)



topmedia.rep <- top_media(
  t.con,
  start.date = Sys.Date()-7,
  where.criteria = "query_users.party = 'republican'",
  media.file.prefix = "REP",
  excel.export.file = "tmr.xlsx"
)

topmedia.dem
topmedia.rep


### Top tweeters democrats and republicans in the last week

toptweeters.dem <- top_tweeters(
  t.con,
  start.date = Sys.Date()-7,
  where.criteria = "query_users.party = 'democrat'"
)

toptweeters.rep <- top_tweeters(
  t.con,
  start.date = Sys.Date()-7,
  where.criteria = "query_users.party = 'republican'"
)

toptweeters.dem
toptweeters.rep

### Most liked tweets democrats and republicans in the last week

mostliked.dem <- most_liked(
  t.con,
  start.date = Sys.Date()-7,
  where.criteria = "query_users.party = 'democrat'",
  excel.export.file = "mld.xlsx"
)

mostliked.rep <- most_liked(
  t.con,
  start.date = Sys.Date()-7,
  where.criteria = "query_users.party = 'republican'",
  excel.export.file = "mlr.xlsx"
)

mostliked.dem
mostliked.rep


### Most retweeted tweets democrats and republicans in the last week

mostretweeted.dem <- most_retweeted(
  t.con,
  start.date = Sys.Date()-7,
  where.criteria = "query_users.party = 'democrat'"
)

mostretweeted.rep <- most_retweeted(
  t.con,
  start.date = Sys.Date()-7,
  where.criteria = "query_users.party = 'republican'",
  excel.export.file = "mrr.xlsx"
)

mostretweeted.dem
mostretweeted.rep


## Most popular retweet in sample

mostpopRT.dem <- most_popular_RT_in_sample(
  t.con,
  start.date = Sys.Date()-7,
  where.criteria = "query_users.party = 'democrat'",

)

mostpopRT.rep <- most_popular_RT_in_sample(
  t.con,
  start.date = Sys.Date()-7,
  where.criteria = "query_users.party = 'republican'"
)

mostpopRT.dem
mostpopRT.rep


### Most reach -- this metric is of dubious value.

mostreach.dem <- most_reach(
  t.con,
  start.date = Sys.Date()-7,
  where.criteria = "query_users.party = 'democrat'"
)

mostreach.rep <- most_reach(
  t.con,
  start.date = Sys.Date()-7,
  where.criteria = "query_users.party = 'republican'"
)

mostreach.dem
mostreach.rep


### Word clouds 

wordcloud_plot(
  t.con,
  start.date = Sys.Date()-7,
  where.criteria = "query_users.party = 'democrat'"
)

wordcloud_plot(
  t.con,
  start.date = Sys.Date()-7,
  where.criteria = "query_users.party = 'republican'"
)

### Sentiment plots

sentiment_plots(
  t.con,
  start.date = Sys.Date()-7,
  where.criteria = "query_users.party = 'democrat'",
  file.name.prefix = "Dem",
  caption = "Democrats"
)

sentiment_plots(
  t.con,
  start.date = Sys.Date()-7,
  where.criteria = "query_users.party = 'republican'",
  file.name.prefix = "Rep",
  caption = "Republicans"
)

### Tweet timeplot

timeplot(
  t.con,
  start.date = Sys.Date()-7,
  where.criteria = "query_users.party = 'republican' OR query_users.party = 'democrat'",
  group.column = "query_users.party"
)



## Friend network analysis

library(igraph)

edgelist.query <- "SELECT query_users_fo.screen_name as follower, query_users_fr.screen_name as friend, query_users_fo.party as follower_party, query_users_fr.party as friend_party FROM query_users as query_users_fo JOIN followers ON query_users_fo.user_id = followers.follower_id JOIN query_users AS query_users_fr ON followers.friend_id = query_users_fr.user_id;"
edgelist.df <- DBI::dbGetQuery(t.con,edgelist.query)
g <- graph_from_data_frame(edgelist.df)
vertices.df <- data.frame(index=as.integer(V(g)), screen_name = names(V(g)),stringsAsFactors = FALSE)
vertex.query <- sprintf("SELECT query_users.screen_name,query_users.party,query_users.body,user.name,user.location,user.description,user.followers_count,user.statuses_count FROM query_users JOIN user ON query_users.user_id = user.id WHERE query_users.screen_name IN (%s);",paste("'",vertices.df$screen_name,"'",sep="",collapse=","))
vertex.query.df <- DBI::dbGetQuery(t.con,vertex.query)
vertex.query.df <- vertex.query.df[match(vertices.df$screen_name,vertex.query.df$screen_name),]

bluered <- function(party){
  if(tolower(party)=="democrat"){
    return("blue")
  } else if(tolower(party)=="republican"){
    return("red")
  } else {
    return("black")
  }
}

vertex.query.df$color <- sapply(vertex.query.df$party,bluered)


#### Eigenvector Centrality

ec <- centr_eigen(g,directed=FALSE)
cbind(vertex.query.df$screen_name,ec$vector)

#### Betweenness centrality

bc <- centr_betw(g,directed=FALSE)
cbind(vertex.query.df$screen_name,ec$vector,bc$res)

#### Clustering (label propagation)

clust.labelprop <- cluster_label_prop(as.undirected(g))
clust.labelprop

#### Visualize

verts <- data.frame(
    name = vertex.query.df$screen_name,
    label = vertex.query.df$screen_name,
    title = vertex.query.df$name,
    color = vertex.query.df$color,
    stringsAsFactors = FALSE
)

for(n in names(verts)){
  g <- set_vertex_attr(g,n,value=verts[,n])
}


plot(g,vertex.color=vertex_attr(g,"color"))

#### More visualizations...

#### With sigmajs

library(sigmajs)

s <- sigmajs()
s <- sg_from_igraph(s,g)
htmlwidgets::saveWidget(s,"followers-sigmajs.html")



#### With visNetwork

library(visNetwork)
v <- visIgraph(g)
v <- visEdges(
  v,
  color="gray"
)
v <- visNodes(
  v,
  size = 10
)
# v <- visOptions(v, autoResize = TRUE)

htmlwidgets::saveWidget(v,"followers-visnetwork.html")





## User mention network analysis

usermention.query <- sprintf("SELECT query_users_mentioner.screen_name,query_users_mentioned.screen_name, status.id as status_id, status.text as status_text, status.created_at as created_at FROM query_users as query_users_mentioner JOIN status ON query_users_mentioner.user_id = status.user_id JOIN user_mention ON status.id = user_mention.status_id JOIN query_users as query_users_mentioned ON user_mention.user_mention_id = query_users_mentioned.user_id WHERE status.created_at > '%s';",format(Sys.Date()-7))
usermentions.df <- DBI::dbGetQuery(t.con,usermention.query)
if(nrow(usermentions.df) > 0){
  g <- graph_from_data_frame(usermentions.df)
  vertices.df <- data.frame(index=as.integer(V(g)), screen_name = names(V(g)),stringsAsFactors = FALSE)
  vertex.query <- sprintf("SELECT query_users.screen_name,query_users.party,query_users.body,user.name,user.location,user.description,user.followers_count,user.statuses_count FROM query_users JOIN user ON query_users.user_id = user.id WHERE query_users.screen_name IN (%s);",paste("'",vertices.df$screen_name,"'",sep="",collapse=","))
  vertex.query.df <- DBI::dbGetQuery(t.con,vertex.query)
  vertex.query.df <- vertex.query.df[match(vertices.df$screen_name,vertex.query.df$screen_name),]

  bluered <- function(party){
    if(tolower(party)=="democrat"){
      return("blue")
    } else if(tolower(party)=="republican"){
      return("red")
    } else {
      return("black")
    }
  }

  vertex.query.df$color <- sapply(vertex.query.df$party,bluered)


  #### Eigenvector Centrality

  ec <- centr_eigen(g,directed=FALSE)
  cbind(vertex.query.df$screen_name,ec$vector)

  #### Betweenness centrality

  bc <- centr_betw(g,directed=FALSE)
  cbind(vertex.query.df$screen_name,ec$vector,bc$res)

  #### Clustering (label propagation)

  clust.labelprop <- cluster_label_prop(as.undirected(g))
  clust.labelprop

  #### Visualize

  verts <- data.frame(
      name = vertex.query.df$screen_name,
      label = vertex.query.df$screen_name,
      title = vertex.query.df$name,
      color = vertex.query.df$color,
      stringsAsFactors = FALSE
  )

  for(n in names(verts)){
    g <- set_vertex_attr(g,n,value=verts[,n])
  }


  plot(g,vertex.color=vertex_attr(g,"color"))

  #### More visualizations...

  #### With sigmajs

  library(sigmajs)

  s <- sigmajs()
  s <- sg_from_igraph(s,g)
  htmlwidgets::saveWidget(s,"usermention-sigmajs.html")
  browseURL("usermention-sigmajs.html")


  #### With visNetwork

  g <- set_edge_attr(g,"title",E(g),paste(edge_attr(g,"created_at"),edge_attr(g,"status_text"),sep=": "))

  library(visNetwork)
  v <- visIgraph(g)
  v <- visEdges(
    v,
    color="gray"
  )
  v <- visNodes(
    v,
    size = 10
  )
  # v <- visOptions(v, autoResize = TRUE)

  htmlwidgets::saveWidget(v,"usermentions-visnetwork.html")
}

## Would be interesting to plot sentiment of blue to red, red to blue, etc.





## Retweet network analysis


retweet.query <- sprintf("SELECT query_users_retweeter.screen_name,query_users_retweeted.screen_name, status_retweet.id as status_id, status_retweet.text as retweet_text, status_original.text as original_text, status_retweet.created_at as created_at FROM query_users as query_users_retweeter JOIN status as status_retweet ON query_users_retweeter.user_id = status_retweet.user_id JOIN status as status_original ON status_retweet.retweet_status_id = status_original.id JOIN query_users as query_users_retweeted ON status_original.user_id = query_users_retweeted.user_id WHERE status_retweet.created_at > '%s' AND status_retweet.retweet = 1;",format(Sys.Date()-7))
retweets.df <- DBI::dbGetQuery(t.con,retweet.query)
if(nrow(retweets.df) > 0){
  g <- graph_from_data_frame(retweet.df)
  vertices.df <- data.frame(index=as.integer(V(g)), screen_name = names(V(g)),stringsAsFactors = FALSE)
  vertex.query <- sprintf("SELECT query_users.screen_name,query_users.party,query_users.body,user.name,user.location,user.description,user.followers_count,user.statuses_count FROM query_users JOIN user ON query_users.user_id = user.id WHERE query_users.screen_name IN (%s);",paste("'",vertices.df$screen_name,"'",sep="",collapse=","))
  vertex.query.df <- DBI::dbGetQuery(t.con,vertex.query)
  vertex.query.df <- vertex.query.df[match(vertices.df$screen_name,vertex.query.df$screen_name),]

  bluered <- function(party){
    if(tolower(party)=="democrat"){
      return("blue")
    } else if(tolower(party)=="republican"){
      return("red")
    } else {
      return("black")
    }
  }

  vertex.query.df$color <- sapply(vertex.query.df$party,bluered)


  #### Eigenvector Centrality

  ec <- centr_eigen(g,directed=FALSE)
  cbind(vertex.query.df$screen_name,ec$vector)

  #### Betweenness centrality

  bc <- centr_betw(g,directed=FALSE)
  cbind(vertex.query.df$screen_name,ec$vector,bc$res)

  #### Clustering (label propagation)

  clust.labelprop <- cluster_label_prop(as.undirected(g))
  clust.labelprop

  #### Visualize

  verts <- data.frame(
      name = vertex.query.df$screen_name,
      label = vertex.query.df$screen_name,
      title = vertex.query.df$name,
      color = vertex.query.df$color,
      stringsAsFactors = FALSE
  )

  for(n in names(verts)){
    g <- set_vertex_attr(g,n,value=verts[,n])
  }


  plot(g,vertex.color=vertex_attr(g,"color"))

  #### More visualizations...

  #### With sigmajs

  library(sigmajs)

  s <- sigmajs()
  s <- sg_from_igraph(s,g)
  htmlwidgets::saveWidget(s,"retweet-sigmajs.html")
  browseURL("retweet-sigmajs.html")


  #### With visNetwork

  g <- set_edge_attr(g,"title",E(g),paste(edge_attr(g,"created_at"),edge_attr(g,"retweet_text"),sep=": "))

  library(visNetwork)
  v <- visIgraph(g)
  v <- visEdges(
    v,
    color="gray"
  )
  v <- visNodes(
    v,
    size = 10
  )
  # v <- visOptions(v, autoResize = TRUE)

  htmlwidgets::saveWidget(v,"retweet-visnetwork.html")
}


DBI::dbDisconnect(t.con)
