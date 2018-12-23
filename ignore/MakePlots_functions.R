#### THIS SCRIPT LOADS 17 DIFFERENT FUNCTIONS THAT PRODUCE PLOTS OR EXCEL SPREADSHEETS 

#### PLOT FUNCTIONS
daily_sentiment_plot <- function(con, where.criteria, start.date=NULL, end.date=NULL, caption="population"){
  require(ggplot2)
  daily.sentiment.data <- daily_sentiment(con, where.criteria, start.date, end.date)
  daily.sentiment.data$created_at <- as.POSIXct(daily.sentiment.data$created_at) # , format = "%a %b %d %H:%M:%S +0000 %Y")#, origin = "1970-01-01")
  daily.sentiment.data$TimeStamp <- as.Date(daily.sentiment.data$created_at, format = '%Y-%m-%d')
  #### Covert Sentiment values into levels
  daily.sentiment.data$sent_result <- daily.sentiment.data$nrc_sentiment_positive - daily.sentiment.data$nrc_sentiment_negative
  daily.sentiment.data$sent_label <- ifelse(is.na(daily.sentiment.data$sent_result), "unknown",
                                            ifelse(daily.sentiment.data$sent_result < -1, "negative",
                                                   ifelse(daily.sentiment.data$sent_result < 0, "slight negative",
                                                          ifelse(daily.sentiment.data$sent_result == 0, "neutral",      
                                                                 ifelse(daily.sentiment.data$sent_result >0 & daily.sentiment.data$sent_result <=1, "slight positive",
                                                                        ifelse(daily.sentiment.data$sent_result >1, "positive",
                                                                               "unknown"))))))
  daily.sentiment.data$sent_label <- as.factor(daily.sentiment.data$sent_label)
  daily.sentiment.data$sent_label <- factor(daily.sentiment.data$sent_label, levels = c('positive', 'slight positive', 'neutral', 'slight negative','negative', 'unknown'))
  # Sentiment bar plot 
  #png(paste(record.dir,"sentiment_barplot_",caption,".png",sep=""),height=600,width=800)  
  png(sprintf("%s/sentiment_barplot_%s_%s_%s.png", record.dir, caption,start.date, end.date),height=600,width=800)
  g <- ggplot(daily.sentiment.data, aes(x = TimeStamp)) +
    geom_bar(stat="count", aes(fill=sent_label))   +                     #fill by sent_label
    labs(title = "Daily Tweet Volume by tweet type", 
         subtitle = sprintf("period: %s thru %s", start.date,end.date),
         x="Date of Tweet", 
         y="Daily Tweet Totals", 
         caption = caption) +
    theme_bw()  +
    scale_fill_manual(name = "Sentiment", values=c(positive = "dark green", "slight positive" = "light green", neutral="grey", "slight negative" = "orange", negative= "red", unknown="black"), na.value = "black") 
  plot(g)
  dev.off()
  
  #100%  stacked bar plot
  #png("sentiment_percent_barplot.png",height=600,width=800)  
  png(sprintf("%s/sentiment_percent_barplot_%s_%s_%s.png", record.dir, caption,start.date, end.date),height=600,width=800)
  g<- ggplot(daily.sentiment.data, aes(x = TimeStamp)) +
    geom_bar(stat="count", position="fill", aes(fill=sent_label))   +                     #fill by sent_label
    labs(title = "Daily Tweet Volume by tweet type", 
         subtitle = sprintf("period: %s thru %s", start.date,end.date),
         x="Date of Tweet", 
         y="Percent of Daily Tweets", 
         caption = caption) +
    theme_bw()  +
    scale_fill_manual(name = "Sentiment", values=c(positive = "dark green", "slight positive" = "light green", neutral="grey", "slight negative" = "orange", negative= "red", unknown="black"), na.value = "black") 
  plot(g)
  dev.off()
  
  # Line Plot of percentage sentiment
  require(dplyr)
  #png("sentiment_lineplot.png",height=600,width=800)  
  png(sprintf("%s/sentiment_percent_lineplot_%s_%s_%s.png", record.dir, caption,start.date, end.date),height=600,width=800)
  d2 <- daily.sentiment.data %>% 
        group_by(TimeStamp,sent_label) %>% 
        summarise(count=n()) %>% 
        mutate(perc=count/sum(count))
  g <- ggplot(d2, aes(x = TimeStamp, y = perc*100)) +
        geom_line(aes(col=sent_label), size = 4) +
        labs(x = "Tweet Date", 
          y = "PERCENT", 
          title = "Daily Tweet Sentiment", 
          subtitle = sprintf("period: %s thru %s", start.date,end.date),
          caption=caption) +
    theme_bw()  +
    scale_color_manual(name = "Sentiment", values=c(positive = "dark green", "slight positive" = "light green", neutral="grey", "slight negative" = "orange", negative= "red", unknown="black"), na.value = "black") 
  plot(g)
  dev.off()
}



#### test to make sure it works::
# con <- dbConnect(RSQLite::SQLite(),"sampleKA.sqlite")
# where.criteria <- ("text LIKE '%%usns comfort%%' or text LIKE '%%a#USNSCOMFORT%%' or text LIKE '%%USNSComfort%%'")
# wordcloud_plot(con, where.criteria, "2018-09-01","2018-10-03")
# daily_sentiment_plot(con, where.criteria,"2018-10-01", "2018-10-30", "Worldwide pop")

#### WORDCLOUD PLOT  ####
wordcloud_plot <- function(con, where.criteria, start.date=NULL, end.date=NULL, caption="population"){
  wordcloud.data <- daily_sentiment(con, where.criteria, start.date, end.date)
  if(nrow(wordcloud.data)>0){                                    #use Caps NROW since df is a vector vice Dataframe
    require(wordcloud)
    require(wordcloud2)
    require(webshot)
    require(tm)
    require(stringi)
    wordcloud.data$text <- sapply(wordcloud.data$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
    wordcloud.data$text <- stringi::stri_trans_tolower(wordcloud.data$text)
    ActivityData <- paste(wordcloud.data$text,collapse="\n")
    review_source <- VectorSource(ActivityData)                        # jams the vector of data into a table.
    corpus <- Corpus(review_source)                                    # load the uncleaned data into a table to be cleaned.
    corpus <- tm_map(corpus, removePunctuation)                       # tm_map code produces insignificant error messages since it is looking for a dataframe vice vector
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, removeWords, stopwords('english'))
    corpus <- tm_map(corpus, removeWords, stopwords('spanish')) 
    corpus <- tm_map(corpus, removeWords, c('que','las','del','con','para','una','esta'))
    corpus <- tm_map(corpus, removeWords, c('the','https','like','can','will','also','best','must','says','amp'))
    ## corpus cleaning complete now build term matrix
    tdm <- as.matrix(TermDocumentMatrix(corpus))  
    w <- rowSums(tdm)
    w <-sort(w,decreasing = TRUE)
    w2 <- data.frame(names(w),w)
    names(w2) <- c("word","freq")
    w2$word <- factor(w2$word, levels=w2$word[order(w2$freq, decreasing = TRUE)])
    # produce the worldcloud.  WORDCLOUD 2 is being used because it doesnt cutoff words.
    wc2 <- wordcloud2 (w2[1:50,], size=.5)                                                                                     # WC2 doesnt leave out words
    #record.filename <- paste(record.dir,sprintf("%s_%s_WordCloud_%s.png",toupper(start.date),toupper(end.date),samp),sep="")
    htmlwidget.filename <- paste(record.dir, "wordcloud2.html",sep="")
    htmlwidgets::saveWidget(wc2, htmlwidget.filename, selfcontained = FALSE)  
    webshot.filename <- sprintf("%s_%s_WordCloud_%s_%s.png",toupper(start.date),toupper(end.date),samp,caption)
    webshot(htmlwidget.filename, paste(record.dir, webshot.filename, sep=""), cliprect = "viewport",delay =5, vwidth = 480, vheight=480)
    }
}


### Top Hashtags excel list
top_hashtags_List <- function(con,where.criteria=NULL,start.date=NULL,end.date=NULL,limit=10){
  require(rJava)
  require(xlsx)
  top.hashtags.data <- top_hashtags(con,where.criteria,start.date,end.date,limit)
  if(nrow(top.hashtags.data) > 1 && var(top.hashtags.data$n) > 0){
    names(top.hashtags.data)<-c("Hashtag","Count")
    m <- min(top.hashtags.data$Count)
    top.hashtags.data<-top.hashtags.data[which(top.hashtags.data$Count != m),]
    top.hashtags.data <- top.hashtags.data[order(top.hashtags.data$Count,decreasing = TRUE),]
    top.hashtags.data$Hashtag <- paste("#",top.hashtags.data$Hashtag,sep="")
    wb <- make_top10_excel(top.hashtags.data,sprintf("For tweets from %s to %s ",toupper(start.date),toupper(end.date)))
    record.filename <- paste(record.dir,sprintf("%s-%s_HT_%s.xlsx",toupper(start.date),toupper(end.date),samp),sep="")
    #daily.filename <- paste(paste(daily.dir,sprintf("HT_%s.xlsx",samp),sep=""))
    saveWorkbook(wb,record.filename)
    #file.copy(record.filename,daily.filename,overwrite=TRUE)
  }
  ## add code to produce a longtail chart
}


#### Top UserMentions
top_usermentions_List <- function(con,where.criteria=NULL,start.date=NULL,end.date=NULL,limit=10){
  top.usermentions.data <- top_usermentions(con,where.criteria,start.date,end.date,limit)
  require(xlsx) 
  require(rJava)
  if(nrow(top.usermentions.data) > 1 && var(top.usermentions.data$n) > 0){
    names(top.usermentions.data)<-c("ID","Count","Screen_Name","User_Mention_Name")
    m <- min(top.usermentions.data$Count)
    top.usermentions.data<-top.usermentions.data[which(top.usermentions.data$Count != m),c(3,2,4)]
    top.usermentions.data <- top.usermentions.data[order(top.usermentions.data$Count,decreasing = TRUE),]
    top.usermentions.data$Screen_Name <- paste("@",top.usermentions.data$Screen_Name,sep="")
    wb <- make_top10_excel(top.usermentions.data,sprintf("For tweets from %s to %s ",toupper(start.date),toupper(end.date)))
    record.filename <- paste(record.dir,sprintf("%s-%s_UM_%s.xlsx",toupper(start.date),toupper(end.date),samp),sep="")
    #daily.filename <- paste(daily.dir,sprintf("UM_%s.xlsx",samp),sep="")
    saveWorkbook(wb,record.filename)
    #file.copy(record.filename,daily.filename,overwrite=TRUE)
  }
}



#### Top URLs
top_urls_List <- function(con,where.criteria=NULL,start.date=NULL,end.date=NULL,limit=10){
  top.urls.data <- top_urls(con,where.criteria,start.date,end.date,limit)
  require(xlsx) 
  require(rJava)
  if(nrow(top.urls.data ) > 1 && var(top.urls.data $n) > 0){
    names(top.urls.data )<-c("URL","Count","Extended URL")
    m <- min(top.urls.data $Count)
    top.urls.data <-top.urls.data [which(top.urls.data $Count != m),]                                                                                 ## drops the minimum entry to avoid arbitrary ties in tenth position being shown
    top.urls.data  <- top.urls.data [order(top.urls.data $Count,decreasing = TRUE),]
    wb <- make_top10_excel(top.urls.data ,sprintf("For tweets from %s to %s ",toupper(start.date),toupper(end.date)))
    record.filename <- paste(record.dir,sprintf("%s-%s_URL_%s.xlsx",toupper(start.date),toupper(end.date),samp),sep="")
    #daily.filename <- paste(daily.dir,sprintf("URL_%s.xlsx",samp),sep="")
    saveWorkbook(wb,record.filename)
    #file.copy(record.filename,daily.filename,overwrite=TRUE)
  }
}  


#### Top Media
top_media_List <- function(con,where.criteria=NULL,start.date=NULL,end.date=NULL,limit=10){
  top.media.data <- top_media(con,where.criteria,start.date,end.date,limit)
  require(xlsx) 
  require(rJava)
  if(nrow(top.media.data) > 1 && var(top.media.data$n) > 0){
    names(top.media.data)<-c("Media_ID","Count","Media_URL")
    m <- min(top.media.data$Count)
    top.media.data<-top.media.data[which(top.media.data$Count != m),]
    top.media.data <- top.media.data[order(top.media.data$Count,decreasing = TRUE),]
    wb <- make_top10_excel(top.media.data[,c(1,3)],sprintf("For tweets from %s to %s ",toupper(start.date),toupper(end.date)))
    record.filename <- paste(record.dir,sprintf("%s-%s_media_%s.xlsx",toupper(start.date),toupper(end.date),samp),sep="")
    #daily.filename <- paste(daily.dir,sprintf("media_%s.xlsx",samp),sep="")
    saveWorkbook(wb,record.filename)
    # file.copy(record.filename,daily.filename)
    for(j in 1:nrow(top.media.data)){
      ext<-strsplit(top.media.data$Media_URL[j],".",fixed=TRUE)[[1]]     #split string in the second column of the media dataframe
      ext <- ext[length(ext)]
      if(nchar(ext)<=4){
        tryCatch(
          download.file(top.media.data$Media_URL[j],paste(record.dir,sprintf("%s_Top%iMedia_%s.%s",format(end.date),j,samp,ext)), mode ="wb")  #get top media and assign name Top x Media_KA
        )
        if(file.exists(paste(record.dir,sprintf("%s_Top%iMedia_%s.%s",format(end.date),j,samp,ext)))){
       #   file.copy(paste(record.dir,sprintf("%s_Top%iMedia_%s.%s",format(end.date),j,samp,ext))) #,
       #             paste(daily.dir,sprintf("Top%iMedia_%s.%s",j,samp,ext)))
       #  }
      }
    }
  }
 }
}

#### TOP TWEETERS
top_tweeters_List <- function(con,where.criteria=NULL,start.date=NULL,end.date=NULL,limit=10){
  top_tweeters.data <- top_tweeters(con,where.criteria,start.date,end.date,limit)
  require(xlsx) 
  require(rjava)
  if(nrow(top_tweeters.data) > 1 && var(top_tweeters.data$n) > 0){
    names(top_tweeters.data)<-c("User_ID","Count","Screen_Name")
    m <- min(top_tweeters.data$Count)
    top_tweeters.data<-top_tweeters.data[which(top_tweeters.data$Count != m),]
    top_tweeters.data$Screen_Name <- paste("@",top_tweeters.data$Screen_Name,sep="")
    top_tweeters.data <- top_tweeters.data[order(top_tweeters.data$Count,decreasing = TRUE),]
    wb <- make_top10_excel(top_tweeters.data[,3:2],sprintf("For tweets from %s to %s ",toupper(start.date),toupper(end.date)))
    record.filename <- paste(record.dir,sprintf("%s-%s_toptweeters_%s.xlsx",toupper(start.date),toupper(end.date),samp),sep="")
    #daily.filename <- paste(daily.dir,sprintf("toptweeters_%s.xlsx",samp),sep="")
    saveWorkbook(wb,record.filename)
    #file.copy(record.filename,daily.filename,overwrite=TRUE)
  }
}

#### MOST LIKED
most_liked_List <- function(con,where.criteria=NULL,start.date=NULL,end.date=NULL,limit=10){
  most.liked.data <- most_liked(con,where.criteria,start.date,end.date,limit)
  if(nrow(most.liked.data) > 1 && var(most.liked.data$favorite_count) > 0){
    names(most.liked.data)<-c("Count","Screen_Name","Text")
    m <- min(most.liked.data$Count)
    most.liked.data<-most.liked.data[which(most.liked.data$Count != m),]
    most.liked.data <- most.liked.data[order(most.liked.data$Count,decreasing = TRUE),]
    most.liked.data$Screen_Name <- paste("@",most.liked.data$Screen_Name,sep="")
    wb <- make_toptweet_excel(most.liked.data[,c(2,3,1)],sprintf("For tweets from %s to %s ",toupper(start.date),toupper(end.date)))
    record.filename <- paste(record.dir,sprintf("%s-%s_most_liked_%s.xlsx",toupper(start.date),toupper(end.date),samp),sep="")
    #  daily.filename <- paste(daily.dir,sprintf("most_liked_%s.xlsx",samp),sep="")
    saveWorkbook(wb,record.filename)
    #  file.copy(record.filename,daily.filename,overwrite=TRUE)
  }
}

#### Most Retweeted
most_retweeted_List <- function(con,where.criteria=NULL,start.date=NULL,end.date=NULL,limit=10){
  most.retweeted.data <- most_retweeted(con,where.criteria,start.date,end.date,limit)
  if(nrow(most.retweeted.data) > 1 && var(most.retweeted.data$retweet_count) > 0){
    names(most.retweeted.data)<-c("Count","Screen_Name","Text")
    m <- min(most.retweeted.data$Count)
    most.retweeted.data<-most.retweeted.data[which(most.retweeted.data$Count != m),]
    most.retweeted.data <- most.retweeted.data[order(most.retweeted.data$Count,decreasing = TRUE),]
    most.retweeted.data$Screen_Name <- paste("@",most.retweeted.data$Screen_Name,sep="")
    wb <- make_toptweet_excel(most.retweeted.data[,c(2,3,1)],sprintf("For tweets from %s to %s ",toupper(start.date),toupper(end.date)))
    record.filename <- paste(record.dir,sprintf("%s-%s_most_retweeted_%s.xlsx",toupper(start.date),toupper(end.date),samp),sep="")
    #  daily.filename <- paste(daily.dir,sprintf("most_retweeted_%s.xlsx",samp),sep="")
    saveWorkbook(wb,record.filename)
    #  file.copy(record.filename,daily.filename,overwrite=TRUE)
  }
}


## test it
# top_urls_List(con, where.criteria, start.date = "2018-10-22", end.date = "2018-10-30", 11)
# top_urls(con,where.criteria,start.date,end.date,limit)
# where.criteria <- ("key_media.sublocation == 'Esmeraldas'")
# start.date = "2018-10-22"
# end.date = "2018-10-30"
# limit = 11
# samp <-"KA"                  
# #record.dir <- "C:/Users/me/Documents/R/TwitterAnalysis/OBT/27Oct/"
# samp <- "KA"
# setwd("C:/Users/me/Documents/R/TwitterAnalysis/OBT/27Oct/")


### Most Popular RT
most_popular_RT_in_sample_List <- function(con,where.criteria=NULL,start.date=NULL,end.date=NULL,limit=10){
  most.popular.RT.in.sample.data <- most_popular_RT_in_sample(con,where.criteria=NULL,start.date=NULL,end.date=NULL,limit=10)
  if(nrow(most.popular.RT.in.sample.data) > 1 && var(most.popular.RT.in.sample.data$n) > 0){
    endpoint <- 'https://api.twitter.com/1.1/statuses/lookup.json'
    names(most.popular.RT.in.sample.data)<-c("status_id","Count","Text","Screen_Name")
    m <- min(most.popular.RT.in.sample.data$Count)
    most.popular.RT.in.sample.data<-most.popular.RT.in.sample.data[which(most.popular.RT.in.sample.data$Count != m),]
    most.popular.RT.in.sample.data <- most.popular.RT.in.sample.data[order(most.popular.RT.in.sample.data$Count,decreasing = TRUE),]
    query.params <- c("id",paste(most.popular.RT.in.sample.data$status_id,collapse=","),"tweet_mode","extended")
    result<-twitter_anything(auth.vector,endpoint,query.params)
    if(result$status_code==200){
      tweets <- httr::content(result)
      tweet_ids <- sapply(tweets,function(x) return(x$id))
      most.popular.RT.in.sample.data$Screen_Name <- "[Deleted]"
      for(j in 1:length(tweets)){
        most.popular.RT.in.sample.data$Screen_Name[which(most.popular.RT.in.sample.data$status_id==tweets[[j]]$id)] <- paste("@",tweets[[j]]$user$screen_name,sep="")
        most.popular.RT.in.sample.data$Text[which(most.popular.RT.in.sample.data$Status_id==tweets[[j]]$id)] <- tweets[[j]]$full_text
      }
      wb <- make_toptweet_excel(most.popular.RT.in.sample.data[,c("Screen_Name","Text","Count")],sprintf("For tweets from %s to %s ",toupper(start.date),toupper(end.date)))
      record.filename <- paste(record.dir,sprintf("%s-%s_most_popular_%s.xlsx",toupper(start.date),toupper(end.date),samp),sep="")
      # daily.filename <- paste(daily.dir,sprintf("most_popular_%s.xlsx",samp),sep="")
      saveWorkbook(wb,record.filename)
      # file.copy(record.filename,daily.filename,overwrite=TRUE)
    }  
  }
}

### Test it
# most_popular_RT_in_sample_List(con, where.criteria, start.date = "2018-10-22", end.date = "2018-10-30", 11)
# most.popular.RT.in.sample.data <- most_popular_RT_in_sample(con, where.criteria, start.date = "2018-10-22", end.date = "2018-10-30", 11)


### MOST REACH
most_reach_List <- function(con,where.criteria=NULL,start.date=NULL,end.date=NULL,limit=10){
  most.reach.data <- most_reach(con,where.criteria=NULL,start.date=NULL,end.date=NULL,limit=10)
    if(nrow(most.reach.data) > 1 && var(most.reach.data$n) > 0){
      names(most.reach.data)[names(most.reach.data) == "n"] <- "Reach"
      m <- min(most.reach.data$Reach)
      most.reach.data<-most.reach.data[which(most.reach.data$Reach != m),]
      most.reach.data <- most.reach.data[order(most.reach.data$Reach,decreasing = TRUE),]
      most.reach.data$Text <- NA
      most.reach.data$Screen_Name <- "[Deleted]"
      for(j in 1:nrow(most.reach.data)){
        first.query <- sprintf("Select screen_name,text from status where id = %s;",as.character(most.reach.data$status_id[j]))
        result<-dbGetQuery(con,first.query)
        if(nrow(result)>0){
          most.reach.data$Text[j] <- result$text[1]
          most.reach.data$Screen_Name[j] <- result$screen_name[1]
        }else{
          second.query <- sprintf("Select text from status where retweet_status_id = %s;",as.character(most.reach.data$status_id[j]))
          result<-dbGetQuery(con,second.query)
          most.reach.data$Text[j] <- result$text[1]
          endpoint <- 'https://api.twitter.com/1.1/statuses/show.json'
          query.params <- c("id",as.character(most.reach.data$status_id[j]),"tweet_mode","extended")
          result<-twitter_anything(auth.vector,endpoint,query.params)
          if(result$status_code==200){
            tweet<-httr::content(result)
            most.reach.data$Screen_Name[j] <-tweet$user$screen_name
          }
        }
      }
      wb <- make_toptweet_excel(most.reach.data[,c("Screen_Name","Text","Reach")],sprintf("For tweets from %s to %s ",toupper(start.date),toupper(end.date)))
      record.filename <- paste(record.dir,sprintf("%s-%s_most_reach_%s.xlsx",toupper(start.date),toupper(end.date),samp),sep="")
      # daily.filename <- paste(daily.dir,sprintf("most_reach_%s.xlsx",samp),sep="")
      saveWorkbook(wb,record.filename)
      # file.copy(record.filename,daily.filename,overwrite=TRUE)
    }
}

### MAKE EXCEL FUNCTIONS

make_top10_excel <- function(top10.df,footnote.date,sheet.name="Sheet1"){
  wb<-createWorkbook(type="xlsx")
  sheet <- createSheet(wb,sheet.name)
  rows <- createRow(sheet,1:(nrow(top10.df)+2))
  cells <- createCell(rows,1:ncol(top10.df))
  TITLE_STYLE <- CellStyle(wb)+
    Font(
      wb,
      heightInPoints=24,
      isBold=TRUE,
      underline=1,
      color="#FFFFFF"
    ) +
    Fill(
      foregroundColor = 'lightblue',
      # backgroundColor = '#FFFFFF00',
      pattern = 'SOLID_FOREGROUND') +
    Border(
      color='#000000FF',
      position=c("BOTTOM","LEFT","TOP","RIGHT")
    ) +
    Alignment(
      horizontal = "ALIGN_CENTER"
    )
  CELL_STYLE_LEFT <- CellStyle(wb)+
    Font(
      wb,
      heightInPoints=24,
      color="#FFFFFF",
      isBold=TRUE,
      underline=0
    ) +
    Fill(
      foregroundColor = '#FFFFFF',
      backgroundColor = 'lightblue',
      pattern = 'SOLID_FOREGROUND')+
    Border(
      color='#000000FF',
      position=c("BOTTOM","LEFT","TOP","RIGHT")
    ) +
    Alignment(
      horizontal = "ALIGN_LEFT"
    )
  CELL_STYLE_RIGHT <- CellStyle(wb)+
    Font(
      wb,
      heightInPoints=24,
      color="#FFFFFF",
      isBold=TRUE,
      underline=0
    ) +
    Fill(
      foregroundColor = '#FFFFFF',
      backgroundColor = 'lightblue',
      pattern = 'SOLID_FOREGROUND')+
    Border(
      color='#000000FF',
      position=c("BOTTOM","LEFT","TOP","RIGHT")
    ) +
    Alignment(
      horizontal = "ALIGN_CENTER"
    )
  CELL_STYLE_FOOT <- CellStyle(wb)+
    Font(
      wb,
      heightInPoints=20,
      color="#FFFFFF",
      isBold=FALSE,
      underline=0
    ) +
    Fill(
      foregroundColor = '#FFFFFF',
      # backgroundColor = 'lightblue',
      pattern = 'SOLID_FOREGROUND')+
    Border(
      color='#000000FF',
      position=c("TOP")
    ) +
    Alignment(
      horizontal = "ALIGN_RIGHT"
    )
  setColumnWidth(sheet, colIndex=1, colWidth=56)
  setColumnWidth(sheet, colIndex=2:ncol(top10.df), colWidth=15)
  mapply(setCellValue,cells[1,],names(top10.df))
  for(j in 1:ncol(top10.df)){
    mapply(setCellValue,cells[2:(nrow(top10.df)+1),j],top10.df[,j])
  }
  setCellValue(cells[[nrow(top10.df)+2,1]],footnote.date)
  i<-1
  for(j in 1:ncol(top10.df)){
    setCellStyle(cells[[i,j]],TITLE_STYLE)
  }
  for(i in 2:(nrow(top10.df)+1)){
    j<-1
    setCellStyle(cells[[i,j]],CELL_STYLE_LEFT)
  }
  for(i in 2:(nrow(top10.df)+1)){
    for(j in 2:ncol(top10.df)){
      setCellStyle(cells[[i,j]],CELL_STYLE_RIGHT)
    }
  }
  setCellStyle(cells[[nrow(top10.df)+2,1]],CELL_STYLE_FOOT)
  ind<-addMergedRegion(sheet,nrow(top10.df)+2,nrow(top10.df)+2,1,ncol(top10.df))
  return(wb)
}
make_toptweet_excel <- function(toptweet.df,footnote.date,sheet.name="Sheet1"){
  wb<-createWorkbook(type="xlsx")
  sheet <- createSheet(wb,sheet.name)
  rows <- createRow(sheet,1:(nrow(toptweet.df)+2))
  cells <- createCell(rows,1:ncol(toptweet.df))
  TITLE_STYLE <- CellStyle(wb)+
    Font(
      wb,
      heightInPoints=18,
      isBold=TRUE,
      underline=1,
      color="#FFFFFF"
    ) +
    Fill(
      foregroundColor = 'lightblue',
      # backgroundColor = '#FFFFFF00',
      pattern = 'SOLID_FOREGROUND') +
    Border(
      color='#000000FF',
      position=c("BOTTOM","LEFT","TOP","RIGHT")
    ) +
    Alignment(
      horizontal = "ALIGN_CENTER"
    )
  CELL_STYLE_LEFT <- CellStyle(wb)+
    Font(
      wb,
      heightInPoints=18,
      color="#FFFFFF",
      isBold=TRUE,
      underline=0
    ) +
    Fill(
      foregroundColor = '#FFFFFF',
      backgroundColor = 'lightblue',
      pattern = 'SOLID_FOREGROUND')+
    Border(
      color='#000000FF',
      position=c("BOTTOM","LEFT","TOP","RIGHT")
    ) +
    Alignment(
      horizontal = "ALIGN_LEFT"
    )
  CELL_STYLE_CENTER <- CellStyle(wb)+
    Font(
      wb,
      heightInPoints=18,
      color="#FFFFFF",
      isBold=FALSE,
      underline=0
    ) +
    Fill(
      foregroundColor = '#FFFFFF',
      backgroundColor = 'lightblue',
      pattern = 'SOLID_FOREGROUND')+
    Border(
      color='#000000FF',
      position=c("BOTTOM","LEFT","TOP","RIGHT")
    ) +
    Alignment(
      horizontal = "ALIGN_LEFT",
      wrapText = TRUE
    )
  CELL_STYLE_RIGHT <- CellStyle(wb)+
    Font(
      wb,
      heightInPoints=18,
      color="#FFFFFF",
      isBold=TRUE,
      underline=0
    ) +
    Fill(
      foregroundColor = '#FFFFFF',
      backgroundColor = 'lightblue',
      pattern = 'SOLID_FOREGROUND')+
    Border(
      color='#000000FF',
      position=c("BOTTOM","LEFT","TOP","RIGHT")
    ) +
    Alignment(
      horizontal = "ALIGN_CENTER"
    )
  CELL_STYLE_FOOT <- CellStyle(wb)+
    Font(
      wb,
      heightInPoints=16,
      color="#FFFFFF",
      isBold=FALSE,
      underline=0
    ) +
    Fill(
      foregroundColor = '#FFFFFF',
      # backgroundColor = 'lightblue',
      pattern = 'SOLID_FOREGROUND')+
    Border(
      color='#000000FF',
      position=c("TOP")
    ) +
    Alignment(
      horizontal = "ALIGN_RIGHT"
    )
  setColumnWidth(sheet, colIndex=1, colWidth=35)
  setColumnWidth(sheet, colIndex=2, colWidth=100)
  setColumnWidth(sheet, colIndex=3:ncol(toptweet.df), colWidth=15)
  mapply(setCellValue,cells[1,],names(toptweet.df))
  for(j in 1:ncol(toptweet.df)){
    mapply(setCellValue,cells[2:(nrow(toptweet.df)+1),j],toptweet.df[,j])
  }
  setCellValue(cells[[nrow(toptweet.df)+2,1]],footnote.date)
  i<-1
  for(j in 1:ncol(toptweet.df)){
    setCellStyle(cells[[i,j]],TITLE_STYLE)
  }
  for(i in 2:(nrow(toptweet.df)+1)){
    j<-1
    setCellStyle(cells[[i,j]],CELL_STYLE_LEFT)
  }
  for(i in 2:(nrow(toptweet.df)+1)){
    j<-2
    setCellStyle(cells[[i,j]],CELL_STYLE_CENTER)
  }
  for(i in 2:(nrow(toptweet.df)+1)){
    for(j in 3:ncol(toptweet.df)){
      setCellStyle(cells[[i,j]],CELL_STYLE_RIGHT)
    }
  }
  setCellStyle(cells[[nrow(toptweet.df)+2,1]],CELL_STYLE_FOOT)
  ind<-addMergedRegion(sheet,nrow(toptweet.df)+2,nrow(toptweet.df)+2,1,ncol(toptweet.df))
  return(wb)
}






