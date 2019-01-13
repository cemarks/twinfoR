#### THIS SCRIPT LOADS 17 DIFFERENT FUNCTIONS THAT PRODUCE PLOTS OR EXCEL SPREADSHEETS 

wordcloud_plot <- function(
  con,
  start.date = NULL,
  end.date = NULL,
  where.criteria = NULL,
  caption="",
  max.words = 50,
  stopword.langs = c('english','spanish'),
  additional.stopwords = c(
    'que',
    'las',
    'del',
    'con',
    'para',
    'una',
    'esta',
    'the',
    'https',
    'like',
    'can',
    'will',
    'also',
    'best',
    'must',
    'says',
    'amp'
  )
){
  word.df <- term_frequencies(
    con,
    start.date = start.date,
    end.date = end.date,
    where.criteria = where.criteria,
    stopword.langs = stopword.langs,
    additional.stopwords = additional.stopwords
  )
  if(nrow(word.df)>10){
    wordcloud::wordcloud(
      words=word.df$word,
      freq = word.df$freq,
      max.words = max.words,
      colors = c("green","red")
    )
  }
}

sentiment_plots <- function(
  con,
  start.date=NULL,
  end.date=NULL,
  where.criteria = NULL,
  file.name.prefix = NULL,
  caption = ""
){
  sentiment.data <- text_sentiment_dataframe(
    con,
    start.date=start.date,
    end.date=end.date,
    where.criteria = where.criteria
  )
  if(nrow(sentiment.data)>0){
    subt <- date_subtitle(
      start.date = start.date,
      end.date = end.date
    )
    sentiment_barplot(
      sentiment.data,
      file.name = paste(c(file.name.prefix,"sentiment-barplot.png"),collapse="_"),
      subtitle = subt,
      caption = caption
    )
    sentiment_lineplot(
      sentiment.data,
      file.name = paste(c(file.name.prefix,"sentiment-lineplot.png"),collapse="_"),
      subtitle = subt,
      caption = caption
    )
  }
}

term_frequencies <- function (
  con,
  start.date = NULL,
  end.date = NULL,
  where.criteria = NULL,
  stopword.langs = c('english','spanish'),
  additional.stopwords = c(
    'que',
    'las',
    'del',
    'con',
    'para',
    'una',
    'esta',
    'the',
    'https',
    'like',
    'can',
    'will',
    'also',
    'best',
    'must',
    'says',
    'amp'
  )
){
  text.data <- text_sentiment_dataframe(
    con,
    start.date = start.date,
    end.date = end.date,
    where.criteria = where.criteria
  )
  text.data$text <- sapply(text.data$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
  text.data$text <- tolower(text.data$text)
  ActivityData <- paste(text.data$text,collapse="\n")
  review_source <- tm::VectorSource(ActivityData)                        # jams the vector of data into a table.
  corpus <- tm::Corpus(review_source)                                    # load the uncleaned data into a table to be cleaned.
  corpus <- tm::tm_map(corpus, tm::removePunctuation)                       # tm_map code produces insignificant error messages since it is looking for a dataframe vice vector
  corpus <- tm::tm_map(corpus, tm::stripWhitespace)
  for(lang in stopword.langs){
    corpus <- tm::tm_map(corpus, tm::removeWords, tm::stopwords(lang))
  }
  corpus <- tm::tm_map(corpus, tm::removeWords, c('que','las','del','con','para','una','esta'))
  corpus <- tm::tm_map(corpus, tm::removeWords, c('the','https','like','can','will','also','best','must','says','amp'))
  ## corpus cleaning complete now build term matrix
  tdm <- as.matrix(tm::TermDocumentMatrix(corpus))  
  w <- rowSums(tdm)
  w <-sort(w,decreasing = TRUE)
  word.df <- data.frame(names(w),w)
  names(word.df) <- c("word","freq")
  return(word.df)
}


sentiment_barplot <- function(
  sentiment.data,
  file.name = NULL,
  subtitle = "",
  caption = ""
){
  g <- ggplot2::ggplot(sentiment.data, ggplot2::aes(x = date)) +
    ggplot2::geom_bar(stat="count", ggplot2::aes(fill=sent_label)) +
    ggplot2::labs(
      title = "Daily Tweet Volume by Sentiment", 
      subtitle = subtitle,
      x="Date", 
      y="Tweet Count", 
      caption = caption
    ) +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_manual(
      name = "Sentiment",
      values=c(
        positive = "dark green",
        "slight positive" = "light green",
        neutral="grey",
        "slight negative" = "orange",
        negative= "red",
        unknown="black"
      ),
      na.value = "black"
    )
  if(is.null(file.name)){
    print(g)
  } else {
    png(file.name)
    print(g)
    dev.off()
  }
}

sentiment_lineplot <- function(
  sentiment.data,
  file.name = NULL,
  subtitle = "",
  caption = ""
){
  #png("sentiment_lineplot.png",height=600,width=800)  
  set.table <- table(sentiment.data$date,sentiment.data$sent_label)
  set.table <- apply(set.table,1,function(x) return(x/sum(x)))
  sent.df <- reshape::melt(100*set.table)
  names(sent.df) <- c("Sentiment","Date","Percentage")
  g <- ggplot2::ggplot(sent.df, ggplot2::aes(x = Date, y = Percentage)) +
        ggplot2::geom_line(ggplot2::aes(col=Sentiment,group=Sentiment), size = 4) +
        ggplot2::labs(
          x = "Date", 
          y = "Sentiment %", 
          title = "Twitter Sentiment Percentage", 
          subtitle = subtitle,
          caption=caption
        ) +
    ggplot2::theme_bw()  +
    ggplot2::scale_color_manual(
      values=c(
        positive = "dark green",
        "slight positive" = "light green",
        neutral="grey",
        "slight negative" = "orange",
        negative= "red",
        unknown="black"
      ),
      na.value = "black"
    ) 
  if(is.null(file.name)){
    print(g)
  } else {
    png(file.name,height=600,width=800)
    print(g)
    dev.off()
  }
}

timeplot <- function(
  con,
  start.date=NULL,
  end.date=NULL,
  where.criteria = NULL,
  group.column = NULL,
  file.name = NULL,
  caption = ""
){
  df <- created_at_df(
    con,
    start.date = start.date,
    end.date = end.date,
    where.criteria = where.criteria,
    additional.columns = group.column
  )
  if(!is.null(group.column)){
    group.column.name <- strsplit(group.column,".",fixed=TRUE)[[1]][2]
  } else {
    group.column.name <- NULL
  }
  subtitle <- date_subtitle(
    start.date,
    end.date
  )
  tweet_timeplot(
    df,
    group.column = group.column.name,
    file.name = file.name,
    subtitle = subtitle,
    caption = caption
  )
}

tweet_timeplot <- function(
  created.at.df,
  group.column = NULL,
  file.name = NULL,
  subtitle = "",
  caption = ""
){
  # created.at.df<-created.at.df[order(created.at.df$TimeStamp),]
  if(is.null(group.column)){
    created.at.df$Tweets <- 1:nrow(created.at.df)
    g <- ggplot2::ggplot(
      data = created.at.df,
      mapping = ggplot2::aes(x=TimeStamp,y=Tweets)
    ) +
      ggplot2::geom_line() +
      ggplot2::labs(
        x = "Date",
        y = "Cumulative Tweets",
        title = "Tweet Rate",
        subtitle = subtitle,
        caption = caption
      ) +
      # ggplot2::scale_x_continuous(labels = created.at.df$date) +
      ggplot2::theme_bw()
  } else {
    unique.groups <- unique(created.at.df[,group.column])
    created.at.df$Tweets <- NA
    for(u in unique.groups){
      w <- which(created.at.df[,group.column] == u)
      created.at.df$Tweets[w] <- 1:length(w)
    }
    g <- ggplot2::ggplot(
      data = created.at.df,
      mapping = ggplot2::aes_string(x="TimeStamp",y="Tweets",group = group.column, color = group.column)
    ) +
      ggplot2::geom_line() +
      ggplot2::labs(
        x = "Date",
        y = "Cumulative Tweets",
        title = "Tweet Rate",
        subtitle = subtitle,
        caption = caption
      ) +
      # ggplot2::scale_x_continuous(labels = created.at.df$date) +
      ggplot2::theme_bw()
  }
  if(is.null(file.name)){
    print(g)
  } else {
    png(file.name,height=600,width=800)
    print(g)
    dev.off()
  }
}