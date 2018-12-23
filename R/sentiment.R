## Sentiment collection

calculate_sentiment <- function(text,lang='en',calc.RSentiment=TRUE,calc.syuzhet=TRUE){
	if(calc.RSentiment && (lang=='en')){
		RSentiment.value <- RSentiment::calculate_score(text)
	} else {
		RSentiment.value <- "NULL"
	}
	if(calc.syuzhet && (lang %in% as.character(syuzhet.langs$code))){
		syu.lang <- as.character(syuzhet.langs$language[which(as.character(syuzhet.langs$code)==lang)])
		syu.value <- syuzhet::get_nrc_sentiment(text,language=syu.lang)
	} else {
		syu.value <- data.frame(
			anger="NULL",
			anticipation="NULL",
			disgust="NULL",
			fear="NULL",
			joy="NULL",
			sadness="NULL",
			surprise="NULL",
			trust="NULL",
			negative="NULL",
			positive="NULL"
		)
	}
	output <- list(
		RSent = RSentiment.value,
		syu = syu.value
	)
}


sent_label <- function(val){
  if(is.na(val)){
    return("unknown")
  } else if(val < -1){
    return("negative")
  } else if(val < 0){
    return("slight negative")
  } else if(val == 0){
    return("neutral")
  } else if(val <= 1){
    return("slight positive")
  } else if(val > 1){
    return("positive")
  } else {
    return("unknown")
  }
}

