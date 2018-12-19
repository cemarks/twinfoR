cons.token <- "0Xiv1GS7kKlK3lL2ruJLa1MQ8"
cons.secret <- "Ql4RddDueaErUgsis4vmgN0QSdWJ1fJNPymFnSvaq5UhkxNoux"

syuzhet.langs <- data.frame(
  matrix(
  	c(
      "en","english",
      "da","danish",
      "de","german",
      "es","spanish",
      "nl","dutch",
      "pt","portuguese",
      "fi","finnish",
      "fr","french",
      "ht","french",
      "it","italian",
      "sv","swedish",
      "tr","turkish"
  	),
    ncol=2,
    byrow=TRUE
  ),
  stringsAsFactors=FALSE
)
names(syuzhet.langs) <- c("code","language")

g <- getwd()
setwd("/home/cemarks/Projects/twinfoR")
devtools::use_data(
	cons.token,
	cons.secret,
	syuzhet.langs,
	internal=TRUE,
	overwrite=TRUE
)
setwd(g)

