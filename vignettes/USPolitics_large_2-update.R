#!/bin/env Rscript

# Vignette to analyze US politics Part II of III (LARGE).  

### This is part 2 of the three-part USPolitics_large vignette.

### This script updates the database from part 1 with the politicians
### Tweets.  This script can be run regularly to collect new Tweets and
### keep the database up-to-date.  Note, Tweets that have already been 
### collected will not be updated, implying that certain data that is dynamic,
### e.g., retweet_count, will not be updated and will remain static in the database.

### Set Java heap space 
options(java.parameters = "-Xmx1024m")

### Load the package

library(twinfoR)
# devtools::load_all("/home/cemarks/Projects/twinfoR")

# Initialize some things
setwd("~/large_politics")
load("auth_vector.RData")

# Connect to database created in part 1.
t.con <- twitter_database(
  "pol.sqlite",
)

##### Now let's get all of the timelines.
##### This could take a while, especially to do all of the sentiment processing.

### Divert output to null device
sink("/dev/null") 
update_user_timelines(
  t.con,
  calc.Rsentiment = TRUE,
  calc.syu = TRUE
)
sink()

### Summarize the database

summarize_database(t.con)

DBI::dbDisconnect(t.con)
