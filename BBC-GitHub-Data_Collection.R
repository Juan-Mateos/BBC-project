#BBC Project R Code: Analysis of GitHub activity
#######
#I.Location
#######

#Set your working directory
#setwd("myworkingdirectory")

if (!file.exists("revised-data")) {dir.create("revised-data")}
if (!file.exists("revised-charts")) {dir.create("revised-charts")}
if (!file.exists("revised-graphs")) {dir.create("revised-graphs")}

######
#II.Goal
######
#Analyse github activity from various BBC departments

######
#III.Tasks
######

#Get Github data for BBC related organisations
#Query the API for each BBC organisation, its members and its repos.
#Query the API for forkers, watchers, subscribers and get their profiles.
#Map BBC organisational network to identify innovation capabilities
#Plot levels of activity in different areas.
#Map individuals influenced by BBC projects

######
#IV.Packages
######
#Data collection
require(XML)
require(httr)
require(RCurl)

#Data manipulation
require(jsonlite)
require(rjson)
require(RJSONIO)
require(dplyr)
require(plyr)
require(lubridate)
require(reshape2)
require(stringr)

#Data visualisation
require(ggplot2)
require(extrafont)
require(RgoogleMaps)
require(ggmap)
require(rgdal)
require(scales)
require(sna)
require(network)
require(igraph)
require(Hmisc)
require(RColorBrewer)

#other
require(packrat)

######
#V. GET DATA (API Code)
######

######
#1. Set up App with code from: 
#https://github.com/hadley/httr/blob/master/demo/oauth2-github.r
######

# a. Find OAuth settings for github: http://developer.github.com/v3/oauth/
oauth_endpoints("github")

# b. To make your own application, register at at
#    https://github.com/settings/applications. Use any URL for the homepage URL
#    (http://github.com is fine) and  http://localhost:1410 as the callback url
#
#    Replace your key and secret below.
myapp <- oauth_app("github",
                   key = "mykey",
                   secret = "mysecret")

# c. Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# d. Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/rate_limit", gtoken)
stop_for_status(req)
content(req)

######
#V. FUNCTIONS
######

source("BBC-GitHub-Functions.R")

#######
#6. DATA EXTRACTION
######

##########
#1. Get BBC organisation data
##########

#GET creates a request that we retrieve. Content parses it.
#We do this for different BBC orgs

#Create a vector of names to apply/loop over.
bbcnames <- as.list(c("bbc",
                      "bbcrd",
                      "BBC-News",
                      "fmtvp",
                      "bbc-fm-nk-core-eng",
                      "iplayer",
                      "BBC-Knowlearn",
                      "BBCConnectedStudio",
                      "bbc-sport",
                      "bbc-gel",
                      "BBC-Data",
                      "BBC-PIPS",
                      "BBCVisualJournalism",
                      "BBC-News-Labs",
                      "bbcarchdev",
                      "bbcsnippets"
))
#Name the list in order to create an id column when using ldply later.
names(bbcnames) <- unlist(bbcnames)

#Extract data
orgDf <- ldply(bbcnames,GetOrgData)

#Write out data
WriteOut(orgDf)

##########
#2. Get BBC member data
##########

#Extract member data from GitHub API
memberDataDf <- ldply(bbcnames,GetMemberData)

#This returns some corrupted results (1s in free-text fields)
corruptedM <- grepl("1",memberDataDf$name) | 
        grepl("1",memberDataDf$company) |
        grepl("1",memberDataDf$location)
corruptedMDf <- memberDataDf[corruptedM,]

corruptedMemberList <- as.list(corruptedMDf$login)

#Extract member data for corrupted outputs with another function
#Extract data
secondExtractionMembers <- ldply(corruptedMemberList,
                                 ExtractPeopleDataRedux2)

#Replace missing data in DF with newly extracted data
corruptedMDf[,c("company","location")] <- secondExtractionMembers[,
                                                                  c("company","location")]
#Remove corrupted data from memberDataDf
memberDataDf <- memberDataDf[!corruptedM,]

#Bind new information
memberDataDf <- rbind(memberDataDf,corruptedMDf)

#Write out data
WriteOut(memberDataDf)

##########
#3. Get BBCRepo data
##########

#Extract repos for all the organisations
reposDf <- ldply(bbcnames,GetRepoData)

#Remove NA repos.
reposDf <- reposDf[!is.na(reposDf$id),]

#Add a repo url for merging later
reposDf$repo.url <- paste0("https://api.github.com/repos/",
                           reposDf$owner,"/",reposDf$name)

#Get the subscriber count with another function
subscriber_count <- ldply(as.list(reposDf$subscribers_url),
                          CountSubscribers)

reposDf2 <- cbind(reposDf,subscriber_count)
names(reposDf2)[length(names(reposDf2))] <- "subscriber_count"

#Write-out
WriteOut(reposDf2)

##########
#4. Get BBC Fork Data
##########
#Again, this is based on an ldply loop over the fork URLS
#NB we are only looking at repos which are 
#Not forks themselves

forkUrlList <- as.list(reposDf2[reposDf2$forked==FALSE,"forks_url"])
names(forkUrlList) <- reposDf2[reposDf2$forked==FALSE,"forks_url"]

forkDf1 <- ldply(forkUrlList,GetForkData,
                 .progress="text")

#Again, some of the data was corrupted:
corruptedForks <- grepl("1",forkDf1$company) | 
        grepl("1",forkDf1$location)

corruptedForksDf <- forkDf1[corruptedForks,]
corruptedForksList <- lapply(split(corruptedForksDf,
                                   row.names(corruptedForksDf)),as.list)
names(corruptedForksList) <- sapply(corruptedForksList,
                                    NameList,y=".id")
#Extract again with another function
secondExtractionForks <- ldply(corruptedForksList,ExtractForkDataRedux,
                               .progress = )

forkDf <- rbind(forkDf1[!corruptedForks,],secondExtractionForks)
WriteOut(forkDf)

##########
#5. Get BBC related people data
##########

#This requires extracting project contributor and subscriber data.
#We use the GetPeopleData function with the repos df with no forks
#(lower threshold estimate of activity)

peopleDf1 <- ldply(bbcnames,GetPeopleData,
                   y=c("contributors_url","subscribers_url"),
                   .progress="text")
#Some of the results returned are corrupted
corrupted <- grepl("1|2|3|4|5|6",peopleDf1$name) | 
        grepl("1|2|3|4|5|6",peopleDf1$company) |
        grepl("1|2|3|4|5|6",peopleDf1$location)
corruptedDf <- peopleDf1[corrupted,]
corruptedList <- lapply(split(corruptedDf,
                              row.names(corruptedDf)),as.list)
#Extract again with another function
secondExtraction <- ldply(corruptedList,ExtractPeopleDataRedux,
                          .progress="text")

#Replace corrupted data with newly extracted data
#Could have been done more elegantly maybe?
peopleDf <- rbind(peopleDf1[!corrupted,],secondExtraction)
WriteOut(peopleDf)
