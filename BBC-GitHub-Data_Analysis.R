##########
#BBC DATA ANALYSIS, VISUALISATION AND MAPPING
##########

#Set your working directory
#setwd("/Users/juanmateos-garcia/Desktop/2015 core/BBC")
#To update the data:
#source("BBC-GitHub-Data_Collection.R")

#To replicate Nesta's analysis we need:
orgDf <- read.csv("source-data/orgDf.csv")
memberDataDf <- read.csv("source-data/memberDataDf.csv")
reposDf2 <- read.csv("source-data/reposDf2.csv")
forkDf <- read.csv("source-data/forkDf.csv")
peopleDf <- read.csv("source-data/peopleDf.csv")
peopleLocationGeoDf <- read.csv("source-data/peopleLocationGeoDf.csv")

########
#1. BBC organisational network + community detection
########
set.seed(8)

#Create a network where every node is an individual,
#edges are shared organisations between them.

#Approach: split by organisation, extract combination of 
#logins in the organisation, create a table.

splitMembersByOrgs <- split(memberDataDf,memberDataDf$.id)

#ldply GetCombinations function to the split object
memberEdgeList <- ldply(splitMembersByOrgs,GetCombinations,y="login")

#Remove the id column
memberEdgeList <- as.matrix(memberEdgeList[,-1])

#Use GetNetworkFromEdges to create network object.
memberNetwork <- GetNetworkFromEdges(memberEdgeList)

#Community detection using a previously developed function which
#returns a list including node memberships (NB we so this 
#to create a network of BBC members, and a network of
#BBC organisations)

memberCommunities <- GetBestClustering(memberNetwork)

#####
#Check robustness of community allocations
#####
#algorithms used: fastgreedy, walktrap community, leading eigenvector, fastgreedy
#Create a list with all the comparisons.

robustCheck <- list()
communityAlgorithms <- paste0(c("fastgreedy","walktrap","leading.eigenvector","label.propagation"),
                              ".community")
algoCombinations <- t(combn(communityAlgorithms,2))

for (i in 1:nrow(algoCombinations)) {
        aff1 <- CommunityAffiliations(memberNetwork,algoCombinations[i,1])
        aff2 <- CommunityAffiliations(memberNetwork,algoCombinations[i,2])
        bothAffs <- cbind(aff1,aff2)
        table <- 100*round(prop.table(table(bothAffs[,c(2,4)]),2),1)
        robustCheck[[i]] <- list(bothAffs[,c(1,2,4)],table)
}

#Relative consistency between FastGreedy and EigenVector.

#Then: merge affiliation vector in the list with
#memberData and produce a table of organisations.
memberDataDf2 <- merge(memberDataDf,memberCommunities[[2]],by="login")

#This table shows % of BBC org members in different groups
orgAffiliationTable <- 100*prop.table(
        table(memberDataDf2$.id, memberDataDf2$membership),1)

#Here, we allocate every BBC organisation to the group where they have
#more employees
orgAffiliation <- apply(orgAffiliationTable,1,which.max)

#IMPORTANT Observation: this is based on the outputs of the community detection
#analysis. Could change with updates in the data
orgAffiliation2 <- factor(orgAffiliation,levels=c(1:6),
                          labels=c("API","Archive","News","Services","R&D",
                                   "Mixed"))

#NB What do we do with "tied" organisations? We put them in
#a mixed group
for (i in 1:nrow(orgAffiliationTable)) {
        maxes <- orgAffiliationTable[i,] == max(orgAffiliationTable[i,])
        if (sum(maxes)>1) {
                orgAffiliation[i] <- 6
                orgAffiliation2[i] <-  "Mixed"
        }
}


#Now we create a 2-mode network representing BBC organisations,
#their members and their affiliations

#We will create this using a graph dataframe method. This requires:
#A df with the edges
#A df with node attributes

#Edges:
memberEdgesDf <- memberDataDf2[,c(1,2)]

#Vertices
memberVertexDf <- rbind(memberCommunities[[2]],
                        data.frame(login=names(orgAffiliation),
                                   membership=as.character(orgAffiliation)))
#Some extra labels
isBBC <- memberVertexDf$login %in% names(orgAffiliation)
#Add a variable with names for bbc orgs, blank space otherwise
memberVertexDf$bbcname <- as.character(memberVertexDf$login)
memberVertexDf$bbcname[!isBBC] <- " "

#Add variable with memberships for bbc orgs, blank space otherwise
memberVertexDf$membership2 <- as.character(memberVertexDf$membership)
memberVertexDf$membership2[!isBBC] <- " "

#Create network object
orgMemberNetwork <- GetNetworkFromDf(memberEdgesDf,memberVertexDf)

#Write out
WriteGraph(orgMemberNetwork)

#########
#2. Create clean DFs with BBC activity membership information
#########
#Create lookup table between BBC organisations and area of activity
orgLookup <- data.frame(org=names(orgAffiliation2),
                        membership=orgAffiliation2,row.names=NULL)

#Merge lookup with repos, remove forked repos and WriteOut
reposDf3 <- merge(reposDf2, orgLookup, by.x=".id",
                  by.y="org",all.x=T)
reposDf.noforks <- reposDf3[!(reposDf3$forked==T),]

#Some cleaning of dates, and adding a date variable with Month-Year
#(for plotting below)
dateVars <- c("created_at","updated_at")
reposDf.noforks[,dateVars] <- lapply(reposDf.noforks[,dateVars],
                                     as.Date)
reposDf.noforks$created.my <- MakeMonthYear(reposDf.noforks$created_at)

WriteOut(reposDf.noforks)

#Create table to colour the nodes in Gephi
affiliationCodeNameLookup <- data.frame(orgAffiliation,orgAffiliation2,
                                        row.names=NULL)
affCodes <- affiliationCodeNameLookup[!duplicated(affiliationCodeNameLookup[,2]),]

######
#4. Plot timeline of activity for repos
######

#NB Import and embed fonts to use Gill Sans MT in the charts
font_import()
loadfonts(device="pdf")

#Drop unused levels (i.e. API repos)
reposDf.noforks <- droplevels(reposDf.noforks)

#Create dataframe for plotting
reposTimeDf <- as.data.frame.matrix(
        table(reposDf.noforks$created.my,reposDf.noforks$membership))
reposTimeDf[,paste0(names(reposTimeDf),".cumul")] <-
        lapply(reposTimeDf,cumsum)

#Focus on cumulative sum, melt for plotting, reformat date for plotting.
reposTimeDf2 <- cbind(date=row.names(reposTimeDf),
                      reposTimeDf[,-c(1:length(levels(reposDf.noforks$membership)))])
row.names(reposTimeDf2) <- NULL
reposTimeDf3 <- melt(reposTimeDf2,id="date")
reposTimeDf3$date <- as.Date(reposTimeDf3$date,
                             format="%Y-%m-%d")

#re-order levels in membership variable consistent with their ranking.
areasOrdered <- paste0(names(sort(table(reposDf.noforks$membership))),".cumul")
reposTimeDf3$variable <- factor(reposTimeDf3$variable,ordered=T,
                                levels=(areasOrdered))

#Create palette (also to be used in the network graphs)
accentPalette <- brewer.pal(8,"Accent")
myPalette <- accentPalette[c(1:3,6:5)]

#Plot
repoAreaPlot <- ggplot(data=reposTimeDf3,aes(x=date,y=value,
                                             fill=variable,order=desc(variable)))+
        geom_area(position="stack")+
        xlim(as.Date(c("2009-01-01","2015-07-01")))+ #To make it consistent with the fork plot below
        scale_fill_manual(values=myPalette,
                          name="Development \n area",
                          labels=gsub(".cumul","",levels(reposTimeDf3$variable)))+
        labs(title="BBC projects in GitHub have multiplied by 10 since 2012",
             x=NULL,y="Original repos \n (stacked, cumulative)",guide="Area of activity")+
        theme(text=element_text(family="Gill Sans MT"))

WriteChart(repoAreaPlot)

#What has happened to BBC repo activity between 2012 and 2015?
yearChanges <- as.data.frame(table(year(reposDf.noforks$created_at)))
yearChanges$cumsum <- cumsum(yearChanges$Freq)
#Compare 2015 with 2012
yearChanges$scaled <- round(yearChanges$cumsum/28,2)

######
#5. Plot timeline of forks
#####
#Tasks:
#Merge forkDf with repoDf to get bbc orgs.
#Merge forkDf with orgAffiliatons to get affiliations
#Clean dates (NB we use forkdate, i.e. date when
#the fork was created, not created_at (date when the
#forking individual joined GitHub))
#Remove forks from BBC members
#Create table, create cumulative sums, plot.
#NB we could rbind the table with reposDf for faceting.

#Clean forkDf (remove "serial forker" jamiepg1)
jamie <- forkDf$login == "jamiepg1"
jamieTable <- table(forkDf[jamie,"forkdate"])
#73 of these forks were created in the same day (11 November 2014)

#Drop #jamiepg1
forkDf2 <- forkDf[!jamie,]

#I need the original name of the repo to merge this df
#with the repoDf (and get the parent BBC organisation and membership)
forkDf2$forked.project <- sapply(forkDf2$.id,
                                 function(x) {
                                         p <- unlist(str_split(x,"/"))[[2]]
                                         return(p)
                                 },USE.NAMES=F)

#There are a small number of projects where the developer
#changed the name of the fork from the "parent"

changedNames <- forkDf2$.id[
        !(forkDf2$forked.project %in% reposDf2$name)]

#For each of these, we need to find the source repo and
#rename it. We do it using the GetParent function
#Get the parent names for those projects
parentNames <-sapply(changedNames,GetParent,USE.NAMES=F)

#Replace forked project with the correct one
forkDf2$forked.project[forkDf2$.id
                       %in% changedNames] <- parentNames

#Remove forks from members in BBC organisations
forkDf2.5 <- forkDf2[!(forkDf2$login %in% memberDataDf$login),]

#Merge with repoDf to get Orgs
forkDf3 <- merge(forkDf2.5, reposDf.noforks[,c(".id","name","membership")],
                 by.x="forked.project",by.y="name")

#Drop levels
forkDf4 <- droplevels(forkDf3)

#Create Month-year variable
forkDf4$forked.my <- MakeMonthYear(forkDf4$forkdate)

#Create df for plotting
forkTimeDf <- as.data.frame.matrix(
        table(forkDf4$forked.my,forkDf4$membership))
forkTimeDf[,paste0(names(forkTimeDf),".cumul")] <-
        lapply(forkTimeDf,cumsum)

#Focus on cumulative sum, melt for plotting, reformat date for plotting.
forkTimeDf2 <- cbind(date=row.names(forkTimeDf),
                     forkTimeDf[,-c(1:length(levels(forkDf4$membership)))])
row.names(forkTimeDf2) <- NULL
forkTimeDf3 <- melt(forkTimeDf2,id="date")
forkTimeDf3$date <- as.Date(forkTimeDf3$date,
                            format="%Y-%m-%d")

#I need to order the levels for plotting (as before)
areasSortedForks <- paste0(names(sort(table(forkDf3$membership))),".cumul")        

forkTimeDf3$variable <- ordered(forkTimeDf3$variable,
                                levels=areasSortedForks)

myPalette2 <- myPalette[c(2:1,3:5)]

#Plot
forkAreaPlot <- ggplot(data=forkTimeDf3,aes(x=date,y=value,
                                            fill=variable,order=desc(variable)))+
        geom_area(position="stack")+
        xlim(as.Date(c("2009-01-01","2015-07-01")))+
        scale_fill_manual(values=myPalette2,                          
                          labels=gsub(".cumul","",levels(forkTimeDf3$variable)),
                          name="Development \n area")+
        labs(title="Forks of BBC projects have multiplied by 25 since 2012",x=NULL,
             y="Forks of BBC projects \n (stacked, cumulative)")+
        theme(text=element_text(family="Gill Sans MT"))
WriteChart(forkAreaPlot)

#What has happened to BBC forking activity between 2012 and 2015?
yearChangesFork <- as.data.frame(table(year(forkDf3$forkdate)))
yearChangesFork$cumsum <- cumsum(yearChangesFork$Freq)
#Compare 2015 with 2012
yearChangesFork$scaled <- round(yearChangesFork$cumsum/33,2)

#########
#Create Graph combining repoDf and forkDf
#########

WriteChart(multiplot(repoAreaPlot,forkAreaPlot),y=9,z=9)

#########
#6. PLOT FORK GRAPH
#########
#Create network where nodes are bbc organisations and
#bbc forkers (based on their login)
#The size of nodes = number of forks.
#Colour of BBC nodes = their membership
#Colour of nodes = forks of "star projects" (top 5?)
#OR: [2] have three levels (bbc org -> repo -> fork)

#forkEdges links bbc organisations and repos
forkEdges <- reposDf.noforks[,c(".id","name")]

#Linktype captures whether the link is between bbc orgs and
#projects, or between projects and forkers
forkEdges$linktype <- 10

#forkEdges2 links bbc repos and forkers
forkEdges2 <- forkDf3[,c("forked.project","login")]
forkEdges2$linktype <- 1

#Rename for binding
names(forkEdges) <- c("e1","e2")
names(forkEdges2) <- c("e1","e2")

#Bind
forkEdgesDf <- rbind(forkEdges,forkEdges2)
names(forkEdgesDf) <- c("e1","e2","weight")

#Dataframe with vertex attributes
forkVertexDf <- data.frame(v=unique(
        unlist(forkEdgesDf[,1:2])))

forkVertexDf$vertex.labels <- as.character(forkVertexDf$v)

#Create membership labels to colour the graph (perhaps also colour projects?)
forkVertexDf$vertex.membership <- NA
forkVertexDf$vertex.membership[forkVertexDf$v %in% forkDf2$forked.project] <- 10
forkVertexDf$vertex.membership[forkVertexDf$v %in% forkDf$login] <- 11
forkVertexDf$vertex.membership[forkVertexDf$v %in% names(orgAffiliation2)] <- 
        orgAffiliation[names(orgAffiliation)%in% forkVertexDf$v]

#Deal with non-forked repo names (which we nevertheless want to visualise)
forkVertexDf$vertex.membership[is.na(forkVertexDf$vertex.membership)] <- 10
forkVertexDf$vertex.membership <- as.character(forkVertexDf$vertex.membership)

#sort out vertex labels for top 10 projects
topRepoNames <- names(sort(table(forkDf4$forked.project),decreasing=T))[1:10]

#Careful with ordering here.
forkVertexDf$vertex.labels[!(forkVertexDf$v %in% names(orgAffiliation)) & 
                                   !(forkVertexDf$v %in% topRepoNames)] <- " "

#Degree counts
#Counts of forks of BBC orgs and repos
forkCounts1 <- as.data.frame(table(forkDf4$.id.y))
forkCounts2 <- as.data.frame(table(forkDf2$forked.project))
forkCounts3 <- data.frame(Var1=unique(forkDf2$login),
                          Freq=1)

#Remove bbc orgs from forkCounts3
forkCounts3 <- forkCounts3[!(forkCounts3$Var1 %in% bbcnames),]

#Gives repos with no forks a count of 1
forkCountsNonF<-data.frame(Var1= setdiff(unique(reposDf.noforks$name),
                                         as.character(forkCounts2$Var1)),Freq=1)

forkCounts <- rbind(forkCounts1,forkCounts2,forkCounts3,forkCountsNonF)

#Merge
forkVertexDf3 <- merge(forkVertexDf,forkCounts,
                       by.x="v",by.y="Var1")

forkVertexDf3 <- forkVertexDf3[!duplicated(forkVertexDf3$v),]
forkNetwork <- GetNetworkFromDf(forkEdgesDf,forkVertexDf3)
WriteGraph(forkNetwork)

##########
#7. MAP individual location of BBC contributors, forkers and subscribers
##########

######
#Clean and prepare the data
#####

#Create a dataframe with all the individual locations and organisations
#Create new Df to work with
peopleDfProcess <- peopleDf

#Get BBC organisation to get areas of activity (via the orgLookup)
orgRoleDf<- ldply(as.list(as.character(peopleDf$.id)),
                  function(x) {
                          la <- unlist(strsplit(x,"/"))
                          return(c(la[5],la[7]))})
names(orgRoleDf) <- c("org","role")
peopleDfProcess2 <- cbind(peopleDfProcess,orgRoleDf)

#Merge with the orgLookup
peopleDfProcess3 <- merge(peopleDfProcess2,orgLookup,by.x="org",
                          by.y="org",all.x=T)
#Drop unused levels
peopleDfProcessed <- droplevels(peopleDfProcess3)

#Drop processing variables.
remove(list=ls(pattern="Process[0-9]$"))

#Merge this with the fork info (including login)
peopleLocationDfProcess1 <- subset(peopleDfProcessed,select = c("login","location","role",
                                                                "membership","company"))
#Get the same information for forkDf
forkLocationDf <- data.frame(forkDf4[,c("login","location","membership","company")],
                             role="forker")
#Add them (NB peopleDf has subscribers, stargazers and contributors,
        #forkDf4 has forkers)
peopleLocationDfProcess2 <- rbind(peopleLocationDfProcess1,
                                  forkLocationDf[,c(1,2,5,3,4)]) 

#Other tidying for geo-coding
#Remove duplicates
peopleLocationDfSplitByRole <- split(peopleLocationDfProcess2,
                                     peopleLocationDfProcess2$role)

peopleLocationDfDeduped <- ldply(peopleLocationDfSplitByRole,
                                 DedupeOverBBCAreas)

#Remove observations with missing locations
na <- peopleLocationDfDeduped$location == "" | 
        is.na(peopleLocationDfDeduped$location)
peopleLocationDf <- peopleLocationDfDeduped[!na,]

#Make London more precise to facilitate geocoding
peopleLocationDf$location[peopleLocationDf$location=="London"] <- "London, UK"  

#Drop unused levels
peopleLocationDf <- droplevels(peopleLocationDf)
peopleLocationDf$bbc <- peopleLocationDf$login %in% memberDataDf2$login

##########
#Geocode data
##########

#Last tidying up:
locationClean <- gsub(", | ","+",peopleLocationDf$location)

#Run the geocoding loop over the cleaned locations vector locationClean

geoCodeKey <- "INSERT YOUR KEY HERE"

#GeoCode via Google, and extract data into dataframe.
geoCodedJSON <- lapply(locationClean,GeoCode)
geoCodedDf <- ldply(geoCodedJSON,ExtractGeoData)

#Cbind with the locationDf
peopleLocationGeoDf <- cbind(peopleLocationDf,geoCodedDf)
WriteOut(peopleLocationGeoDf)

#Final cleaning
#Get rid of those in the top percentile of >1 matches
peopleLocationGeoDf.clean <- subset(peopleLocationGeoDf,
                                    peopleLocationGeoDf$nresults <
                                            quantile(peopleLocationGeoDf$nresults,
                                                     probs=c(0.99)))
peopleLocationGeoDf.clean$role <- factor(peopleLocationGeoDf.clean$role)

#########
#Mapping localities
#########
#Tasks: create a dataframe where rows are locations, and roles are
#columns. We'll then match it with another that contains longitude and
#latitude. Having done this, we melt and plot.

#Remove a few N/A localities
moreNas <- peopleLocationGeoDf.clean$locality == "N/A" |
        is.na(peopleLocationGeoDf.clean$role)
peopleLocationGeoDf.clean2 <- peopleLocationGeoDf.clean[!moreNas,]
peopleLocationGeoDf.clean2 <- droplevels(peopleLocationGeoDf.clean2)

#Fix a location (St Petersburg) that was detected once in
#Russia and one in Florida and messes up the mapping
stPetersburg <- grepl("Petersburg",peopleLocationGeoDf.clean2$locality)
stPetersburgCoords <- peopleLocationGeoDf.clean2[stPetersburg,9:13][1,]

peopleLocationGeoDf.clean2[stPetersburg,9:13] <- stPetersburgCoords

#Split apply combine to get uniques over roles
locationOverRole <- split(peopleLocationGeoDf.clean2,
                          peopleLocationGeoDf.clean2$role)

peopleLocationGeoDf.clean3 <- ldply(locationOverRole,
                                    function(x) {
                                            dupes <- duplicated(x$login)
                                            return(x[!dupes,])
                                    })
#Change levels because previously we had the label for forkers as "forker"
levels(peopleLocationGeoDf.clean3$role) <- c("contributors",
                                             "forkers","subscribers")

#Remove BBC member logins for plotting
bbcMembers <- peopleLocationGeoDf.clean3$login %in% memberDataDf$login

peopleLocationGeoDf.clean4 <- peopleLocationGeoDf.clean3[!bbcMembers,]

#Use functions to produce dfs for mapping
bbcWorldDf <- GetMappingDf(peopleLocationGeoDf.clean4)

#And for the UK
peopleLocationGeoDfUK <- peopleLocationGeoDf.clean4[
        peopleLocationGeoDf.clean4$country=="United Kingdom",]
bbcUKdf <- GetMappingDf(peopleLocationGeoDfUK)

#Download shapefiles
DownloadUnzip("http://www.statsilk.com/files/country/StatPlanet_UK.zip")
DownloadUnzip("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_0_countries.zip")

world <- readOGR(dsn = "ne_10m_admin_0_countries",layer = "ne_10m_admin_0_countries")
uk <- readOGR("StatPlanet_UK/map/map.shp",
              layer="map")
ukbounds <- fortify(uk,region="DIVISION")

#Remove an extreme westward observation.
west <- ukbounds$long < -10
ukbounds <- ukbounds[!west,]

myPal <- brewer.pal(3,"Accent")

#Themes for plotting
mapTheme <- theme(axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.ticks=element_blank(),
                  axis.text=element_blank(),
                  panel.grid=element_blank(),
                  panel.background=element_blank(),
                  text=element_text(family="Gill Sans MT"))

#Create a loop to output charts for each of our roles

for (i in levels(bbcWorldDf$role)) {
        #Plot world map
        name <- paste0("world",i,"map")
        myset <- subset(bbcWorldDf,role==i & count>0)
        worldMap <-ggplot()+
                geom_polygon(data=world,aes(x=long,y=lat,group=group),
                             fill="bisque2")+
                geom_point(data=myset,
                           aes(x=lon,y=lat,group=NULL,
                               size=log(count),fill=role),
                           alpha=0.9,pch=22)+
                labs(title=paste("Location of",i,"of BBC projects in GitHub",sep=" "))+
                mapTheme
        
        #Plot UK map
        myUKset <- subset(bbcUKdf,role==i & count>0)
        ukMap <- ggplot()+
                geom_polygon(data=ukbounds,
                             aes(x=long,y=lat,group=group),
                             fill="bisque2")+
                geom_point(data=myUKset,
                           aes(x=lon,y=lat,group=NULL,
                               size=log(count),fill=role),
                           alpha=0.9,pch=22)+
                scale_colour_manual(values=myPal)+
                guides(colour=FALSE,size=FALSE,fill=FALSE)+
                labs(x=NULL,y=NULL,title="UK Zoom")+
                mapTheme+
                theme(
                        panel.border = element_rect(colour = "black", fill=NA, size=1))
        
        #Set viewport
        vp <- viewport(x=0.25,y=0.55,width = 0.2,height=0.4,just=c("right","top"))
        
        #Print
        pdf(paste0("revised-charts/",name,".pdf"),width=12,height=8)
        print(worldMap)
        print(ukMap,vp=vp)
        dev.off()
}

#Some numbers for the blog:
paste("Number of organisations:",nrow(orgDf))
paste("Number of members:",length(unique(memberDataDf$login)))
paste("Number of original projects:",nrow(reposDf.noforks))
paste("Number of forks:",nrow(forkDf4))

aggregate(reposDf.noforks$forks_count,list(reposDf.noforks$.id),
          "mean")
aggregate(reposDf.noforks$forks_count,list(reposDf.noforks$.id),
          "median")

#Get some descriptives over roles after deduping
#and taking out BBC members
roleSplits <- split(peopleLocationDfProcess2,
                    peopleLocationDfProcess2$role)

#This returns unique individuals in non BBC orgs, top companies,
#and company names
roleDescriptives <- lapply(roleSplits,
                           function(x) {
                                   noDupes <- x[!duplicated(x$login),]
                                   beebs <- noDupes$login %in% memberDataDf$login
                                   clean <- noDupes[!beebs,]
                                   num <- nrow(clean)
                                   nacomps <- clean$company == "" | 
                                           is.na(clean$company) |
                                           clean$company == " "
                                   myt <- sort(table(clean[!nacomps,"company"]),decreasing=T)[1:50]
                                   names <- sort(unique(clean[!nacomps,"company"]))
                                   output <- list(num,myt,names)
                                   names(output) <- c("people","companies","uniquecomps")
                                   return(output)
                           })

uniquePeople <- ldply(roleDescriptives,function(x) {return(x[[1]])})

#Finally. Something on geography.
#Basic stats:
#How many people outside of the UK?
#Top localities?

#How many individuals had location data
#Missing values for location
100*sum(na)/nrow(peopleLocationDfDeduped)
nrow(peopleLocationGeoDf.clean4)
names(peopleLocationGeoDf.clean4)

#Get the info above.
placeDescriptives <- lapply(split(peopleLocationGeoDf.clean4,
                                  peopleLocationGeoDf.clean4$role),
                            function(x) {
                                    countries <- 100*round(prop.table(
                                            sort(table(x$country),
                                                 decreasing=T)),3)
                                    localities <- 100*round(prop.table(
                                            sort(table(x$locality),
                                                 decreasing=T)),3)
                                    return(list(countries,localities))
                            })

dupesOverRoles <- duplicated(peopleLocationGeoDf.clean3$login)
bbcGeographyDescriptives <- peopleLocationGeoDf.clean3[!dupesOverRoles,]

length(unique(peopleLocationGeoDf.clean4$country))
countryTable <- 100*round(sort(
        prop.table(table(bbcGeographyDescriptives$country)),decreasing=T),3)
localityTable <- 100*round(sort(
        prop.table(table(bbcGeographyDescriptives$locality)),decreasing=T),3)

names(localityTable)[1:20]




