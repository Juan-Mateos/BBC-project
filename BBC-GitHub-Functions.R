######
#FUNCTIONS FOR BBC DATA COLLECTION + ANALYSIS
######

#######
#Utilities
#######

#WriteOut Function to write out files including the date

#Args: input= file name
#Output: writes out the file
WriteOut <- function(x) {
        name <- deparse(substitute(x))
        today <- paste0("revised-data/",name,"-",Sys.Date(),".csv")
        write.csv(x,today,row.names=F)
}

#Check Nulls helper function
#Takes as arg a value. If it is not null), then
#it returns the value. otherwise it returns NA
CheckNulls <- function(x) {
        if (is.list(x)==TRUE) {
                x <- unlist(x) #To get to the value in the list 
        }
        if (is.null(x)==TRUE) {return(NA)
        } else {return(x)}
}    

#NameList helper function 
#Args:
#Inputs: x a list, y the name of the variable
#we want to use to name its elements
#Returns the names for the list
NameList <- function(x,y) {
        return(x[y])
}

# Multiple plot function (from:http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/)
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
        library(grid)
        
        # Make a list from the ... arguments and plotlist
        plots <- c(list(...), plotlist)
        
        numPlots = length(plots)
        
        # If layout is NULL, then use 'cols' to determine layout
        if (is.null(layout)) {
                # Make the panel
                # ncol: Number of columns of plots
                # nrow: Number of rows needed, calculated from # of cols
                layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                                 ncol = cols, nrow = ceiling(numPlots/cols))
        }
        
        if (numPlots==1) {
                print(plots[[1]])
                
        } else {
                # Set up the page
                grid.newpage()
                pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
                
                # Make each plot, in the correct location
                for (i in 1:numPlots) {
                        # Get the i,j matrix positions of the regions that contain this subplot
                        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                        
                        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                        layout.pos.col = matchidx$col))
                }
        }
}

#DownloadUnzip Function:
#Args: x = url
#downloads and unzips the file
DownloadUnzip <- function(x) {
        dest1 <- unlist(strsplit(x,"/"))
        dest2 <- dest1[length(dest1)]
        download.file(x,dest=dest2)
        unzip(dest2)
}

######################
#I: Functions to get data
######################

###########
#1. Get BBC organisation function
###########

#GetOrgData
#Args:
#Input: organisation name
#Returns a vector of relevant metrics / urls

GetOrgData <- function(x) {
        orgData <- content(GET(paste0("https://api.github.com/orgs/",
                                      x),gtoken))
        df <-
                data.frame(login=CheckNulls(orgData["login"]),
                           id=CheckNulls(orgData["id"]),
                           repos_url=CheckNulls(orgData["repos_url"]),
                           members_url=CheckNulls(orgData["members_url"]),
                           location=CheckNulls(orgData["location"]),
                           public_repos=CheckNulls(orgData["public_repos"]),
                           created_at=CheckNulls(orgData["created_at"]),
                           updated_at=CheckNulls(orgData["updated_at"]))
        return(df)
}

##########
#2. GET BBC MEMBER DATA
##########

#GetMemberData
#Function that extracts all members for each organisation
#and combines them into a dataframe (with org ids)
#Args:  input = org name
#       output: dataframe with member information

GetMemberData <- function(x) {
        memberUnp <- GET(paste0("https://api.github.com/orgs/",
                                x,"/members?per_page=100"),
                         gtoken) #None of the BBC orgs has > 100 members
        members <- content(memberUnp)
        #memberNames <- ldply(members,ExtractMemberId)
        userData <- ldply(members,ExtractUserData)
        userOrgs <- ldply(members,ExtractUserOrgs)
        names(userOrgs) <- "orgs"
        return(data.frame(userData,
                          userOrgs))
}

#ExtractUserData Helper function
#Args: a list of members
#Input: a user name
#returns a dataframe of relevant fields for each user.

ExtractUserData <- function(x) {
        Sys.sleep(sample(seq(0,1,0.1),1))
        login <- x$login
        userUnp <- GET(paste0("https://api.github.com/users/",
                              login),
                       gtoken)
        userP <- content(userUnp)
        return(data.frame(login=CheckNulls(userP$login),
                          id=CheckNulls(userP$id),
                          name=CheckNulls(userP$name),
                          company=CheckNulls(userP$company),
                          location=CheckNulls(userP$location),
                          followers=CheckNulls(userP$followers),
                          following=CheckNulls(userP$following),
                          created_at=CheckNulls(userP$created_at)))
}    

#ExtractUserOrgs helper function
#args
#input: a list of members
#returns the user organisations as a string
ExtractUserOrgs <- function(x) {
        Sys.sleep(sample(seq(0,1,0.1),1))
        login <- x$login
        userOunp <- GET(paste0("https://api.github.com/users/",login,"/orgs"),
                        gtoken)
        userOrg <- content(userOunp)
        orgs <- ldply(userOrg,OrgVector)
        return(paste0(unlist(orgs),collapse=", "))
}

#OrgVector function to extract org names from the Org list
OrgVector <- function(x) {
        return(CheckNulls(x$login))
}

#Function ExtractPeopleDataRedux2 (extract corrupted data
#adapted to memberDataDf)
#Args input a list of logins
#returns personal data on the login
ExtractPeopleDataRedux2 <- function(x) {
        targetUrl <- paste0("https://api.github.com/users/",x[1])
        url <- content(GET(targetUrl,gtoken))
        df <- data.frame(
                #.id = x$.id,
                login = CheckNulls(url$login),
                name = CheckNulls(url$name),
                company = CheckNulls(url$company),
                location = CheckNulls(url$location),
                created_at = CheckNulls(url$created_at))
        return(df)
}

##########
#3. EXTRACT REPO DATA
#########
#GetRepoData function:
#args input: a BBC-related organisation
#returns a nice dataframe with repo data and urls for 
#further querying (through GetIndividualData)

GetRepoData <- function(x) {
        index <- orgDf$login==x
        if (orgDf$public_repos[index]>0) {
                results <- orgDf$public_repos[index]
                repos <- content(GET(paste0(
                        "https://api.github.com/orgs/",x,"/repos?per_page=",results),
                        gtoken))
                reposDf <- ldply(repos, ExtractRepoData)
                return(reposDf) 
        } else {
                otherdf <- as.data.frame(t(rep(NA,14)))
                #NB names to keep the data-frame consistent
                names(otherdf) <- 
                        c("id",
                          "name",
                          "owner",
                          "description",
                          "forked",
                          "forks_url",
                          "forks_count",
                          "stargazers_url",
                          "stargazers_count",
                          "contributors_url",
                          "subscribers_url",
                          "created_at",
                          "updated_at",
                          "language")
                return(otherdf)
        }
}

#Helper function ExtractRepoData 
#Args input: an element in a list of repos
#Returns relevant information for that list, 
#including urls for further querying.

ExtractRepoData <- function(x) {
        df <- data_frame(
                id = CheckNulls(x$id),
                name = CheckNulls(x$name),  
                owner = CheckNulls(x$owner$login),
                description = CheckNulls(x$description),
                forked = CheckNulls(x$fork),
                forks_url = CheckNulls(x$forks_url),
                forks_count = CheckNulls(x$forks_count),
                stargazers_url = CheckNulls(x$stargazers_url),
                stargazers_count = CheckNulls(x$stargazers_count),
                contributors_url = CheckNulls(x$contributors_url),
                subscribers_url = CheckNulls(x$subscribers_url),
                created_at = CheckNulls(x$created_at),
                updated_at = CheckNulls(x$updated_at),
                language = CheckNulls(x$language))
        return(df)
}  

##Extract subscriber count:
#Args: Input: a list of urls.
#Returns number of subscribers for the repo (count of elements in a list)

CountSubscribers <- function(x) {
        subsUnp <- GET(x,gtoken)
        subsParsed <- content(subsUnp) 
        #NB some repos have large number of subscribers
        #We need more than one link to obtain the complete list
        if (is.null(subsUnp$headers$link)==FALSE) {
                #Fiddle with the link to get the last url number
                breaklink <- unlist(str_split(subsUnp$headers$link,">"))
                npages <- as.numeric(
                        last(unlist(str_split(breaklink[[2]],""))))
                counter <- 2
                while (counter <= npages) {
                        link <- paste0(x,"?page=",counter)
                        subsUnp <- GET(link,gtoken)
                        subsP <- content(subsUnp)
                        subsParsed <- c(subsParsed,subsP)
                        counter <- counter+1
                        Sys.sleep(sample(seq(0,0.5,0.1),1))
                }
        }
        return(length(subsParsed))
}

##########
#4. GET FORK DATA
##########

#GetForkData function.
#Args: A list of fork urls.
#Returns relevant information

GetForkData <- function(x) {
        allForks <- GET(x,gtoken)
        if (allForks$status_code == 404) {
                df <- data.frame(forkdate=paste0("errorcode=",allForks$status_code),
                                 url=NA,
                                 login=NA,
                                 location=NA,
                                 company=NA,
                                 created_at=NA)
                return(df)
        }
        else if ("link" %in% names(allForks$headers)) {
                #NB dealing with APIs with many results    
                linkString <- unlist(
                        str_split(allForks$headers$link,"page"))[3]
                index2 <- str_locate(linkString,">")
                nPages <- as.numeric(substring(linkString,2,index2[1]-1))
                myPages <- as.list(paste0(x,"?page=",1:nPages))
                
                #NB: we create a list of people involved 
                #with the repo
                myResults <- unlist(lapply(myPages,GetLongResults),
                                    recursive=F)
                names(myResults) <- sapply(myResults,NameList,y="full_name")
                forkData <- ldply(myResults,ExtractForkData2)
                return(forkData)
                
        } else {
                allForks <- content(allForks)
                names(allForks) <- sapply(allForks,
                                          NameList,y="full_name")
                forkData <- ldply(allForks,ExtractForkData2)
                return(forkData)
        }
}

#ExtractForkData2 helper function:
#Input is a list of forks
#returns a df with fork information of interest
ExtractForkData2 <- function(x) {
        forkdate <- x$created_at
        url <- x$url
        login <- x$owner$login
        ownerPerson <- content(GET(x$owner$url,
                                   gtoken))
        location <- CheckNulls(ownerPerson$location)
        company <- CheckNulls(ownerPerson$company)
        created_at <- CheckNulls(ownerPerson$created_at)
        df <- data.frame(forkdate=forkdate,
                         url=url,
                         login=login,
                         location = location,
                         company = company,
                         created_at = created_at)
        return(df)
}

#ExtractForkDataRedux function
#Args: input a list of forks
#Extracts the relevant data again
ExtractForkDataRedux <- function(x) {
        myUrl <- content(GET(as.character(x$url),gtoken))
        forkdate <- myUrl$created_at
        url <- myUrl$url
        login <- myUrl$owner$login
        ownerPerson <- content(GET(myUrl$owner$url,
                                   gtoken))
        location <- CheckNulls(ownerPerson$location)
        company <- CheckNulls(ownerPerson$company)
        created_at <- CheckNulls(ownerPerson$created_at)
        df <- data.frame(forkdate=forkdate,
                         url=url,
                         login=login,
                         location = location,
                         company = company,
                         created_at = created_at)
        return(df)
        
}

#Extract name of fork source in forkDf
#Function GetFork.
#Args: a fork id
#Returns the source of the fork
GetFork <- function(x) {
        split <- str_split(x,"/")
        return(unlist(split)[2])
}


##########
#5. GET PEOPLE DATA
##########

#GetPeopleData function
#Args
#input: a bbc name
#returns a df with information on each
#watchers, contributors and subscribers.


GetPeopleData <- function(x,y) {
        #NB I'm using the version of the repo Df without forked repos
        index <- reposDf2[reposDf2$forked==FALSE,".id"] == x
        repos <- as.list(unlist(as.list(reposDf2[reposDf2$forked==FALSE & index,y])))
        if (length(repos)>0){
                names(repos) <- repos
                people <- ldply(repos,ExtractPeopleData)
                return(people)
        }
}

#SplitGet Helper function
#Args: a string
#Returns a component of the string after splitting it
SplitGet <- function(x) {
        return(unlist(str_split(x,"_"))[1])
}

#ExtractPeopleData helper function
#Args: input = a repo URL
#Returns a df with contributors, watchers and subscribers (with repo ids)
#Observation: contains a sub-routine to deal
#with projects that have lots of people involved.

ExtractPeopleData <- function(x) {
        Sys.sleep(sample(seq(0.72,0.92,by=0.01),1))
        allPeople <- GET(as.character(x),gtoken)
        if (length(allPeople) > 0) {
                if ("link" %in% names(allPeople$headers)) {
                        #NB dealing with APIs with many results
                        
                        linkString <- unlist(
                                str_split(allPeople$headers$link,"page"))[3]
                        index2 <- str_locate(linkString,">")
                        nPages <- as.numeric(substring(linkString,2,index2[1]-1))
                        myPages <- as.list(paste0(x,"?page=",1:nPages))
                        
                        #NB: we create a list of people involved 
                        #with the repo
                        
                        myResults <- unlist(lapply(myPages,GetLongResults),
                                            recursive=F)
                        peopleData <- ldply(myResults,ExtractPeopleData2)
                        return(peopleData)
                } else {allPeopleP <- content(allPeople)
                        peopleData <- ldply(allPeopleP,ExtractPeopleData2)
                        return(peopleData)
                }
        } else {
                df <- data.frame(
                        login = NA,
                        name = NA,
                        company = NA,
                        location = NA,
                        created_at = NA)
                return(df)
        }       
}

#GetLongResults helper function
#args: input: list of urls when this is longer
#than a single query could return
#Returns the parsed elements.
GetLongResults <- function(x) {
        return(content(GET(x,gtoken)))
}

#ExtractPeopleData2 Helper function
#Args: x is an element in a list of people
#Returns a data frame with relevant information.

ExtractPeopleData2 <- function(x) {
        #Sys Sleep to manage my API call budget
        Sys.sleep(sample(seq(0.72,0.92,by=0.01),1))
        url <- content(GET(x$url,gtoken))
        df <- data.frame(
                login = CheckNulls(url$login),
                name = CheckNulls(url$name),
                company = CheckNulls(url$company),
                location = CheckNulls(url$location),
                created_at = CheckNulls(url$created_at))
        return(df)
}

#ExtractPeopleDataRedux function
#Args:
#Input: a vector of logins that returned corrupted data first time 
#around        
#Returns: the fields I am interested in.
ExtractPeopleDataRedux <- function(x) {
        targetUrl <- paste0("https://api.github.com/users/",x$login)
        url <- content(GET(targetUrl,gtoken))
        df <- data.frame(
                .id = x$.id,
                login = CheckNulls(url$login),
                name = CheckNulls(url$name),
                company = CheckNulls(url$company),
                location = CheckNulls(url$location),
                created_at = CheckNulls(url$created_at))
        return(df)
}

#GetRepoUrl function to extract the repo url from the person url
#Args: input = url for a type of agent involved in a repo
#returns a part of the string with the repo url
GetRepoUrl <- function(x) {
        url <- paste0(unlist(str_split(x,"/"))[1:6],collapse="/")
        return(url)
}

####################
#II. FUNCTIONS TO CLEAN DATA
####################

#MakeMonthYear function:
#Args:
#Input a date
#Returns its month and year
MakeMonthYear <- function(x) {
        return(as.Date(paste("01",month(x),year(x),sep="/"),
                       format="%d/%m/%Y"))
}

#ReDate function. Args
#Input: a character string that contains a date in GH's format
#returns a date.
ReDate <- function(x) {
        date <- as.Date(substring(
                as.character(x),0,10),format="%Y-%m-%d")
        return(date)
}

#Function AllocatePersonToOrg:
#Args: input a login.
#Returns their BBC member org (having randomly allocated them
#to one where they have several)
AllocatePersonToOrg <- function(x) {
        charname <- as.character(x)
        #Remove duplicates from MemberData
        member.noDupes <- memberDataDf[!duplicated(memberDataDf$login),]
        #Create vector of organisations an individual is involved with
        myVector <- as.vector(member.noDupes$.id[
                member.noDupes$login==charname])
        theirName <- as.character(member.noDupes$login[
                member.noDupes$login==charname])
        theirOrg <- sample(myVector,1)
        myDf <- cbind(theirName,theirOrg)
        return(myDf)
}

#TidyCompanies function: 
#Args input: a company name
#Returns a tidied version (lowercase, strings trimmed etc.)
TidyCompanies <- function(x) {
        clean1 <- tolower(x)
        clean2 <- gsub("[[:punct:]]","",clean1,ignore.case=T)
        clean3 <- gsub("ltd|inc|gmbh|limited|the|llc|plc","",clean2,
                       ignore.case=T)
        clean4 <- gsub("research and development|
                       research & development|
                       research development","rd",clean3,
                       ignore.case=T)
        clean5 <- gsub("freelancer","freelance",clean4,fixed=T,
                       ignore.case=T)
        clean6 <- gsub("  "," ",clean5,
                       ignore.case=T)
        clean7 <- str_trim(clean6)
        return(as.factor(clean7))
}

####################
#III. FUNCTIONS TO ANALYSE AND VISUALISE DATA
####################

##########
#1. Community detection
##########

#Function GetCombinations
#Arguments: x, an element in a list
#y, the name of the variable with the vector we want to 
#extract combinations for.
#Returns a dataframe with all the combinations
GetCombinations <- function(x,y) {
        if (length(x[,y])>1) { #Makes no sense to get combs of 1 object
                if (length(x[,y])>2){
                        combs <- t(combn(x[,y],2))
                        combs2 <- data.frame(e1=combs[,1],e2=combs[,2])
                        return(combs2)
                } else { #Misbehaved output when taking combs of two objects
                        combs <-  str_split(combn(x[,y],2)," ")
                        combs2 <- data.frame(e1=combs[[1]],e2=combs[[2]])
                        return(combs2)
                }
        }
}

#GetNetworkFromEdges function.
#Args input: an edgelist matrix.
#Returns a network
GetNetworkFromEdges <- function(x) {
        xm <- as.matrix(x)
        xNet <- graph.edgelist(xm,directed=F)
        E(xNet)$weight <- 1
        #Use simplify to remove multiple edges
        xNet2 <-simplify(xNet, 
                         edge.attr.comb=list(weight="sum"))
        return(xNet2)
}

#GetNetworkFromDf
#Takes as arguments x (edge df) and y (vertex attributes)
#Returns a graph
GetNetworkFromDf <- function(x,y) {
        xNet <- graph.data.frame(x,
                                 directed=F,vertices=y)
        return(xNet)
}

#GetBestClustering function to select the community detection approach
#that generates the most modular outputs.
#Arguments:
# x = graph object
#Returns a list with
#element 1- graph object based on the best performing community
#detection algorithm
#element 2 - dataframe with logins names and the community they
#have been allocated to.
#NB uses the GetMembership + GetModularity functions

GetBestClustering <- function(x) {
        membs <- GetMembership(x)
        mods <- apply(membs,2,FUN=GetModularity,x=x)
        index <- which.max(mods)
        print(names(mods)[index])
        x <- set.vertex.attribute(x,"comm",value=as.character(membs[,index]))
        vname <- get.vertex.attribute(x,"name")
        y <- data.frame(login=vname,membership=membs[,index])
        y <- y[order(y$membership),]
        return(list(x,y))
}

#Function GetMembership to extract community membership vector from a graph.
#Arguments: 
# x = graph object
#returns data-frame with community membership vectors
GetMembership <- function(x) {
        a <- fastgreedy.community(x)
        b <- walktrap.community(x)
        c <- leading.eigenvector.community(x)
        d <- label.propagation.community(x)
        df <- data.frame(a$membership,
                         b$membership,
                         c$membership,
                         d$membership)
        names(df) <- c("fastgreedy","walktrap","leading eigenvector",
                       "label propagation")
        return(df)
}

#GetModularity function to calculate modularity of graph partitions based on different
#community detection algorithms.
#Arguments:
#y = membership list
#x = graph
#Returns the modularity score
GetModularity <- function(x,y) {
        return(modularity(x,y))
}

#Community affiliations function
#Takes as arguments a network object and the name of a 
        #community detection algorithm
#Returns a list object with membership allocations and 
        #comparison of allocations between both algorithms

CommunityAffiliations <- function(x,y) {
        f <- get(y)
        aff <- f(x)
        aff2 <- data.frame(aff$names,aff$membership)
        names(aff2) <- paste0(y,c("names","membership"))
        return(aff2)
}

#WriteGraph function.
#Args: a graph
#Outputs a graphml file
WriteGraph <- function(x) {
        name <- deparse(substitute(x))
        write.graph(x,paste0("revised-graphs/",name,Sys.Date(),".graphml"),format=c("graphml"))
}

#######
#DATA CLEANING AND ANALYSIS
#######

#MakeCumSum function
#Args: a df with times and counts
#returns the cumulative sum
MakeCumSum <- function(x) {
        x$cumulative <- cumsum(x$Freq)
        return(x[,c("Var1","cumulative")])
}

#GetParent function
#Extracts parent forks for forks with changed names
GetParent <- function(x) {
        forkData <- content(GET(paste0(
                "https://api.github.com/repos/",
                x),gtoken))
        parentName <- forkData$source$name
        return(parentName)
}

#WriteChart to output charts and plots.
#Args: a plot object, width (y) and height (z)
#Saves the plot in the charts folder
WriteChart <- function(x,y=7,z=4.5) {
        name <- deparse(substitute(x))
        pdf(paste0("revised-charts/",name,Sys.Date(),".pdf"),width=y,height=z)
        print(x)
        dev.off()
}

########
#DESCRIPTIVES
########

#TO DELETE
# #Function GetOrg.
# #Args: input: a url
# #returns a string with relevant BBC org
# GetOrg <- function(x) {
#         mySplit <- unlist(str_split(x,"/"))
#         return(as.factor(mySplit[5]))
# }
# #Function GetRepo
# #Args: input a url
# #Returns a string with relevant repo
# GetRepo <- function(x) {
#         mySplit <- unlist(str_split(x,"/"))
#         return(as.factor(mySplit[6]))
# }

#Function GetVarDescriptives
#Args:
#Input: A dataframe we want to extract descriptives for by var y
#Returns some descriptive statistics about the distribution of
#people over repos
GetVarDescriptives <- function(x,y) {
        catname <- 
                myTable <- table(x[y])
        myTable <- myTable[myTable>0]
        #NB add this to deal with orgs that have no stargazers
        if (length(myTable>0)) {
                myDf <- data.frame(catname=length(myTable),
                                   sum=sum(myTable,na.rm=T),
                                   mean=mean(myTable,na.rm=T),
                                   median=median(myTable,na.rm=T),
                                   sd=sd(myTable,na.rm=T),
                                   max=max(myTable,na.rm=T),
                                   min=min(myTable,na.rm=T))
                names(myDf)[1] <- names(x[names(x)==y])
                return(myDf)
        }
}

#LabelDf Function to add a category field to a dataframe
#args:
#Input a data-frame x and a label name y
#returns a data-frame with a label field
LabelDf <- function(x,y) {
        labeledDf <- data.frame(
                label=rep(y,nrow(x)),
                login = x$login,
                location=x$location)
        return(labeledDf)
} 

#####
#GEOCODING + MAPPING
#####

#Function DedupeOverBBCAreas.
#Takes as an argument an element in a list. Splits it over areas, and 
#returns a dataframe with no dupes.
DedupeOverBBCAreas <- function(x) {
        splitAgain <- split(x,x$membership)
        dedupedDf <- ldply(splitAgain,function(x) {
                return(x[!duplicated(x$login),])})
        return(dedupedDf)
}

#GeoCode function.
#Args:
#Input: a location name
#Returns Google Geocode API outputs
GeoCode <- function(x){
        Sys.sleep(sample(seq(1,2,by=0.1),1))
        url <- paste0("https://maps.googleapis.com/maps/api/geocode/json?address=",
                      x,"&key=",geoCodeKey)
        myUrl <- getURL(url)
        geos <- fromJSON(myUrl)
        return(geos)
}

#ExtractGeoData function.
#Args:
#input: A JSON list with data from Google Geo-codeApi
#Returns a df with:
        #country
        #locality
        #formated address
        #long
        #lat

#This isn't the most elegant function.

ExtractGeoData <- function(x) {
        #Some of the elements in the list come with no results
        if (length(x$results) > 0) {
                #Extracting data from lists given that in some 
                #cases we have one location, in others more than one
                resultsReturned <- length(x$results)
                
                #Get Locations
                unlisted <- unlist(x$results[[1]]$address_components)
                names(unlisted) <- NULL
                havecountry <- sum(grepl("country",unlisted))
                if (havecountry==0) {
                        country <- "N/A"
                } else {
                        countryloc <- grep("country",unlisted)
                        country <- unlisted[countryloc-2]
                }
                localityloc <- which(unlisted=="locality") 
                if(length(localityloc)==0) {
                        locality <- "N/A"
                } else {
                        #One of the cities had more than one locality
                        if (length(localityloc)>1) {
                                localityloc <- localityloc[1]
                        }
                        locality <- unlisted[localityloc-2]
                }
                
                #Get Address and coordinates
                address <- x$results[[1]]$formatted_address
                lat <- x$results[[1]]$geometry$location[1]
                lon <- x$results[[1]]$geometry$location[2]
                
                df <- data.frame(nresults = resultsReturned,
                                 country = country,
                                 locality = locality,
                                 address = address,
                                 lat = lat,
                                 lon = lon)
                row.names(df) <- NULL
                return(df)
        } else {
                df <- data.frame(matrix(ncol=6,nrow=1,
                                        data=
                                                c(0,rep(NA,5))))
                names(df) <- c("nresults",
                               "country",
                               "locality",
                               "address",
                               "lat",
                               "lon")
                row.names(df) <- NULL
                return(df)
        }
}

#GetMappingDf function to create dfs for mapping.
#Arg: x is a data frame with geocoded entities.
#It returns a data frame with locations aggregated over
#localities, ready for plotting.

GetMappingDf <- function(x) {
        localityDf <- as.data.frame(table(x$locality,x$role))
        coords <- aggregate(x[,c("lat","lon")],
                            list(x$locality),mean)
        localityDfcoords <- merge(localityDf,coords,by.x="Var1",
                                  by.y="Group.1")
        nas <- localityDfcoords$Var1=="N/A"
        localityDfcoordsCompletes <- localityDfcoords[!nas,]
        names(localityDfcoordsCompletes) <- c("locality",
                                              "role",
                                              "count",
                                              "lat",
                                              "lon")
        return(localityDfcoordsCompletes)
}






