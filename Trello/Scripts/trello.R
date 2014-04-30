library(jsonlite)
library(stringr)
library(plyr)

# Set currrent working directory to the directory where json file is
setwd("~/Documents/Git Repositories/ops/Roopali/Data")

# read json file into a R string
jsonFile <- "~/Documents/Git Repositories/ops/Roopali/Data/Bidm.json"
jsonTxt <- readLines("Bidm.json")

# parse json string into a R object
trelloData <- fromJSON( jsonTxt)

# Understand the data structure of trello Data
names(trelloData)
lapply(trelloData,class)

# Read cards and checklistitems into separate objects
trelloCards <- trelloData$cards
trelloCards <- trelloCards[!trelloCards$closed,]
fakeCards <- grep("DISCOVERY LANE DOCS|PROJECT PLANNING LANE DOCS|DATA MODEL LANE DOCS|PRODUCTION SUPPORT LANE DOCS|ETL LANE DOCS",trelloCards$name)

# Remove fake cards
trelloCards <- trelloCards[-(fakeCards),]

trelloChecklistItems <- trelloData$checklists
trelloMembers <- trelloData$members


#************ Functions Start ****************

getCardMembersNames <- function(cardMembers,membersMaster) {
  # Function to get member full names from card member IDs
  # Args: 
  #   cardMembers: List of member IDs per card
  #   membersMaster: List of member IDs and corresponding full names
  #
  # Returns:
  #   String of concatenated member names per card
  sapply(cardMembers, function(x) paste(sapply(x, function(y) membersMaster$fullName[grep(y,membersMaster$id)]),collapse=","))
  
}

getProjectType <- function(cardNames) {
  # Function to determine project type based on some rules
  # Args: 
  #   cardNames: card names for all cards
  #
  # Returns:
  #   String containing Project Type
  projectType <- ifelse(grepl("[Bb]usiness [pP]roject",cardNames),"Business","")
}

getChecklistPercentComplete <- function(cardIds,checkListMaster) {
  # Function to get percent complete for checklist items
  # Percent is calculate using the formula (no. of completed items * 100/total.items)
  # Args: 
  #   cardCheckListIds: List of checklist IDs per card
  #   checkListMaster: List of all checklist IDs and corresponding data
  #
  # Returns:
  #   Completion percentage per card
  sapply(cardIds, function(x) {
                            print(x)
                            cardCheckList <- as.data.frame(checkListMaster$checkItems[grep(x,checkListMaster$idCard)][1])
                            percentComplete <- ifelse(length(cardCheckList$state)==0,"",(100 * length(grep("^complete",cardCheckList$state)))/length(cardCheckList$state))
                            percentComplete <- round(as.numeric(percentComplete),0)
                            })
  
}

getCurrentMilestoneStep <- function(cardIds,checkListMaster) {
  # Function to get percent complete for checklist items
  # Percent is calculate using the formula (no. of completed items * 100/total.items)
  # Args: 
  #   cardCheckListIds: List of checklist IDs per card
  #   checkListMaster: List of all checklist IDs and corresponding data
  #
  # Returns:
  #   Completion percentage per card
  sapply(cardIds, function(x) {
    print(x)
    cardCheckList <- as.data.frame(checkListMaster$checkItems[grep(x,checkListMaster$idCard)][1])
    if (nrow(cardCheckList) > 0) {
      arrange(cardCheckList,state,pos)
      currentStep <- cardCheckList$name[which(cardCheckList$state=="incomplete")][1]
      
    } 
    else {
      currentStep <- ""  
    }
    
  })
  
}



#************ Functions End ****************

# Create an output object that will be used for the final report
# and initialize wit with trelloCards
output <- trelloCards

# Determine functional area for each card by looking for strings with [...]
sqBracket <- regexpr("\\[(.*?)\\]",trelloCards$name)
output$FunctionalArea <- ifelse(sqBracket==-1,"",str_sub(trelloCards$name,sqBracket+1,sqBracket+attr(sqBracket,"match.length")-2))

# Determine members for each card
output$ProjectTeam <- getCardMembersNames(trelloCards$idMembers,trelloMembers)

# Add Project Type field
output$projectType <- getProjectType(trelloCards$name)


# Add BIO number [or any other number before a colon]
bioNumber <- regexpr("[0-9\\./]+:",trelloCards$name)
output$BIO <- ifelse(bioNumber==-1,"",str_sub(trelloCards$name,bioNumber,bioNumber+attr(bioNumber,"match.length")-2))

# Add PMO number
output$PMONumber <- ""

# Add Project Status
output$ProjectStatus <- ""

# Add Schedule Indicator
output$Schedule <- ""

# Add Scope Indicator
output$Scope <- ""

# Add BIDM Start Date
output$BIDMStartDate <- ""

# Add BIDM Due Date
output$BIDMDueDate <- as.Date(trelloCards$due)

# Add BIDM Current Milestone Step
output$BIDMCurrentMilestoneStep <- getCurrentMilestoneStep(trelloCards$id,trelloChecklistItems)

# Add %BIDM Current Milestone [Percent Complete]
output$BIDMPercentCompleted <- getChecklistPercentComplete(trelloCards$id,trelloChecklistItems)


# Add Comments
output$Comments <- ""

# Add Last Activity Date on Trello Card
output$trelloCardLastActivityDate <- as.Date(trelloCards$dateLastActivity)

# Add aging field in Days - age since last updated in days
output$ageSinceLastUpdate <- Sys.Date() - output$trelloCardLastActivityDate

# Add BIDM Trello Link
output$BIDMTrelloLink <- trelloCards$url



# Drop fields not needed from Output
output1 <- output[,c("name","BIO","PMONumber","projectType","FunctionalArea","ProjectStatus","Schedule","Scope","ProjectTeam","BIDMStartDate","BIDMDueDate","BIDMCurrentMilestoneStep","BIDMPercentCompleted","trelloCardLastActivityDate","Comments","BIDMTrelloLink","ageSinceLastUpdate")]

# Save output file
fileName <- paste("Project Status Report",Sys.time(),".csv")
write.csv(output1,fileName)
