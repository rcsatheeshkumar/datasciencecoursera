rankhospital<-function(state,outcome,rank="best")
{
  acceptedOutcomes <- c("heart attack","pneumonia","heart failure")
  lowerMortalityName <- "Hospital.30.Day.Death..Mortality..Rates.from."
  outcomeData<-read.csv("outcome-of-care-measures.csv",stringsAsFactors = FALSE)
  UniqueStates<-unique(outcomeData$State)
  if(!(state %in% UniqueStates))
  {
    stop("invalid state")
  }
  if(!(tolower(outcome) %in% acceptedOutcomes))
  {
    stop("invalid outcome")
  }
  simpleCap <- function(x) {
    
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=".")
  }
  sortByColumns<- function (data, col1,col2){
    orderdata <- data[order(data[,col1],data[,col2]),]
    return(orderdata)
  }
  outcome<-paste(lowerMortalityName,simpleCap(outcome),sep = "")
  
  outcomeData<- subset(outcomeData,outcomeData[,"State"]==state & outcomeData[,outcome]!="Not Available" ,select=c("Hospital.Name",outcome))
  
  outcomeData[,outcome] = as.numeric(as.character(outcomeData[,outcome]))
  sortedData <- sortByColumns(outcomeData,outcome,c("Hospital.Name"))
  
  
  
  if(nrow(sortedData)>0 && !is.numeric(rank))
  {
    if(rank=="best")
      rank=1
    else if(rank=="worst")
      rank= nrow(sortedData)
    as.character(sortedData[rank,"Hospital.Name"])
  }
  else if (nrow(sortedData)>0 && is.numeric(rank) && nrow(sortedData)>=rank )
  {
    as.character(sortedData[rank,"Hospital.Name"])
  }
  else
  {
        c("NA")
  }
}