rankall<-function(outcome,rank="best")
{
  i<-1
  resultdataframe<- data.frame(Hospital=character(0),State=character(0),stringsAsFactors = FALSE)
  acceptedOutcomes <- c("heart attack","pneumonia","heart failure")
  lowerMortalityName <- "Hospital.30.Day.Death..Mortality..Rates.from."
  outcomeData<-read.csv("outcome-of-care-measures.csv",stringsAsFactors = FALSE)
  UniqueStates<-unique(outcomeData$State)
  UniqueStates<-UniqueStates[order(UniqueStates)]
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
  
  for(state in UniqueStates)
  {
  outcomeDataSel<- subset(outcomeData,outcomeData[,"State"]==state & outcomeData[,outcome]!="Not Available" ,select=c("Hospital.Name",outcome))
  
  outcomeDataSel[,outcome] = as.numeric(as.character(outcomeDataSel[,outcome]))
  sortedData <- sortByColumns(outcomeDataSel,outcome,c("Hospital.Name"))
  
  ##topHospitals <- subset(sortedData,sortedData[,outcome]==sortedData[1,2],select=c("Hospital.Name"))
  
  if(nrow(sortedData)>0 && !is.numeric(rank))
  {
    if(rank=="best")
      rank=1
    else if(rank=="worst")
      rank= nrow(sortedData)
    resultdataframe[i,] <-c(as.character(sortedData[rank,"Hospital.Name"]),state)
  }
  else if (nrow(sortedData)>0 && is.numeric(rank) && nrow(sortedData)>=rank )
  {
    resultdataframe[i,] <-c(as.character(sortedData[rank,"Hospital.Name"]),state)
  }
  else
  {
    resultdataframe[i,] <-c("NA",state)
    
  }
  i<-i+1
  }
  resultdataframe
  
}