best<-function(state,outcome)
{
  acceptedOutcomes <- c("heart attack","pneumonia","heart failure")
  lowerMortalityName <- "Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from."
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
  sortByColumn<- function (data, col1){
    orderdata <- data[order(data[,col1]),]
    return(orderdata)
  }
  outcome<-paste(lowerMortalityName,simpleCap(outcome),sep = "")
  
  outcomeData<- subset(outcomeData,outcomeData[,"State"]==state & outcomeData[,outcome]!="Not Available" ,select=c("Hospital.Name",outcome))
  
  outcomeData[,outcome] = as.numeric(as.character(outcomeData[,outcome]))
  sortedData <- sortByColumn(outcomeData,outcome)
  
  topHospitals <- subset(sortedData,sortedData[,outcome]==sortedData[1,2],select=c("Hospital.Name"))
  
  topHospitals<-sortByColumn(topHospitals,"Hospital.Name")
  
  if(length(topHospitals)>0)
  {
  as.character(topHospitals[1])
  }else
  {
  C("No Data")
  }
}