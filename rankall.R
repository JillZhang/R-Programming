rankall <- function(outcome,num="best") {
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings="Not Available")
  
  ## Check that state and outcome are valid
  validOutcome = c("heart attack","heart failure","pneumonia")
  if (!outcome %in% validOutcome) { stop("invalid outcome")}
  
  ## convert outcome name into column name
  fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  colName <- fullColName[match(outcome,validOutcome)]
  
  all.state<-unique(data[,7])
  return.row<-0
  
  #loop all states
  for (i in all.state) {
  data.state <- data[data$State==i,]
  #sort by the outcome
   order.rate<-order(as.numeric(data.state[,colName]),data.state[,2],na.last = TRUE,decreasing=FALSE)
  new.data<-data.state[order.rate,2]
  last<-sum(!is.na(data.state[,colName]))
  return.row<-return.row+1
  
  #add the "best and "worst" value to num
  if (num=="best") num=1
  if (num=="worst") num=last 
  hospital[return.row]<-new.data[num]
  }
  return.data<-data.frame(hospital,state=all.state,row.names = all.state)
  #rank the final output by state
  return.data<-return.data[order(return.data$state,decreasing=FALSE),]
  return.data
  }
