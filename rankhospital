rankhospital <- function(state, outcome,num="best") {
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings="Not Available")
  
  ## Check that state and outcome are valid
  validOutcome = c("heart attack","heart failure","pneumonia")
  if (!outcome %in% validOutcome) { stop("invalid outcome")}
  
  validState = unique(data[,7])
  if (!state %in% validState) stop("invalid state")
  
  ## convert outcome name into column name
  fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  colName <- fullColName[match(outcome,validOutcome)]
  
  ## Return hospital name in that state with lowest 30-day death rate
  data.state <- data[data$State==state,]
  order.rate<-order(as.numeric(data.state[,colName]),data.state[,2],na.last = TRUE,decreasing=FALSE)
  new.data<-data.state[order.rate,2]
  last<-sum(!is.na(data.state[,colName]))
  if (num=="best") num=1
  if (num=="worst") num=last 
    new.data[num]
}
