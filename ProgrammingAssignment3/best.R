best <- function(stateVal, outcome) {
  ## Read outcome data
  outcomeData<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  newData<-subset(outcomeData, outcomeData$State==stateVal)
  ## Check that state and outcome are valid
  outcomeList<-list()
  key<-"heart failure"
  value<-"Heart.Failure"
  outcomeList[[key]]<-value
  key<-"heart attack"
  value<-"Heart.Attack"
  outcomeList[[key]]<-value
  key<-"pneumonia"
  value<-"Pneumonia"
  outcomeList[[key]]<-value
  outcomeVector<-c("heart attack", "heart failure", "pneumonia")
  stateVector<-c("AL" ,"AK" ,"AZ" ,"AR" ,"CA" ,"CO" ,"CT" ,"DE" ,"DC" ,"FL" ,"GA" ,"HI" ,"ID" ,"IL" ,"IN" ,"IA" ,"KS" ,"KY" ,"LA" ,"ME"
                 ,"MD" ,"MA" ,"MI" ,"MN" ,"MS" ,"MO" ,"MT" ,"NE" ,"NV" ,"NH" ,"NJ" ,"NM" ,"NY" ,"NC" ,"ND" ,"OH" ,"OK" ,"OR" ,"PA" ,"PR"
                 ,"RI" ,"SC" ,"SD" ,"TN" ,"TX" ,"UT" ,"VT" ,"VI" ,"VA" ,"WA" ,"WV" ,"WI" ,"WY" ,"GU")
  
  if(!(outcome %in% outcomeVector) ) {
    stop("invalid outcome")
  } else if(!(stateVal %in% stateVector)) {
    stop("invalid state")
  } else {
    ## Return hospital name in that state with lowest 30-day death rate
    outcomeNewVal<-outcomeList[outcome]
    columnName<-paste("Hospital.30.Day.Death..Mortality..Rates.from.",outcomeNewVal,sep="")
    newData[which.min(newData[,columnName]),"Hospital.Name"]
  }
}
