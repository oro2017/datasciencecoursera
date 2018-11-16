rankall <- function(outcome, num ="best") {
  ## Read outcome data
  outcomeData<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
 
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
  stateVector<-c("AK" ,"AL" ,"AR" ,"AZ" ,"CA" ,"CO" ,"CT" ,"DC" ,"DE" ,"FL" ,"GA" ,"GU" ,"HI" ,"IA" ,"ID" ,"IL" ,"IN" ,"KS" ,"KY" ,"LA" ,"ME"
                 ,"MA" ,"MD" ,"MI" ,"MN" ,"MO" ,"MS" ,"MT" ,"NC" ,"ND" ,"NE" ,"NH" ,"NJ" ,"NM" ,"NV" ,"NY" ,"OH" ,"OK" ,"OR" ,"PA" ,"PR"
                 ,"RI" ,"SC" ,"SD" ,"TN" ,"TX" ,"UT" ,"VA" ,"VI" ,"VT" ,"WA" ,"WI" ,"WV" ,"WY")
  
  if(!(outcome %in% outcomeVector) ) {
    stop("invalid outcome")
  } else {
  
    outcomeNewVal<-outcomeList[outcome]
    columnName<-paste("Hospital.30.Day.Death..Mortality..Rates.from.",outcomeNewVal,sep="")
    ##newData<-subset(newData,!(is.na(newData[columnName]) | is.na(newData["Hospital.Name"])))
    df <- data.frame(state = character(), hospital = character(), stringsAsFactors = FALSE)
    ## For each state, find the hospital of the given rank
    i = 0
    for (st in stateVector) {
 
      finalData<-subset(outcomeData, outcomeData$State==st)
    
      if (num == "best") {
        df <- rbind(df, data.frame(state = st, hospital = finalData[which.min(finalData[,columnName]),"Hospital.Name"]))
      } else if(num == "worst") {
        df <- rbind(df, data.frame(state = st, hospital = finalData[which.max(finalData[,columnName]),"Hospital.Name"]))
      } else {
        rank<-num
  
        finalData<-subset(finalData,!(is.na(finalData[columnName]) | is.na(finalData["Hospital.Name"])))
        finalData[,columnName]<-as.numeric(as.character(finalData[,columnName]))
        finalData[,"Hospital.Name"]<-as.character(finalData[,"Hospital.Name"])
      
        finalData<-finalData[order(finalData[,columnName],finalData[,"Hospital.Name"],decreasing = FALSE, na.last = TRUE),]
        df <- rbind(df, data.frame(state = st, hospital = finalData[rank,c("Hospital.Name")]))
      }
    }
  }
  df
}
