setwd("D:/Data Science/Rprogramming_Assignment3")

source("best.R")

rankhospital<-function(state,outcome,num ="best"){
  ## Read outcome data
  data<-read.csv("outcome-of-care-measures.csv")
  
  ## Check that state and outcome are valid
  if(!state %in% data$State){
    stop("invalid state")
  }
  if(!outcome %in% c("heart attack","heart failure","pneumonia")){
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with the given rank 30-day death rate
  stateselected<-subset(data,State==state)
  stateselected[,11]<-as.numeric(stateselected[,11])
  stateselected[,17]<-as.numeric(stateselected[,17])
  stateselected[,23]<-as.numeric(stateselected[,23])
  if(outcome == "heart attack"){
    stateselected<-subset(stateselected,!is.na(stateselected[,11]))
    sorted<-stateselected[order(stateselected[,11],stateselected$Hospital.Name),] 
    #sort the data frame by column 11
  } else if (outcome == "heart failure") {
    stateselected<-subset(stateselected,!is.na(stateselected[,17]))
    sorted<-stateselected[order(stateselected[,17],stateselected$Hospital.Name),] 
  } else {
    stateselected<-subset(stateselected,!is.na(stateselected[,23]))
    sorted<-stateselected[order(stateselected[,23],stateselected$Hospital.Name),] 
  }
  if (num %in% 1:nrow(sorted)){
    rankname<-sorted[num,]$Hospital.Name
  } else if (num == "best") {
    rankname<- best(state,outcome)
  } else if (num == "worst") {
    rankname<- tail(sorted,n=1)$Hospital.Name
  } else {
    rankname<-NA
  }
 rankname
}


## Test

rankhospital("TX","heart failure",4)
rankhospital("MD","heart attack","worst")
rankhospital("MN","heart attack",5000)



