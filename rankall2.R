setwd("D:/Data Science/Rprogramming_Assignment3")

rankall<-function(outcome,num ="best"){
  ## Read outcome data
  data<-read.csv("outcome-of-care-measures.csv")
  data<-data[order(data[,7]),]
  
  ## Check that state and outcome are valid
  if(!outcome %in% c("heart attack","heart failure","pneumonia")){
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with the given rank 30-day death rate
  states<-unique(data$State)
  output<-data.frame()
  for (i in 1:length(states)) {
    statename<-states[i]
    statename<-as.character(statename)
    stateselected<-subset(data,State==statename)
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
      rankstate<-statename
    } else if (num == "best") {
      rankname<- sorted[1, ]$Hospital.Name
      rankstate<-statename
    } else if (num == "worst") {
      rankname<- tail(sorted,n=1)$Hospital.Name
      rankstate<-tail(sorted,n=1)$State
    } else {
      rankname<-NA
      rankstate<-statename
    }
    result<-c(rankname,rankstate)
    output<-rbind(output,result)
  }
  
  output<-as.data.frame(output)
  colnames(output)<-c("hospital","state")
  output
  
}




#test

head(rankall("heart attack",20),10)
tail(rankall("pneumonia","worst"),3)
tail(rankall("heart failure"),10)
