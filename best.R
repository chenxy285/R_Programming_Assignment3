outcome<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
head(outcome)
names(outcome)
class(outcome[,11])
outcome[,11]<-as.numeric(outcome[,11])
hist(outcome[,11])
str(outcome)

best<-function (state, outcome){
  ## Read outcome data
  data<-read.csv("outcome-of-care-measures.csv")
  ## Check that state and outcome are valid
  if(!state %in% data$State){
    stop("invalid state")
  }
  if(!outcome %in% c("heart attack","heart failure","pneumonia")){
    stop("invalid outcome")
  }
  ## Return hospital name in that state with lowest 30-day death rate
  stateselected<-subset(data,State==state)
  if(outcome == "heart attack"){
    bestrow<-stateselected[which.min(stateselected[,11]),] 
  } else if(outcome == "heart failure"){
    bestrow<-stateselected[which.min(stateselected[,17]),] 
  } else {
    bestrow<-stateselected[which.min(stateselected[,23]),] 
  }
  besthos<-bestrow$Hospital.Name
  besthos<-sort(besthos)
  besthos[1]
}

# rank v.s. order: https://towardsdatascience.com/r-rank-vs-order-753cc7665951
best("TX","heart attack")


