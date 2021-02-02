best=function(state, outcome){
  
  #variable
  options(warn=-1)
  mainDataSet<-read.csv("outcome-of-care-measures.csv",colClasses="character")
  stateSubset<-mainDataSet[mainDataSet$State==toupper(state),]  
  listOfStates<-levels(as.factor(mainDataSet$State))
  listOfOutcomes<-c("heart failure", "heart attack","pneumonia")
  colNumber<-0
  
  # check for valid state
  if(!(toupper(state) %in% listOfStates)) {
    errorMsg<-"Invalid State"
    stop(errorMsg)
  }
  #check for valid outcome
  if(!(toupper(outcome) %in% toupper(listOfOutcomes))) {
    errorMsg<-"Invalid Outcome"
    stop(errorMsg)
  }
    
  #get the minimum value from the required state subset column  
  
  if(toupper(outcome) == toupper("heart attack")) {
     colNumber<-11
  }
  else if(toupper(outcome) == toupper("heart failure")) {
    colNumber<-17
   }
  else if(toupper(outcome) == toupper("pneumonia")) {
    colNumber<-23
   }
  
    
  #try to get the final subset 
  stateSubset[[colNumber]]<-as.numeric(stateSubset[[colNumber]],length=5)
  stateSubset<-stateSubset[complete.cases(stateSubset),]
  minVal<-min(stateSubset[[colNumber]],na.rm=TRUE)
  result1Set<-stateSubset[stateSubset[[colNumber]]==minVal, 2]
  if(length(result1Set)>1){
     #print("More than one result")
     result2Set<-sort(result1Set)
     result1Set<-result2Set[1]
  }
  options(warn=0)
  return(result1Set)
}