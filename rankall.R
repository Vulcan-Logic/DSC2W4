rankall<-function(outcome=NULL,rank='BEST'){

    
  mainDataSet<-read.csv("outcome-of-care-measures.csv",colClasses="character")
  listOfStates<-levels(as.factor(mainDataSet$State))
  
  listOfOutcomes<-c("heart failure", "heart attack","pneumonia")
  
  
  #check for valid outcome
  if(!(toupper(outcome) %in% toupper(listOfOutcomes))) {
    errorMsg<-"Invalid Outcome"
    stop(errorMsg)
  }
  else{
    ctr<-1 #set counter to 1
    returnHospitalName<-NULL #declare an empty vector need this as append does not work for a null valued vector
    for(state in listOfStates){ #loop thru all values of states in list
      returnHospitalName[ctr]<-rankHospital2(state=state,outcome=outcome,index=rank,mainDataSet=mainDataSet) #add to return vector
      ctr<-ctr+1
    }
  }
  
  result<-data.frame(cbind(returnHospitalName,listOfStates))
  names(result)<-c('hospital','state')
  return(result)  #return a data frame
}
