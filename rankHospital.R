rankHospital<-function(state=NULL,outcome=NULL,index='BEST'){
  options(warn=-1)
  mainDataSet<-read.csv("outcome-of-care-measures.csv",colClasses="character")
  listOfStates<-levels(as.factor(mainDataSet$State))
  listOfOutcomes<-c("heart failure", "heart attack","pneumonia")
  colNumber<-0
  
  # check for valid state
  if(!(toupper(state) %in% listOfStates)) {
    errorMsg<-"Invalid State"
    stop(errorMsg)
  }
  else{
      stateSubset<-mainDataSet[mainDataSet$State==toupper(state),]
  }
  
  #check for valid outcome
  if(!(toupper(outcome) %in% toupper(listOfOutcomes))) {
    errorMsg<-"Invalid Outcome"
    stop(errorMsg)
  }
  
  #get the required reason column index 
  
  if(toupper(outcome) == toupper("heart attack")) {
    colNumber<-11
  }
  else if(toupper(outcome) == toupper("heart failure")) {
    colNumber<-17
  }
  else if(toupper(outcome) == toupper("pneumonia")) {
    colNumber<-23
  }
  
  stateSubset2<-stateSubset[,c(2,colNumber)] #subset the hospital name and rate into a child data frame
  stateSubset2[2]<-as.numeric(stateSubset2[[2]]) #convert the rate from character entries to numeric ones and store it back into the same column 
  stateSubset2<-stateSubset2[complete.cases(stateSubset2),] #compute result only for complete cases i.e. no NA rankings
  nRows<-nrow(stateSubset2)
  
  if ((toupper(index)=='BEST')||(toupper(index)=='WORST')){ #check if index value is best or worst
    if (toupper(index)=='BEST'){
      nIndex<-1 #best index
    }
    else if (toupper(index)=='WORST'){
      nIndex<-nRows #worst index
    }
  }
  else{
    nIndex<-as.numeric(index) #index as passed into function
  }

  if (nIndex<=nRows){ #check if requested index is within bounds 
    indx<-order(stateSubset2[2],stateSubset2[1],na.last=TRUE)#sort the data frame according to the rate and name of hospital
    preResult<-stateSubset2[indx,] #rearrange all the entries in the stateSubset2 data frame according to the sorting index
    result<-preResult[nIndex,1] #if so return the hospital name for the given index
  }
  else{
    errorMsg<-"Invalid Index, Index out of Bounds"
    result<-NA #return NA if index is out of bounds (beyond the no of rows of subset)
    message(errorMsg)
  }

  return(result)
}