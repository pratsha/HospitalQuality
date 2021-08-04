# The function reads the outcome-of-care-measures.csv ﬁle and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the speciﬁed outcome
# in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
# be one of “heart attack”, “heart failure”, or “pneumonia”. Hospitals that do not have data on a particular
# outcome should be excluded from the set of hospitals when deciding the rankings.

best<-function(state, outcome){
  
  caremeasures<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
  caremeasures<-caremeasures[caremeasures$State==state,]
  
  if(nrow(caremeasures)==0)
  {
    stop("Invalid State")
  }
  
  if(outcome=="Heart attack"){
  
    lowest=min(as.numeric(caremeasures[!caremeasures[,11]=="Not Available",11]))
    lowest=min(caremeasures[,11])
   
    sort(caremeasures[caremeasures[,11]==lowest,2])[1]
  }
  else if(outcome=="Heart failure")
  {
    lowest=min(as.numeric(caremeasures[!caremeasures[,17]=="Not Available",17]))
    print(lowest)
    sort(caremeasures[caremeasures[,17]==lowest,2])
  }
  else if(outcome=="Pneumonia")
  {
    lowest=min(as.numeric(caremeasures[!caremeasures[,23]=="Not Available",23]))
    lowest=min(caremeasures[,23])
   
    sort(caremeasures[caremeasures[,23]==lowest,2])[1]
  }
}