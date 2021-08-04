# Returns best hospital for different outcomes i.e. "Heart attack", "Heart failure"
# and "Pneumonia

best<-function(state, outcome){
  caremeasures<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
  caremeasures<-caremeasures[caremeasures$State==state,]
  
  
  if(nrow(caremeasures)==0)
  {
    stop("Invalid State")
  }
  
  if(outcome=="heart attack"){
  
    lowest=min(caremeasures[!caremeasures[,11]=="Not Available",11])
    sort(caremeasures[caremeasures[,11]==lowest,2])[1]
  }
  else if(outcome=="heart failure")
  {
    lowest=min(as.numeric(caremeasures[!caremeasures[,17]=="Not Available",17]))
    sort(caremeasures[caremeasures[,17]==lowest,2])[1]
  }
  else if(outcome=="pneumonia")
  {
    lowest=min(as.numeric(caremeasures[!caremeasures[,23]=="Not Available",23]))
    sort(caremeasures[caremeasures[,23]==lowest,2])[1]
  }
  else
  {
    stop("invalid outcome")
  }
}