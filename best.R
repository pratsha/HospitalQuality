best<-function(state, outcome){
  caremeasures<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
  caremeasures<-caremeasures[caremeasures$State==state,]
  
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