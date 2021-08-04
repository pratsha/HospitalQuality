rankhospital<-function(state,outcome,num="best"){
  
  caremeasures<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
  caremeasures<-caremeasures[caremeasures$State==state,]
  
  if(outcome=="heart attack")
  {
    sorted<-caremeasures[!caremeasures[,11]=="Not Available",]
    sorted[order(as.numeric(sorted[,11])),2]
    x<-sorted[order(as.numeric(sorted[,11]),sorted[,2]),][,c(2,7,11)]
    x[num,]
    
  }
  else if(outcome=="heart failure")
  {
    sorted<-caremeasures[!caremeasures[,17]=="Not Available",]
    x<-sorted[order(as.numeric(sorted[,17]),sorted[,2]),][,c(2,7,17)]
    x[num,]
  
  }
  else if(outcome=="pneumonia")
  {
    sorted<-caremeasures[!caremeasures[,23]=="Not Available",]
    sorted[order(as.numeric(sorted[,23])),][,c(2,7,23)][num,]
    x<-sorted[order(as.numeric(sorted[,23]),sorted[,2]),][,c(2,7,23)]
    x[num,]
  }
}