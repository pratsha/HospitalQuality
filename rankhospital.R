# Would return a character vector containing the name of the hospital with the 5th lowest 
# 30-day death rate for heart attack/heart failure/pneumonia
rankhospital<-function(state,outcome,num="best"){
  
  caremeasures<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
  caremeasures<-caremeasures[caremeasures$State==state,]
 
  if(outcome=="heart attack")
  {
    sorted<-caremeasures[!caremeasures[,11]=="Not Available",]
    x<-sorted[order(as.numeric(sorted[,11]),sorted[,2]),][,c(2,7,11)]
    
    
    if(num=="best")
    {
      num<-1
    }
    else if(num=="worst")
    {
      num<-nrow(sorted)
    }
    
    if(num>nrow(sorted))
    {
      stop("NA")
    }
    x[num,]
    
  }
  else if(outcome=="heart failure")
  {
    sorted<-caremeasures[!caremeasures[,17]=="Not Available",]
    x<-sorted[order(as.numeric(sorted[,17]),sorted[,2]),][,c(2,7,17)]
    
    
    if(num=="best")
    {
      num<-1
    }
    else if(num=="worst")
    {
      num<-nrow(sorted)
      
    }
    if(num>nrow(sorted))
    {
      stop("NA")
    }
    x[num,]
  
  }
  else if(outcome=="pneumonia")
  {
    sorted<-caremeasures[!caremeasures[,23]=="Not Available",]
    x<-sorted[order(as.numeric(sorted[,23]),sorted[,2]),][,c(2,7,23)]
    
   
    if(num=="best")
    {
      num<-1
    }
    else if(num=="worst")
    {
      num<-nrow(sorted)
    }
    if(num>nrow(sorted))
    {
      stop("NA")
    }
    x[num,]
  }
}