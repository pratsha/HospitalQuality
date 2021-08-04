rankall<-function(outcome,num="best")
{
  caremeasures<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
  
  if(num=="best")
  {
    num<-1
  }
 
  
  
  
  if(outcome=="heart attack")
  {
    sorted<-caremeasures[!caremeasures[,11]=="Not Available",][,c(2,7,11)]
    ranked<-NULL
    
    if(num=="best")
    {
      num<-1
    }
    
    
    if(num>nrow(sorted))
    {
      stop("NA")
    }
    x<-split(sorted,sorted[2])
    print(length(x))
   
    for (lets in x) {
      lets<-as.data.frame(lets)
      
      lets<-lets[order(as.numeric(lets[,3]),lets[,1]),]
      if(num=="worst")
      {
        num<-nrow(sorted)
      }
      if(is.na(lets[num,3]))
      {
        lets[num,2]<-lets[length(lets),2]
      }
      ranked<-rbind(ranked,lets[num,])
    }
    ranked
    
  
  }
  else if(outcome=="heart failure")
  {
    sorted<-caremeasures[!caremeasures[,11]=="Not Available",][,c(2,7,17)]
    ranked<-NULL
    
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
    x<-split(sorted,sorted[2])
    print(length(x))
    
    for (lets in x) {
      lets<-as.data.frame(lets)
      
      lets<-lets[order(as.numeric(lets[,3]),lets[,1]),]
      if(is.na(lets[num,3]))
      {
        lets[num,2]<-lets[length(lets),2]
      }
      ranked<-rbind(ranked,lets[num,])
    }
    ranked
  }
  
  else if(outcome=="pneumonia")
  {
    sorted<-caremeasures[!caremeasures[,11]=="Not Available",][,c(2,7,23)]
    ranked<-NULL
    
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
    x<-split(sorted,sorted[2])
    
    
    for (lets in x) {
      
      lets<-as.data.frame(lets)
      
      lets<-lets[order(as.numeric(lets[,3]),lets[,1]),]
      if(is.na(lets[num,3]))
      {
        lets[num,2]<-lets[length(lets),2]
      }
      ranked<-rbind(ranked,lets[num,])
    }
    ranked
  }
}