outcome<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
head(outcome)
outcome[,11]<-as.numeric(outcome[,11])
hist(outcome[,11],xlab = "30 days Mortality",main="30 days Mortality")

