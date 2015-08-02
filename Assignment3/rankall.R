rankall<-function(outcome,num="best"){
	oc<-read.csv("outcome-of-care-measures.csv")
	try(if(!is.element(outcome,list("heart attack","heart failure","pneumonia"))) stop("invalid state"))
	if(outcome=="heart attack"){p<-11}
	else if(outcome=="heart failure") {p<-17}
	else {p<-23}
	df<-data.frame(h=oc$Hospital.Name,s=oc$State,oc[,p])
	ff<-data.frame()
	df = df[!(df[,3]=="Not Available"),]
  id<-unique(df$s)
  id<-id[order(id)]

	count<-1
	rankall <- data.frame(hospital = character(54), state = character(54), stringsAsFactors = FALSE)
  for(i in id){  
    
    ff<-df[(df[,2]==i),]
    ff<-ff[order(as.numeric(as.character(ff[,3])),ff[,1]),]    ##é”™è¯¯ï¼Œä¸çŸ¥åŽŸå›?
   
    
    if(num=="best"){
      return_rank=1	
    }
    else if(num=="worst"){
      return_rank=nrow(ff)	
    }
    else {
      return_rank=as.numeric(num)
    }
    
    rankall$hospital[count] = as.character(ff[return_rank,1])
    rankall$state[count] = i;

    count<-count+1
  }
  rankall
     
}
