rankhospital<-function(state,outcome,num="best"){
		oc<-read.csv("outcome-of-care-measures.csv")
	try(if(!is.element(state,oc$State)) stop("invalid state"))
	try(if(!is.element(outcome,list("heart attack","heart failure","pneumonia"))) stop("invalid state"))
	if(outcome=="heart attack"){p<-11}
	else if(outcome=="heart failure") {p<-17}
	else {p<-23}
	df<-data.frame(oc$Hospital.Name,oc$State,oc[,p])
	df<-df[with(df,df[,2]==state),]   
	df = df[!(df[,3]=="Not Available"),]
	
	df<-df[order(as.numeric(as.character(df[,3])),df[,1]),]
		if(num=="best"){
	    return_rank=1	
	}
	else if(num=="worst"){
	    return_rank=nrow(df)	
	}
	else {
		return_rank=as.numeric(num)
	}
	df[return_rank,1]
}
