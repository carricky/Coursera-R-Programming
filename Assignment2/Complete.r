complete<-function(directory,id=1:332)
{
  a<-list.files(directory)
  dir<-paste("./",directory,"/",a,sep="")
  df = data.frame() 
  for(i in id){

    y<-dir[i]
    tempcsv<-read.csv(y)
    tempRow <- cbind(id=i, nobs=sum(complete.cases(tempcsv)==TRUE))
    df = rbind(df,tempRow)
  }
  df
}
