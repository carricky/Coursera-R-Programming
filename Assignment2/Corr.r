corr<-function(directory,threshold=0)
{
  a<-list.files(directory)
  dir<-paste("./",directory,"/",a,sep="")
  id<-1:332
  df=vector()
  for(i in id){
    y<-dir[i]
    tempcsv<-read.csv(y)
    if(sum(complete.cases(tempcsv)==TRUE)>threshold){
      temp2<-na.omit(tempcsv)
      tempcorr<-cor(temp2$sulfate,temp2$nitrate)
      df<-c(df,tempcorr)
    }
  }
  df
}
