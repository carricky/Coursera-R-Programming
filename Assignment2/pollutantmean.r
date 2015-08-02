pollutantmean<-function(directory,pollutant,id=1:332)
{
  a<-list.files(directory)
  dir<-paste("./",directory,"/",a,sep="")
  sump<-0
  sumq<-0
  for(i in id)
  {
    y<-dir[i]
    tempcsv<-read.csv(y)
    
    x<-tempcsv[pollutant]
    p<-sum(x,na.rm=TRUE)
    q<-sum(!is.na(x))
    sump<-sump+p
    sumq<-sumq+q
    
  }
  print(sump/sumq)
}
