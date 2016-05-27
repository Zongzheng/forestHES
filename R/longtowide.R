##A R function for converting  to long data to wide data
longtowide<-function(longdata,left,up,inner=NULL,
                     fun=rep("sum",length(inner)),freq=FALSE){
  if(is.null(inner)){
    abundance<-table(left=longdata[,left],up=longdata[,up])
    if(freq){
      count<-t(t(preabs(abundance))*colSums(preabs(abundance)))
      frequency<-count/nrow(count)
      widedata<-list(count=count,frequency=frequency)
    }else{
      widedata=list(abundance)}
  }else if(!is.null(inner)){
    widevar<-list()
    for(i in 1:length(inner)){
      widevar[[i]]<-tapply(longdata[,inner[i]],longdata[c(left,up)],match.fun(fun[i]))
    }
    names(widevar)<-inner
    widedata<-widevar
  }
  widedata<-lapply(widedata,function(x) {x[which(is.na(x))]<-0;return(x)})
  if(length(widedata)==1){
    widedata<-widedata[[1]]
  }else if(length(widedata)>1){
    widedata<-widedata
  }
  return(widedata)
}
