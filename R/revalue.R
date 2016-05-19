revalue<-function(data,plot.id,spe,var=NULL,fun,area=NULL){
  install.packages("reshpe2")
  plot.id=data[,plot.id]
  spe=data[,spe]
  frequency<-as.data.frame(t(t(as.data.frame(table(plot.id,spe)>0))*colSums(table(plot.id,spe)>0)))/nrow(table(plot.id,spe))
  frequency<-frequency[,which(colSums(frequency)>0)]
  if(!is.null(var)){
    library(reshape2)
    var.melt<-list()
    var.dcast<-list()
    for (i in 1:length(var)){
      var.melt[[i]]<-melt(as.data.frame(cbind(plot.id,spe,data[,which(colnames(data)==var[i]),drop=FALSE])),id=c("plot.id","spe"))
      var.dcast[[i]]<-dcast(var.melt[[i]],plot.id~variable+spe,match.fun(fun[i]))[,-1]
      var.dcast[[i]][is.na(var.dcast[[i]])]<-0
    }
    names(var.dcast)<-var
    if(!is.null(area)){
      var.dcast<-lapply(var.dcast,function(x) x/area)
    }else{
      var.dcast<-var.dcast
    }
    var.dcast<-c(var.dcast,frequency=list(frequency))
    re.var<-lapply(var.dcast,function(x) x/rowSums(x))
    names(re.var)<-paste("re",names(var.dcast),sep="")
    AV<-c(var.dcast,re.var)
    for (i in 1:length(AV)){
      colnames(AV[[i]])<-colnames(frequency)}
  }else{
    refrequency<-frequency/rowSums(frequency)
    AV=list(frequency=frequency,refrequency=refrequency) 
  }
  return(AV)
}


