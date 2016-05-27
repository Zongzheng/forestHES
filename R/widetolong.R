##A R function for converting  to wide data to long data
widetolong<-function(widedata,leftlab="left",uplab="up",innerlab="inner")
{
  data<-as.data.frame(widedata)
  innerdata<-vector()
  row.name<-rownames(data)
  col.name<-colnames(data)
  for(i in 1:dim(data)[1])
  {
    for(j in 1:dim(data)[2])
    {
      x<-c(row.name,i)
      y<-c(col.name,j)
      innerdata<-c(innerdata,data[i,j])
    }
  }
  longdata<-data.frame(row=rep(row.name,length(col.name)),
                       col=rep(col.name,length(row.name)),
                       inner=innerdata)
  names(longdata)<-c(leftlab,uplab,innerlab)
  return(longdata)
}
