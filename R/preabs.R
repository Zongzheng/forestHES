##A R function for converting species matrix (wide data) to present-absence matrix
preabs<-function(widedata){
  widedata[is.na(widedata)]<-0
  widedata[widedata!=0]<-1
  return(widedata)
}
