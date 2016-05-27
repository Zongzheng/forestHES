##A R function for calculating the mcIntosh diversity index

mcIntosh<-function(x,MARGIN = 1){
  x <- drop(as.matrix(x))
  if (length(dim(x)) > 1) {
    total <- apply(x, MARGIN, sum)
    Dm<-(total-(rowSums(x^2))^0.5)/(total-total^0.5)
  }
  else {
    Dm <- (sum(x)-(sum(x^2))^0.5)/(sum(x)-sum(x)^0.5)}
  return(Dm)
}
