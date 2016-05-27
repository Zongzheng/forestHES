\name{widetolong}
\alias{widetolong}
\title{
A R function for converting  to wide data to long data
}
\description{
Converting wide data, such as species matrix, and attributes matrix to long data,such  as the treedata and herbdata.
}
\usage{
widetolong(widedata, leftlab = "left", uplab = "up", innerlab = "inner")
}
\arguments{
  \item{widedata}{
Data similar to the species matrix, and attributes matrix, etc.
}
  \item{leftlab}{
Row labels of wide data
}
  \item{uplab}{
Column labels of wide data
}
  \item{innerlab}{
Attributes labels of inner data of wide data
}
}
\value{
Long data,such as treedata,herbdata, etc.
}
\author{
Zongzheng Chai
}
\examples{
herb.spematrix<-longtowide(longdata=herbdata,left="plot.id",up="spe.id",inner="abundance")
herb.spedata<-widetolong(herb.spematrix,
                              leftlab="plot.id",uplab="spe.id",innerlab="abundance")
herb.spedata
##After removed zero in innerdata
rd0herb.spedata<-herb.spedata[-which(herb.spedata$abundance==0),]
rd0herb.spedata
}
