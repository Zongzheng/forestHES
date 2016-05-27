\name{membership}
\alias{membership}
\title{Standardize the indices by membership functional models
}
\description{
Obtained indices always can not be used to evaluate the forest health because the indices with different dimension. And three membership functional models were thus used to standardize these indices,
}
\usage{
membership(mode = c("up", "down", "mid"), X, L, U, O1, O2)
}
\arguments{
  \item{mode}{
Three membership functional models were thus used to standardize these indices, which are up mode, down mode, and middle mode, respectively. For the up mode, a higher value is better; for the down mode, lower is better; for the middle mode, which have an effective range.
}
  \item{X}{The actual observed value of indices}
  \item{L}{Lower limit of the indices}
  \item{U}{Upper limit of the indices}
  \item{O1}{O1 and O2 are the effective range of the indices}
  \item{O2}{O1 and O2 are the effective range of the indices.}
}
\value{
Standardized the indices by membership functional models
}
\references{
Zhang HR, and Lei XD. 2014. Health management techniques for typical forest types. Beijing: Publishing house of forestry, China
Chai ZZ.2016.National forest health evaluation system at the forest stand level in China\url{http://www.forest-soil.net/Upload/ueditor/file/20160514/1463221795322480.pdf}
}
\author{
Zongzheng Chai
}
\examples{
up.index<-membership(mode="up",X=0.67,L=0,U=2.173)
up.index
down.index<-membership(mode="down",X=0.8,L=0,U=2.73)
down.index
mid.index<-membership(mode="mid",X=c(0.1,0.3,0.4,0.6,0.9),L=0.2,U=1.0,O1=0.5,O2=0.7)
mid.index
}
