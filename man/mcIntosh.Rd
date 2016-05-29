\name{mcIntosh}
\alias{mcIntosh}
\title{
Calculation of mcIntosh diversity index
}
\description{
Calculation of mcIntosh diversity index
}
\usage{
mcIntosh(x, MARGIN = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The species matrix
}
  \item{MARGIN}{
Compute the mcIntosh diversity index by row, MARGIN can be set 1;
compute the mcIntosh diversity index by column, MARGIN can be set 2;
}
}
\value{
The mcIntosh diversity index
}
\references{
McIntosh RP.1967. An index of diversity and the relation of certain concepts to diversity. Ecol.Soc.Am. 48:329-404.
}
\author{
Zongzheng Chai
}
\examples{
######Calculation based on multiple plots
####convert raw data to species matrix
##raw data of multiple plots
data(herbdata)
herb.spematrix<-longtowide(longdata=herbdata,left="plot.id",up="spe.id",inner="abundance")
##compute species mcIntosh diversity index for each plot
herb.mcIntosh<-mcIntosh(herb.spematrix)
herb.mcIntosh
}
