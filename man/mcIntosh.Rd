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
data(treedata)
herb.spematrix<-longtowide(longdata=herb,left="plotname",up="species",inner="abundance")
##compute diversity index for each plot
library(vegan)
#species richness index
herb.richness <- specnumber(herb.spematrix)
herb.richness
#species shannon-Wienner index
herb.shannon<-diversity(herb.spematrix,index="shannon")
herb.shannon
#species simpson index
herb.simpson<-diversity(herb.spematrix,index="simpson")
herb.simpson
#species pielou index
herb.pielou <- herb.shannon/log(herb.shannon)
herb.pielou
#species mcIntosh index
herb.mcIntosh<-mcIntosh(herb.spematrix)
herb.mcIntosh
}
