\name{preabs}
\alias{preabs}
\title{
A R function for converting species matrix (wide data) to present-absent matrix
}
\description{
Converting species matrix (wide data) to present-absent matrix
}
\usage{
preabs(widedata)
}
\arguments{
  \item{widedata}{A species matrix}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A present-absent matrix
}
\examples{
herb.spematrix<-longtowide(longdata=herb,left="plotname",up="species",inner="abundance")
herb.preabs<-preabs(herb.spematrix)
herb.preabs
}
