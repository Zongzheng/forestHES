\name{fhes}
\alias{fhes}
\title{
The national forest health evaluation system
}
\description{
Zhang & Lei (2014) proposed and developed a national forest health evaluation system at the forest stand level using analytic hierarchy process (AHP), has a high application value and practical significance.
}
\usage{
fhes(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14,
c15, c16, c17, c18, c19, c20, c21, L_8 = 3.2, U_8 = 6.4,
L_15 = 30, U_15 = 60, L_16 = 0.8, U_16 = 1.3, O1_16 = 1.15,
O2_16 = 1.25, L_17 = 5, U_17 = 8.5, O1_17 = 6.5, O2_17 = 7,
L_18 = 10, U_18 = 80, L_19 = 1.49, U_19 = 13.19, L_20 = 5,
U_20 = 30, L_21 = 80, U_21 = 200)
}
\arguments{
  \item{c1}{
Tree diversity index
}
  \item{c2}{
Shrub diversity index
}
  \item{c3}{
Herb diversity index
}
  \item{c4}{
Storey quantity
}
  \item{c5}{
Tree size diversity index
}
  \item{c6}{
Age structure
}
  \item{c7}{
Canopy closure
}
  \item{c8}{
Average volume increment per hectare
}
  \item{c9}{
Bio-disaster ratio
}
  \item{c10}{
Non bio-disaster ratio
}
  \item{c11}{
Crown length ratio
}
  \item{c12}{
Crown fade ratio
}
  \item{c13}{
Foliage falling ratio
}
  \item{c14}{
Regeneration density
}
  \item{c15}{
Soil depth
}
  \item{c16}{
Soil density
}
  \item{c17}{
Soil pH
}
  \item{c18}{
Soil organic matter
}
  \item{c19}{
Soil total nitrogen
}
  \item{c20}{
Soil available phosphorus
}
  \item{c21}{
Soil available potassium
}
  \item{L_8}{
Lower limit of Average volume increment per hectare
}
  \item{U_8}{
Upper limit of average volume increment per hectare
}
  \item{L_15}{
Lower limit of average volume increment per hectare
}
  \item{U_15}{
Upper limit of average volume increment per hectare
}
  \item{L_16}{
Lower limit of soil density
}
  \item{U_16}{
Upper limit of soil density
}
  \item{O1_16}{
Effective range of soil density
}
  \item{O2_16}{
Effective range of soil density
}
  \item{L_17}{
Lower limit of soil pH
}
  \item{U_17}{
Upper limit of soil pH
}
  \item{O1_17}{
Effective range of soil pH
}
  \item{O2_17}{
Effective range of soil pH
}
  \item{L_18}{
Lower limit of soil organic matter
}
  \item{U_18}{
Upper limit of soil organic matter
}
  \item{L_19}{
Lower limit of soil total nitrogen
}
  \item{U_19}{
Upper limit of soil total nitrogen
}
  \item{L_20}{
Lower limit of soil available phosphorus
}
  \item{U_20}{
Upper limit of soil available phosphorus
}
  \item{L_21}{
Lower limit of soil available potassium
}
  \item{U_21}{
Upper limit of soil available potassium
}
}
\details{
None
}
\value{
\item{indexSystem}{index system}
\item{legend}{legend}
\item{index}{index}
\item{coverted.Index}{coverted.Index}
\item{weighted.Index}{weighted.Index}
\item{unweighted.Criterion}{unweighted.Criterion}
\item{weighted.Criterion}{weighted.Criterion}
\item{evaluation}{evaluation results}
}
\references{
Zhang HR, and Lei XD. 2014. Health management techniques for typical forest types. Beijing: Publishing house of forestry, China
Chai ZZ.2016.National forest health evaluation system at the forest stand level in China\url{http://www.forest-soil.net/Upload/ueditor/file/20160514/1463221795322480.pdf}
}
\author{
Zongzheng Chai
}
\note{
None
}
\seealso{
None
}
\examples{
##forest health evaluation for singel sample plot
tree.singlefhes<-fhes(c1=0.67,c2=1.89,c3=2.04,c4=4,c5=2.04,c6=1,c7=0.3,
               c8=3.4839,c9=0.75,c10=0.15,c11=0,c12=0.75,c13=0.35,
               c14=2500,c15=53,c16=1.56,c17=5.49,c18=34.03,c19=2.05,
               c20=21.38,c21=91.67)
tree.singlefhes

##Assigned the threshold of some variables, according to specific
##realistic condition or research contents
tree.spefhes<-fhes(c1=0.67,c2=1.89,c3=2.04,c4=4,c5=2.04,c6=1,c7=0.3,
               c8=3.4839,c9=0.75,c10=0.15,c11=0,c12=0.75,c13=0.35,
               c14=2500,c15=53,c16=1.56,c17=5.49,c18=34.03,c19=2.05,
               c20=21.38,c21=91.67,
               L_18=15,U_18=90,L_19=2,U_19=14,
               L_20=8,U_20=40,L_21=60,U_21=120)
tree.spefhes

##forest health evaluation for multiple sample plot
tree.multiplefhes<-fhes(c1=c(0.67,0,0.68),c2=c(1.89,1.33,1.79),c3=c(2.04,1.91,2.19),
               c4=c(4,3,4),c5=c(2.04,1.47,2.19),c6=c(1,0,1),c7=c(0.3,0.95,0.9),
               c8=c(3.4839,3.1579,5.9231),c9=c(0.75,0,0.3),c10=c(0.15,0,0.09),
               c11=c(0,0,0),c12=c(0.75,0.07,0.84),c13=c(0.35,0.11,0.84),
               c14=c(833.33,833.33,2500),c15=c(53,59,43),c16=c(1.28,1.56,1.33),
               c17=c(5.49,5.91,5.27),c18=c(38.32,34.03,25.37),c19=c(2.05,1.04,1.27),
               c20=c(21.38,34.66,14.19),c21=c(86.67,80,91.67))
tree.multiplefhes

##More regulare method of forest health evaluation for multiple sample plot
data(testIndex)
tree.fhes<-fhes(c1=testIndex$C1,c2=testIndex$C2,c3=testIndex$C3,c4=testIndex$C4,
     c5=testIndex$C5,c6=testIndex$C6,c7=testIndex$C7,c8=testIndex$C8,
     c9=testIndex$C9,c10=testIndex$C10,c11=testIndex$C11,c12=testIndex$C12,
    c13=testIndex$C13,c14=testIndex$C14,c15=testIndex$C15,c16=testIndex$C16,
    c17=testIndex$C17,c18=testIndex$C18,c19=testIndex$C19,c20=testIndex$C20,
    c21=testIndex$C21)
tree.fhes
pie(table(tree.fhes$evaluation$grade))
}

