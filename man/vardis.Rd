\name{vardis}
\alias{vardis}
\title{
Distribution of species attributes in the sample plots
}
\description{
To show the distribution od species attributes (such as DBH,tree height) in the sample plots
}
\usage{
vardis(data, varcat = "numeric", plot.id, var, breaks,
include.lowest = FALSE, right = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Data class should be a dataframe, the colnames with plot.id, species name, and some attributes (sucha as DBH,tree height), see data (treedata)
}
  \item{varcat}{
Catory of species attributes, numeric and character, respectively.
}
  \item{plot.id}{
Identification number of sample plots
}
  \item{var}{
Attributes of species in the sample plot
}
  \item{breaks}{
Either a numeric vector of two or more unique cut points or a single number (greater than or equal to 2) giving the number of intervals into which x is to be cut.See the explanation of breaks of cut function
}
  \item{include.lowest}{
Labels for the levels of the resulting category. By default, labels are constructed using "(a,b]" interval notation. If labels = FALSE, simple integer codes are returned instead of a factor.See the explanation of include.lowest of cut function
}
  \item{right}{
Logical, indicating if the intervals should be closed on the right (and open on the left) or vice versa.See the explanation of right of cut function
}
}
\value{
Distribution od species attributes (such as DBH,tree height) in the sample plots
}
\author{
Zongzheng Chai
}
\examples{
######when variabel catogory is numeric######
####Variable distribution according to the actual range of variable and sizeclass=1
data(treedata)
numericVar.actualsize1<-vardis(treedata,plot.id="plot.id",var="ht.m",
                                breaks=seq(min(treedata$ht.m),max(treedata$ht.m),1))
numericVar.actualsize1
####Variable distribution according to the actual range of variable and sizeclass=2
numericVar.actualsize2<-vardis(treedata,plot.id="plot.id",var="ht.m",
                                breaks=seq(min(treedata$ht.m),max(treedata$ht.m),2))
numericVar.actualsize2
##when the lower value of actual range of variable included,include.lowest can  set TRUE
numericVar.actualInlower<-vardis(treedata,plot.id="plot.id",var="ht.m",
                                  breaks=seq(min(treedata$ht.m),max(treedata$ht.m),1),
                                  include.lowest=TRUE)
numericVar.actualInlower
##when the upper value of actual range of variable excluded,include.lowest can be set FALSE
numericVar.actualExupper<-vardis(treedata,plot.id="plot.id",var="ht.m",
                                  breaks=seq(min(treedata$ht.m),max(treedata$ht.m),1),
                                  right=FALSE)
numericVar.actualExupper

####Variable distribution according to the specific range of variable
range(treedata$ht.m)
##[1]  2.6 32.2
##specific range can be set between 2 and 34, sizeclass is 2
numericVar.specific<-vardis(treedata,plot.id="plot.id",var="ht.m",
                             breaks=2*1:17)
numericVar.specific

######when variabel catogory is character######
characterVar<-vardis(treedata,varcat="character",plot.id="plot.id",var="spe.id")
characterVar
}
