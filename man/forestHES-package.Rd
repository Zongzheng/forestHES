\name{forestHES-package}
\alias{forestHES-package}
\alias{forestHES}
\docType{package}
\title{Forest health evaluation system at the forest stand level in China}
\description{
Assessing forest ecosystem health is an effective way for forest resource
management.Zhang & Lei (2014) proposed and developed a national forest health evaluation system at the forest stand level using analytic hierarchy process (AHP), has a high application value and practical significance.The R packge concentrates
on to relize the calculation of the national forest health evaluation system.
}
\author{Zongzheng Chai,chaizz@126.com}
\references{
Zhang HR, and Lei XD. 2014. Health management techniques for typical forest types. Beijing: Publishing house of forestry, China.
Chai ZZ.2016.National forest health evaluation system at the forest stand level in China\url{http://www.forest-soil.net/Upload/ueditor/file/20160514/1463221795322480.pdf}
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
