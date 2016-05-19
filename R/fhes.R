fhes <-
function(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,
               c15,c16,c17,c18,c19,c20,c21,
               L_8=3.2,U_8=6.4,L_15=30,U_15=60,
               L_16=0.8,U_16=1.3,O1_16=1.15,O2_16=1.25,
               L_17=5,U_17=8.5,O1_17=6.5,O2_17=7,
               L_18=10,U_18=80,L_19=1.49,U_19=13.19,
               L_20=5,U_20=30,L_21=80,U_21=200){
  indexSystem$L[8]<-L_8
  indexSystem$U[8]<-U_8
  indexSystem$L[15]<-L_15
  indexSystem$U[15]<-U_15
  indexSystem$L[16]<-L_16
  indexSystem$U[16]<-U_16
  indexSystem$O1[16]<-O1_16
  indexSystem$O2[16]<-O2_16
  indexSystem$L[17]<-L_17
  indexSystem$U[17]<-U_17
  indexSystem$O1[17]<-O1_17
  indexSystem$O2[17]<-O2_17
  indexSystem$L[18]<-L_18
  indexSystem$U[18]<-U_18
  indexSystem$L[19]<-L_19
  indexSystem$U[19]<-U_19
  indexSystem$L[20]<-L_20
  indexSystem$U[20]<-U_20
  indexSystem$L[21]<-L_21
  indexSystem$U[21]<-U_21
  legend<-data.frame(cbind(id=c(paste("b",1:7,sep=""),paste("c",1:21,sep="")),
                           index=c("Species diversity","Stand structure complexity","Stand productivity",
                                   "Harmful factors occurrence","Tree vitality","Natural regenerating capability",
                                   "Soil quality","Tree diversity index","Shrub diversity index","Herb diversity index",
                                   "Storey quantity","Tree size diversity index","Age structure","Canopy closure",
                                   "Average volume increment per hectare","Bio-disaster ratio","Non bio-disaster ratio",
                                   "Crown length ratio","Crown fade ratio","Foliage falling ratio","Regeneration density",
                                   "Soil depth","Soil density","Soil pH","Soil organic matter","Soil total nitrogen",
                                   "Soil available phosphorus","Soil available potassium")))
  datamat<-as.data.frame(cbind(c1,c2,c3,c4,c5,c6,c7,c8,
                               c9,c10,c11,c12,c13,c14,
                               c15,c16,c17,c18,c19,c20,c21))
  d1<-membership(datamat$c1,mode="up",L=0,U=2.173)
  d2<-membership(datamat$c2,mode="up",L=0,U=2.173)
  d3<-membership(datamat$c3,mode="up",L=0,U=2.173)
  d4<-membership(datamat$c4,mode="up",L=1,U=4)
  d5<-membership(datamat$c5,mode="up",L=0,U=2.173)
  d6<-membership(datamat$c6,mode="up",L=0,U=1)
  d7<-membership(datamat$c7,mode="mid",L=0.2,U=1.0,O1=0.5,O2=0.7)
  d8<-membership(datamat$c8,mode="up",L=L_8,U=U_8)
  d9<-membership(datamat$c9,mode="down",L=0,U=1)
  d10<-membership(datamat$c10,mode="down",L=0,U=1)
  d11<-membership(datamat$c11,mode="mid",L=0,U=1,O1=0.2,O2=0.4)
  d12<-membership(datamat$c12,mode="down",L=0,U=1)
  d13<-membership(datamat$c13,mode="down",L=0,U=1)
  d14<-membership(datamat$c14,mode="up",L=500,U=2500)
  d15<-membership(datamat$c15,mode="up",L=L_15,U=U_15)
  d16<-membership(datamat$c16,mode="mid",L=L_16,U=U_16,O1=O1_16,O2=O2_16)
  d17<-membership(datamat$c17,mode="mid",L=L_17,U=U_17,O1=O1_17,O2=O2_17)
  d18<-membership(datamat$c18,mode="up",L=L_18,U=U_18)
  d19<-membership(datamat$c19,mode="up",L=L_19,U=U_19)
  d20<-membership(datamat$c20,mode="up",L=L_20,U=U_20)
  d21<-membership(datamat$c21,mode="up",L=L_21,U=U_21)
  covertmat<-as.data.frame(cbind(d1,d2,d3,d4,d5,d6,d7,d8,
                                 d9,d10,d11,d12,d13,d14,
                                 d15,d16,d17,d18,d19,d20,d21))
  weightC=c(0.480,0.231,0.289,0.291,0.232,0.233,0.245,1.000,0.630,0.370,
            0.226,0.469,0.306,1.000,0.131,0.217,0.171,0.145,0.133,0.104,0.100)
  weighted.index<-as.data.frame(t(t(covertmat)*weightC))
  names(weighted.index)<-paste("wc",1:21,sep="")
  criterion<-as.data.frame(cbind(cri1=rowSums(weighted.index[,1:3]),cri2=rowSums(weighted.index[,4:7]),
                                 cri3=weighted.index[,8],cri4=rowSums(weighted.index[,9:10]),
                                 cri5=rowSums(weighted.index[,11:13]),cri6=weighted.index[,14],
                                 cri7=rowSums(weighted.index[,15:21])))
  weightB=c(0.1887,0.1267,0.1303,0.2278,0.1263,0.0699,0.1303)
  unweighted.criterion<-as.data.frame(criterion)
  names(unweighted.criterion)<-paste("b",1:7,sep="")
  weighted.criterion<-as.data.frame(t(t(criterion)*weightB))
  names(weighted.criterion)<-paste("wb",1:7,sep="")
  score=rowSums(weighted.criterion)
  grade=c()
  length(grade)=length(score)
  grade[which(score>0&score<=0.25)]<-"Unhealthy"
  grade[which(score>0.25&score<=0.50)]<-"Subhealthy"
  grade[which(score>0.50&score<=0.75)]<-"Healthy"
  grade[which(score>0.75&score<=1.00)]<-"Exehealthy"
  evaluation<-as.data.frame(cbind(score,grade))
  list(indexSystem=indexSystem,legend=legend,
    index=datamat,coverted.Index=covertmat,weighted.Index=weighted.index,
       unweighted.Criterion=unweighted.criterion,
       weighted.Criterion=weighted.criterion,evaluation= evaluation)
}
