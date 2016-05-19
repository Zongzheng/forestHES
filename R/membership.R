membership <-
function(mode=c("up","down","mid"),X,L,U,O1,O2){
  mode<-match.arg(mode)
  covert<-function(mode,x,L,U,O1,O2){
    if(mode=="up"){
      if(x>=U){covdata=1}
      if(x>L&x<U){covdata=0.1+0.9*(x-L)/(U-L)}
      if(x<=L){covdata=0.1}
    }
    else if(mode=="down"){
      if(x>=U){covdata=0.1}
      if(x>L&x<U){covdata=1.0-0.9*(x-L)/(U-L)}
      if(x<=L){covdata=1.0}
    }
    else if(mode=="mid"){ 
      if(any(x<=L,x>=U)){covdata=0.1}
      if(x>L&x<U){covdata=0.1+0.9*(x-L)/(O1-L)}
      if(x>=O1&x<=O2){covdata=1.0}
      if(x>O2&x<U){covdata=1.0-0.9*(x-O2)/(U-O2)}
    }
    return(covdata)
  }
  if(length(X)==1){
    covdata<-covert(mode=mode,x=X,L,U,O1,O2)
  }
  if(length(X)>1){
    covdata<-list()
    for(i in 1:length(X)){
      covdata[i]<-covert(mode=mode,x=X[i],L,U,O1,O2)
    }
    covdata<-unlist(covdata)
  }
  return(covdata)
}
