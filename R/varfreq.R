varfreq<-function(data,varcat="numeric",plot.id,var,sizeclass=1,
        breaks=seq(min(var),max(var),sizeclass),
        include.lowest = FALSE, right = TRUE){
        index=c("numeric","character")
        varcat <- match.arg(varcat, index)
        INDEX=names(data)
        plot.id<-match.arg(plot.id,INDEX)
        var<-match.arg(var,INDEX)
        plot.id=data[,plot.id]
        var=data[,var]
        if(varcat=="numeric"){
          varFrequency<-table(plot.id,cut(var,breaks = breaks,
                                          include.lowest = include.lowest, right = right))
        }
        else if(varcat=="character"){
          varFrequency<-table(plot.id,var)
        }
        return(varFrequency)
}


