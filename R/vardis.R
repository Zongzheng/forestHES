vardis<-function(data,varcat="numeric",plot.id,var,breaks,
                  include.lowest = FALSE, right = TRUE){
        index=c("numeric","character")
        varcat <- match.arg(varcat, index)
        plot.id=data[,plot.id]
        var=data[,var]
        if(varcat=="numeric"){
          vardis<-table(plot.id,cut(var,breaks = breaks,
                                          include.lowest = include.lowest, right = right))
        }
        else if(varcat=="character"){
          vardis<-table(plot.id,var)
        }
        return(vardis)
}


