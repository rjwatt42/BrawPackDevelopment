
#' @export
prepareData4File<-function(result=braw.res$result,both=FALSE) {
  
  newVariables2<-newVariables<-c()
  if (is.null(result$hypothesis$IV2)) {
    useOrder<-order(result$participant)
    newVariables<-data.frame(result$participant[useOrder],
                             result$dv[useOrder],result$iv[useOrder],
                             result$dv+NA)
    names(newVariables)<-c("ID",result$hypothesis$DV$name,result$hypothesis$IV$name,"-")
    if (result$design$sIV1Use=="Within") {
      newVariables2<-c()
      m<-levels(result$iv)
      for (i in 1:length(m)) {
        use<-result$iv==m[i]
        newVariables2<-cbind(newVariables2,result$dv[use])
      }
      if (braw.env$fullWithinNames)
        newNames2<-paste0(result$hypothesis$DV$name,"|",result$hypothesis$IV$name,"=",result$hypothesis$IV$cases)
      else
        newNames2<-paste0(result$hypothesis$IV$cases)
      newVariables2<-cbind(result$participant[1:sum(use)],newVariables2)
      newNames2<-c("ID",newNames2)
      
      newVariables2<-newVariables2[order(newVariables2[,1]),]
      newVariables2<-data.frame(newVariables2)
      names(newVariables2)<-newNames2
      
      if (!both && braw.def$design$sDataFormat=="wide") newVariables<-newVariables2
      if (both) newVariables<-list(long=newVariables,wide=newVariables2)
    }
  } else {
    newVariables<-data.frame(result$participant,result$dv,result$iv,result$iv2)
    newVariables<-newVariables[order(newVariables[,1]),]
    names(newVariables)<-c("ID",result$hypothesis$DV$name,result$hypothesis$IV$name,result$hypothesis$IV2$name)
    
    if (result$design$sIV1Use=="Within" && result$design$sIV2Use=="Between") {
      newVariables2<-c()
      m1<-levels(result$iv)
      for (i1 in 1:length(m1)) {
        use<-result$iv==m1[i1]
        newVariables2<-cbind(newVariables2,result$dv[use])
      }
      newVariables2<-data.frame(newVariables2)
      if (braw.env$fullWithinNames)
        newNames2<-paste0(result$hypothesis$DV$name,"|",result$hypothesis$IV$name,"=",result$hypothesis$IV$cases)
      else
        newNames2<-paste0(result$hypothesis$IV$cases)
      newVariables2<-cbind(result$participant[1:sum(use)],newVariables2,result$iv2[1:sum(use)])
      names(newVariables2)<-c("IDwide",newNames2,paste0(result$hypothesis$IV2$name,"w"))
      
      if (!both && braw.def$design$sDataFormat=="wide") newVariables<-newVariables2
      if (both) newVariables<-list(long=newVariables,wide=newVariables2)
    } 
    if (result$design$sIV1Use=="Between" && result$design$sIV2Use=="Within") {
      m2<-levels(result$iv2)
      for (i2 in 1:length(m2)) {
        use<-result$iv2==m2[i2]
        newVariables2<-cbind(newVariables2,result$dv[use])
      }
      newVariables2<-newVariables2[order(newVariables2[,1]),]
      newVariables2<-data.frame(newVariables2)
      if (braw.env$fullWithinNames)
        newNames2<-paste0(result$hypothesis$DV$name,"|",result$hypothesis$IV2$name,"=",result$hypothesis$IV2$cases)
      else
        newNames2<-paste0(result$hypothesis$IV2$cases)
      newVariables2<-cbind(result$participant[1:sum(use)],newVariables2,result$iv[1:sum(use)])
      names(newVariables2)<-c("ID",newNames2,paste0(result$hypothesis$IV$name,"w"))
      
      if (!both && braw.def$design$sDataFormat=="wide") newVariables<-newVariables2
      if (both) newVariables<-list(long=newVariables,wide=newVariables2)
    } 
    if (result$design$sIV1Use=="Within" && result$design$sIV2Use=="Within") {
      newNames2<-c()
      m1<-levels(result$iv)
      m2<-levels(result$iv2)
      for (i1 in 1:length(m1)) 
        for (i2 in 1:length(m2)) {
          use<-result$iv==m1[i1] & result$iv2==m2[i2]
          newVariables2<-cbind(newVariables2,result$dv[use])
          if (braw.env$fullWithinNames)
            newNames2<-c(newNames2,paste0(result$hypothesis$DV$name,"|",result$hypothesis$IV$name,"=",result$hypothesis$IV$cases[i1],
                                          "|",result$hypothesis$IV2$name,"=",result$hypothesis$IV2$cases[i2]))
          else
            newNames2<-c(newNames2,paste0(result$hypothesis$IV$cases[i1],"&",result$hypothesis$IV2$cases[i2]))
        }
      newVariables2<-newVariables2[order(newVariables2[,1]),]
      newVariables2<-data.frame(newVariables2)
      newVariables2<-cbind(result$participant[1:sum(use)],newVariables2)
      names(newVariables2)<-c("ID",newNames2)
      
      if (!both && braw.def$design$sDataFormat=="wide") newVariables<-newVariables2
      if (both) newVariables<-list(long=newVariables,wide=newVariables2)
    }
  }
  
  return(newVariables)
}
