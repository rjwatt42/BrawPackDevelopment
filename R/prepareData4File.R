
#' @export
prepareData4File<-function(both=FALSE) {
  
newVariables2<-newVariables<-c()
if (is.null(braw.res$result$hypothesis$IV2)) {
  useOrder<-order(braw.res$result$participant)
  newVariables<-data.frame(braw.res$result$participant[useOrder],
                           braw.res$result$dv[useOrder],braw.res$result$iv[useOrder],
                           braw.res$result$dv+NA)
  names(newVariables)<-c("ID",braw.res$result$hypothesis$DV$name,braw.res$result$hypothesis$IV$name,"-")
  if (braw.res$result$design$sIV1Use=="Within") {
    newVariables2<-c()
    m<-levels(braw.res$result$iv)
    for (i in 1:length(m)) {
      use<-braw.res$result$iv==m[i]
      newVariables2<-cbind(newVariables2,braw.res$result$dv[use])
    }
    if (braw.env$fullWithinNames)
      newNames2<-paste0(braw.res$result$hypothesis$DV$name,"|",braw.res$result$hypothesis$IV$name,"=",braw.res$result$hypothesis$IV$cases)
    else
      newNames2<-paste0(braw.res$result$hypothesis$IV$cases)
    newVariables2<-cbind(braw.res$result$participant[1:sum(use)],newVariables2)
    newNames2<-c("ID",newNames2)
    
    newVariables2<-newVariables2[order(newVariables2[,1]),]
    newVariables2<-data.frame(newVariables2)
    names(newVariables2)<-newNames2
    if (!both) newVariables<-newVariables2
  }
} else {
  newVariables<-data.frame(braw.res$result$participant,braw.res$result$dv,braw.res$result$iv,braw.res$result$iv2)
  newVariables<-newVariables[order(newVariables[,1]),]
  names(newVariables)<-c("ID",braw.res$result$hypothesis$DV$name,braw.res$result$hypothesis$IV$name,braw.res$result$hypothesis$IV2$name)
  
  if (braw.res$result$design$sIV1Use=="Within" && braw.res$result$design$sIV2Use=="Between") {
    newVariables2<-c()
    m1<-levels(braw.res$result$iv)
    for (i1 in 1:length(m1)) {
      use<-braw.res$result$iv==m1[i1]
      newVariables2<-cbind(newVariables2,braw.res$result$dv[use])
    }
    newVariables2<-data.frame(newVariables2)
    if (braw.env$fullWithinNames)
      newNames2<-paste0(braw.res$result$hypothesis$DV$name,"|",braw.res$result$hypothesis$IV$name,"=",braw.res$result$hypothesis$IV$cases)
    else
      newNames2<-paste0(braw.res$result$hypothesis$IV$cases)
    newVariables2<-cbind(braw.res$result$participant[1:sum(use)],newVariables2,braw.res$result$iv2[1:sum(use)])
    names(newVariables2)<-c("IDwide",newNames2,paste0(braw.res$result$hypothesis$IV2$name,"w"))
  } 
  if (braw.res$result$design$sIV1Use=="Between" && braw.res$result$design$sIV2Use=="Within") {
    m2<-levels(braw.res$result$iv2)
    for (i2 in 1:length(m2)) {
      use<-braw.res$result$iv2==m2[i2]
      newVariables2<-cbind(newVariables2,braw.res$result$dv[use])
    }
    newVariables2<-newVariables2[order(newVariables2[,1]),]
    newVariables2<-data.frame(newVariables2)
    if (braw.env$fullWithinNames)
      newNames2<-paste0(braw.res$result$hypothesis$DV$name,"|",braw.res$result$hypothesis$IV2$name,"=",braw.res$result$hypothesis$IV2$cases)
    else
      newNames2<-paste0(braw.res$result$hypothesis$IV2$cases)
    newVariables2<-cbind(braw.res$result$participant[1:sum(use)],newVariables2,braw.res$result$iv[1:sum(use)])
    names(newVariables2)<-c("ID",newNames2,paste0(braw.res$result$hypothesis$IV$name,"w"))
  } 
  if (braw.res$result$design$sIV1Use=="Within" && braw.res$result$design$sIV2Use=="Within") {
    newNames2<-c()
    m1<-levels(braw.res$result$iv)
    m2<-levels(braw.res$result$iv2)
    for (i1 in 1:length(m1)) 
      for (i2 in 1:length(m2)) {
        use<-braw.res$result$iv==m1[i1] & braw.res$result$iv2==m2[i2]
        newVariables2<-cbind(newVariables2,braw.res$result$dv[use])
        if (braw.env$fullWithinNames)
          newNames2<-c(newNames2,paste0(braw.res$result$hypothesis$DV$name,"|",braw.res$result$hypothesis$IV$name,"=",braw.res$result$hypothesis$IV$cases[i1],
                                        "|",braw.res$result$hypothesis$IV2$name,"=",braw.res$result$hypothesis$IV2$cases[i2]))
        else
          newNames2<-c(newNames2,paste0(braw.res$result$hypothesis$IV$cases[i1],"&",braw.res$result$hypothesis$IV2$cases[i2]))
      }
    newVariables2<-newVariables2[order(newVariables2[,1]),]
    newVariables2<-data.frame(newVariables2)
    newVariables2<-cbind(braw.res$result$participant[1:sum(use)],newVariables2)
    names(newVariables2)<-c("ID",newNames2)
  }
    if (!both) newVariables<-newVariables2
  }
  
  if (both) return(list(long=newVariables,wide=newVariables2))
  else      return(newVariables)
}
