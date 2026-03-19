get_Stheta<-function(L,B,phi,psy) {
  
  nan_action<-"complete.obs" # "complete.obs"
  
  P<-nrow(psy)
  Q<-nrow(phi)
  
  term1<-eye(nrow(B))-B
  term2<-L%*%phi%*%t(L)+psy
  term3<-t(inv(term1))
  
  SYYtheta<-(solve((term1),(term2)))%*%term3
  SYYtheta<-inv(term1)%*%term2%*%term3
  SXXtheta<-phi
  SYXtheta<-phi%*%t(L)%*%term3
  Stheta<-rbind(cbind(SYYtheta, zeros(P,Q)),cbind(SYXtheta, SXXtheta))
  Stheta<-tril(Stheta)+t(tril(Stheta,-1))
  return(Stheta)
}

Stheta2Cor<-function(Stheta) {
  
  variances<-replicate(nrow(Stheta),diag(Stheta))
  Stheta<-Stheta/sqrt(t(variances)*variances)
  # Stheta<-tril(Stheta,-1)
  return(Stheta)  
}

path2Sample<-function(pathmodel,np=100000) {
  endo_names<-names(pathmodel$links)
  exo_names<-setdiff(pathmodel$variables,endo_names)
  
  P<-length(endo_names)
  Q<-length(exo_names)

    data<-c()
  for (i in 1:Q) {
    data<-cbind(data,rnorm(np,0,1))
  }
  colnames(data)<-exo_names
  
  rp2<-rp^2
  for (i in 1:P) {
    cn<-colnames(data)
    links<-pathmodel$links[[i]]
    predictors<-names(links)
    # print(c(endo_names[i],predictors))
    corrs<-unlist(links)
    if (length(predictors)>1)
      slice<-rowSums(data[,predictors]*corrs)+rnorm(np,0,sqrt(1-sum(corrs^2)))
    else 
      slice<-data[,predictors]*corrs+rnorm(np,0,sqrt(1-corrs^2))
    data<-cbind(data,slice)
    colnames(data)<-c(cn,endo_names[i])
  }
  return(data)
}

path2ES_table<-function(pathmodel) {
  endo_names<-names(pathmodel$links)
  exo_names<-setdiff(pathmodel$variables,endo_names)
  
  P<-length(endo_names)
  Q<-length(exo_names)
  
  endogenous<-1:P
  Bdesign<-zeros(P,P); rownames(Bdesign)<-endo_names; colnames(Bdesign)<-endo_names
  
  if (Q>0)  {
    exogenous<-P+(1:Q) 
    Ldesign<-zeros(P,Q); rownames(Ldesign)<-endo_names; colnames(Ldesign)<-exo_names
  } else {
    exogenous<-c()
    Ldesign<-c()
  }
  
  for (i in 1:P) {
    links<-pathmodel$links[[i]]
    row<-endo_names[i]
    cols<-names(links)
    use<-is.element(cols,endo_names)
    if (any(use))
      Bdesign[row,cols[use]]<-unlist(links[use])
    if (any(!use))
      Ldesign[row,cols[!use]]<-unlist(links[!use])
  }
  
  return(cbind(Ldesign,Bdesign))
}

path2Stheta<-function(pathmodel) {
  endo_names<-names(pathmodel$links)
  exo_names<-setdiff(pathmodel$variables,endo_names)
  
  P<-length(endo_names)
  Q<-length(exo_names)
  
  endogenous<-1:P
  Bdesign<-zeros(P,P); rownames(Bdesign)<-endo_names; colnames(Bdesign)<-endo_names
  
  if (Q>0)  {
    exogenous<-P+(1:Q) 
    Ldesign<-zeros(P,Q); rownames(Ldesign)<-endo_names; colnames(Ldesign)<-exo_names
  } else {
    exogenous<-c()
    Ldesign<-c()
  }
  
  for (i in 1:P) {
    links<-pathmodel$links[[i]]
    row<-endo_names[i]
    cols<-names(links)
    use<-is.element(cols,endo_names)
    if (any(use))
      Bdesign[row,cols[use]]<-unlist(links[use])
    if (any(!use))
      Ldesign[row,cols[!use]]<-unlist(links[!use])
  }
  
  phi<-diag(1,Q,Q)
  psy<-(1-rowSums(cbind(Bdesign,Ldesign)^2))
  psy<-diag(psy,P,P)
  
  Stheta<-get_Stheta(Ldesign,Bdesign,phi,psy)
  
  use<-c(P+(1:Q),1:P)
  Stheta<-Stheta[use,]
  Stheta<-Stheta[,use]
  colnames(Stheta)<-c(exo_names,endo_names)
  rownames(Stheta)<-c(exo_names,endo_names)
  return(Stheta)
}

pathData2Stheta<-function(data,digits=NA) {
  
  Stheta1<-cor(data)
  # Stheta1<-Stheta1*lower.tri(Stheta1)
  if (!is.na(digits)) Stheta1<-round(Stheta1,digits=digits)
  return(Stheta1)
}


