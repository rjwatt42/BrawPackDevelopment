########################

# network structure
nrows=16
ncols=8
rangeLink=4
probLink=0.5
strengthLink=0.2
randomLink=strengthLink*0.1
randomSign<-TRUE

# graph
ncount<-100
nbins=500
logXscale<-TRUE
logYscale<-FALSE

pathmodel<-list(variables=c(),
                stages=NULL,
                links=NULL)

pathmodel$variables<-paste0("v",1:(nrows*ncols))
for (i in 1:nrows) {
  thisStage<-pathmodel$variables[(i-1)*ncols+1:ncols]
  pathmodel$stages<-c(pathmodel$stages,list(thisStage))
}

# rs_show=0
Stotal<-c()
if (logXscale) {
  Shist<-0
  Sbins<-seq(-10,0,length.out=nbins)
} else {
  Shist<-0
  Sbins<-seq(0,0.1,length.out=nbins)
}
for (ni in 1:ncount) {
#   
  # make a network
  fullLinks=matrix(0,nrows*ncols,nrows*ncols)
  for (j in 1:nrows) {
    for (i in 1:ncols) {
      if (j>1) {
        use<-i+(-rangeLink:rangeLink)
        use<-use[use>=1 & use<=ncols]
        links=runif(length(use))<=probLink
        dest<-i+(j-1)*ncols
        sources<-use[links]+(j-2)*ncols
        effects<-strengthLink+rnorm(length(sources))*randomLink
        if (randomSign) effects<-effects*sign(runif(length(sources),-1,1))
        fullLinks[dest,sources]<-effects
      }
    }
  }

  links<-NULL
  for (j in 1:(nrows*ncols)) {
    use<-which(fullLinks[j,]!=0)
    sources<-pathmodel$variables[use]
    if (!isempty(sources)) {
      n<-names(links)
      theList<-fullLinks[j,use]
      names(theList)<-sources
      links<-c(links,list(theList))
      names(links)<-c(n,pathmodel$variables[j])
    }
  }
  pathmodel$links<-links
  
  Stheta<-path2Stheta(pathmodel)
  Stheta<-Stheta2Cor(Stheta)
  
  # pathmodel$ES_table<-path2ES_table(pathmodel)
  # plotSEMModel(pathmodel)
  
  Stheta<-Stheta+diag(NA,ncols*nrows,ncols*nrows)
  
  if (logXscale) {
    use<-atanh(abs(Stheta))>10^min(Sbins) & !is.na(Stheta)
    Shist<-Shist+hist(log10(atanh(abs(Stheta[use]))),breaks=Sbins,plot=FALSE)$counts
  } else {
    use<-atanh(abs(Stheta))<max(Sbins) & !is.na(Stheta)
    Shist<-Shist+hist((atanh(abs(Stheta[use]))),breaks=Sbins,plot=FALSE)$counts
  }
  # Stotal<-c(Stotal,Stheta)
}
# Stotal<-abs(Stotal)
# Stotal<-Stotal[Stotal!=0 & !is.na(Stotal)]
# hist(log10(atanh(Stotal)),breaks = seq(log10(min(atanh(Stotal))),0,length.out=100))

x<-Sbins[1:(length(Shist))]
xlabel<-"z[s]"
if (logXscale) xlabel<-"log(z[s])"
y<-Shist/sum(Shist*diff(Sbins[1:2]))
ylabel<-"PDF"
if (logYscale) {
  y[y==0]<-NA
  y<-log10(y)
  ylabel<-'log(PDF)'
}

dataGraph(data.frame(x=x,y=y),
          xlabel=xlabel,ylabel=ylabel,poly=TRUE)

