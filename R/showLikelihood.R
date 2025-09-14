

showLikelihood<-function(rs=braw.res$result$rIV,n=NULL,showType="mean(R+)",
                         world=braw.def$hypothesis$effect$world,design=braw.def$design,
                         fontsize=1,markRs=NULL,
                         plotArea=c(0,0,1,1),g=NULL,new=TRUE) {
  
  switch(world$PDF,
         "Uniform"={PDF<-SingleSamplingPDF},
         "Single"={PDF<-SingleSamplingPDF},
         "Gauss"={PDF<-GaussSamplingPDF},
         "Exp"={PDF<-ExpSamplingPDF},
         "Gamma"={PDF<-GammaSamplingPDF},
         "GenExp"={PDF<-GenExpSamplingPDF}
  )
  if (is.null(n)) n<-design$sN
  if (world$RZ=="z") rs<-atanh(rs)
  if (length(n)<length(rs)) n<-rep(n,length(rs))
  
  switch(showType,
    "mean(R+)"={
      range<-seq(0,0.6,length.out=201)
      xlabel<-showType
      dens<-0
      for (i in 1:length(rs)) {
        nextDens<-PDF(rs[i],range,1/sqrt(n[i]-3))$pdf
        if (any(nextDens>0))
          dens<-dens+log(nextDens)
      }
    }
  )
  dens[is.infinite(dens)]<-NA
  dens<-exp(dens)
  dens[is.na(dens)]<-0
  use<-which.max(dens)
  
  xlim<-c(min(range),max(range))
  ylim<-c(min(dens),max(dens))+c(-1,1)*(max(dens)-min(dens))*0.2
  ylim[1]<-0
  
  braw.env$plotArea<-plotArea
  
  if (new)
  g<-startPlot(xlim=xlim,ylim=ylim,top=TRUE,
               xlabel=makeLabel(xlabel),ylabel=makeLabel("S"),
               xticks=makeTicks(seq(0,0.6,0.1)),yticks=makeTicks(),
               fontScale=fontsize,g=g
               )
  
  n<-length(range)
  col<-darken(braw.env$plotColours$sampleC,off=0)
  g<-addG(g,dataPath(data.frame(x=range,y=dens),colour=col,linewidth=1.25))
  g<-addG(g,dataPath(data.frame(x=c(0,0)+range[use],y=c(0,dens[use])),linetype="dotted"))
  g<-addG(g,dataPoint(data.frame(x=range[use],y=dens[use])))
  g<-addG(g,dataText(data.frame(x=range[use],y=dens[use]+diff(ylim)/30),
                     paste0("MLE = ",brawFormat(range[use])),
                     fontface="bold",size=0.65))

  if (!is.null(markRs)) {
      h<-approx(range,dens,markRs)$y
      g<-addG(g,dataPoint(data.frame(x=markRs,y=h),fill="#CCCCCC"))
  }
  
  if (braw.env$graphicsType=="HTML" && braw.env$autoShow) {
    showHTML(g)
    return(invisible(g))
  }
  if (braw.env$graphicsType=="ggplot" && braw.env$autoPrint) {
    print(g)
    return(invisible(g))
  }
  return(g)  
  
}