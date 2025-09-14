

showLikelihood<-function(rs=braw.res$result$rIV,showType="mean(R+)",
                         world=braw.def$hypothesis$effect$world,design=braw.def$design) {
  
  switch(world$PDF,
         "Uniform"={PDF<-SingleSamplingPDF},
         "Single"={PDF<-SingleSamplingPDF},
         "Gauss"={PDF<-GaussSamplingPDF},
         "Exp"={PDF<-ExpSamplingPDF},
         "Gamma"={PDF<-GammaSamplingPDF},
         "GenExp"={PDF<-GenExpSamplingPDF}
  )
  if (world$RZ=="z") rs<-atanh(rs)
  switch(showType,
    "mean(R+)"={
      range<-seq(0,0.6,0.01)
      xlabel<-showType
      dens<-0
      for (i in 1:length(rs))
        dens<-dens+PDF(rs[i],range,1/sqrt(design$sN-3))$pdf
    }
  )
  use<-which.max(dens)
  dens<-exp(dens)
  
  xlim<-c(min(range),max(range))
  ylim<-c(min(dens),max(dens))+c(-1,1)*(max(dens)-min(dens))*0.2
  
  g<-startPlot(xlim=xlim,ylim=ylim,top=TRUE,
               xlabel=makeLabel(xlabel),ylabel=makeLabel("S"),
               xticks=makeTicks(seq(0,0.6,0.1)),yticks=makeTicks()
               )
  
  n<-length(range)
  col<-darken(braw.env$plotColours$sampleC,off=0)
  g<-addG(g,dataPath(data.frame(x=range,y=dens),colour=col,linewidth=1.25))
  g<-addG(g,dataPath(data.frame(x=c(0,0)+range[use],y=c(0,dens[use])),linetype="dotted"))
  g<-addG(g,dataPoint(data.frame(x=range[use],y=dens[use])))
  g<-addG(g,dataText(data.frame(x=range[use],y=dens[use]+diff(ylim)/30),
                     paste0("MLE = ",brawFormat(range[use])),
                     fontface="bold",size=0.65))

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