#######################################
svgBox(height=350,aspect=1.5,fontScale=1.2,margin=10)
setBrawEnv("graphicsType","HTML")
setBrawEnv("RZ","z")

zk<-atanh(0.3)
setWorld(TRUE,"Exp","z",zk,pRplus=1)

result<-doSingle()
result$rpIV<-0.35
result$rIV<-0.45
result$pIV<-rn2p(result$rIV,braw.def$design$sN)

showHTML(showInference(result,showType="rp",orientation="horz"))

setBrawEnv("constantArea",TRUE)
rp_use<-seq(0.1,1,length.out=4)
for (i in 1:length(rp_use)) {
  result$hypothesis$effect$world$PDFk<-rp_use[i]
  showHTML(showInference(result,showType="rs",orientation="horz"))
}
setBrawEnv("constantArea",FALSE)

#################################

rs<-braw.res$result$rIV

rvals<-seq(-0.995,0.995,length.out=1001)

rp<-seq(0.1,0.9,length.out=101)
lk<-rp*0
  for (i in 1:length(rp)) {
    world$PDFk<-rp[i]
    world$pRplus<-1
    rd<-fullSamplingDist(rvals,world,design)
    lk[i]<-sum(log(approx(rvals,rd,rs)$y))
  }

use<-order(rp)
rp<-rp[use]
lk<-lk[use]
mle[m]<-rp[which.max(lk)]
if (m==nreps)
  showHTML(dataGraph(data.frame(x=rp,y=lk),
                     title=paste0("MLE = ",format(mle[m],digits=4),"  |",format(zk,digits=4),"|"),
                     xlabel="lambda",ylabel='log likelihood',fill=NA))


######################################

result$hypothesis$effect$world<-makeWorld(TRUE,"Single","z",0.35,pRplus=1)
showHTML(showInference(result,showType="rs",orientation="horz"))
result$hypothesis$effect$world<-makeWorld(TRUE,"Exp","z",zk,pRplus=1)
showHTML(showInference(result,showType="rs",orientation="horz"))

######################################
zk<-atanh(0.3)
setWorld(TRUE,"Exp","z",zk,pRplus=1)

nreps=100
mle<-rep(0,nreps)
for (m in 1:nreps) {
  results<-doMultiple(100,NULL)
  if (m==nreps)
    showHTML(showMultiple(showType="rs",orientation="horz"))
  
  rs<-results$result$rIV
  
  
  world<-results$result$hypothesis$effect$world
  design<-results$result$design
  
  
  rvals<-seq(-0.995,0.995,length.out=1001)
  
  lk<-rp<-c()
  rp_use<-seq(0.1,0.9,length.out=11)
  lk_use<-rp_use*0
  for (j in 1:10) {
    for (i in 1:length(rp_use)) {
      world$PDFk<-rp_use[i]
      world$pRplus<-1
      rd<-fullSamplingDist(rvals,world,design)
      lk_use[i]<-sum(log(approx(rvals,rd,rs)$y))
    }
    rp<-c(rp,rp_use)
    lk<-c(lk,lk_use)
    use<-which.max(lk_use)
    rp_use<-seq(rp_use[use-2],rp_use[use+2],length.out=11)
  }
  
  use<-order(rp)
  rp<-rp[use]
  lk<-lk[use]
  mle[m]<-rp[which.max(lk)]
  if (m==nreps)
    showHTML(dataGraph(data.frame(x=rp,y=lk),
                     title=paste0("MLE = ",format(mle[m],digits=4),"  |",format(zk,digits=4),"|"),
                     xlabel="lambda",ylabel='log likelihood',fill=NA))
  
}
print(paste(mean(mle),'+/-',sd(mle)))

#################################

svgBox(height=350,aspect=1.5,fontScale=1.2,margin=10)
setBrawEnv("graphicsType","HTML")
doShortHand()

zk<-atanh(0.3)
setWorld(TRUE,"Exp","z",zk,pRplus=1)

results<-doMultiple(000000)
# rs<-results$result$rIV
# rp<-results$result$rpIV
# g<-startPlot(c(-1,1),c(-1,1),
#              xlabel="r[s]",xticks=seq(-1,1,0.25),
#              ylabel="r[p]",yticks=seq(-1,1,0.25))
# g<-addG(g,dataContour(data.frame(x=rs,y=rp),nbins=101,fill="red",linewidth=0.1))
# g<-addG(g,dataLine(data.frame(x=c(-1,1),y=c(-1,1))))
# showHTML(g)
# 
# use<-abs(rs-0.35)<0.05
# hist(rp[use],51)
# 

zs<-atanh(results$result$rIV)
zp<-atanh(results$result$rpIV)

maxz<-1
use<-(abs(zs)<maxz) & (abs(zp)<maxz)
zs<-zs[use]
zp<-zp[use]

nbins<-49
ybreaks<-xbreaks<-seq(-1,1.001,length.out=nbins+1)

z<-matrix(0,nbins,nbins)
for (ix in 1:nbins)
  for (iy in 1:nbins) {
    z[iy,ix]<-sum( (zs>=xbreaks[ix] & zs<xbreaks[ix+1]) &
                     (zp>=ybreaks[iy] & zp<ybreaks[iy+1])  )
  }
zmax<-rep(0,nbins)
for (ix in 1:nbins) {
  zmax[ix]<-ybreaks[which.max(z[,ix])]
}
y<-x<-(xbreaks[1:nbins]+xbreaks[2:(nbins+1)])/2
g<-startPlot(c(-1,1),c(-1,1),
             xlabel="z[s]",xticks=seq(-1,1,0.25),
             ylabel="z[p]",yticks=seq(-1,1,0.25))
g<-addG(g,dataContour(list(x=x,y=y,z=z),
                      fill="red",linewidth=0.1)
        )
g<-addG(g,dataLine(data.frame(x=x,y=zmax),colour="red",linewidth=0.3))
g<-addG(g,dataLine(data.frame(x=c(-1,1),y=c(-1,1))))
showHTML(g)

use<-abs(zs-0.35)<0.05
hist(zp[use],51)

###############################
n<-50
zk<-0.3
nbins<-501
zs<-zp<-seq(-1,1,length.out=nbins)
z<-matrix(0,length(zs),length(zp))

for (ix in 1:length(zs)) {
  z[,ix]<-exp(-(zs[ix]-zp)^2*(n-3)/2)*exp(-abs(zp/zk))
}

# (3*zk*zs - n*zk*zs + 1)/(3 - n)/zk
# (3*zs - n*zs + 1/zk)/(3 - n)
# (3*zs - n*zs)/(3-n) + 1/zk/(3 - n)
# zs - 1/zk/(n-3)

# (3*zk*zs + 3*zk*zs1 - n*zk*zs - n1*zk*zs1 + 1)/(n*zk - 6*zk + n1*zk)
# (3*zk*zs + 3*zk*zs1 - n*zk*zs - n1*zk*zs1)/(n*zk - 6*zk + n1*zk) + 1/(n*zk - 6*zk + n1*zk)
# (3*zs + 3*zs1 - n*zs - n1*zs1)/(n - 6 + n1) + 1/(n - 6 + n1)/zk
# (zs*(3-n) + zs1*(3-n1))/(n - 6 + n1) + 1/(n - 6 + n1)/zk


zmax<-rep(0,nbins)
for (ix in 1:nbins) {
  zmax[ix]<-zp[which.max(z[,ix])]
}

g<-startPlot(c(-1,1),c(-1,1),
             xlabel="z[s]",xticks=seq(-1,1,0.25),
             ylabel="z[p]",yticks=seq(-1,1,0.25))
g<-addG(g,dataContour(list(x=zs,y=zp,z=z),
                      fill="red",linewidth=0.1)
)

g<-addG(g,dataLine(data.frame(x=zs,y=zmax),colour="red",linewidth=0.5))
g<-addG(g,dataLine(data.frame(x=c(-1,1),y=c(-1,1))))
showHTML(g)
