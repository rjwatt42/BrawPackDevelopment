
######################

pathmodel<-list(path=c(),links=c())
pathmodel$variables<-c("a","b","c","d","e","f","g","h","i")
pathmodel$links<-list(d=c("a","b"),
                      e=c("b"),
                      f=c("a","c"),
                      g=c("d"),
                      h=c("e","f"),
                      i=c("d","e")
                      )
pathmodel$variables<-c("a","b","c","d","e","g")
pathmodel$links<-list(d=list(a=0.3,b=0.3),
                      e=list(c=0.3),
                      g=list(d=0.3,e=0.3)
                      )
pathmodel$variables<-c(
  "a","b","c","d",
  "e","f","g","h",
  "i","j","k","l",
  "m","n","o","p",
  "q","r","s","t",
  "u","v","w","x"
)
pathmodel$stages<-list(
  c("u","v","w","x"),
  c("q","r","s","t"),
  c("m","n","o","p"),
  c("i","j","k","l"),
  c("e","f","g","h"),
  c("a","b","c","d")
)

rp=0.3
pathmodel$links<-list(
  t=list(x=rp),
  s=list(v=rp,w=rp),
  r=list(u=rp,v=rp),
  q=list(u=rp),
  p=list(s=rp),
  o=list(r=rp,s=rp),
  n=list(s=rp),
  m=list(q=rp),
  l=list(p=rp),
  k=list(p=rp),
  j=list(m=rp,n=rp,o=rp),
  i=list(n=rp),
  h=list(k=rp),
  g=list(k=rp),
  f=list(i=rp,j=rp,k=rp),
  e=list(i=rp),
  d=list(g=rp),
  c=list(f=rp,g=rp,h=rp),
  b=list(e=rp,f=rp),
  a=list(e=rp)
)

Stheta<-path2Stheta(pathmodel)
pathmodel$ES_table<-path2ES_table(pathmodel)
plotSEMModel(pathmodel)

pathdata<-path2Sample(pathmodel,np=10000000)
Stheta1<-pathData2Stheta(pathdata,digits=2)


######################

path<-paste(
  "t~x",
  "s~v+w",
  "r~u+v",
  "q~u",
  "p~s",
  "o~r+s",
  "n~s",
  "m~q",
  sep="\n")
semResult <- lavaan::sem(path, pathdata)



