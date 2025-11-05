

setHypothesis(IV2=makeVariable("IV2"),effect=makeEffect(0.2,0.2,-0.25))
setDesign(sN=50)
setEvidence(AnalysisTerms = c(TRUE,TRUE,FALSE,TRUE),doSEM=TRUE)

data<-doAnalysis()

model_data<-list(varnames=c(data$hypothesis$IV$name,
                            data$hypothesis$IV2$name,
                            data$hypothesis$DV$name
                            ),
           varcat=c(data$hypothesis$IV$type=="Categorical",
                    data$hypothesis$IV2$type=="Categorical",
                    data$hypothesis$DV$type=="Categorical"
                    ),
           data=cbind(data$iv,data$iv2,data$dv)
           )
colnames(model_data$data)<-model_data$varnames

pathmodel1<-makeSEMPath(data=model_data,stages="sequence",depth=2)
z1<-fit_sem_model(pathmodel1,model_data)

data$rIV<-z1$ES_table["DV","IV"]
data$rIV2<-z1$ES_table["DV","IV2"]
data$rIVIV2<-z1$ES_table["IV2","IV"]

data$r$direct<-cbind(data$r$direct,NA,data$rIVIV2)
data$p$direct<-cbind(data$p$direct,NA,rn2p(data$rIVIV2,data$nval))

dt<-data
dt$hypothesis$DV<-data$hypothesis$IV2
dt$dv<-data$iv2
dt$hypothesis$IV2<-data$hypothesis$DV
dt$iv2<-data$dv
dt$evidence$AnalysisTerms<-c(TRUE,TRUE,FALSE,FALSE)
dt<-doAnalysis(dt)

data$r$unique<-cbind(data$r$unique,NA,dt$r$unique[1,1])
data$p$unique<-cbind(data$p$unique,NA,rn2p(dt$r$unique[1,1],data$nval))

dt$hypothesis$IV2<-NULL
dt$iv2<-NULL
dt$evidence$AnalysisTerms<-c(TRUE,FALSE,FALSE,FALSE)
dt<-doAnalysis(dt)

data$r$total<-cbind(data$r$total,NA,dt$rIV)
data$p$total<-cbind(data$p$total,NA,rn2p(dt$rIV,dt$nval))

print(showInference(data,effectType = "direct"))

# return()
# 
# print(plotSEMModel(z1))
# pathmodel2<-makeSEMPath(data=model_data,stages="sequence",depth=1)
# z2<-fit_sem_model(pathmodel2,model_data)
# print(plotSEMModel(z2))
