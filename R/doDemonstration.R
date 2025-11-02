
#' @export
stepDM<-function(doing) gsub('[A-Za-z]*([0-9]*)[A-Da-d]*','\\1',doing)
#' @export
partDM<-function(doing) toupper(gsub('[A-Za-z]*[0-9]*([A-Da-b]*)','\\1',doing))
#' @export
singleDM<-function(doing) !grepl('m',tolower(gsub('[A-Za-z]*[0-9]*[A-Da-b]*([crm]*)','\\1',doing)),fixed=TRUE)

makePanel<-function(g,r=NULL) {
  paste0('<div style="display:inline-block;margin-bottom:10px;margin-top:10px;">',
                '<table>',
                '<tr><td>', g, '</td></tr>',
                '<tr><td>', r, '</td></tr>',
                # '<tr style="height:10px;"></tr>',
                # '<tr><td>', moreHTML(reportWorldDesign(),"see Plan","p1"), '</td></tr>',
                '</table>',
                '</div>'
  )
}

#' @export
doDemonstration<-function(doingDemo="Step1A",showOutput=TRUE,showJamovi=TRUE,
                          variables=list(IV="Perfectionism",IV2=NULL,DV="ExamGrade"),
                          rIV=NULL,rIV2=NULL,rIVIV2=NULL,rIVIV2DV=NULL,
                          world="Binary",pRplus=0.5,
                          sN=NULL,sMethod=NULL,sBudget=320,sSplits=16,sCheating="grow",
                          sReplicationPower=0.9,sReplicationSigOriginal=TRUE,
                          group="a",
                          nreps=200
                          ) {
  
  setHTML()
  rootInv<-stepDM(doingDemo)
  partInv<-partDM(doingDemo)
  single<-singleDM(doingDemo)

  if (is.null(rIV)) rIV<-0.3
  
  if (is.null(sN)) {
    if (paste0(rootInv,partInv)=="1C") sN<-500
    if (paste0(rootInv)=="2") sN<-100
    if (paste0(rootInv)=="3") sN<-100
    if (paste0(rootInv)=="4") sN<-100
    if (paste0(rootInv)=="5") sN<-1500
    if (paste0(rootInv)=="6") sN<-1500
    if (is.null(sN)) sN<-42
  }
  if (is.null(sMethod)) {
    if (is.element(paste0(rootInv,partInv),c("1B"))) sMethod<-"Convenience"
    if (is.null(sMethod)) sMethod<-"Random"
  }
  
  hideReport<-FALSE
  switch(rootInv,
         "1"={ # making samples and analysing them in Jamovi
           switch(partInv,
                  "A"={showNow<-"Basic"},
                  "B"={showNow<-"Sample"},
                  "C"={showNow<-"Basic"}
           )
         },
         "2"={ # 3 basic tests with Interval DV
           variables$DV<-"ExamGrade"
           switch(partInv,
                  "A"={variables$IV<-"Perfectionism"},
                  "B"={variables$IV<-"Smoker?"},
                  "C"={variables$IV<-"BirthOrder"},
                  {}
           )
           showNow<-"Basic"
         },
         "3"={ # 2 basic tests with Categorical DV
           variables$DV<-"TrialOutcome"
           switch(partInv,
                  "A"={variables$IV<-"Treatment?"},
                  "B"={variables$IV<-"Sessions"},
                  {}
           )
           showNow<-"Basic"
         },
         "4"={ # Revision of all basic tests with 2 variables
           DVs<-c("ExamGrade","ExamPass?","TrialOutcome","Happiness")
           variables$DV<-DVs[ceiling(runif(1)*length(DVs))]
           
           if (is.element(variables$DV,c("ExamGrade","ExamPass?")))
                 IVs<-c("Perfectionism","Musician?","RiskTaking","RiskTaker?")
           else  IVs<-c("Session","Treatment?","Smoker?","Diligence?")
           variables$IV<-IVs[ceiling(runif(1)*length(IVs))]
           
           showNow<-"Sample"
           hideReport<-TRUE
           single<-TRUE
         },
         "5"={ # Main effects in multiple IVs
           variables$DV<-"ExamGrade"
           switch(partInv,
                  "A"={variables$IV<-"Perfectionism";variables$IV2<-"HoursSleep"},
                  "B"={variables$IV<-"Smoker?";variables$IV2<-"Anxiety"},
                  "C"={variables$IV<-"BirthOrder";variables$IV2<-"Musician?"},
                  "D"={
                    IVs<-c("IQ","Musician?","Anxiety","RiskTaker?","SelfConfidence","Diligence","Coffee?")
                    variables$IV<-IVs[ceiling(runif(1)*length(IVs))]
                    IVs<-IVs[IVs!=variables$IV]
                    variables$IV2<-IVs[ceiling(runif(1)*length(IVs))]
                  }
           )
           if (is.null(rIV2)) rIV2<- -0.3
           rIVIV2<- 0
           rIVIV2DV<-0
           setEvidence(AnalysisTerms=c(TRUE,TRUE,FALSE))
           showNow<-"Basic"
         },
         "6"={ # Interactions
           variables$DV<-"ExamGrade"
           switch(partInv,
                  "A"={variables$IV<-"Coffee?";variables$IV2<-"Musician?"},
                  "B"={variables$IV<-"Smoker?";variables$IV2<-"Anxiety"},
                  "C"={variables$IV<-"Perfectionism";variables$IV2<-"HoursSleep"},
                  "D"={
                    IVs<-c("IQ","Musician?","Anxiety","RiskTaker?","SelfConfidence","Diligence","Coffee?")
                    variables$IV<-IVs[ceiling(runif(1)*length(IVs))]
                    IVs<-IVs[IVs!=variables$IV]
                    variables$IV2<-IVs[ceiling(runif(1)*length(IVs))]
                  }
           )
           if (is.null(rIV2)) rIV2<- -0.3
           if (is.null(rIVIV2DV)) rIVIV2DV<-0.3
           rIVIV2<- 0
           setEvidence(AnalysisTerms=c(TRUE,TRUE,TRUE))
           showNow<-"Basic"
         }
  )
  hypothesis<-makeHypothesis(IV=variables$IV,IV2=variables$IV2,DV=variables$DV,
                             effect=makeEffect(rIV,rIV2=rIV2,rIVIV2=rIVIV2,rIVIV2DV=rIVIV2DV)
                             )
  if (rootInv=="5") hypothesis$layout<-"simple"
  design<-makeDesign(sN=sN,sMethod=makeSampling(sMethod))
  setBrawDef("hypothesis",hypothesis)
  setBrawDef("design",design)
  
  if (single) {
    setBrawRes("result",NULL)
    doSingle()
  } else  {
    doMultiple(100)
  }      
  
  # display the results
  svgBox(height=350,aspect=1.5)
  setBrawEnv("graphicsType","HTML")
  setBrawEnv("fontSize",0.75)

  if (single) {
    schematic<-makePanel(showInference(effectType="direct"),reportInference())
  } else  {
    schematic<-makePanel(showMultiple(effectType="direct"),reportMultiple())
    showNow<-"Schematic"
  }      
  
  if (hideReport) {
    tabs<-c("Plan","Sample","Basic","Schematic")
    tabContents<-c(
      makePanel(showPlan()),
      makePanel(showSample(),NULL),
      makePanel(showDescription(),
                paste0(reportInference(),reportDescription(plain=TRUE))),
      schematic
    )
  } else {
    tabs<-c("Plan","Sample","Basic","Schematic")
    tabContents<-c(
      makePanel(showPlan()),
      makePanel(showMarginals(style="all"),reportSample()),
      makePanel(showDescription(),
                paste0(reportInference(),reportDescription(plain=TRUE))),
      schematic
    )
  }
  if (showJamovi) {
    tabs<-c(tabs,"Jamovi")
    tabContents<-c(tabContents,JamoviInstructions())
  }
  open<-which(showNow==tabs)
  
  linkLabel<-paste0(substr(doingDemo,1,6))
  investgResults<-
    generate_tab(
      title="Demo:",
      plainTabs=FALSE,
      titleWidth=100,
      width=550,
      tabs=tabs,
      tabContents=tabContents,
      tabLink=paste0('https://doingpsychstats.wordpress.com/demonstration-',partInv,'#','A'),
      tabLinkLabel=paste0('\U24D8',partInv),
      open=open
    )
  
  if (showOutput) {
    showHTML(investgResults)
    return(invisible(NULL))
  }
  
  return(investgResults)
}
