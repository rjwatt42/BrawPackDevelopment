
#' @export
stepDM<-function(doing) gsub('[A-Za-z]*([0-9]*)[A-Da-d]*','\\1',doing)

#' @export
partDM<-function(doing) toupper(gsub('[A-Za-z]*[0-9]*([A-Da-b]*)','\\1',doing))

#' @export
singleDM<-function(doing) !grepl('m',tolower(gsub('[A-Za-z]*[0-9]*[A-Da-b]*([crm]*)','\\1',doing)),fixed=TRUE)

#' @export
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
doDemonstration<-function(doingDemo="Step1A",showOutput=TRUE,showJamovi=TRUE,showPlanOnly=FALSE,doHistory=TRUE,
                          IV="Perfectionism",IV2=NULL,DV="ExamGrade",
                          rIV=NULL,rIV2=NULL,rIVIV2=NULL,rIVIV2DV=NULL,
                          sN=NULL,sMethod=NULL,
                          sOutliers=0, sDependence=0,
                          analyse="Main1",
                          nreps=200
                          ) {
  
  setHTML()
  stepDM<-stepDM(doingDemo)
  partDM<-partDM(doingDemo)
  single<-singleDM(doingDemo)
  rootDM<-paste0("Step",stepDM,partDM)

  variables=list(IV=IV,IV2=IV2,DV=DV)
  
  if (is.null(rIV)) rIV<-0.3
  
  if (is.null(sN)) {
    if (paste0(stepDM,partDM)=="1C") sN<-500
    if (paste0(stepDM)=="2") sN<-100
    if (paste0(stepDM)=="3") sN<-100
    if (paste0(stepDM)=="4") sN<-100
    if (paste0(stepDM)=="5") sN<-150
    if (paste0(stepDM)=="6") sN<-150
    if (paste0(stepDM)=="7") sN<-500
    if (is.null(sN)) sN<-42
  }
  if (is.null(sMethod)) {
    if (is.element(paste0(stepDM,partDM),c("1B"))) sMethod<-"Convenience"
    if (is.null(sMethod)) sMethod<-"Random"
  }
  switch(analyse,
         "Main1"={analyse<-c(TRUE,FALSE,FALSE)},
         "Main2"={analyse<-c(FALSE,TRUE,FALSE)},
         "Main12"={analyse<-c(TRUE,TRUE,FALSE)},
         "Main1x2"={analyse<-c(TRUE,TRUE,TRUE)},
         "InteractionOnly"={analyse<-c(FALSE,FALSE,TRUE)}
  )
  hideReport<-FALSE
  makeData<-TRUE
  switch(stepDM,
         "1"={ # making samples and analysing them in Jamovi
           switch(partDM,
                  "A"={showNow<-"Basic"},
                  "B"={showNow<-"Sample"},
                  "C"={showNow<-"Basic"}
           )
         },
         "2"={ # 3 basic tests with Interval DV
           variables$DV<-"ExamGrade"
           switch(partDM,
                  "A"={variables$IV<-"Perfectionism"},
                  "B"={variables$IV<-"Smoker?"},
                  "C"={variables$IV<-"BirthOrder"},
                  {}
           )
           showNow<-"Basic"
         },
         "3"={ # 2 basic tests with Categorical DV
           variables$DV<-"TrialOutcome"
           switch(partDM,
                  "A"={variables$IV<-"Treatment?"},
                  "B"={variables$IV<-"Sessions"},
                  "C"={variables$IV<-"Diligence"},
                  {}
           )
           showNow<-"Basic"
         },
         "4"={ # Revision of all basic tests with 2 variables
           DVs<-c("ExamGrade","ExamPass?","TrialOutcome","Happiness")
           variables$DV<-DVs[ceiling(runif(1)*length(DVs))]
           
           if (is.element(variables$DV,c("ExamGrade","ExamPass?")))
                 IVs<-c("Perfectionism","Musician?","RiskTaking","RiskTaker?")
           else  IVs<-c("Sessions","Treatment?","Smoker?","Diligence")
           variables$IV<-IVs[ceiling(runif(1)*length(IVs))]

           switch(partDM,
                  "A"={hideReport<-TRUE;showJamovi<-FALSE},
                  "B"={hideReport<-FALSE;makeData<-FALSE},
                  {}
           )
           showNow<-"Basic"
           single<-TRUE
         },
         "5"={ # Main effects in multiple IVs
           variables$DV<-"ExamGrade"
           switch(partDM,
                  "A"={variables$IV<-"BirthOrder";variables$IV2<-"Musician?"},
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
           rIVIV2<- 0
           rIVIV2DV<-0
           setEvidence(AnalysisTerms=c(TRUE,TRUE,FALSE))
           showNow<-"Basic"
         },
         "6"={ # Interactions
           variables$DV<-"ExamGrade"
           switch(partDM,
                  "A"={variables$IV<-"Coffee?";variables$IV2<-"Musician?"},
                  "B"={variables$IV<-"Anxiety";variables$IV2<-"Smoker?"},
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
         },
         "7"={ # Interactions
           variables$DV<-"ExamGrade"
           switch(partDM,
                  "A"={variables$IV<-"Coffee?";variables$IV2<-"Musician?"},
                  "B"={variables$IV<-"Anxiety";variables$IV2<-"Smoker?"},
                  "C"={variables$IV<-"Perfectionism";variables$IV2<-"HoursSleep"},
                  "D"={
                    IVs<-c("IQ","Musician?","Anxiety","RiskTaker?","SelfConfidence","Diligence","Coffee?")
                    variables$IV<-IVs[ceiling(runif(1)*length(IVs))]
                    IVs<-IVs[IVs!=variables$IV]
                    variables$IV2<-IVs[ceiling(runif(1)*length(IVs))]
                  }
           )
           if (is.null(rIV2)) rIV2<- -0.5
           if (is.null(rIVIV2)) rIVIV2<-0.7
           rIVIV2DV<- 0
           setEvidence(AnalysisTerms=c(TRUE,TRUE,FALSE))
           showNow<-"Basic"
         }
  )
  hypothesis<-makeHypothesis(IV=variables$IV,IV2=variables$IV2,DV=variables$DV,
                             effect=makeEffect(rIV,rIV2=rIV2,rIVIV2=rIVIV2,rIVIV2DV=rIVIV2DV)
                             )
  if (stepDM=="5") hypothesis$layout<-"simple"
  if (stepDM=="6") hypothesis$layout<-"noCovariation"
  if (stepDM=="7") hypothesis$layout<-"noInteraction"
  design<-makeDesign(sN=sN,sMethod=makeSampling(sMethod),sOutliers=sOutliers, sDependence=sDependence)
  setBrawDef("hypothesis",hypothesis)
  setBrawDef("design",design)
  
  if (makeData) {
    if (single) {
      setBrawRes("result",NULL)
      doSingle()
    } else  {
      doMultiple(100)
    }      
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
      makePanel(showMarginals(style="all"),NULL),
      makePanel(showDescription(dataOnly=TRUE),NULL),
      makePanel(nullPlot(),NULL)
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
  } else {
    tabs<-c(tabs,"Jamovi")
    tabContents<-c(tabContents,nullPlot())
  }
  open<-which(showNow==tabs)
  
  history<-braw.res$demoHistory
  if (is.null(history)) history<-list(content='')
  if (!doHistory) history$content<-NULL
  
  linkLabel<-paste0(rootDM)
  demoResults<-
    generate_tab(
      title="Basics:",
      plainTabs=FALSE,
      titleWidth=100,
      width=550,
      tabs=tabs,
      tabContents=tabContents,
      tabLink=paste0('https://doingpsychstats.wordpress.com/basics-',partDM,'#','A'),
      tabLinkLabel=paste0('&#x24D8 ',linkLabel),
      history=history$content,
      open=open
    )
  
  if (doHistory) {
    history$content<-demoResults
    history$place<-length(history$content)
    setBrawRes("demoHistory",history)
  }
  if (showOutput) {
    showHTML(demoResults)
    return(invisible(NULL))
  }
  
  return(demoResults)
}
