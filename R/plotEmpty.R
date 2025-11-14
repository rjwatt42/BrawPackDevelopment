
#' @export
emptyPlot<-function(mode,useHelp=FALSE) {

    switch(mode,
           "Basics"=tabs<-c("Plan","Sample","Data","Schematic","Jamovi"),
           "MetaScience"=tabs<-c("Plan","Data","Schematic"),
           "Simulation"=tabs<-c("Plan","Single","Multiple","Explore")
    )
    switch(mode,
           "Basics"=tabTitle<-"Basics:",
           "MetaScience"=tabTitle<-"MetaScience:",
           "Simulation"=tabTitle<-"Simulation:"
    )
  tabContents<-c(rep(nullPlot(),length(tabs)))
  
  if (useHelp) {
    tabs<-c(tabs,"Help")
    switch(mode,
           "Basics"=tabContents<-c(tabContents,brawBasicsHelp(indent=100)),
           "MetaScience"=tabContents<-c(tabContents,brawMetaSciHelp(indent=100)),
           "Simulation"=tabContents<-c(tabContents,brawSimHelp(indent=100))
    )
  }
    nullResults<-generate_tab(
      title=tabTitle,
      plainTabs=TRUE,
      titleWidth=100,
      tabs=tabs,
      tabContents=tabContents,
      height=450,
      outerHeight=450,
      open=1
    )
    return(nullResults)
    # self$results$simGraphHTML$setContent(nullResults)
  
}