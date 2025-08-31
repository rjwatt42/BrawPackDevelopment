

setHypothesis(IV2=makeVariable("IV2"),effect=makeEffect(0.3,0,0,-0.3))
setDesign(420)
setEvidence(AnalysisTerms=c(TRUE,FALSE,FALSE))

print(showMultiple())


