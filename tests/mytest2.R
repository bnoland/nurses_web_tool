app <- ShinyDriver$new("../")
app$snapshotInit("mytest2")

#app$setInputs(selection_panels = "States")
app$setInputs(state_selection = character(0))
app$setInputs(state_selection = "CT")
app$snapshot()
