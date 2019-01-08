app <- ShinyDriver$new("../")
app$snapshotInit("mytest")

#app$setInputs(selection_panels = "Sex")
app$setInputs(sex_selection = "Female")
app$snapshot()

vals <- app$getAllValues()
#str(vals)
print(vals$input$educ_selection)
