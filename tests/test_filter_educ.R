app <- ShinyDriver$new("../")
app$snapshotInit("test_filter_educ")

# All education levels.
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()

# None selected.
app$setInputs(educ_selection = character(0))
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()

# Single education level selected.

app$setInputs(educ_selection = "Completed high school")
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()

app$setInputs(educ_selection = "Associate degree")
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()

app$setInputs(educ_selection = "Graduate degree")
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()

app$setInputs(educ_selection = "No high school")
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()

# A variety of education level selections.

app$setInputs(educ_selection = c("Completed high school", "Associate degree", "Bachelor's degree"))
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()

app$setInputs(educ_selection = c("Completed high school", "Some college", "Associate degree", "Bachelor's degree"))
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()

app$setInputs(educ_selection = c("Completed high school", "Associate degree", "Bachelor's degree", "Graduate degree"))
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()

app$setInputs(educ_selection = c("No high school", "Completed high school", "Graduate degree"))
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()

app$setInputs(educ_selection = c("No high school", "Completed high school"))
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()
