app <- ShinyDriver$new("../")
app$snapshotInit("test_filter_citizen")

# Every citizenship status.
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()

# None selected.
app$setInputs(citizen_selection = character(0))
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()

# Single citizenship status selected.

app$setInputs(citizen_selection = "US native")
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()

app$setInputs(citizen_selection = "Foreign-born, citizen")
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()

app$setInputs(citizen_selection = "Foreign-born, non-citizen")
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()

# A variety of citizenship status selections.

app$setInputs(citizen_selection = c("US native", "Foreign-born, citizen"))
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()

app$setInputs(citizen_selection = c("Foreign-born, citizen", "Foreign-born, non-citizen"))
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()

app$setInputs(citizen_selection = c("US native", "Foreign-born, non-citizen"))
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()
