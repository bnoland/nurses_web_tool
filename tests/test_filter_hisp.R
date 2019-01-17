app <- ShinyDriver$new("../")
app$snapshotInit("test_filter_hisp")

# Both Hispanic and non-Hispanic.
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "Geography")
app$snapshot()

# None selected.
app$setInputs(hisp_status_selection = character(0))
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "Geography")
app$snapshot()

# Hispanic only.
app$setInputs(hisp_status_selection = "Hispanic")
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "Geography")
app$snapshot()

# Non-Hispanic only.
app$setInputs(hisp_status_selection = "Non-Hispanic")
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "Geography")
app$snapshot()
