app <- ShinyDriver$new("../")
app$snapshotInit("test_filter_sex")

# Both sexes.
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "Geography")
app$snapshot()

# Females only.
app$setInputs(sex_selection = "Female")
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "Geography")
app$snapshot()

# Males only.
app$setInputs(sex_selection = "Male")
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "Geography")
app$snapshot()

# None selected.
app$setInputs(sex_selection = character(0))
app$snapshot()
