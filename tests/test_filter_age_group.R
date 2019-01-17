app <- ShinyDriver$new("../")
app$snapshotInit("test_filter_age_group")

# All age groups.
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "Geography")
app$snapshot()

# None selected.
app$setInputs(age_selection = character(0))
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "Geography")
app$snapshot()

# Single age group selected.
app$setInputs(age_selection = "16-24")
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "Geography")
app$snapshot()

# A variety of age group selections.

app$setInputs(age_selection = c("25-54", "55 and over"))
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "Geography")
app$snapshot()

app$setInputs(age_selection = c("16-24", "25-54"))
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "Geography")
app$snapshot()
