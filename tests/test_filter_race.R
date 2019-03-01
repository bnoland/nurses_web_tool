app <- ShinyDriver$new("../")
app$snapshotInit("test_filter_race")

# All races.
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()

# None selected.
app$setInputs(race_selection = character(0))
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()

# Single race selected.

app$setInputs(race_selection = "Asian")
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()

app$setInputs(race_selection = "Black")
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()

app$setInputs(race_selection = "White")
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()

# A variety of race selections.

app$setInputs(race_selection = c("American Indian (Alaskan Native)", "Asian", "Hawaiian/Pacific Islander"))
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()

app$setInputs(race_selection = c("White", "Black", "Other"))
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()

app$setInputs(race_selection = c("White", "Black", "Asian", "Other"))
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()

app$setInputs(race_selection = c("Black", "American Indian (Alaskan Native)", "Asian", "Other"))
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()
