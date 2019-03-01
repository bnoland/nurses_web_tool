app <- ShinyDriver$new("../")
app$snapshotInit("test_filter_state")

# All states.
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()

# None selected.
app$setInputs(state_selection = character(0))
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()

# Single state selected.

app$setInputs(state_selection = "CA")
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()

app$setInputs(state_selection = "MO")
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()

app$setInputs(state_selection = "SC")
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()

# A variety of state selections.

app$setInputs(state_selection = c("HI", "MA", "MN", "MT", "SC", "TN", "VA"))
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()

app$setInputs(state_selection = c("AL", "CO", "LA", "MS", "PA", "WA"))
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()

app$setInputs(state_selection = c("CO", "ME"))
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()

app$setInputs(state_selection = c("NC", "SD", "WY"))
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()
