app <- ShinyDriver$new("../")
app$snapshotInit("test_map_show_selected_only")

app$setInputs(tabs = "Geography")

# All states.
app$setInputs(selected_states_only = TRUE)
app$snapshot()

# No states.
app$setInputs(state_selection = character(0))
app$snapshot()

# A single state.
app$setInputs(state_selection = "AL")
app$snapshot()

# A variety of state selections.

app$setInputs(state_selection = c("AL", "AR", "LA", "MS"))
app$snapshot()

app$setInputs(state_selection = c("AZ", "KS", "ME", "MI", "ND"))
app$snapshot()

app$setInputs(state_selection = c("AL", "AK", "AZ", "AR", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"))
app$snapshot()
