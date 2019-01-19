app <- ShinyDriver$new("../")
app$snapshotInit("test_trend_group")

# Group by sex.
app$setInputs(trends_group_var = "sex")
app$snapshot()

# Group by race.
app$setInputs(trends_group_var = "race")
app$snapshot()

# Now on a subset.
app$setInputs(race_selection = c("White", "Black"))
app$snapshot()

# Back to no grouping, on the full dataset.
app$setInputs(trends_group_var = "none")
app$setInputs(race_selection = c("White", "Black", "American Indian (Alaskan Native)", "Asian", "Hawaiian/Pacific Islander", "Other"))
app$snapshot()
