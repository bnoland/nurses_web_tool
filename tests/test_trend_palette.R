app <- ShinyDriver$new("../")
app$snapshotInit("test_trend_palette")

app$setInputs(trends_group_var = "race")

# Default palette.
app$snapshot()

# Set to viridis palette.
app$setInputs(trends_use_viridis = TRUE)
app$snapshot()

# Switch back to default palette.
app$setInputs(trends_use_viridis = FALSE)
app$snapshot()
