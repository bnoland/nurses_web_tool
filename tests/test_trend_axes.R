app <- ShinyDriver$new("../")
app$snapshotInit("test_trend_axes")

# Initial axes.
app$snapshot()

# Set to fixed axes.
app$setInputs(trends_fixed_axis = TRUE)
app$snapshot()

# Switch back.
app$setInputs(trends_fixed_axis = FALSE)
app$snapshot()
