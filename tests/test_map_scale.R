app <- ShinyDriver$new("../")
app$snapshotInit("test_map_scale")

app$setInputs(tabs = "States")

# Default scale.
app$snapshot()

# Switch to fixed scale.
app$setInputs(maps_fixed_scale = TRUE)
app$snapshot()

# Switch back to default scale.
app$setInputs(maps_fixed_scale = FALSE)
app$snapshot()
