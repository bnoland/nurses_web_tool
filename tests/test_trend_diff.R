app <- ShinyDriver$new("../")
app$snapshotInit("test_trend_diff")

# Sex, same levels.
app$setInputs(trend_plot_type = "diff")
app$snapshot()

# Sex, different levels.
app$setInputs(trends_diff_level1 = "Female")
app$snapshot()

# Race, different levels.
app$setInputs(trends_diff_var = "race")
app$setInputs(trends_diff_level2 = "Black")
app$snapshot()
