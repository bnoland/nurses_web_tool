app <- ShinyDriver$new("../")
app$snapshotInit("test_filter_year")

# Full year range.
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()

# Single year.
app$setInputs(year_range = c(2017, 2017))
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()

# A variety of year ranges.

app$setInputs(year_range = c(2015, 2017))
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()

app$setInputs(year_range = c(2010, 2014))
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()

app$setInputs(year_range = c(2009, 2010))
app$setInputs(tabs = "Trends")
app$setInputs(tabs = "States")
app$snapshot()
