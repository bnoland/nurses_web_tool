context("blah")

source("../global.R", chdir = TRUE)
source("../server.R", chdir = TRUE)

#print(head(nurses))

data1 <- trend_data(nurses, FALSE, "sex", NULL, NULL, type = "membership")
data2 <- trend_data(nurses, FALSE, "sex", NULL, NULL, type = "coverage")

test_that("Fiddling with data frames", {
    expect_equal(data1, data1)
})

plot1 <- trend_plot(nurses, FALSE, "sex", NULL, NULL, FALSE, type = "membership")

test_that("Fiddling with plots", {
    vdiffr::expect_doppelganger("Plot 1", plot1)
})
