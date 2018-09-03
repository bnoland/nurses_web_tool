context("Plot generation")

test_that("Trend grouped plots", {
    expect_data_equal <- function(nurses_subset, group_var, type) {
        expected_data <- trend_data(
            nurses_subset = nurses_subset,
            group_var = group_var,
            type = type
        )
        
        p <- trend_plot(
            nurses_subset = nurses_subset,
            group_var = group_var,
            type = type
        )
        
        expect_equal(p$data, expected_data)
    }
    
})

test_that("Trend difference plots", {
})

test_that("State maps", {
})
