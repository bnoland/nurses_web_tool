context("Plot generation")
library(usmap)

source("../global.R", chdir = TRUE)
source("../server.R", chdir = TRUE)

# TODO: Test on random subset of the full dataset?
# TODO: Test attributes of plots besides underlying data (e.g., correct layers).

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
        
        actual_data <- p$data %>% select(-matches("^.group$"))
        
        expect_equal(actual_data, expected_data)
    }
    
    # expect_geom_line_layer <- function(nurses_subset, group_var, type) {
    #     p <- trend_plot(
    #         nurses_subset = nurses_subset,
    #         group_var = group_var,
    #         type = type
    #     )
    #     
    #     layers <- sapply(p$layers, function(x) class(x$geom)[[1]])
    #     
    #     expect_true("GeomLine" %in% layers)
    # }
    
    expect_data_equal(nurses, "none", type = "membership")
    expect_data_equal(nurses, "sex", type = "membership")
    expect_data_equal(nurses, "age_group", type = "membership")
    expect_data_equal(nurses, "race", type = "membership")
    expect_data_equal(nurses, "hisp", type = "membership")
    expect_data_equal(nurses, "educ", type = "membership")
    expect_data_equal(nurses, "citizen", type = "membership")
    expect_data_equal(nurses, "state", type = "membership")
    
    expect_data_equal(nurses, "none", type = "coverage")
    expect_data_equal(nurses, "sex", type = "coverage")
    expect_data_equal(nurses, "age_group", type = "coverage")
    expect_data_equal(nurses, "race", type = "coverage")
    expect_data_equal(nurses, "hisp", type = "coverage")
    expect_data_equal(nurses, "educ", type = "coverage")
    expect_data_equal(nurses, "citizen", type = "coverage")
    expect_data_equal(nurses, "state", type = "coverage")
})

test_that("Trend difference plots", {
    expect_data_equal <- function(nurses_subset, diff_var, diff_levels, type) {
        expected_data <- trend_data(
            nurses_subset = nurses_subset,
            plot_diff = TRUE,
            diff_var = diff_var,
            diff_levels = diff_levels,
            type = type
        )
        
        p <- trend_plot(
            nurses_subset = nurses_subset,
            plot_diff = TRUE,
            diff_var = diff_var,
            diff_levels = diff_levels,
            type = type
        )
        
        actual_data <- p$data %>% select(-matches("^.group$"))
        
        expect_equal(actual_data, expected_data)
    }
    
    # TODO: Test random levels?
    expect_data_equal(nurses, "sex", c("male", "female"), type = "membership")
    expect_data_equal(nurses, "sex", c("male", "female"), type = "coverage")
})

test_that("State maps", {
    expect_data_equal <- function(nurses_subset, type) {
        state_data <- state_data(nurses_subset, type)
        
        # TODO: Probably want to handle state selections...
        expected_data <- plot_usmap(data = state_data, value = "prop")$data
        
        p <- state_map(
            nurses_subset = nurses_subset,
            type = type
        )
        
        actual_data <- p$data
        
        expect_equal(actual_data, expected_data)
    }
    
    expect_data_equal(nurses, type = "membership")
    expect_data_equal(nurses, type = "coverage")
})
