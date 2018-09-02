context("Data processing")
library(dplyr)

source("../global.R", chdir = TRUE)
source("../server.R", chdir = TRUE)

test_that("Trend grouped data", {
    expect_grouped_data_equal <- function(nurses_subset, group_var, type) {
        actual_data <- trend_data(
            nurses_subset = nurses_subset,
            group_var = group_var,
            type = type
        )
        
        if (group_var == "none")
            group_var <- NULL
        
        if (type == "membership") {
            expected_data <- nurses_subset %>%
                group_by(.dots = c("year", group_var)) %>%
                summarize(
                    prop = mean(member, na.rm = TRUE),
                    n = n()
                )
        } else if (type == "coverage") {
            expected_data <- nurses_subset %>%
                group_by(.dots = c("year", group_var)) %>%
                summarize(
                    prop = mean(covered, na.rm = TRUE),
                    n = n()
                )
        }
        
        expect_equal(actual_data, expected_data)
    }
    
    expect_grouped_data_equal(nurses, "none", type = "membership")
    expect_grouped_data_equal(nurses, "sex", type = "membership")
    expect_grouped_data_equal(nurses, "age_group", type = "membership")
    expect_grouped_data_equal(nurses, "race", type = "membership")
    expect_grouped_data_equal(nurses, "hisp", type = "membership")
    expect_grouped_data_equal(nurses, "educ", type = "membership")
    expect_grouped_data_equal(nurses, "citizen", type = "membership")
    expect_grouped_data_equal(nurses, "state", type = "membership")
    
    expect_grouped_data_equal(nurses, "none", type = "coverage")
    expect_grouped_data_equal(nurses, "sex", type = "coverage")
    expect_grouped_data_equal(nurses, "age_group", type = "coverage")
    expect_grouped_data_equal(nurses, "race", type = "coverage")
    expect_grouped_data_equal(nurses, "hisp", type = "coverage")
    expect_grouped_data_equal(nurses, "educ", type = "coverage")
    expect_grouped_data_equal(nurses, "citizen", type = "coverage")
    expect_grouped_data_equal(nurses, "state", type = "coverage")
})

test_that("Trend difference data", {
    expect_diff_data_equal <- function(nurses_subset, diff_var, diff_levels, type) {
        actual_data <- trend_data(
            nurses_subset = nurses_subset,
            plot_diff = TRUE,
            diff_var = diff_var,
            diff_levels = diff_levels,
            type = type
        )
        
        if (type == "membership") {
            expected_data <- nurses_subset %>%
                group_by(.dots = c("year", diff_var)) %>%
                summarize(
                    prop = mean(member, na.rm = TRUE),
                    n = n()
                )
        } else if (type == "coverage") {
            expected_data <- nurses_subset %>%
                group_by(.dots = c("year", diff_var)) %>%
                summarize(
                    prop = mean(covered, na.rm = TRUE),
                    n = n()
                )
        }
        
        expected_data_level1 <- expected_data %>%
            filter(eval(diff_var) == diff_levels[[1]])
        
        expected_data_level2 <- expected_data %>%
            filter(eval(diff_var) == diff_levels[[2]])
        
        expected_data <- inner_join(expected_data_level1, expected_data_level2,
                                    by = "year", suffix = c(".level1", ".level2"))
        
        expected_data <- expected_data %>%
            mutate(prop_diff = prop.level1 - prop.level2)
        
        expect_equal(actual_data, expected_data)
    }
    
    # TODO: Test random levels?
    expect_diff_data_equal(nurses, "sex", c("male", "female"), type = "membership")
    expect_diff_data_equal(nurses, "sex", c("male", "female"), type = "coverage")
})

test_that("State data", {
    expect_state_data_equal <- function(nurses_subset, type) {
        actual_data <- state_data(nurses_subset, type)
        
        if (type == "membership") {
            expected_data <- nurses_subset %>%
                group_by(state) %>%
                summarize(
                    prop = mean(member, na.rm = TRUE),
                    n = n()
                )
        } else if (type == "coverage") {
            expected_data <- nurses_subset %>%
                group_by(state) %>%
                summarize(
                    prop = mean(covered, na.rm = TRUE),
                    n = n()
                )
        }
        
        expect_equal(actual_data, expected_data)
    }
    
    expect_state_data_equal(nurses, type = "membership")
    expect_state_data_equal(nurses, type = "coverage")
})
