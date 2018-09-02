context("Data processing")
library(dplyr)

source("../global.R", chdir = TRUE)
source("../server.R", chdir = TRUE)

nurses_subset <- nurses

test_that("Trend grouped data", {
    expect_trend_data_equal <- function(type) {
        group_var_list <- list(
            "sex",
            "age_group",
            "race",
            "hisp",
            "educ",
            "citizen",
            "state"
        )
        
        for (group_var in group_var_list) {
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
    }
    expect_trend_data_equal(type = "membership")
    expect_trend_data_equal(type = "coverage")
})

test_that("Trend difference data", {
})

test_that("State data", {
})
