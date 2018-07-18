# A few helper functions that return the appropriate factor levels for pertinent categorical
# variables.

sex_factor_levels <- function() {
    c(
        "Male",
        "Female"
    )
}

age_group_factor_levels <- function() {
    c(
        "16-24",
        "25-54",
        "55 and over"
    )
}

# TODO: Right factor levels?
race_factor_levels <- function() {
    c(
        "White",
        "Black",
        "American Indian (Alaskan Native)",
        "Asian",
        "Hawaiian/Pacific Islander",
        "Other"
    )
}

education_factor_levels <- function() {
    c(
        "No high school",
        "Completed high school",
        "Some college",
        "Associate degree",
        "Bachelor's degree",
        "Graduate degree"
    )
}

citizenship_factor_levels <- function() {
    c(
        "US native",
        "Foreign-born, citizen",
        "Foreign-born, non-citizen"
    )
}

state_factor_levels <- function() {
    source("state_table.R", local = TRUE)
    as.character(state_table)
}
