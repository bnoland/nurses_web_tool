# File for defining global data, including loading the nurses data set.

library(readr)

source("factor_levels.R")

# Load the data -------------------------------------------------------------------------------

nurses <- read_csv("nurses_preprocessed.csv",
    col_types = cols(
        year = col_integer(),
        sex = col_factor(levels = sex_factor_levels()),
        member = col_logical(),
        covered = col_logical(),
        age = col_integer(),
        age_group = col_factor(levels = age_group_factor_levels()),
        race = col_factor(race_factor_levels()),
        hisp = col_factor(levels = hispanic_factor_levels()),
        educ = col_factor(levels = education_factor_levels()),
        citizen = col_factor(levels = citizenship_factor_levels()),
        state = col_factor(levels = state_factor_levels())
    )
)

# Define some useful globals ------------------------------------------------------------------

min_year <- as.double(min(nurses$year, na.rm = TRUE))
max_year <- as.double(max(nurses$year, na.rm = TRUE))
