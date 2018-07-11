# Preprocess the raw CPS data for use by the app.

# TODO: Note that, for the sake of posterity, this doesn't make use of any of the variables created
# by Doug Kruse's script. In the future, it would probably be better to use the actual raw CPS data.

library(tidyverse)

source("factor_levels.R")

# Load the raw data ---------------------------------------------------------------------------

# TODO: Column specifications?
nurses <- read_csv("nurses.csv")

# Do the preprocessing ------------------------------------------------------------------------

# TODO: Technically, we don't need to actually do factor coding here.

# Code sex from the appropriate CPS variable.
sex_code <- function(pesex) {
    pesex %>% map_chr(function(x) {
        if (is.na(x)) {
            NA
        } else if (x == 1) {
            "Male"
        } else if (x == 2) {
            "Female"
        }
        # TODO: Add assertion here.
    }) %>% factor(levels = sex_factor_levels())
}

# Code age group from age.
age_group_code <- function(age) {
    age_group <- cut(age, breaks = c(16, 25, 55, Inf), right = FALSE)
    fct_recode(age_group,
       "16-24" = "[16,25)",
       "25-54" = "[25,55)",
       "55 and over" = "[55,Inf)"
    )
}

# Code race from the pertinent CPS variable.
race_code <- function(ptdtrace) {
    ptdtrace %>% map_chr(function(x) {
        if (is.na(x)) {
            NA
        } else if (x == 1) {
            "White"
        } else if (x == 2) {
            "Black"
        } else if (x == 3) {
            "American Indian (Alaskan Native)"
        } else if (x == 4) {
            "Asian"
        } else if (x == 5) {
            "Hawaiian/Pacific Islander"
        } else {
            "Other"
        }
    }) %>% factor(levels = race_factor_levels())
}

# Code education level from the appropriate CPS variable.
education_code <- function(peeduca) {
    peeduca %>% map_chr(function(x) {
        if (is.na(x)) {
            NA
        } else if (x < 39) {
            "No high school"
        } else if (x == 39) {
            "Completed high school"
        } else if (x == 40) {
            "Some college"
        } else if (x == 41 || x == 42) {
            "Associate degree"
        } else if (x == 43) {
            "Bachelor's degree"
        } else if (x > 43) {
            "Graduate degree"
        }
        # TODO: Add assertion here.
    }) %>% factor(levels = education_factor_levels())
}

# Code citizenship status from the appropriate CPS variable.
citizenship_code <- function(prcitshp) {
    prcitshp %>% map_chr(function(x) {
        if (is.na(x)) {
            NA
        } else if (x %in% c(1, 2, 3)) {
            "US native"
        } else if (x == 4) {
            "Foreign-born, citizen"
        } else if (x == 5) {
            "Foreign-born, non-citizen"
        }
        # TODO: Add assertion here.
    }) %>% factor(levels = citizenship_factor_levels())
}

# Code state from appropriate CPS variable.
state_code <- function(gestfips) {
    state_table <- state_table()
    gestfips %>% map_chr(function(x) {
        if (is.na(x))
            NA
        else {
            key <- as.character(x)
            state <- state_table[[key]]
            if (is_null(state)) {
                # TODO: Put assertion here.
            }
            
            state
        }
    }) %>% factor(levels = state_factor_levels())
}

nurses_preprocessed <- nurses %>%
    # TODO: For now, I'm dropping the unused variables. May probably want to keep them in the end
    # so that they can be included in a download selection (another option: just use the raw data
    # for downloads).
    transmute(
        # TODO: Doug Kruse's script already renamed ``hryear4'' to ``year''.
        #year = hryear4,
        year = year,
        sex = sex_code(pesex),
        member = (peernlab == 1),
        covered = (member | peerncov == 1),
        age = ifelse(is.na(peage), prtage, peage),
        age_group = age_group_code(age),
        race = race_code(ptdtrace),
        hisp = (pehspnon == 1),
        educ = education_code(peeduca),
        citizen = citizenship_code(prcitshp),
        state = state_code(gestfips)
    ) %>%
    filter(age >= 16)

# Write the preprocessed data to disk ---------------------------------------------------------

write_csv(nurses_preprocessed, "nurses_preprocessed.csv")
