# Preprocess the raw CPS data for use by the app.

# TODO: Note that, for the sake of posterity, this doesn't make use of any of the variables created
# by Doug Kruse's script. In the future, it would probably be better to use the actual raw CPS data.

library(tidyverse)

# Load the raw data ---------------------------------------------------------------------------

# TODO: Column specifications?
nurses <- read_csv("nurses.csv")

# Do the preprocessing ------------------------------------------------------------------------

# Put the sex code in the right format.
sex_code <- function(pesex) {
    levels <- c(
        "Male",
        "Female"
    )
    pesex %>% map_chr(function(x) {
        if (is.na(x)) {
            NA
        } else if (x == 1) {
            "Male"
        } else if (x == 2) {
            "Female"
        }
        # TODO: Add assertion here.
    }) %>% factor(levels = levels)
}

# Put the education level code in the right format.
education_code <- function(peeduca) {
    levels <- c(
        "No high school",
        "Completed high school",
        "Some college",
        "Associate degree",
        "Bachelor's degree",
        "Graduate degree"
    )
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
    }) %>% factor(levels = levels)
}

# Put the citizenship status code in the right format.
citizenship_status_code <- function(prcitshp) {
    levels <- c(
        "US native",
        "Foreign-born, citizen",
        "Foreign-born, non-citizen"
    )
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
    }) %>% factor(levels = levels)
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
        covered = (member | peerncov),
        age = ifelse(is.na(peage), prtage, peage),
        age_group = cut(age, breaks = c(0, 16, 25, 55, Inf), right = FALSE),
        # TODO: Define race variable.
        educ = education_code(peeduca),
        citizen = citizenship_status_code(prcitshp)
        # TODO: Define geography variable(s).
    ) %>%
    filter(age >= 16)

nurses_preprocessed

# Write the preprocessed data to disk ---------------------------------------------------------

write_csv(nurses_preprocessed, "nurses_preprocessed.csv")
