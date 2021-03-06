# Preprocess the raw CPS data for use by the app.

library(readr)
library(dplyr)
library(purrr)
library(forcats)

data_dir <- "data"

path <- file.path(data_dir, "nurses_raw.csv")
nurses_raw <- read_csv(path,
  col_types = cols(
    hryear4 = col_integer(),
    pesex = col_integer(),
    peernlab = col_integer(),
    peerncov = col_integer(),
    peage = col_integer(),
    ptdtrace = col_integer(),
    pehspnon = col_integer(),
    peeduca = col_integer(),
    prcitshp = col_integer(),
    gestfips = col_integer(),
    pworwgt = col_double(),
    peio1ocd = col_integer()
  )
)

# Code sex from the CPS variable PESEX.
sex_code <- function(pesex) {
  pesex %>% map_chr(function(x) {
    if (is.na(x)) {
      NA
    } else if (x == 1) {
      "Male"
    } else if (x == 2) {
      "Female"
    } else {
      stop("Miscoded CPS sex: ", x)
    }
  })
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

# Code race from the CPS variable PTDTRACE.
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
      # TODO: This is a catch-all. May want to detect miscodes in raw data.
      "Other"
    }
  })
}

# Code Hispanic status from the CPS variable PEHSPNON.
hispanic_code <- function(pehspnon) {
  pehspnon %>% map_chr(function(x) {
    if (is.na(x)) {
      NA
    } else if (x == 1) {
      "Hispanic"
    } else if (x == 2) {
      "Non-Hispanic"
    } else {
      stop("Miscoded CPS Hispanic status: ", x)
    }
  })
}

# Code education level from the CPS variable PEEDUCA.
education_code <- function(peeduca) {
  peeduca %>% map_chr(function(x) {
    # TODO: Some miscodes in the raw data might fall through the cracks here.
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
    } else {
      stop("Miscoded CPS education level: ", x)
    }
  })
}

# Code citizenship status from the CPS variable PRCITSHP.
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
    } else {
      stop("Miscoded CPS citizenship status: ", x)
    }
  })
}

# Code state from CPS variable GESTFIPS.
state_code <- function(gestfips) {
  source("state_table.R", local = TRUE)

  gestfips %>% map_chr(function(x) {
    if (is.na(x)) {
      NA
    } else {
      key <- as.character(x)
      state <- state_table[[key]]
      if (is_null(state)) {
        stop("Miscoded CPS state: ", x)
      }
      state
    }
  })
}

nurses_preprocessed <- nurses_raw %>%
  transmute(
    year = hryear4,
    sex = sex_code(pesex),
    member = (peernlab == 1),
    covered = (member | peerncov == 1),
    #age = ifelse(is.na(peage), prtage, peage),
    age = peage,
    age_group = age_group_code(age),
    race = race_code(ptdtrace),
    hisp = hispanic_code(pehspnon),
    educ = education_code(peeduca),
    citizen = citizenship_code(prcitshp),
    state = state_code(gestfips),
    weight = pworwgt / 10000
  ) %>%
  filter(age >= 16) %>%
  arrange(year)

# Sanity check for missing values.
for (name in names(nurses_preprocessed)) {
  col <- nurses_preprocessed[[name]]
  if (sum(is.na(col)) > 0) {
    warning("Column ", name, " contains missing values")
  }
}

path <- file.path(data_dir, "nurses_preprocessed.csv")
write_csv(nurses_preprocessed, path)
