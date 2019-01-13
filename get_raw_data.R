# Get the raw CPS data for registered nurses.

library(readr)
library(dplyr)

# TODO: Right now this simply extracts the observations corresponding to
# registered nurses from Doug Kruse's data. Might want to download the data from
# the CPS website in the future for the sake of extensibility.

load("orgees20092017.rdata")
nurses_raw <- orgees20092017 %>%
  filter(peio1ocd == 3255 | peio1ocd == 3130)

write_csv(nurses_raw, "nurses_raw.csv")
