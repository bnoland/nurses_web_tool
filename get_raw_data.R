# Extract the raw data from files downloaded from the CPS website, merge it, and
# write it to a CSV on disk.

library(readr)
library(dplyr)

# Should the raw data be appended to the output file? In either case, if the
# output file doesn't exist, it will be created.
append_raw_data <- FALSE

data_dir <- "data"
raw_data_dir <- "raw_data"
data_paths <- list.files(raw_data_dir, full.names = TRUE)
data_frames <- list()

for (path in data_paths) {
  cat("Processing file ", path, "...\n", sep = "")
  
  data <- read_fwf(path,
    col_positions = fwf_cols(
      hryear4 = c(18, 21),
      pesex = c(129, 130),
      peernlab = c(561, 562),
      peerncov = c(563, 564),
      peage = c(122, 123),
      ptdtrace = c(139, 140),
      pehspnon = c(157, 158),
      peeduca = c(137, 138),
      prcitshp = c(172, 173),
      gestfips = c(93, 94),
      pworwgt = c(603, 612),
      peio1ocd = c(860, 863)
    ),
    col_types = strrep("c", 12)
  )
  
  # Restrict to registered nurses with PEERNLAB >= 0.
  data <- data %>%
    filter(peio1ocd == 3255 | peio1ocd == 3130, peernlab >= 0)
  
  data_frames[[length(data_frames) + 1]] <- data
}

nurses_raw <- bind_rows(data_frames)

path <- file.path(data_dir, "nurses_raw.csv")
write_csv(nurses_raw, path, append = append_raw_data)
