
# Look at the first 10 lines as raw text
readLines("CVSupplementalMaterial/casestudies/transitflocks.txt", n = 10)
# Also check how many "words" are on each of the first few lines
sapply(readLines("CVSupplementalMaterial/casestudies/transitflocks.txt", n = 10),
       function(x) length(strsplit(trimws(x), "\\s+")[[1]]))
transit <- read.table("CVSupplementalMaterial/casestudies/transitflocks.txt")

library(tidyverse)

read_jackdaw <- function(path) {
  lines <- readLines(path)

  # Skip the header (line 1), process the rest
  lines <- lines[-1]

  flock_id  <- NA_character_
  rows      <- list()

  for (line in lines) {
    tokens <- strsplit(trimws(line), "\\s+")[[1]]

    if (length(tokens) == 3) {
      # Flock label line e.g. "transit flock 01"
      flock_id <- paste(tokens, collapse = "_")  # "transit_flock_01"
    } else if (length(tokens) == 8) {
      # Data row
      rows[[length(rows) + 1]] <- c(flock = flock_id, tokens)
    }
    # ignore blank lines or anything else
  }

  df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  colnames(df) <- c("flock", "bird_id", "x", "y", "z", "t", "vx", "vy", "vz")

  # Convert numeric columns
  df <- df %>%
    mutate(across(c(bird_id, x, y, z, t, vx, vy, vz), as.numeric))

  df
}

transit <- read_jackdaw("CVSupplementalMaterial/casestudies/transitflocks.txt")

# Quick check
glimpse(transit)
transit %>% count(flock)
