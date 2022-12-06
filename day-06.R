## --- Day 6: Tuning Trouble ---

## Part 1
library(tidyverse)

input <- strsplit(read_lines("inputs/input-06.txt"), "")[[1]]

first_marker <- function(vec, n = 4) {
  i <- n
  repeat {
    packet <- vec[(i - n + 1):i]
    dupes <- n - length(unique(packet))
    if (dupes == 0) {
      return(i)
    }
    i <- i + dupes
  }
}

first_marker(input)

## Part 2
first_marker(input, 14)
