## --- Day 7: No Space Left On Device ---

## Part 1 

library(tidyverse)

input <- read_delim("inputs/input-07.txt", delim = "\n", skip_empty_rows = FALSE, col_names = FALSE) |>
  unnest(cols = c())

names(input) <- "line"

cd <- function(path, dir = NA) {
  if (any(is.na(dir))) {
    return(path)
  }
  if (any(dir == "..")) {
    return(head(path, -1))
  }
  return(c(path, paste0(tail(path, 1), "/", dir)))
}

paths <- input |>
  mutate(path = line |>
    str_extract("cd (.*)") |>
    str_remove("cd ")) |>
  mutate(path = accumulate(path, cd)) |>
  unnest(path)

sizes <- paths |>
  filter(str_detect(line, "^[0-9]")) |>
  group_by(path) |>
  summarize(size = line |>
    str_extract("^[0-9]+") |>
    as.numeric() |>
    sum()) |>
  arrange(-size)

sizes |>
  filter(size < 100000) |>
  pull(size) |>
  sum()


## Part 2
total <- 70000000
required <- 30000000

used <- head(sizes, 1) |>
  pull(size)

sizes |>
  filter(size >= (required - (total - used))) |>
  arrange(size) |>
  head(1)
