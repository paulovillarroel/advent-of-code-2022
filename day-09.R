## --- Day 9: Rope Bridge ---

## Part 1

library(tidyverse)

input <- read_delim("inputs/input-09.txt", delim = "\n", skip_empty_rows = FALSE, col_names = FALSE) |>
  unnest(cols = c())

moves <- list(
  "R" = c(1, 0),
  "L" = c(-1, 0),
  "U" = c(0, -1),
  "D" = c(0, 1)
)

find_status <- function(status, rope_size) {
  for (knot in 2:rope_size) {
    dx <- status[[knot - 1]][1] - status[[knot]][1];
    dy <- status[[knot - 1]][2] - status[[knot]][2];
    if (abs(dx) > 1) {
      if (dx > 0) status[[knot]][1] <- status[[knot]][1] + 1
      if (dx < 0) status[[knot]][1] <- status[[knot]][1] - 1
      if (dy > 0) status[[knot]][2] <- status[[knot]][2] + 1
      if (dy < 0) status[[knot]][2] <- status[[knot]][2] - 1
    } else if (abs(dy) > 1) {
      if (dy > 0) status[[knot]][2] <- status[[knot]][2] + 1
      if (dy < 0) status[[knot]][2] <- status[[knot]][2] - 1
      if (dx > 0) status[[knot]][1] <- status[[knot]][1] + 1
      if (dx < 0) status[[knot]][1] <- status[[knot]][1] - 1
    }
  }
  status
}

move_rope <- function(input, rope_size) {
  data <- pull(input) |> 
    str_split(" ")
  result <- list()
  status <- list()
  for (i in 1:rope_size) {
    status <- append(status, list(c(0, 0)))
  }
  for (motion in seq_along(data)) {
    direction <- data[[motion]][1]
    steps <- as.integer(data[[motion]][2])
    for (j in 1:steps) {
      status[[1]] <- c(
        status[[1]][1] + moves[[direction]][1],
        status[[1]][2] + moves[[direction]][2]
      )
      status <- find_status(status, rope_size)
      result <- append(result, status[rope_size])
    }
  }
  result
}

move_rope(input, 2) |> 
  map_chr(~paste(.x, collapse=",")) |> 
  unique() |> 
  length()


## Part 2
move_rope(input, 10) |> 
  map_chr(~paste(.x, collapse=",")) |> 
  unique() |> 
  length()
