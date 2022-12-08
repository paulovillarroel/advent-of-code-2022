## --- Day 8: Treetop Tree House ---

## Part 1

library(tidyverse)
library(adventdrob)

input <- read_delim("inputs/input-08.txt", delim = "\n", skip_empty_rows = FALSE, col_names = FALSE) |>
  unnest(cols = c())

grid <- input |> 
  grid_tidy(X1)

## Strongly based in code of https://jrosell.github.io/AdventOfCode/2022/08.html

visible_vec <- function(line) {
  line |> 
    accumulate(max) |> 
    lag(default = -1)
}

is_visible <- function(line) {
  line > visible_vec(line)
}

grid |> 
  group_by(col) |> 
  mutate(top = is_visible(value)) |> 
  mutate(bottom = rev(is_visible(rev(value)))) |> 
  group_by(row) |> 
  mutate(left = is_visible(value)) |> 
  mutate(right = rev(is_visible(rev(value)))) |> 
  ungroup() |> 
  mutate(visible = top + bottom + left + right) |> 
  summarise(visible = sum(visible > 0))


## Part 2
direcctions_up <- function(df, x, y) {
  df |> 
    filter(row < x, col == y) |> 
    pull(value) |> 
    rev() |> 
    list()
}

direcctions_down <- function(df, x, y) {
  df |> 
    filter(row > x, col == y) |> 
    pull(value) |> 
    list()
}

direcctions_right <- function(df, x, y) {
  df |> 
    filter(row == x, col > y) |> 
    pull(value) |> 
    list()
}

direcctions_left <- function(df, x, y) {
  df |> 
    filter(row == x, col < y) |> 
    pull(value) |> 
    rev() |> 
    list()
}

distances <- function(value, row, col, up, left, down, right) {
  d_up <- d_down <- d_right <- d_left <- 0
  if (length(up) > 0) d_up <- value > visible_vec(unlist(up))
  if (length(left) > 0) d_left <- value > visible_vec(unlist(left))
  if (length(down) > 0) d_down <- value > visible_vec(unlist(down))
  if (length(right) > 0) d_right <- value > visible_vec(unlist(right))
  list(c(sum(d_up), sum(d_left), sum(d_down), sum(d_right)))
}

scores <- grid |> 
  filter(value > 5) |> 
  rowwise() |> 
  mutate(up = direcctions_up(grid, row, col)) |> 
  mutate(down = direcctions_down(grid, row, col)) |> 
  mutate(right = direcctions_right(grid, row, col)) |> 
  mutate(left = direcctions_left(grid, row, col)) |> 
  mutate(distances = distances(
    value, row, col, up, left, down, right
  )) |> 
  mutate(scenic_score = prod(distances)) |> 
  arrange(-scenic_score)

head(scores)
