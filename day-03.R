## --- Day 3: Rucksack Reorganization ---

## Part 1
library(tidyverse)
library(stringi)

input <- read_delim("inputs/input-03.txt", delim = "\n", skip_empty_rows = FALSE, col_names = FALSE) |>
  unnest(cols = c())

names(input) <- "items"

for (items in input) {
  half_1 <- substr(items, start = 1, stop = nchar(items) / 2)
  half_2 <- substr(items, start = nchar(items) / 2 + 1, stop = nchar(items))
  items_separated <- data.frame(half_1, half_2)
}

items_separated <- items_separated |>
  mutate(
    items_1 = strsplit(half_1, ""),
    items_2 = strsplit(half_2, "")
  ) |>
  rowwise() |>
  mutate(
    common_item = unique(list(unlist(items_1)[unlist(items_1) %in% unlist(items_2)])),
    common_item = ifelse(is.list(common_item), unnest(common_item), common_item)
  )

common_item <- items_separated |>
  select(common_item)

priority_matrix <- data.frame(
  item = c(letters, LETTERS),
  points = 1:52
)

priorities <- left_join(common_item, priority_matrix, by = c("common_item" = "item"))

sum(priorities$points)


## Part 2
teams <- input |>
  mutate(
    team = rep(1:(nrow(input) / 3), each = 3),
    items = strsplit(items, "")
  )

rucksacks <- list()
common_letter <- list()

for (i in 1:100) {
  rucksacks[[i]] <- teams |>
    filter(team == i)

  line_1 <- rucksacks[[i]]$items[1] |>
    unlist()

  line_2 <- rucksacks[[i]]$items[2] |>
    unlist()

  line_3 <- rucksacks[[i]]$items[3] |>
    unlist()

  first_comparison <- unique(line_1[line_1 %in% line_2])
  second_comparison <- unique(first_comparison[first_comparison %in% line_3])
  common_letter[[i]] <- second_comparison
}

common_letter <- common_letter |>
  unlist() |>
  tibble() |>
  rename(letter = "unlist(common_letter)")

priorities_rucksacks <- left_join(common_letter, priority_matrix, by = c("letter" = "item"))

sum(priorities_rucksacks$points)
