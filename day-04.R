## --- Day 4: Camp Cleanup ---

## Part 1
library(tidyverse)

input <- read_delim("inputs/input-04.txt", delim = ",", skip_empty_rows = FALSE, col_names = FALSE) |>
  unnest(cols = c())

names(input) <- c("elf_1", "elf_2")

assignments <- input |>
  separate(elf_1, into = c("start_1", "end_1"), sep = "-") |>
  separate(elf_2, into = c("start_2", "end_2"), sep = "-") |>
  mutate_if(is.character, as.numeric) |>
  mutate(fully_overlap = ifelse((start_1 >= start_2 & end_1 <= end_2) | (start_2 >= start_1 & end_2 <= end_1), 1, 0))

sum(assignments$fully_overlap)


## Part 2
assignments <- assignments |> 
  mutate(overlap = ifelse((end_1 >= start_2 & end_2 >= start_1) | (start_1 <= end_2 & end_1 >= start_2), 1, 0))

sum(assignments$overlap)
