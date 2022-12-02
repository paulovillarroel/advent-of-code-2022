## --- Day 1: Calorie Counting ---

## Part 1
library(tidyverse)

input <- read_delim("inputs/input-01.txt", delim = "\n", skip_empty_rows = FALSE, col_names = FALSE) |> 
  unnest(cols = c())

elf_calories <- input |> 
  mutate(elf = cumsum(lag(is.na(X1), default = TRUE))) |> 
  group_by(elf) |> 
  summarise(sum_calories = sum(X1, na.rm = TRUE)) |> 
  arrange(desc(sum_calories))

elf_calories

## Part 2
most_calories <- elf_calories |> 
  head(3)

sum(most_calories$sum_calories)
