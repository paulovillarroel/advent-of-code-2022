## --- Day 1: Calorie Counting ---

library(tidyverse)

input <- read_delim("inputs/input-1.txt", delim = "\n", skip_empty_rows = FALSE, col_names = FALSE) |> 
  unnest(cols = c())

input |> 
  mutate(elf = cumsum(lag(is.na(X1), default = TRUE))) |> 
  group_by(elf) |> 
  summarise(sum_calories = sum(X1, na.rm = TRUE)) |> 
  arrange(desc(sum_calories)) |> 
  head(3)