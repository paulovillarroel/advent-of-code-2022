## --- Day 10: Cathode-Ray Tube ---

## Part 1

library(tidyverse)

input <- read.table("inputs/input-10.txt", fill = TRUE, col.names = c("op", "val"))

cpu <- input |> 
  add_row(op = "init", val = 1, .before = 1) |> 
  mutate(x = accumulate(val, sum, na.rm = TRUE),
         cycle = cumsum(1 + (op == "addx")) -1)

cycles <- c(20, 60, 100, 140, 180, 220)

cpu |> 
  filter(cycle %in% map_dbl(cycles, ~ max(cycle[cycle < .x]))) |> 
  mutate(strenght = cycles * x) |> 
  summarise(sum(strenght))


## Part 2
cpu |> 
  complete(cycle = full_seq(cycle, 1)) |> 
  fill(x) |> 
  mutate(row = cycle %/% 40, col = cycle %% 40) |> 
  filter(abs(col - x) <= 1) |> 
  ggplot(aes(col, row)) +
  geom_tile() +
  scale_y_reverse() +
  coord_fixed()
