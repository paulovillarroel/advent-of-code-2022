## --- Day 2: Rock Paper Scissors ---

## Part 1
library(tidyverse)

input <- read_delim("inputs/input-02.txt", delim = " ", skip_empty_rows = FALSE, col_names = FALSE) |> 
  unnest(cols = c())

names(input) <- c("opponent", "me")

results <- input |>
  mutate(
    result = case_when(
      opponent == "A" & me == "X" ~ "Draw",
      opponent == "A" & me == "Y" ~ "Win",
      opponent == "A" & me == "Z" ~ "Lose",
      opponent == "B" & me == "X" ~ "Lose",
      opponent == "B" & me == "Y" ~ "Draw",
      opponent == "B" & me == "Z" ~ "Win",
      opponent == "C" & me == "X" ~ "Win",
      opponent == "C" & me == "Y" ~ "Lose",
      opponent == "C" & me == "Z" ~ "Draw"
    ),
    outcome = case_when(
      result == "Win" ~ 6,
      result == "Draw" ~ 3,
      result == "Lose" ~ 0
    ),
    shape_points = case_when(
      me == "X" ~ 1,
      me == "Y" ~ 2,
      me == "Z" ~ 3
    ),
    score = outcome + shape_points
  )
    
sum(results$score)


## Part 2
results <- results |>
  mutate(
    new_shape = case_when(
      opponent == "A" & me == "X" ~ "Z",
      opponent == "A" & me == "Y" ~ "X",
      opponent == "A" & me == "Z" ~ "Y",
      opponent == "B" & me == "X" ~ "X",
      opponent == "B" & me == "Y" ~ "Y",
      opponent == "B" & me == "Z" ~ "Z",
      opponent == "C" & me == "X" ~ "Y",
      opponent == "C" & me == "Y" ~ "Z",
      opponent == "C" & me == "Z" ~ "X",
    ),
    new_result = case_when(
      opponent == "A" & new_shape == "X" ~ "Draw",
      opponent == "A" & new_shape == "Y" ~ "Win",
      opponent == "A" & new_shape == "Z" ~ "Lose",
      opponent == "B" & new_shape == "X" ~ "Lose",
      opponent == "B" & new_shape == "Y" ~ "Draw",
      opponent == "B" & new_shape == "Z" ~ "Win",
      opponent == "C" & new_shape == "X" ~ "Win",
      opponent == "C" & new_shape == "Y" ~ "Lose",
      opponent == "C" & new_shape == "Z" ~ "Draw"
    ),
    new_outcome = case_when(
      new_result == "Win" ~ 6,
      new_result == "Draw" ~ 3,
      new_result == "Lose" ~ 0
    ),
    new_shape_points = case_when(
      new_shape == "X" ~ 1,
      new_shape == "Y" ~ 2,
      new_shape == "Z" ~ 3
    ),
    new_score = new_outcome + new_shape_points
  )

sum(results$new_score)
