## --- Day 5: Supply Stacks ---

## Part 1
library(tidyverse)

input <- read_delim("inputs/input-05.txt", delim = "\n", skip_empty_rows = FALSE, col_names = FALSE) |>
  unnest(cols = c())

moves <- input |> 
  filter(str_detect(X1, "move")) |> 
  extract(X1, c("count", "from", "to"), "move (\\d+) from (\\d+) to (\\d+)") |> 
  mutate_if(is.character, as.numeric)

# Code heavily influenced by David Robinson and his grid_tidy() function https://github.com/dgrtwo/adventdrob/blob/main/R/grid.R"
stacks <- input |> 
  filter(!str_detect(X1, "move")) |> 
  mutate(row = row_number()) |> 
  mutate(value = stringr::str_split(X1, "")) |> 
  select(-X1) |> 
  unnest(value) |>
  mutate(col = rep(1:35, times = 9)) |>
  filter(str_detect(value, "[A-Z]")) |> 
  arrange(desc(row)) |> 
  mutate(crate = match(col, unique(sort(col)))) |> 
  select(crate, value) |> 
  group_by(crate) |> 
  summarise(crates = list(value)) |> 
  deframe()

for (i in 1:nrow(moves)){
  
  movement <- moves[i, ]
  
  to_move <- rev(tail(stacks[[movement$from]], movement$count))
  stacks[[movement$from]] <- head(stacks[[movement$from]], -movement$count)
  stacks[[movement$to]] <- c(stacks[[movement$to]], to_move)
}

stacks |> 
  map_chr(last) |> 
  paste(collapse = "")


## Part 2
stacks <- input |> 
  filter(!str_detect(X1, "move")) |> 
  mutate(row = row_number()) |> 
  mutate(value = stringr::str_split(X1, "")) |> 
  select(-X1) |> 
  unnest(value) |>
  mutate(col = rep(1:35, times = 9)) |>
  filter(str_detect(value, "[A-Z]")) |> 
  arrange(desc(row)) |> 
  mutate(crate = match(col, unique(sort(col)))) |> 
  select(crate, value) |> 
  group_by(crate) |> 
  summarise(crates = list(value)) |> 
  deframe()

for (i in 1:nrow(moves)){
  
  movement <- moves[i, ]
  
  to_move <- tail(stacks[[movement$from]], movement$count)
  stacks[[movement$from]] <- head(stacks[[movement$from]], -movement$count)
  stacks[[movement$to]] <- c(stacks[[movement$to]], to_move)
}

stacks |> 
  map_chr(last) |> 
  paste(collapse = "")
