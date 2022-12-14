## --- Day 13: Distress Signal ---

## Part 1

library(tidyverse)

input <- read_lines("inputs/input-13.txt")

input_processed <- input |>
  gsub(x = _, "\\[", "list(") |>
  gsub(x = _, "\\]", ")") |>
  as.tibble() |>
  mutate(pair = cumsum(value == "") + 1) |>
  filter(value != "") |>
  mutate(packet = map(value, ~ eval(parse(text = .))))

compare <- function(x, y) {
  if (is.numeric(x) && is.numeric(y)) {
    return(sign(y - x))
  }

  if (!is.list(x)) x <- list(x)
  if (!is.list(y)) y <- list(y)

  for (i in seq_len(length(x))) {
    if (i > length(y)) {
      return(-1)
    }

    comp <- compare(x[[i]], y[[i]])
    if (comp != 0) {
      return(comp)
    }
  }

  if (length(x) == length(y)) {
    return(0)
  }
  return(1)
}

packets <- input_processed |>
  group_by(pair) |>
  summarise(
    packet1 = packet[1],
    packet2 = packet[2]
  ) |>
  mutate(comp = map2_dbl(packet1, packet2, compare)) |>
  filter(comp != -1) |>
  summarise(sum(pair))

packets


## Part 2
divitions <- input_processed |> 
  mutate(div1 = map_dbl(packet, compare, list(list(2))),
         div2 = map_dbl(packet, compare, list(list(6)))) |> 
  summarise(position1 = sum(div1 == 1) + 1,
            position2 = sum(div2 == 1) + 2)

divitions$position1 * divitions$position2
