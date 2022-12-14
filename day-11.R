## --- Day 11: Monkey in the Middle ---

## Part 1

library(tidyverse)

input <- read_lines("inputs/input-11.txt")

monkeys <- list()
current_monkey <- 0

for (line in input) {
  if (grepl("Monkey \\d+:", line)) {
    current_monkey <- current_monkey + 1
    monkeys[[current_monkey]] <- list()
  } else if (grepl("Starting items:", line)) {
    monkeys[[current_monkey]][["items"]] <- line |> 
      str_replace("  Starting items: ", "") |> 
      str_split(", ") |> 
      unlist() |> 
      as.numeric()
  } else if (grepl("Operation:", line)) {
    monkeys[[current_monkey]][["operation"]] <- line |> 
      str_replace("  Operation: new = ", "") |> 
      paste0("function(old){return(", ., ")}") |> 
      parse(text = .) |> 
      eval()
  } else if (grepl("Test: ", line)) {
    monkeys[[current_monkey]][["test div"]] <- line |> 
      str_replace("  Test: divisible by ", "") |> 
      as.numeric()
  } else if (grepl("If true:", line)) {
    monkeys[[current_monkey]][["true throw"]] <- line |> 
      str_replace("    If true: throw to monkey ", "") |> 
      as.numeric() |> 
      sum(., 1)
  } else if (grepl("If false:", line)) {
    monkeys[[current_monkey]][["false throw"]] <- line |> 
      str_replace("    If false: throw to monkey ", "") |> 
      as.numeric() |> 
      sum(., 1)
  }
}

for (i in 1:length(monkeys)) {
  monkeys[[i]][["inspection count"]] <- 0
}

monkeys_bkup <- monkeys

watch_monkeys <- function(monkeys, n_rounds, chill_func){
  for(round in 1:n_rounds){
    for (turn in 1:length(monkeys)){
      while (length(monkeys[[turn]][["items"]] > 0)){
        
        item <- monkeys[[turn]][["items"]][1]
        monkeys[[turn]][["items"]] <- monkeys[[turn]][["items"]][-1]
        
        inspected_item <- item %>% monkeys[[turn]][["operation"]](.) %>% chill_func
        monkeys[[turn]][["inspection count"]] <- monkeys[[turn]][["inspection count"]] + 1
        
        if (inspected_item %% monkeys[[turn]][["test div"]] == 0){
          target_monkey <- monkeys[[turn]][["true throw"]]
        } else {
          target_monkey <- monkeys[[turn]][["false throw"]]
        }
        
        monkeys[[target_monkey]][["items"]] <- c(monkeys[[target_monkey]][["items"]], inspected_item)
      }
    }
  }
  return(monkeys)
}

monkeys <- watch_monkeys(monkeys_bkup, 20, function(x) {
  return(x %/% 3)
})

sapply(monkeys, function(x) {
  x[["inspection count"]]
}) %>%
  sort(decreasing = TRUE) %>%
  .[1:2] %>%
  prod()


# Part 2
gcd <- sapply(monkeys, function(x) {
  x[["test div"]]
}) |> prod()

monkeys <- watch_monkeys(monkeys_bkup, 10000, function(x) {
  return(x %% gcd)
})

sapply(monkeys, function(x) {
  x[["inspection count"]]
}) %>%
  sort(decreasing = TRUE) %>%
  .[1:2] %>%
  prod()
