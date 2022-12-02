
library(tidyverse)

data <- readLines("input.txt")

df <- tibble(
  text = data
)

df |>
  separate(text,
           into = c("first", "second"),
           sep = " ") |>
  mutate(
    my_selection = case_when(
      second == "X" ~ 1,
      second == "Y" ~ 2,
      second == "Z" ~ 3
    ),
    outcome = case_when(
      first == "A" & second == "Y" ~ 6, # rock paper
      first == "B" & second == "Y" ~ 3, # paper paper
      first == "C" & second == "Y" ~ 0, # sci paper
      first == "A" & second == "X" ~ 3, # rock rock
      first == "B" & second == "X" ~ 0, # paper rock
      first == "C" & second == "X" ~ 6, # scissors rock
      first == "A" & second == "Z" ~ 0, # rock scissors
      first == "B" & second == "Z" ~ 6, # paper scissors
      first == "C" & second == "Z" ~ 3  # sci sci
    ),
    total = outcome + my_selection
  ) |>
  summarize(total = sum(total))

df |>
  separate(text,
           into = c("first", "outcome"),
           sep = " ") |>
  mutate(
    second = case_when(
      first == "A" & outcome == "Y" ~ "X",
      first == "B" & outcome == "Y" ~ "Y",
      first == "C" & outcome == "Y" ~ "Z",
      first == "A" & outcome == "Z" ~ "Y",
      first == "B" & outcome == "Z" ~ "Z",
      first == "C" & outcome == "Z" ~ "X",
      first == "A" & outcome == "X" ~ "Z",
      first == "B" & outcome == "X" ~ "X",
      first == "C" & outcome == "X" ~ "Y",
    ),
    my_selection = case_when(
      second == "X" ~ 1,
      second == "Y" ~ 2,
      second == "Z" ~ 3
    ),
    outcome_score = case_when(
      first == "A" & second == "Y" ~ 6, # rock paper
      first == "B" & second == "Y" ~ 3, # paper paper
      first == "C" & second == "Y" ~ 0, # sci paper
      first == "A" & second == "X" ~ 3, # rock rock
      first == "B" & second == "X" ~ 0, # paper rock
      first == "C" & second == "X" ~ 6, # scissors rock
      first == "A" & second == "Z" ~ 0, # rock scissors
      first == "B" & second == "Z" ~ 6, # paper scissors
      first == "C" & second == "Z" ~ 3  # sci sci
    ),
    total = my_selection + outcome_score
  ) |>
  summarize(total = sum(total))
