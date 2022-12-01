
# https://adventofcode.com/

library(tidyverse)

# part 1

input <- readLines("input.txt")

input <- as.numeric(input)

df <- tibble(
  input = input
)

df |>
  mutate(flag = if_else(is.na(input), 1, 0)) |>
  mutate(elf = cumsum(flag)) |>
  group_by(elf) |>
  summarise(calories = sum(input, na.rm = TRUE)) |>
  arrange(desc(calories)) |>
  slice_head(n = 1)

# part 2

df |>
  mutate(flag = if_else(is.na(input), 1, 0)) |>
  mutate(elf = cumsum(flag)) |>
  group_by(elf) |>
  summarise(calories = sum(input, na.rm = TRUE)) |>
  arrange(desc(calories)) |>
  slice_head(n = 3) |>
  summarise(calories = sum(calories))
