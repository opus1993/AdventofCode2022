library(tidyverse)

camp_section <- as_tibble(readLines("input.txt"))

camp_section |>
  separate(value, ",|-",
           into = c("first_start",
                    "first_end",
                    "second_start",
                    "second_end")) |>
  mutate(across(everything(), as.numeric)) |>
  mutate(inside_other = case_when(
        first_start >= second_start &
          first_end <= second_end ~ TRUE,
        second_start >= first_start &
          second_end <= first_end ~ TRUE,
        TRUE ~ FALSE
      )
  ) |>
  summarise(answer = sum(inside_other))

camp_section |>
  separate(value, ",|-",
           into = c("first_start",
                    "first_end",
                    "second_start",
                    "second_end")) |>
  mutate(across(everything(), as.numeric)) |>
  mutate(inside_other = case_when(
    first_end >= second_start &
      first_start <= second_start ~ TRUE,
    second_end >= first_start &
      second_start <= first_start ~ TRUE,
    TRUE ~ FALSE
  )
  )|>
  summarise(answer = sum(inside_other))
