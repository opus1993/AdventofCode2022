library(tidyverse)

signal <- readLines("input.txt") |> as_tibble() |>
  rownames_to_column(var = "cycle") |>
  separate(
    value,
    sep = " ",
    into = c("instr", "value"),
    fill = "right",
    convert = TRUE
  ) |>
  mutate(idx = cumsum(if_else(instr == "addx", 2L, 1L)) + 1) |>
  mutate(value = replace_na(value, 0))

dummy <- signal |>
  filter(instr == "addx") |>
  mutate(value = 0, idx = idx - 1)

x_positions <- bind_rows(signal, dummy) |>
  arrange(idx) |>
  mutate(x_pos = cumsum(value) + 1)

x_positions |>
  filter(idx %% 40 == 20) |>
  summarize(sum(x_pos * idx))

# part 2

x_positions |>
  mutate(x = (row_number() - 1) %% 40,
         y = (row_number() - 1) %/% 40) |>
  filter(abs(x - lag(x_pos, default = 1)) <= 1) |>
  ggplot(aes(x,-y)) + geom_tile()
