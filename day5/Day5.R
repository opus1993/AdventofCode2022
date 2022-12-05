library(tidyverse)

input <- readLines("input.txt")

stacks <- head(input, 8) |>
  str_c(collapse = "\n") |>
  readr::read_fwf(show_col_types = FALSE) |>
  set_names(paste0("s", 1:9)) |>
  as.list() |>
  map(na.omit)

moves <- as_tibble(tail(input,-10)) |>
  mutate(
    move = parse_number(str_sub(value, start = 5)),
    value = str_remove(value, paste0("move ", move)),
    from_col = parse_number(value),
    value = str_remove(value, paste0(" from ", from_col)),
    to_col = parse_number(value)
  ) |>
  select(-value)

s_p1 <- stacks

for (i in 1:nrow(moves)){

  n <- moves$move[i]
  from <- moves$from_col[i]
  to <- moves$to_col[i]

  crates <- head(s_p1[[from]], n)

# make the additions to left sides of the lists
  s_p1[[to]] <- c(rev(crates), s_p1[[to]])
  s_p1[[from]] <- tail(s_p1[[from]], length(s_p1[[from]]) - n)

}

str_c(map(s_p1, ~ head(.x, 1) |>
          str_remove_all("\\[|\\]")), collapse = "")

s_p1 <- stacks

for (i in 1:nrow(moves)){

  n <- moves$move[i]
  from <- moves$from_col[i]
  to <- moves$to_col[i]

  crates <- head(s_p1[[from]], n)

  # make the additions to left sides of the lists
  s_p1[[to]] <- c(crates, s_p1[[to]])
  s_p1[[from]] <- tail(s_p1[[from]], length(s_p1[[from]]) - n)

}

str_c(map(s_p1, ~ head(.x, 1) |>
            str_remove_all("\\[|\\]")), collapse = "")


