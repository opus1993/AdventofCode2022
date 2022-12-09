library(tidyverse)

# How many positions does the tail of the rope visit at least once?

moves <- readLines("input.txt") |> as_tibble() |>
  separate(value, sep = " ", into = c("dir","steps"), convert = TRUE) |>
  mutate(steps = as.integer(steps)) |>
  uncount(steps) |>
  pull(dir)

head_pos <- c(row = 0, col = 0)
tail_pos <- c(row = 0, col = 0)
# track positions of tail, including 0 0
pos <- c(character(length(moves)),"0 0")

i <- 0

for (move in moves) {

  i <- i + 1

   if (move == "D") head_pos[1] <- head_pos[1] - 1
   if (move == "U") head_pos[1] <- head_pos[1] + 1
   if (move == "L") head_pos[2] <- head_pos[2] - 1
   if (move == "R") head_pos[2] <- head_pos[2] + 1

  # distance
   diff <- head_pos - tail_pos

   # move tail by one step towards head in the relevant direction
   # sign() returns 1, 0, or -1 to represent +, =, -
   if (any(abs(diff) > 1))  tail_pos <- tail_pos + sign(diff)

   # store tail pos
   pos[i] <- paste(tail_pos, collapse = " ")

}

tibble(values = pos) |>
  separate(values, sep = " ", into = c("x","y")) |>
  ggplot(aes(x, y)) +
  geom_point()

length(unique(pos))

# part 2, now with a rope of length 10

pos_1 <- c(row = 0, col = 0)
pos_2 <- c(row = 0, col = 0)
pos_3 <- c(row = 0, col = 0)
pos_4 <- c(row = 0, col = 0)
pos_5 <- c(row = 0, col = 0)
pos_6 <- c(row = 0, col = 0)
pos_7 <- c(row = 0, col = 0)
pos_8 <- c(row = 0, col = 0)
pos_9 <- c(row = 0, col = 0)
pos_t <- c(row = 0, col = 0)
pos <- c(character(length(moves)),"0 0")
i <- 0


for (move in moves) {

  i <- i + 1

  if (move == "L") pos_1[2] <- pos_1[2] - 1
  if (move == "R") pos_1[2] <- pos_1[2] + 1
  if (move == "U") pos_1[1] <- pos_1[1] + 1
  if (move == "D") pos_1[1] <- pos_1[1] - 1

  diff2 <- pos_1 - pos_2
  if (any(abs(diff2) > 1))  pos_2 <- pos_2 + sign(diff2)
  diff3 <- pos_2 - pos_3
  if (any(abs(diff3) > 1))  pos_3 <- pos_3 + sign(diff3)
  diff4 <- pos_3 - pos_4
  if (any(abs(diff4) > 1))  pos_4 <- pos_4 + sign(diff4)
  diff5 <- pos_4 - pos_5
  if (any(abs(diff5) > 1))  pos_5 <- pos_5 + sign(diff5)
  diff6 <- pos_5 - pos_6
  if (any(abs(diff6) > 1))  pos_6 <- pos_6 + sign(diff6)
  diff7 <- pos_6 - pos_7
  if (any(abs(diff7) > 1))  pos_7 <- pos_7 + sign(diff7)
  diff8 <- pos_7 - pos_8
  if (any(abs(diff8) > 1))  pos_8 <- pos_8 + sign(diff8)
  diff9 <- pos_8 - pos_9
  if (any(abs(diff9) > 1))  pos_9 <- pos_9 + sign(diff9)
  difft <- pos_9 - pos_t
  if (any(abs(difft) > 1))  pos_t <- pos_t + sign(difft)

  pos[i] <- paste(pos_t,collapse = " ")
}

tibble(values = pos) |>
  separate(values, sep = " ", into = c("x","y")) |>
  ggplot(aes(x, y)) +
  geom_point()


length(unique(pos))
