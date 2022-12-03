
library(tidyverse)

raw <- as_tibble(readLines(here::here("input.txt")))

dataframe <- raw |>
  mutate(length = str_length(value),
         first = str_sub(value, start = 0L, end = length/2) ,
         end = str_sub(value, start = 1 + length/2, end = length ))


matcher <- function(string){
  x <- string |>
    str_squish() |>
    str_split("", simplify = TRUE)

 return(unique(x[1,]))

}


for (i in 1:nrow(dataframe)) {
dataframe$type[i] <- matcher(dataframe$first[i])[
   matcher(dataframe$first[i]) %in% matcher(dataframe$end[i])]
}

dataframe |>
   inner_join(tibble(type = c(letters, str_to_upper(letters)) , priority =  1:52)) |>
  summarise(sum = sum(priority))

# part 2

dataframe <- raw |>
  add_rownames() |>
  mutate(elf_group = (as.integer(rowname) - 1 ) %/% 3,
         elf_pos = as.integer(rowname) - elf_group * 3) |>
  select(-rowname) |>
  pivot_wider(names_from = elf_pos,values_from = value)

for (i in 1:nrow(dataframe)) {
  dataframe$group_id[i] <- matcher(dataframe$`1`[i])[
    matcher(dataframe$`1`[i]) %in%
         matcher(dataframe$`2`[i])[
           matcher(dataframe$`2`[i]) %in% matcher(dataframe$`3`[i])
         ]
    ]
}

dataframe |>
  inner_join(tibble(group_id = c(letters, str_to_upper(letters)) , priority =  1:52)) |>
  summarise(sum = sum(priority))


### David Robinson's work

raw |>
  mutate(first = map2_chr(value, str_length(value) / 2, ~ str_sub(.x, 1, .y)),
         second = map2_chr(value, str_length(value) / 2, ~ str_sub(.x, .y + 1))) |>
  mutate(first = str_split(first, ""),
         second = str_split(second, "")) |>
  mutate(intersect = map2_chr(first, second, intersect)) |>
  summarize(result = sum(match(intersect, c(letters,LETTERS))))

raw |>
  mutate(spl = str_split(value, "")) |>
  group_by(g = (row_number() - 1) %/% 3 ) |>
  summarize(intersect = reduce(spl, intersect)) |>
  summarize(result = sum(match(intersect, c(letters, LETTERS))))





