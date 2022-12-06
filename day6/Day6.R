library(tidyverse)

string <- readLines("input.txt")

for (i in 1:nchar(string)) {

  substring <- str_sub(string, start = i,
          end = i + 3)

  substring_v <- unique(str_split(substring, "", simplify = TRUE)[1,])

  if (length(substring_v) == 4) {
    print(i + 3)
    break
  }

}

for (i in 1:nchar(string)) {

  substring <- str_sub(string, start = i,
                       end = i + 13)

  substring_v <- unique(str_split(substring, "", simplify = TRUE)[1,])

  if (length(substring_v) == 14) {
    print(i + 13)
    break
  }

}
