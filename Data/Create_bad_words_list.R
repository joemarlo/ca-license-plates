library(tidyverse)

# Create list of bad words from these links
# https://www.kaggle.com/nicapotato/bad-bad-words
# https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en


bad.words.1 <-
  read_table(
    "https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en",
    col_names = FALSE
  )

bad.words.2 <- read_csv("Inputs/bad-words.csv", col_names = FALSE)

bad.words <-
  bind_rows(bad.words.1, bad.words.2) %>% 
  filter(X1 != '\U0001f595') %>% 
  pull() %>% 
  unique() %>% 
  sort()

write(bad.words, "Data/bad_words.txt")