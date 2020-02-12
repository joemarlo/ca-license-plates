library(tidyverse)
library(tidytext)
library(igraph)
library(ggraph)
library(stringdist)
library(knitr)
source("Analyses/Helper_functions.R")
set.seed(44)

# read in the data then filter to plates that were either accepted
#  or rejected and add spaces between plate characters so we can split it 
#  it into ngrams later
app.plates <- read_csv("Inputs/applications.csv") %>% 
  filter(status %in% c("Y", "N")) %>% 
  rowid_to_column() %>%
  rename(id = rowid) %>% 
  mutate(source = 'CA')

faux.plates <- replicate(25000, generate_plate()) %>% 
  enframe() %>% 
  select(value) %>% 
  rename(plate = value) %>% 
  mutate(status = "Y",
         source = 'generated')
  
all.plates <- app.plates %>% 
  select(plate, status, source) %>% 
  bind_rows(faux.plates)

# create tokenzied data: custom method
plate.ngrams <- all.plates %>% 
  rowwise() %>%
  mutate(word = list(parse_plate(plate, ngram.nchar = 2:4))) %>%
  ungroup() %>% 
  unnest(word)

bad.words <- read_delim("Data/bad_words.txt",
                        delim = "\n",
                        col_names = FALSE) %>% 
  pull()


# test if there is a perfect match, and calculate two string distance measures
#   between the ngram and it's best match in the bad.words list
plate.ngrams <- plate.ngrams %>% 
  select(word) %>% 
  distinct() %>% 
  mutate(perfect.match = word %in% bad.words) %>%
  rowwise() %>% 
  mutate(soundex = stringsim(word, bad.words, method = 'soundex') %>% max(),
         osa = stringsim(word, bad.words, method = 'osa') %>% max()) %>%
  ungroup() %>% 
  right_join(plate.ngrams, by = 'word') %>% 
  select(source, plate, word, perfect.match, soundex, osa) %>% 
  left_join(app.plates[, c("plate", "status")], by = 'plate')


score_plate <- function(perfect.match, soundex, osa){
  plate.ngrams %>% 
    filter(plate == plate) %>% 
    
}

scored.plates <- plate.ngrams %>% 
  group_by(plate, status) %>% 
  mutate(score = if_else(max(perfect.match) == 1, "N", 
                         if_else(max(soundex * .8 + osa * .2) > 0.95, "N", "Y"))) %>% 
  summarize(score = max(score)) %>% 
  xtabs(~status + score, data = .)

  



#  then summarize the string dist results by plate 
summ.ngrams <- plate.ngrams %>%
  left_join(app.plates[, c("plate", "status")], by = 'plate') %>% 
  mutate(status = recode(status, Y = "Plate approved", N = "Plate rejected")) %>% 
  pivot_longer(cols = c("soundex", "osa", "perfect.match"), names_to = "algo") %>% 
  group_by(plate, status, algo) %>% 
  summarize(Maximum = max(value),
            Nintieth.percentile = quantile(value, .90),
            Fiftieth.percentile = quantile(value, .50),
            Twentieth.percentile = quantile(value, .10)) %>%
  ungroup()

