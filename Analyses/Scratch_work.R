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

# generate fake plates to balance the dataset between accepted and rejected
# we will assume all of these will be approved although there is a small chance
#   some would accidently include negative phrases and would be rejected in real-life
n.faux <- sum(app.plates$status == "N") - sum(app.plates$status == "Y")

faux.plates <- replicate(n.faux, generate_plate()) %>% 
  enframe() %>% 
  select(value) %>% 
  rename(plate = value) %>% 
  mutate(status = "Y",
         source = 'generated')
rm(n.faux)

# bind back to app.plates to create one dataframe
all.plates <- app.plates %>% 
  select(plate, status, source) %>% 
  bind_rows(faux.plates)

# create tokenzied data: custom method
plate.ngrams <- all.plates %>% 
  rowwise() %>%
  mutate(word = list(parse_plate(plate, ngram.nchar = 2:4))) %>%
  ungroup() %>% 
  unnest(word)

##### need to split the dataset to training and testing

# read in the list of bad words
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
  left_join(all.plates[, c("plate", "status")], by = 'plate')


plate.ngrams %>% 
  group_by(plate, status) %>% 
  # mutate(score = if_else(max(perfect.match) == 1, "N", "Y")) %>% 
  mutate(score = if_else(max(perfect.match) == 1, "N",
                         if_else(max(soundex * .3 + osa * .7) > 0.8, "N", "Y"))) %>%
  summarize(score = max(score)) %>% 
  xtabs(~status + score, data = .) %>% 
  prop.table() %>% 
  round(., 2)

# create grid of various weightings of the soundex and OSA algos
#  along with a threshold to build a aggregate score based on 
#  this formula (soundex score * weight + osa score  * (1 - weight)) > threshold
#  which results in a boolean telling us to accept or reject the plate
grid.search <- expand.grid(sd = seq(0, 1, by = 0.1),
                           threshold = seq(.1, 1, by = 0.1)) %>% 
  mutate(os = 1 - sd) %>% 
  select(sd, os, threshold)
  
error.rates <- pmap(list(grid.search$sd, grid.search$os, grid.search$threshold), 
                    function(sd, os, threshold) {
  # mapping returns type 1 and type 2 rates for each combination of
  #  values in the grid.search grid
                      
  rslts <- plate.ngrams %>%
    group_by(plate, status) %>%
    # mutate(score = if_else(max(perfect.match) == 1, "N", "Y")) %>%
    mutate(score = if_else(
      max(perfect.match) == 1, "N",
      if_else(max(soundex * sd + osa * os) > threshold, "N", "Y")
    )) %>%
    summarize(score = max(score)) %>%
    xtabs(~ status + score, data = .)
  
  TPR <- rslts[2,2] / (rslts[2,2] + rslts[2, 1])
  FPR <- rslts[1,2] / (rslts[1,2] + rslts[1, 1])
  
  return(list(TPR, FPR))
})
  
error.rates <- error.rates %>% 
  unlist() %>% 
  matrix(ncol = 2, byrow = TRUE) %>% 
  as_tibble() %>% 
  rename(TPR = V1,
         FPR = V2) %>% 
  bind_cols(grid.search, .) %>% 
  mutate(Inputs = paste0(sd, '-', os, '-', threshold),
         AUC = DescTools::AUC(x = FPR, y = TPR)) %>% 
  select(FPR, TPR, Inputs, AUC)

error.rates %>% 
  mutate(Total = TPR / FPR) %>% 
  filter(floor(FPR*10) == 2) %>%
  group_by(Total) %>% 
  slice(c(1, n())) %>% 
  ungroup() %>% 
  mutate(label = Inputs) %>% 
  right_join(error.rates) %>% 
  ggplot(aes(x = FPR, y = TPR, label = label)) +
  geom_point() +
  geom_smooth(color = 'grey70',
              se = FALSE) +
  ggrepel::geom_label_repel(alpha = 0.5) +
  labs(title = 'Classifying rejections: ROC curve for edit distance tuning parameters',
       subtitle = paste0('AUC: ', round(error.rates$AUC[[1]], 2)),
       caption = 'format: soundex - osa - threshold') +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1))

ggsave(filename = "Plots/ROC.svg",
       plot = last_plot(),
       device = "svg",
       width = 8,
       height = 7)


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

