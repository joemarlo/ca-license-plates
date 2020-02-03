library(tidyverse)
library(tidytext)
library(igraph)
library(ggraph)
source("Analyses/Helper_functions.R")
set.seed(44)

# cosine similarity -------------------------------------------------------

# read in the data then filter to plates that were either accepted
#  or rejected and add spaces between plate characters so we can split it 
#  it into ngrams later
app.plates <- read_csv("Inputs/applications.csv") %>% 
  filter(status %in% c("Y", "N")) %>% 
  rowid_to_column() %>%
  mutate(plate.sep = gsub(" ", "", plate, fixed = TRUE),
         plate.sep = gsub("*", "\\1 \\2", plate.sep)) %>% 
  rename(id = rowid)

# create the tokenized data
plate.ngrams <- app.plates %>%
  unnest_tokens(input = plate.sep, output = word, token = "skip_ngrams") %>%
  select(id, word) %>% 
  filter(nchar(word) > 4)


cosine_matrix <- function(tokenized_data, lower = 0, upper = 1, filt = 0) {
  # function builds a cosine matrix from the tokenized data
  # https://www.markhw.com/blog/word-similarity-graphs
  
  if (!all(c("word", "id") %in% names(tokenized_data))) {
    stop("tokenized_data must contain variables named word and id")
  }
  
  if (lower < 0 | lower > 1 | upper < 0 | upper > 1 | filt < 0 | filt > 1) {
    stop("lower, upper, and filt must be 0 <= x <= 1")
  }
  
  docs <- length(unique(tokenized_data$id))
  
  out <- tokenized_data %>%
    count(id, word) %>%
    group_by(word) %>%
    mutate(n_docs = n()) %>%
    ungroup() %>%
    filter(n_docs < (docs * upper) & n_docs > (docs * lower)) %>%
    select(-n_docs) %>%
    mutate(n = 1) %>%
    spread(word, n, fill = 0) %>%
    select(-id) %>%
    as.matrix() %>%
    lsa::cosine()
  
  filt <- quantile(out[lower.tri(out)], filt)
  out[out < filt] <- diag(out) <- 0
  out <- out[rowSums(out) != 0, colSums(out) != 0]
  
  return(out)
}

# calculate the cosine matrix of our ngrams
cos.mat <- cosine_matrix(plate.ngrams, lower = .0045, upper = .80, filt = .80)

# plot the matrix
ngram.graph <- graph_from_adjacency_matrix(cos.mat,
                                           mode = "undirected",
                                           weighted = TRUE)
ngram.graph %>%   
  ggraph(layout = "nicely") +
  geom_edge_link(aes(alpha = weight), show.legend = FALSE) +
  geom_node_label(aes(label = name))

# frequency of denials vs. approved for top covariate ngrams
plate.ngrams %>% 
  filter(word %in% colnames(cos.mat)) %>% 
  inner_join(app.plates, by = "id") %>% 
  select(id, word, status) %>% 
  group_by(word, status) %>% 
  count() %>% 
  group_by(word) %>% 
  mutate(score = n / sum(n)) %>% 
  ggplot(aes(x = status, y = score)) +
  geom_col() +
  facet_wrap(~word)

# plot for rejected plates
plate.ngrams %>% 
  left_join(app.plates[, c("id", "status")]) %>% 
  filter(status == "N") %>% 
  cosine_matrix(lower = .0045, upper = .80, filt = .80) %>% 
  graph_from_adjacency_matrix(mode = "undirected", weighted = TRUE) %>%
  ggraph(layout = "nicely") +
  geom_edge_link(aes(alpha = weight), show.legend = FALSE) +
  geom_node_label(aes(label = name))
  

# clustering

walktrap_topics <- function(g, ...) {
  # https://www.markhw.com/blog/word-similarity-graphs
  
  wt <- igraph::cluster_walktrap(g, ...)
  
  membership <- igraph::cluster_walktrap(g, ...) %>% 
    igraph::membership() %>% 
    as.matrix() %>% 
    as.data.frame() %>% 
    rownames_to_column("word") %>% 
    arrange(V1) %>% 
    rename(group = V1)
  
  dendrogram <- stats::as.dendrogram(wt)
  
  return(list(membership = membership, dendrogram = dendrogram))
}

topics <- walktrap_topics(ngram.graph)

# png("Plots/dendrogram.png", width = 1500, height = 1200)
par(cex = 1.6)
plot(topics$dendrogram)
# dev.off()

# grab the identifiers for the clusters
V(ngram.graph)$cluster <- arrange(topics$membership, word)$group

# plot the matrix but color by cluster
ngram.graph %>%   
  ggraph(layout = "nicely") +
  geom_edge_link(aes(alpha = weight), show.legend = FALSE) +
  geom_node_label(aes(label = name,
                      color = factor(cluster)),
                  show.legend = FALSE)

# ggsave(filename = "Plots/clustered_cloud.svg",
#        plot = last_plot(),
#        device = "svg",
#        width = 8,
#        height = 7)

# frequency of denials vs. approved by cluster
plate.ngrams %>% 
  filter(word %in% colnames(cos.mat)) %>% 
  inner_join(app.plates, by = "id") %>% 
  select(id, word, status) %>% 
  inner_join(topics$membership, by = "word") %>% 
  rename(cluster = group) %>% 
  # group_by(cluster) %>% 
  count(cluster, status) %>% 
  group_by(cluster) %>% 
  mutate(n = n/sum(n)) %>% 
  ungroup() %>% 
  mutate(cluster = factor(paste0("Cluster: ", cluster),
                          levels = paste0("Cluster: ", 1:11))) %>% 
  ggplot(aes(x = status, y = n, fill = cluster)) +
  geom_col() +
  facet_wrap(~cluster) +
  labs(title = "Reject (N) vs. accept (Y) rates per cluster",
       x = "",
       y = "") +
  theme(legend.position = 'none')
  
# ggsave(filename = "Plots/rates_by_cluster.svg",
#        plot = last_plot(),
#        device = "svg",
#        width = 8,
#        height = 7)
  