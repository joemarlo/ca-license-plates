
# custom theme for ggplot
theme_custom <- function() {
  ggplot2::theme_gray() +
    theme(
      panel.grid.minor.y = element_line(color = NA),
      panel.grid.major.y = element_line(color = "gray95"),
      panel.grid.minor.x = element_line(color = NA),
      panel.grid.major.x = element_line(color = "gray95"),
      panel.background = element_rect(fill = NA),
      plot.background = element_rect(
        fill = NA,
        color = "gray95",
        size = 10
      ),
      plot.margin = unit(c(1, 1, 1, 1), "cm"),
      axis.title = element_text(color = "gray30"),
      axis.ticks = element_line(color = NA),
      strip.background = element_rect(fill = "gray95"),
      strip.text = element_text(
        color = "gray30",
        size = 11,
        face = "bold"
      ),
      plot.title = element_text(color = "gray30",
                                face = "bold"),
      plot.subtitle = element_text(size = 10,
                                   color = "gray30"),
      text = element_text(family = "Helvetica")
    )
}

ggplot2::theme_set(theme_custom())


fix_NA <- function(x, replacement.value = "-") {
  # replaces NAs with replacement.value
  
  x[is.na(x)] <- replacement.value
  return(x)
}


generate_plate <- function(plate.nchar = 7) {
  # function generates a single random string
  #  of 7 letters and numbers 
  
  index <- sample(c(TRUE, FALSE), size = plate.nchar, replace = TRUE)
  plate <- NA
  plate[index] <- sample(LETTERS, size = plate.nchar, replace = TRUE)[index]
  plate[!index] <- sample(0:9, size = plate.nchar, replace = TRUE)[!index]
  return(paste0(plate, collapse = ""))
}

parse_plate <- function(plate, ngram.nchar = 2:nchar(plate)) {
  # function takes a single plate and returns all 
  #  "forward" combinations of the letters
  # e.g. plate "1234" would return "12, 123, 1234, 23, 234, 34"
  # ngram.nchar is the amount of characters the ngrams should be
  
  # remove all spaces
  plate <- gsub(" ", "", plate, fixed = TRUE)
  
  tokens <- c()
  # if plate is just one character
  if (nchar(plate) == 1) {
    tokens <- plate
  } else {
    plate.split <- strsplit(plate, split = NULL)[[1]]
    len <- length(plate.split)
    
    # index for storing results
    i <- 1
    # loop through the plate and take every "forward"
    #  combination of letters
    for (bgn in 1:(len - 1)) {
      for (end in (bgn + 1):len) {
        tokens[[i]] <- paste0(plate.split[bgn:end], collapse = "")
        i <- i + 1
      }
    }
  }
  
  tokens <- tokens[nchar(tokens) %in% ngram.nchar]
  tokens <- unique(tokens) 
  tokens <- tolower(tokens)
  return(tokens)
}

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


walktrap_topics <- function(g, ...) {
  # https://www.markhw.com/blog/word-similarity-graphs
  # function computes a wallktrap cluster and returns 
  #  the membership and dendrogram
  
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

