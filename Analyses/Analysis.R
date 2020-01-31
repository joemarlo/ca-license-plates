library(tidyverse)
library(ranger)
library(xgboost)
library(vtreat)


# # rejected plates ---------------------------------------------------------
# 
# app.plates <- read_csv("/home/joemarlo/Dropbox/Data/Projects/Other/applications.csv")
# table(app.plates$status)
# 
# max.char <- max(nchar(app.plates$plate))
# 
# parsed.plates <- app.plates[, c("plate", "status")]
# 
# split.lets <- strsplit(parsed.plates$plate, split = NULL)
# split.lets <- lapply(split.lets, function(x){
#   length(x) <- max.char
#   return(x)
# })
# names(split.lets) <- 1:length(split.lets)
# split.lets <- bind_rows(split.lets) %>% t()
# colnames(split.lets) <- paste0("V", 1:max.char)
# 
# parsed.plates <- bind_cols(parsed.plates[, "status"], as_tibble(split.lets))
# 
# # random plates -----------------------------------------------------------
# 
# generate_plate <- function() {
#   # function generates a single random string
#   #  of 7 letters and numbers 
#   
#   bool <- sample(c(TRUE, FALSE), size = 7, replace = TRUE)
#   plate <- NA
#   plate[bool] <- LETTERS[sample(1:26, size = 7, replace = TRUE)][bool]
#   plate[!bool] <- sample(0:9, size = 7, replace = TRUE)[!bool]
#   return(plate)
# }
# 
# # generate n random plates
# rand.plates <- replicate(n = sum(app.plates$status == "N", na.rm = TRUE),
#                     expr = generate_plate()) %>% 
#   t() %>% 
#   as.matrix() %>% 
#   as_tibble()
# plates$status <- "Y"
# 
# 
# # xgboost --------------------------------------------------------------------
# 
# plates <- bind_rows(plates, parsed.plates) %>% select(status, paste0("V", 1:10))
# 
# fix_NA <- function(x) {
#     x[is.na(x)] <- "-"
#     x
# }
# 
# plates <- plates %>% filter(status %in% c("Y", "N"))
# plates <- map_df(plates, fix_NA)
# plates$status <- (plates$status == "Y") %>% as.numeric()
# # plates <- map_df(plates, as.factor)
# 
# # The input variables
# vars <- names(plates)[names(plates) != "status"]
# 
# # Create the treatment plan from bikesJuly
# treatplan <- designTreatmentsZ(plates, vars, verbose = FALSE)
# 
# # Get the "clean" and "lev" variables from the scoreFrame
# newvars <- treatplan %>%
#   magrittr::use_series(scoreFrame) %>%        
#   filter(code %in% c("clean", "lev")) %>%  # get the rows you care about
#   magrittr::use_series(varName)           # get the varName column
# 
# # Prepare the training data
# plates.treat <- prepare(treatplan, plates,  varRestriction = newvars)
# 
# 
# # Run xgb.cv
# # cv <- xgb.cv(data = as.matrix(plates.treat), 
# #              label = plates$status,
# #              nrounds = 100,
# #              nfold = 5,
# #              objective = "binary:logistic",
# #              eta = 0.5,
# #              max_depth = 5,
# #              early_stopping_rounds = 10,
# #              verbose = 0    # silent
# # )
# # 
# # # Get the evaluation log 
# # elog <- cv$evaluation_log
# # 
# # # Determine and print how many trees minimize training and test error
# # elog %>% 
# #   summarize(ntrees.train = which.min(train_rmse_mean),   # find the index of min(train_rmse_mean)
# #             ntrees.test  = which.min(test_rmse_mean))   # find the index of min(test_rmse_mean)
# # nTrees <- elog %>% summarize(ntrees.test = which.min(test_rmse_mean)) %>% as.integer()
# 
# # Run xgboost
# plate_model_xgb <- xgboost(data = as.matrix(plates.treat), # training data as matrix
#                           label = plates$status,  # column of outcomes
#                           nrounds = 50,       # number of trees to build
#                           objective = "binary:logistic", # objective
#                           eta = 0.5,
#                           depth = 5,
#                           verbose = 0  # silent
# )
# 
# # Make predictions
# plates$pred <- as.numeric(predict(plate_model_xgb, as.matrix(plates.treat)) > 0.5)
# 
# # evaluate model
# table(plates$status, plates$pred)
# 
# 
# 
# predict_plate <- function(plate, max.char = 10) {
#   if (nchar(plate) > 10) stop("Plate must be 10 characters or less")
#   plate <- toupper(plate)
#   
#   # if plate is already in the dataset
#   if (plate %in% app.plates$plate) {
#     rslt <- app.plates$status[app.plates$plate == plate]
#     txt <-
#       if_else(rslt == "Y",
#               "Congrats! Plate approved!",
#               "Sorry! Try something less vugar, you idiot")
#     return(txt)
#   }
#   
#   split.lets <- strsplit(plate, split = NULL) %>% unlist()
#   length(split.lets) <- max.char
#   split.lets <- as_tibble(split.lets) %>% t()
#   colnames(split.lets) <- paste0("V", 1:max.char)
#   split.treat <-
#     prepare(treatplan, as_tibble(split.lets),  varRestriction = newvars)
#   pred <- predict(plate_model_xgb, as.matrix(split.treat))
#   txt <- if_else(pred >= 0.5,
#                  "Congrats! Plate approved!",
#                  "Sorry! Try something less vugar, you idiot")
#   return(list(txt, pred))
# }
# 
# predict_plate("HVNNHEL")
# 
# 
# plates %>% 
#   filter(pred == 0,
#          status == 1) %>% 
#   mutate(plate = paste0(V1, V2, V3, V4, V5, V6, V7, V8, V9, V10)) %>% 
#   View
# 


# ranger on tokens --------------------------------------------------------------------


app.plates <- read_csv("Inputs/applications.csv")
table(app.plates$status)

parse_plate <- function(plate) {
  # function takes a single plate and returns all 
  #  "forward" combinations of the letters
  # e.g. plate "1234" would return "12, 123, 1234, 23, 234, 34"
  
  if (nchar(plate) > 10) stop("Plate must be 10 or less characters)")
  tokens <- c()
  # if plate is just one character
  if (nchar(plate) == 1) {
    tokens <- plate
  } else {
    plate <- strsplit(plate, split = NULL)[[1]]
    len <- length(plate)
    
    # index for storing results
    i <- 1
    # loop through the plate and take every "forward"
    #  combination of letters
    for (bgn in 1:(len - 1)) {
      for (end in (bgn + 1):len) {
        tokens[[i]] <- paste0(plate[bgn:end], collapse = "")
        i <- i + 1
      }
    }
  }
  
  #value controls the maximum characters
  #  is calculated parse_plate('vect with 10 char') %>% length()
  length(tokens) <- 45
  return(tokens)
}

parsed.plates <- lapply(app.plates$plate, function(plate) {
  parse_plate(plate = plate) %>%
    matrix(ncol = 45) %>%
    as_tibble
}) %>% bind_rows()

generate_plate <- function() {
  # function generates a single random string
  #  of 7 letters and numbers 
  
  bool <- sample(c(TRUE, FALSE), size = 7, replace = TRUE)
  plate <- NA
  plate[bool] <- LETTERS[sample(1:26, size = 7, replace = TRUE)][bool]
  plate[!bool] <- sample(0:9, size = 7, replace = TRUE)[!bool]
  return(paste0(plate, collapse = ""))
}

# generate n random plates
rand.plates <- replicate(n = sum(app.plates$status == "N", na.rm = TRUE),
                         expr = generate_plate())
rand.plates <- lapply(rand.plates, function(plate) {
  parse_plate(plate = plate) %>%
    matrix(ncol = 45) %>%
    as_tibble
}) %>% bind_rows()
rand.plates$status <- "Y"


plates <- cbind(app.plates$status, parsed.plates)
names(plates) <- c("status", paste0("V", 1:45))
plates <- bind_rows(plates, rand.plates)

fix_NA <- function(x) {
  x[is.na(x)] <- "-"
  x
}

plates <- plates %>% filter(status %in% c("Y", "N"))
plates <- map_df(plates, fix_NA)
plates$status <- (plates$status == "Y") %>% as.numeric()
# plates[, -1] <- map_df(plates[, -1], as.factor)

rf.model <- ranger(status ~ ., data = plates, classification = TRUE)

# preds <- if_else(rf.model$predictions > 0.5, 1, 0)
xtabs(~plates$status + rf.model$predictions)

# Make predictions
plates$pred <- predict(rf.model, plates[,-1])$predictions

plates[!(plates$pred == predict(rf.model, plates[,-1])$predictions),]

table(predict(rf.model, plates[1:100,-1])$predictions)

predict_plate <- function(plate, max.char = 10) {
  # plate <- "asdasd"
  
  if (nchar(plate) > 10) stop("Plate must be 10 characters or less")
  
  plate <- "EGGPUTT"
  plate <- toupper(plate)
  
  # if plate is already in the dataset
  # if (plate %in% app.plates$plate) {
  #   rslt <- app.plates$status[app.plates$plate == plate]
  #   txt <-
  #     if_else(rslt == "Y",
  #             "Congrats! Plate approved!",
  #             "Sorry! Try something less vulgar, you idiot")
  #   return(rslt)
  # }
  
  plate <- parse_plate(plate = plate) %>% 
    matrix(ncol = 45) %>%
    as_tibble
  
  plate <- map_df(plate, fix_NA)
  # plate <- map_df(plate, as.factor)
  
  pred <- predict(rf.model, plate)$predictions
  txt <- if_else(pred == 1,
                 "Congrats! Plate approved!",
                 "Sorry! Try something less vulgar, you idiot")
  return(pred)
}

predict_plate("EGGPUTT")


sapply(replicate(n = 2500,
                 expr = generate_plate()),
       predict_plate) %>% table()


