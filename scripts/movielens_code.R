# ___________________________________########
# DATA DOWNLOAD AND SETS CREATION #######
# ___________________________________########

# In order to make this code fully reproducible, we have included here the code 
# provided in the Edx Harvard Data Science Capstone Course (HarvardX PH125.9x), 
# to download the dataset and create the test and validation sets.

# IMPORTANT NOTES ABOUT R VERSION

# In "Data Wrangling" section we use the code 
# provided for  R 4.0 or later.
# Please be sure that you are using the proper R version. Otherwise, choose 
# the code for R 3.6 or earlier (included but comented).

# In "Creation of test and validation sets" section we use the 
# set.seed(1, sample.kind="Rounding") method. If you are using R 3.5 or 
# earlier, use `set.seed(1)`

#_________________

# Create edx set, validation set (final hold-out test set) 

# Note: this process could take a couple of minutes

# Packages and libraries #######

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(recommenderlab)) install.packages("recommenderlab", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(recommenderlab)
library(dplyr)
library(lubridate)
library(stringr)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

# Data download #####

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

# Data wrangling #####

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

head(ratings)

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

head(movies)

# __if using R 3.6 or earlier: #####

# movies <- as.data.frame(movies) %>% 
# mutate(movieId = as.numeric(levels(movieId))[movieId],
# title = as.character(title),
# genres = as.character(genres))

# __if using R 4.0 or later: #######

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

head(movies)

movielens <- left_join(ratings, movies, by = "movieId")

head(movielens)


# Creation of test and validation sets ######

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

test_index <- createDataPartition(y = movielens$rating, times = 1, 
                                  p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

str(validation$rating)

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

head(edx)
class(edx)

# ___________________________________########
# ADDING NEW COLUMNS#######
# movie_year, rating_date, rating_year ####
# ___________________________________########

# In order to visualize and filter data for exploration, we will add in the 
# data frame three new columns:
# - movie_year (integer)
# - rating_year (integer)
# - rating_date (YY-MM-DD HH:MM:SS POSIXct format)

# This change also could be useful to add time effects 
# in our final model to enhanced the RMSE results

# To test the code, we will create a very reduced version of "edx" data frame
# first (edx_little). Once the code has been tested, will be applied in the
# entire dataset. This reduced version will be used along this project for 
# similar purposes.


# Creating and index with short and large movie names (with parenthesis), 
# needed to datawrangling tests
ind <- c(1, 2, 3, 4, 5, 6, 21, 29, 39, 47, 67, 107, 108, 109, 112, 121, 
         122, 123, 124, 128)

# Creating litlle version
edx_little <- edx[ind]

edx_little

# Adding rating_date and rating_year
edx_little <- edx_little %>% mutate(rating_date = as_datetime(timestamp), 
                                    rating_year = as.integer(year(rating_date)))
class(edx_little$rating_year)

# Creating the new column "movie_year", extracting year from "title"

# We use the "string" package, so if necessary you need to download it

edx_little <- edx_little %>% 
  mutate(movie_year = str_extract(title, "\\(\\d\\d\\d\\d\\)"))

# Removing (parenthesis)  from "movie_year" column
movie_years_temp <- edx_little %>% pull(movie_year)
movie_years_temp <- str_remove_all(movie_years_temp, "\\(")
movie_years_temp <- str_remove(movie_years_temp, "\\)")

# Adding the cleaned column "movie_year" to data set
edx_little <- edx_little %>% 
  mutate(movie_year = as.integer(movie_years_temp))

# Review of final results
edx_little

# Applying changes to entire data set
# ______________________________________

# Adding rating_date and rating_year ######
edx <- edx %>% mutate(rating_date = as_datetime(timestamp), 
                      rating_year = as.integer(year(rating_date)))

# Creating the new column "movie_year", extracting year from "title" #####
edx <- edx %>% mutate(movie_year = str_extract(title, "\\(\\d\\d\\d\\d\\)"))

# Removing (parenthesis)  from "movie_year" column ####
movie_years_temp <- edx %>% pull(movie_year)
movie_years_temp <- str_remove_all(movie_years_temp, "\\(")
movie_years_temp <- str_remove(movie_years_temp, "\\)")

# Adding the cleaned "movie_years_temp" to data set #####
edx <- edx %>% 
  mutate(movie_year = as.integer(movie_years_temp))

# Review of final results
edx[ind]

# ___________________________________########
#### TEST SET AND TRAIN SET ########
# ___________________________________########

# Creation of test set y trin set from edx data frame ######

head(edx)

set.seed(1970, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1,
                                  list = FALSE)
train_edx <- edx[-test_index,]
test_edx <- edx[test_index,]

# SemiJoin #####

test_edx <- test_edx %>% 
  semi_join(train_edx, by = "movieId") %>% 
  semi_join(train_edx, by = "userId")

# ___________________________________########
# RMSE function #####
# ___________________________________########

RMSE <- function(true_ratings, pred_ratings) {
  sqrt(mean((true_ratings - pred_ratings)^2))
}

# ___________________________________########
##### GOALS ######
# ___________________________________########
# 10 points: 0.86550 <= RMSE <= 0.89999 ####
# 15 points: 0.86500 <= RMSE <= 0.86549 ####
# 20 points: 0.86490 <= RMSE <= 0.86499 ####
# 25 points: RMSE < 0.86490 ####

# ___________________________________########
# EXPLORATORY DATA ANALYSIS #######
# ___________________________________########

# Ratings frecuency 
edx %>% ggplot(aes(rating)) +
  geom_density()

mu <- mean(edx$rating)
mu

# Visualization of ratings vs movie_year

edx %>% 
  group_by(movie_year) %>% 
  summarise(movie_year_avgs = mean(rating)) %>% 
  ggplot(aes(movie_year, movie_year_avgs)) +
  geom_point() +
  geom_smooth()

# Visualization of ratings vs rating_year

edx %>% 
  group_by(rating_year) %>% 
  summarise(rating_year_avgs = mean(rating)) %>% 
  ggplot(aes(rating_year, rating_year_avgs)) +
  geom_point() +
  geom_smooth()

# ___________________________________########
##### TESTING BASIC MODELS ######
# ___________________________________########
# We will start testing very basic models (included on bibliography of the 
# course, just for basic testing process)

# _______No regularization_________ #####

# Naive: mu ######

naive_pred <- mean(train_edx$rating)

str(naive_pred)

naive_rmse <- RMSE(naive_pred, test_edx$rating)
naive_rmse
#__1.060167 = 0 points #####

# Movie Effects: b_i ######

mu <- mean(train_edx$rating)

movie_avgs <- train_edx %>% 
  group_by(movieId) %>% 
  summarise(b_i = mean(rating-mu))

head(movie_avgs)

movie_avgs %>% 
  ggplot(aes(b_i)) +
  geom_histogram(bins = 100, color = "black")

b_i <- test_edx %>% 
  left_join(movie_avgs, by = 'movieId') %>% 
  pull(b_i)

movie_effect_pred <- mu + b_i

str(movie_effect_pred)

movie_effect_rmse <- RMSE(test_edx$rating, movie_effect_pred)
movie_effect_rmse
# __0.9432171 = 0 points #####

# Movie User Effects: b_i + b_u #######

user_avgs <- train_edx %>% 
  left_join(movie_avgs, by = "movieId") %>% 
  group_by(userId) %>% 
  summarise(b_u = mean(rating - mu - b_i))

movie_user_effect_pred <- test_edx %>% 
  left_join(movie_avgs, by = 'movieId') %>% 
  left_join(user_avgs, by = 'userId') %>%  
  mutate(pred = mu + b_i + b_u) %>% 
  pull(pred)

movie_user_effect_rmse <- RMSE(test_edx$rating, movie_user_effect_pred)
movie_user_effect_rmse
# __0.8651897 = 15 points #####

# _______Regularization_________######

lambdas <- seq(0, 10, 0.25)

# Lambda best-tune to Move Effects and User Efects Regularization #####

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_edx$rating)
  
  b_i <- train_edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    test_edx %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  
  return(RMSE(predicted_ratings, test_edx$rating))
})

qplot(lambdas, rmses)  

lambda <- lambdas[which.min(rmses)]
lambda

min(rmses)

# Predictions with best tune lambda #######

mu <- mean(train_edx$rating)

b_i <- train_edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

head(b_i)

b_u <- train_edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

reg_movie_user_effect_pred <- 
  test_edx %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

str(reg_movie_user_effect_pred)

# Regularized Movie + User Effects #####

reg_movie_user_effect_rmse <- RMSE(reg_movie_user_effect_pred, test_edx$rating)
reg_movie_user_effect_rmse
# __0.864565 = 20 points (prov) #####


# ___________________________________########
##### ADDING TIME MOVIE YEAR EFFECT: b_my ######
# ___________________________________########

head(edx)

# Visualization of ratings vs years

edx %>% 
  group_by(movie_year) %>% 
  summarise(movie_year_avgs = mean(rating)) %>% 
  ggplot(aes(movie_year, movie_year_avgs)) +
  geom_point() +
  geom_smooth()

edx %>% 
  group_by(movie_year) %>% 
  summarise(ratings = n()) %>% 
  arrange(ratings)

# Adding column b_my #########

movie_year_avgs <- train_edx %>% 
  left_join(movie_avgs, by = 'movieId') %>% 
  left_join(user_avgs, by = 'userId') %>% 
  group_by(movie_year) %>% 
  summarise(b_my = mean(rating - mu - b_i - b_u))

head(movie_year_avgs)

movie_user_effect_myear_pred <- test_edx %>% 
  left_join(movie_avgs, by = 'movieId') %>% 
  left_join(user_avgs, by = 'userId') %>%  
  left_join(movie_year_avgs, by = 'movie_year') %>% 
  mutate(pred = mu + b_i + b_u + b_my) %>% 
  pull(pred)

movie_user_effect_myear_rmse <- RMSE(movie_user_effect_myear_pred, test_edx$rating)
movie_user_effect_myear_rmse
#__ 0.8648795 : 15 points (prov) ######## 


# Regularization #########

lambdas <- seq(0, 10, 0.25)

# Lambda best-tune to Movie +  User + Year Effects Regularization #####

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_edx$rating)
  
  b_i <- train_edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_my <- train_edx %>% 
    left_join(b_i, by = 'movieId') %>% 
    left_join(b_u, by = 'userId') %>% 
    group_by(movie_year) %>% 
    summarise(b_my = sum(rating - b_i - b_u - mu)/(n()+l))
  
  predicted_ratings <- 
    test_edx %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_my, by = 'movie_year') %>% 
    mutate(pred = mu + b_i + b_u + b_my) %>%
    .$pred
  
  return(RMSE(predicted_ratings, test_edx$rating))
})

qplot(lambdas, rmses)  

lambda <- lambdas[which.min(rmses)]
lambda

min(rmses)

# Predictions with best tune lambda #######

mu <- mean(train_edx$rating)

b_i <- train_edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

b_u <- train_edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

b_my <- train_edx %>% 
  left_join(b_i, by = 'movieId') %>% 
  left_join(b_u, by = 'userId') %>% 
  group_by(movie_year) %>% 
  summarise(b_my = sum(rating - b_i - b_u - mu)/(n()+lambda))

reg_movie_user_myear_effect_pred <- 
  test_edx %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_my, by = 'movie_year') %>% 
  mutate(pred = mu + b_i + b_u + b_my) %>%
  .$pred

str(reg_movie_user_myear_effect_pred)

# Regularized Movie + User + Movie_Year #####

reg_movie_user_effect_myear_rmse <- RMSE(reg_movie_user_myear_effect_pred, 
                                         test_edx$rating)
reg_movie_user_effect_myear_rmse
#__0.8643021 = 25 Points (prov)


# ___________________________________########
##### ADDING RATING YEAR EFFECTS ######
# ___________________________________########

# Visualization of ratings vs rating_years

edx %>% 
  group_by(rating_year) %>% 
  summarise(rating_year_avgs = mean(rating)) %>% 
  ggplot(aes(rating_year, rating_year_avgs)) +
  geom_point() +
  geom_smooth()

edx %>% 
  group_by(rating_year) %>% 
  summarise(ratings = n()) %>% 
  arrange(ratings)

# Adding column b_ry #########

rating_year_avgs <- train_edx %>% 
  left_join(movie_avgs, by = 'movieId') %>% 
  left_join(user_avgs, by = 'userId') %>% 
  left_join(movie_year_avgs, by = 'movie_year') %>%
  group_by(rating_year) %>% 
  summarise(b_ry = mean(rating - mu - b_i - b_u - b_my))

head(rating_year_avgs)

movie_user_effect_myear_ryear_pred <- test_edx %>% 
  left_join(movie_avgs, by = 'movieId') %>% 
  left_join(user_avgs, by = 'userId') %>%  
  left_join(movie_year_avgs, by = 'movie_year') %>% 
  left_join(rating_year_avgs, by = 'rating_year') %>% 
  mutate(pred = mu + b_i + b_u + b_my + b_ry) %>% 
  pull(pred)

movie_user_effect_myear_ryear_rmse <- RMSE(movie_user_effect_myear_ryear_pred, 
                                           test_edx$rating)
movie_user_effect_myear_ryear_rmse
## __0.8647913 #######

# Regularization #########

lambdas <- seq(0, 10, 0.25)

# Lambda best-tune to Movie +  User + Year Effects Regularization #####

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_edx$rating)
  
  b_i <- train_edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_my <- train_edx %>% 
    left_join(b_i, by = 'movieId') %>% 
    left_join(b_u, by = 'userId') %>% 
    group_by(movie_year) %>% 
    summarise(b_my = sum(rating - b_i - b_u - mu)/(n()+l))
  
  b_ry <- train_edx %>% 
    left_join(b_i, by = 'movieId') %>% 
    left_join(b_u, by = 'userId') %>% 
    left_join(b_my, by = 'movie_year') %>% 
    group_by(rating_year) %>% 
    summarise(b_ry = sum(rating - b_i - b_u - b_my - mu)/(n()+l))
  
  predicted_ratings <- 
    test_edx %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_my, by = 'movie_year') %>%
    left_join(b_ry, by = 'rating_year') %>% 
    mutate(pred = mu + b_i + b_u + b_my + b_ry) %>%
    .$pred
  
  return(RMSE(predicted_ratings, test_edx$rating))
})

qplot(lambdas, rmses)  

lambda <- lambdas[which.min(rmses)]
lambda

min(rmses)

# Predictions with best tune lambda #######

mu <- mean(train_edx$rating)

b_i <- train_edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

b_u <- train_edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

b_my <- train_edx %>% 
  left_join(b_i, by = 'movieId') %>% 
  left_join(b_u, by = 'userId') %>% 
  group_by(movie_year) %>% 
  summarise(b_my = sum(rating - b_i - b_u - mu)/(n()+lambda))

b_ry <- train_edx %>% 
  left_join(b_i, by = 'movieId') %>% 
  left_join(b_u, by = 'userId') %>% 
  left_join(b_my, by = 'movie_year') %>% 
  group_by(rating_year) %>% 
  summarise(b_ry = sum(rating - b_i - b_u - b_my - mu)/(n()+lambda))

reg_movie_user_myear_ryear_effect_pred <- 
  test_edx %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_my, by = 'movie_year') %>%
  left_join(b_ry, by = 'rating_year') %>% 
  mutate(pred = mu + b_i + b_u + b_my + b_ry) %>%
  .$pred

str(reg_movie_user_myear_ryear_effect_pred)

# Regularized Movie + User + Movie Year + Rating Year#####

reg_movie_user_myear_ryear_effect_rmse <- RMSE(reg_movie_user_myear_ryear_effect_pred, 
                                               test_edx$rating)
reg_movie_user_myear_ryear_effect_rmse
#__0.8643021 = 25 Points (prov)

# Provisional table 4: 

data.frame("mu" = naive_rmse, 
           "bi" = movie_effect_rmse, 
           "bi_bu" = movie_user_effect_rmse, 
           "reg_bi_bu" = reg_movie_user_effect_rmse, 
           "reg_bi_bu_bmy" = reg_movie_user_effect_myear_rmse, 
           "reg_bi_bu_bmy_bry" = reg_movie_user_myear_ryear_effect_rmse) %>% 
  knitr::kable()


# ___________________________________########
##### APPLYING THE FINAL MODEL OVER VALIDATION SET ######
# ___________________________________########

# Adding same columns to validation set
# ______________________________________

# Adding rating_date and rating_year ######
validation_2 <- validation %>% mutate(rating_date = as_datetime(timestamp), 
                                      rating_year = as.integer(year(rating_date)))

# Creating the new column "movie_year", extracting year from "title" #####
validation_2 <- validation_2 %>% mutate(movie_year = str_extract(title, "\\(\\d\\d\\d\\d\\)"))

# Removing (parenthesis)  from "movie_year" column ####
movie_years_temp <- validation_2 %>% pull(movie_year)
movie_years_temp <- str_remove_all(movie_years_temp, "\\(")
movie_years_temp <- str_remove(movie_years_temp, "\\)")

# Adding the cleaned "movie_years_temp" to data set #####
validation_2 <- validation_2 %>% 
  mutate(movie_year = as.integer(movie_years_temp))

# Review of final results
validation_2[ind]

validation_preds <- 
  validation_2 %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_my, by = 'movie_year') %>% 
  left_join(b_ry, by = 'rating_year') %>% 
  mutate(mu = mu, pred = mu + b_i + b_u + b_my + b_ry)

head(validation_preds)

# 4 Films have b_i = NA, thus pred = NA
# If there are NAs, RMSE function doens't work, and return NA
validation_preds %>%  filter(is.na(pred))

# To solve the problem, we will replace the 4 NAs with "mu"
# (the avg rating of all movies)
validation_preds$pred[is.na(validation_preds$pred)] <- mu

# Then we have remove all 4 NAs with mu
validation_preds %>%  filter(is.na(pred))
validation_preds %>%  filter(is.na(b_i))

# We can now extract predictions...
validation_preds <-   validation_preds %>% .$pred

# ...and calculate RMSE over validation test
validation_preds_rmse <- RMSE(validation_preds, validation$rating)
validation_preds_rmse
# _0.864852 = 25 points ######

final_table <- data.frame("mu" = naive_rmse, 
                          "bi" = movie_effect_rmse, 
                          "bi_bu" = movie_user_effect_rmse, 
                          "reg_bi_bu" = reg_movie_user_effect_rmse, 
                          "reg_bi_bu_bmy" = reg_movie_user_effect_myear_rmse, 
                          "reg_bi_bu_bmy_bry" = reg_movie_user_myear_ryear_effect_rmse,
                          "validation_final_model" = validation_preds_rmse) %>% 
  knitr::kable()

final_table


# rm(list = ls())