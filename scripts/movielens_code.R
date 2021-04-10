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
class(edx_little)

# ___________________________________########
# DEALING WITH GENRES ####
# ___________________________________########

edx_little %>% separate_rows(genres, sep = "\\|")


# Applying changes to entire edx data frame (except genres)
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

# Creation of test set y train set from edx data frame ######
# Note that validation set will not be used during training and tuning

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
# course, just for basic testing process, and see how rmse improves after each
# model change)

# _______No regularization_________ #####

# Naive: mu ######

naive_pred <- mean(train_edx$rating)

str(naive_pred)

naive_rmse <- RMSE(naive_pred, test_edx$rating)
naive_rmse
#__1.060167 #####

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
# __0.9432171  #####

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
# __0.8651897 #####

# _______Regularization_________######

# Now: we will apply regularization to see if we get some improvements in our 
# RMSE

lambdas <- seq(0, 10, 0.25)

# Lambda best-tune to Move Effects and User Efects Regularization #####
# Here we don't use the test set at all, only the train set.

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
    train_edx %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  
  return(RMSE(predicted_ratings, train_edx$rating))
})

qplot(lambdas, rmses)  

lambda <- lambdas[which.min(rmses)]
lambda
# 0.5

min(rmses)
# __0.8562963 #######

# Predictions with best tune lambda #######
# Here we make predictions over the test set, using best tune

mu <- mean(train_edx$rating)

b_i <- train_edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

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
# __0.8650327 #####


# ___________________________________########
##### ADDING TIME MOVIE YEAR EFFECT: b_my ######
# ___________________________________########

# Data exploration suggest that there is a relation between movie_year and
# ratings. So, we will add the effect of the movie_year to the model

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

# Previous attempt without regularization #########

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
#__ 0.8648795 ######## 


# Regularization #########

lambdas <- seq(0, 10, 0.25)

# Lambda best-tune to Movie +  User + Year Effects Regularization #####
# Here we don't use the test set at all, only the train set.
# This process takes some time. Please be patient.

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
    train_edx %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_my, by = 'movie_year') %>% 
    mutate(pred = mu + b_i + b_u + b_my) %>%
    .$pred
  
  return(RMSE(predicted_ratings, train_edx$rating))
})

qplot(lambdas, rmses)  

lambda <- lambdas[which.min(rmses)]
lambda
# 0.25

min(rmses)
# 0.855974

# Predictions with best tune lambda #######
# Here we make predictions over the test set, using best tune

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
#__0.8647968 #####

# Adding Movie Year Effect improve the RSME 


# ___________________________________########
##### ADDING RATING YEAR EFFECTS: b_ry  ######
# ___________________________________########

# Data exploration also suggest that there is a relation between rating_year 
# and ratings. So, we will add the effect of the rating_year to the model

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


# Previous attempt without regularization #########

rating_year_avgs <- train_edx %>% 
  left_join(movie_avgs, by = 'movieId') %>% 
  left_join(user_avgs, by = 'userId') %>% 
  left_join(movie_year_avgs, by = 'movie_year') %>%
  group_by(rating_year) %>% 
  summarise(b_ry = mean(rating - mu - b_i - b_u - b_my))

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
# Here we don't use the test set at all, only the train set.
# This process takes some time. Please be patient.

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
    train_edx %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_my, by = 'movie_year') %>%
    left_join(b_ry, by = 'rating_year') %>% 
    mutate(pred = mu + b_i + b_u + b_my + b_ry) %>%
    .$pred
  
  return(RMSE(predicted_ratings, train_edx$rating))
})

qplot(lambdas, rmses)  

lambda <- lambdas[which.min(rmses)]
lambda
# 0.5

min(rmses)
# 0.8558851

# Predictions with best tune lambda #######
# Here we make predictions over the test set, using best tune

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
#__0.864636 #####

# We have get a RMSE under the Goal

# Provisional table: 

data.frame("mu" = naive_rmse, 
           "bi" = movie_effect_rmse, 
           "bi_bu" = movie_user_effect_rmse, 
           "reg_bi_bu" = reg_movie_user_effect_rmse, 
           "reg_bi_bu_bmy" = reg_movie_user_effect_myear_rmse, 
           "reg_bi_bu_bmy_bry" = reg_movie_user_myear_ryear_effect_rmse) %>% 
  knitr::kable()


# ___________________________________########
##### APPLYING THE MODEL OVER ENTIRE EDX SET ######
# ___________________________________########

# Here we use the lambdas that we got in the previous step, over the
# entire edx set.

mu <- mean(edx$rating)

b_i <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

b_my <- edx %>% 
  left_join(b_i, by = 'movieId') %>% 
  left_join(b_u, by = 'userId') %>% 
  group_by(movie_year) %>% 
  summarise(b_my = sum(rating - b_i - b_u - mu)/(n()+lambda))

b_ry <- edx %>% 
  left_join(b_i, by = 'movieId') %>% 
  left_join(b_u, by = 'userId') %>% 
  left_join(b_my, by = 'movie_year') %>% 
  group_by(rating_year) %>% 
  summarise(b_ry = sum(rating - b_i - b_u - b_my - mu)/(n()+lambda))

edx_reg_movie_user_myear_ryear_effect_pred <- 
  edx %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_my, by = 'movie_year') %>%
  left_join(b_ry, by = 'rating_year') %>% 
  mutate(pred = mu + b_i + b_u + b_my + b_ry) %>%
  .$pred

str(edx_reg_movie_user_myear_ryear_effect_pred)

# Regularized Movie + User + Movie Year + Rating Year#####

edx_reg_movie_user_myear_ryear_effect_rmse <- RMSE(edx_reg_movie_user_myear_ryear_effect_pred, 
                                               edx$rating)
edx_reg_movie_user_myear_ryear_effect_rmse
# __0.8562849 #####

# ___________________________________########
##### EXPLORING GENDER EFFECT ######
# ___________________________________########

edx_g <- edx %>% separate_rows(genres, sep = "\\|") # It takes some time!

train_edx_g <- train_edx %>% separate_rows(genres, sep = "\\|")
test_edx_g <- test_edx %>% separate_rows(genres, sep = "\\|")


# Regularization #########

lambdas <- seq(0, 10, 0.25)

# Lambda best-tune to Movie +  User + Year Effects Regularization #####
# Here we don't use the test set at all, only the train set.
# This process takes some time. Run it only if you want to prove that the code
# Works

rmses_g <- sapply(lambdas, function(l){
  
  mu <- mean(train_edx_g$rating) 
  # mu <- mean(train_edx$rating)
  
  b_i <- train_edx_g %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_edx_g %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_my <- train_edx_g %>% 
    left_join(b_i, by = 'movieId') %>% 
    left_join(b_u, by = 'userId') %>% 
    group_by(movie_year) %>% 
    summarise(b_my = sum(rating - b_i - b_u - mu)/(n()+l))
  
  b_ry <- train_edx_g %>% 
    left_join(b_i, by = 'movieId') %>% 
    left_join(b_u, by = 'userId') %>% 
    left_join(b_my, by = 'movie_year') %>% 
    group_by(rating_year) %>% 
    summarise(b_ry = sum(rating - b_i - b_u - b_my - mu)/(n()+l))
  
  b_g <- train_edx_g %>% 
    left_join(b_i, by = 'movieId') %>% 
    left_join(b_u, by = 'userId') %>% 
    left_join(b_my, by = 'movie_year') %>% 
    left_join(b_ry, by = 'rating_year') %>% 
    group_by(genres) %>% 
    summarise(b_g = sum(rating - b_i - b_u - b_my - b_ry - mu)/(n()+l))
  
  predicted_ratings_g <- 
    train_edx_g %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_my, by = 'movie_year') %>%
    left_join(b_ry, by = 'rating_year') %>% 
    left_join(b_g, by = 'genres') %>% 
    group_by(userId, movieId) %>% 
    mutate(pred = mean(mu + b_i + b_u + b_my + b_ry + b_g)) %>%
    .$pred
  
  return(RMSE(predicted_ratings_g, train_edx_g$rating))
})

qplot(lambdas, rmses_g)  

lambda <- lambdas[which.min(rmses)]
lambda
# 0.5

min(rmses_g)
# 0.8558851

# Predictions with best tune lambda #######
# Here we make predictions over the test set, using best tune

mu <- mean(train_edx_g$rating) 
# mu <- mean(train_edx$rating)

b_i <- train_edx_g %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

b_u <- train_edx_g %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

b_my <- train_edx_g %>% 
  left_join(b_i, by = 'movieId') %>% 
  left_join(b_u, by = 'userId') %>% 
  group_by(movie_year) %>% 
  summarise(b_my = sum(rating - b_i - b_u - mu)/(n()+lambda))

b_ry <- train_edx_g %>% 
  left_join(b_i, by = 'movieId') %>% 
  left_join(b_u, by = 'userId') %>% 
  left_join(b_my, by = 'movie_year') %>% 
  group_by(rating_year) %>% 
  summarise(b_ry = sum(rating - b_i - b_u - b_my - mu)/(n()+lambda))

b_g <- train_edx_g %>% 
  left_join(b_i, by = 'movieId') %>% 
  left_join(b_u, by = 'userId') %>% 
  left_join(b_my, by = 'movie_year') %>% 
  left_join(b_ry, by = 'rating_year') %>% 
  group_by(genres) %>% 
  summarise(b_g = sum(rating - b_i - b_u - b_my - b_ry - mu)/(n()+lambda))

reg_movie_user_myear_ryear_effect_pred_g <- 
  test_edx_g %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_my, by = 'movie_year') %>%
  left_join(b_ry, by = 'rating_year') %>% 
  left_join(b_g, by = 'genres') %>% 
  group_by(userId, movieId) %>% 
  mutate(pred = mean(mu + b_i + b_u + b_my + b_ry + b_g)) %>%
  .$pred

str(reg_movie_user_myear_ryear_effect_pred_g)

# Regularized Movie + User + Movie Year + Rating Year + Gender #####

reg_movie_user_myear_ryear_effect_rmse_g <- RMSE(reg_movie_user_myear_ryear_effect_pred_g, 
                                               test_edx_g$rating)
reg_movie_user_myear_ryear_effect_rmse_g
#__0.864636 orig #####
#__0.8631034 mu orig #####
#__0.8631031 mu g #####
#__0.8630654 mean pred g ######

unique(revision_preds_g$movieId)

revision_preds_g <- 
  test_edx_g %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_my, by = 'movie_year') %>%
  left_join(b_ry, by = 'rating_year') %>% 
  left_join(b_g, by = 'genres') %>% 
  group_by(userId, movieId) %>% 
  mutate(pred = mu + b_i + b_u + b_my + b_ry + b_g) 


%>%
  .$pred

revision_preds_g %>% filter(movieId == 1092)
revision_preds_g %>% filter(userId == 798)


# ___________________________________########
##### APPLYING THE FINAL MODEL OVER VALIDATION SET ######
# ___________________________________########

#Now, we apply the final model trained with edx set, over the validation set.
# Note that this is the first and unique time that we use the validation set.

# Adding rating_date and rating_year to validation set ######
validation <- validation %>% 
  mutate(rating_date = as_datetime(timestamp), 
                                      rating_year = as.integer(year(rating_date)))

# Creating the new column "movie_year", extracting year from "title" #####
validation <- validation %>% 
  mutate(movie_year = str_extract(title, "\\(\\d\\d\\d\\d\\)"))

# Removing (parenthesis)  from "movie_year" column ####
movie_years_temp <- validation %>% pull(movie_year)
movie_years_temp <- str_remove_all(movie_years_temp, "\\(")
movie_years_temp <- str_remove(movie_years_temp, "\\)")

# Adding the cleaned "movie_years_temp" to data set #####
validation <- validation %>% 
  mutate(movie_year = as.integer(movie_years_temp))

# Review of final results
validation[ind]

# Preds over validation set #####

validation_preds <- 
  validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_my, by = 'movie_year') %>% 
  left_join(b_ry, by = 'rating_year') %>% 
  mutate(mu = mu, pred = mu + b_i + b_u + b_my + b_ry) %>% 
  .$pred

head(validation_preds)
str(validation_preds)

# RMSE over validation set #####
validation_preds_rmse <- RMSE(validation_preds, validation$rating)
validation_preds_rmse
# __0.8648047 ######

final_table <- data.frame(model = c("mu", 
                                    "bi", 
                                    "bi_bu", 
                                    "reg_bi_bu", 
                                    "reg_bi_bu_bmy", 
                                    "reg_bi_bu_bmy_bry", 
                                    "edx_reg_bi_bu_bmy_bry", 
                                    "validation_final_model"), 
                          rmse = c(naive_rmse, 
                                   movie_effect_rmse,  
                                   movie_user_effect_rmse, 
                                   reg_movie_user_effect_rmse,  
                                   reg_movie_user_effect_myear_rmse,
                                   reg_movie_user_myear_ryear_effect_rmse,
                                   edx_reg_movie_user_myear_ryear_effect_rmse,
                                   validation_preds_rmse)) %>% 
  knitr::kable()

final_table




# rm(list = ls())