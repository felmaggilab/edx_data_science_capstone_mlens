# ___________________________________########
# DATA DOWNLOAD AND SETS CREATION #######
# ___________________________________########

# In order to make this project fully reproducible, we have included here the code 
# provided in the Edx Harvard Data Science Capstone Course (HarvardX PH125.9x), 
# to download the dataset and create the test and validation sets.

# IMPORTANT NOTES ABOUT R VERSION

# In "Data Wrangling" section we use the code 
# provided for  R 4.0 or later.
# Please be sure that you are using the proper R version. Otherwise, choose 
# the code for R 3.6 or earlier (included but commented).

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
if(!require(ggalt)) install.packages("ggalt", repos = "http://cran.us.r-project.org")
if(!require(ggcorrplot)) install.packages("ggcorrplot", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(recommenderlab)
library(dplyr)
library(lubridate)
library(stringr)
library(gridExtra)
library(ggalt)
library(scales)
library(ggcorrplot)

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
# ADDING NEW COLUMNS #######
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

# We will use 90% of data to train, and 10% of data to test.

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

# color-blind-friendly palette (gray)

cbPalette <- c("gray" = "#999999", 
               "orange" = "#E69F00", 
               "soft_blue" = "#56B4E9", 
               "yellow" = "#009E73",
               "green" = "#F0E442", 
               "dark_blue" = "#0072B2", 
               "red" = "#D55E00", 
               "pink" = "#CC79A7")

dim(edx) # dim edx######
# 9000055       9

length(unique(edx$movieId)) # Number of movies #####
# 10677

length(unique(edx$userId)) # Number of users #####
# 69878

# Ratings frecuency 
edx %>% ggplot(aes(rating)) +
  geom_density(fill = "#0072B2") +
  labs(title = "Rating frecuency")

avg_rating <- mean(edx$rating) # avg rating ######
avg_rating
# 3.512465 

sd_rating <- sd(edx$rating) # sd rating ######
sd_rating
# 1.060331

# Distribution of avg. ratings per movie ######
edx %>% 
  group_by(movieId) %>% 
  mutate(avg_rating = mean(rating)) %>% 
  ggplot(aes(avg_rating)) +
  geom_histogram(bins = 100, color = "#999999", fill = "#0072B2") +
  labs(title = "Distribution of avg. ratings per movie")

# Distribution of avg. ratings per user ######
edx %>% 
  group_by(userId) %>% 
  mutate(avg_rating = mean(rating)) %>% 
  ggplot(aes(avg_rating)) +
  geom_histogram(bins = 100, color = "#999999", fill = "#0072B2") +
  labs(title = "Distribution of avg. ratings per user")

edx %>% arrange(movie_year)
# Films from 1915 to 2008

edx %>% arrange(rating_year)
# Ratings from 1995 to 2008

# Top 15 rated movies  #####
edx %>% 
  group_by(title) %>% 
  summarise(ratings = n()) %>% 
  arrange(desc(ratings)) %>% 
  head(15) %>% 
  knitr::kable()

edx %>% 
  group_by(title) %>% 
  summarise(ratings = n()) %>% 
  arrange(desc(ratings)) %>% 
  tail(15) %>% 
  knitr::kable()

# Top 15 active users ##### 
edx %>% 
  group_by(userId) %>% 
  summarise(ratings = n()) %>% 
  arrange(desc(ratings)) %>% 
  head(15) %>% 
  knitr::kable()

edx %>% 
  group_by(userId) %>% 
  summarise(ratings = n()) %>% 
  arrange(desc(ratings)) %>%
  tail(n = 15) %>% 
  knitr::kable()

# Visualization of average ratings vs movie_year #####

edx %>% 
  group_by(movie_year) %>% 
  summarise(movie_year_avgs = mean(rating)) %>% 
  ggplot(aes(movie_year, movie_year_avgs)) +
  geom_point() +
  geom_smooth()

# There is a clear effect of the premiere year, on the average ratings. 
# This must be taken into account in our final model


# Visualization of average ratings vs rating_year #####

edx %>% 
  group_by(rating_year) %>% 
  summarise(rating_year_avgs = mean(rating)) %>% 
  ggplot(aes(rating_year, rating_year_avgs)) +
  geom_point() +
  geom_smooth()

# There is a certain effect of the rating year on the average ratings. 
# We will add this to the model, but the result on the rmse should not 
# be very pronounced.

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
  summarise(b_i = mean(rating-mu))  # Movie effect is calculated as the mean
# of rating minus the general mean for all movies.

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
  summarise(b_u = mean(rating - mu - b_i)) # User effect is calculated as 
# the mean of rating minus the general mean minus the movie effect 
# for each user. This is an approximation due to apply a lm model is two low in 
# this case

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

# Data exploration suggest that there is a strong relation between movie_year 
# and average ratings. So, we will add the effect of the movie_year to the model

# However, there are some years with very few ratings. So, regularization is 
# necessary

edx %>% 
  group_by(movie_year) %>% 
  summarise(ratings = n()) %>% 
  arrange(ratings) %>% 
  top_n(-10) %>% 
  knitr::kable()

edx %>% 
  group_by(movie_year) %>% 
  summarise(ratings = n()) %>% 
  arrange(desc(ratings)) %>% 
  top_n(10) %>% 
  knitr::kable()

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
    summarise(b_my = sum(rating - b_i - b_u - mu)/(n()+l)) # We apply here the
  # same process used for user effects. This approach will be used for every new
  # element that we will add at the model
  
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

# Regularized Movie + User + Movie Year + Rating Year #####

edx_reg_movie_user_myear_ryear_effect_rmse <- RMSE(edx_reg_movie_user_myear_ryear_effect_pred, 
                                                   edx$rating)
edx_reg_movie_user_myear_ryear_effect_rmse
# __0.8562849 #####

# ___________________________________########
##### EXPLORING GENRE EFFECT ######
# ___________________________________########

# In order to use all the data provided in the data set, we will explore the
# genre effect. At this point, we are not sure if this approach will introduce
# noise to the model or, on the contrary, it will improve the rmse.

# To add genre effect, we will separate geres by row (this change add more)
# rows to the data sets.

# Separation of genres by rows
# Each combination of userId and movieId will now have as many rows as 
# genres the movie has.

edx_g <- edx %>% separate_rows(genres, sep = "\\|") # It takes some time!

train_edx_g <- train_edx %>% separate_rows(genres, sep = "\\|")
test_edx_g <- test_edx %>% separate_rows(genres, sep = "\\|")

# Revision 

head(edx_g) %>% knitr::kable()

# Only 1 film has no genres listed
edx_g %>% filter(genres == "(no genres listed)")

# Average Rating by Genres ######
avg_genres <- edx_g %>% 
  group_by(genres) %>% 
  filter(!genres == "(no genres listed)") %>% 
  summarise(avg_rating = mean(rating)) %>% 
  arrange(desc(avg_rating))

knitr::kable(avg_genres)

# Number of Ratings by Genres #####
ratings_genres <-  edx_g %>% 
  group_by(genres) %>% 
  filter(!genres == "(no genres listed)") %>% 
  summarise(ratings = n()) %>% 
  arrange(desc(ratings))

knitr::kable(ratings_genres)


# Relation between number of ratings and average rating by gender #####
genres_table <- avg_genres %>% 
  left_join(ratings_genres, by = 'genres') 

knitr::kable(genres_table)

genres_selected_top <- genres_table %>% 
  filter(genres %in% c("Film-Noir", "Documentary", "War"))

genres_selected_bottom <- genres_table %>% 
  filter(genres %in% c("Horror", "Children", "Sci-Fi", "Children "))

genres_table %>% 
  ggplot(aes(ratings, avg_rating, color = genres)) +
  geom_point() +
  geom_encircle(aes(x = ratings, y = avg_rating), 
                data = genres_selected_top, 
                color="red", 
                size=2, 
                expand=0.04) +
  geom_encircle(aes(x = ratings, y = avg_rating), 
                data = genres_selected_bottom, 
                color="red", 
                size=2, 
                expand=0.07) +
  geom_text(aes(ratings, avg_rating, label = genres), nudge_y = -0.02)+
  labs(title = "Number of ratings versus average rating by gender")

# Movie year versus genders rating counts ######

edx_g %>% 
  filter(!genres == "(no genres listed)") %>%
  group_by(movie_year, genres) %>% 
  summarise(genres_r_count = n()) %>% 
  ggplot(aes(movie_year, genres_r_count , color = genres)) +
  geom_point() +
  labs(title = "Movie year versus genders rating counts")

# Movie year versus genders rating counts (scale y = log2) ######
edx_g %>% 
  filter(!genres == "(no genres listed)") %>%
  group_by(movie_year, genres) %>% 
  summarise(genres_r_count = n()) %>% 
  ggplot(aes(movie_year, genres_r_count , color = genres)) +
  geom_point() +
  scale_y_continuous(trans = "log2") +
  labs(title = "Movie year versus genders rating counts")


# Distributions of Ratings by Genres: NOT RUN! ######
edx_g %>% 
  filter(!genres == "(no genres listed)") %>% 
  ggplot(aes(genres, rating)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Distributions of Ratings by Genres")

# Selecting and Active User ####### 
edx_g %>% 
  group_by(userId) %>% 
  mutate(count = n()) %>% 
  arrange(desc(count))

user_59269 <- edx_g %>% filter(userId == 59269)
# 6616 rated movies

# Distributions of Ratings by Genres: User 59269 ######
user_59269 %>% 
  filter(!genres == "(no genres listed)") %>% 
  ggplot(aes(genres, rating)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Distributions of Ratings by Genres")



# ___________________________________########
##### ADDING GENRE EFFECT: b_g  ######
# ___________________________________########

# Regularization #########
# In this case we directly apply regularization

lambdas <- seq(0, 10, 0.25)

# Lambda best-tune to Movie + User + Year + Gender Effects Regularization #####
# Here we don't use the test set at all, only the train set.

# _______________________________________
# NOTE: This process takes a lot of TIME! Run it only if you want to be sure 
# that the code works: Lambda best tune is 0.5
# ________________________________________

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
  
  # Here he change the previous code: final preds are the mean of all preds
  # related to every movieId and userId combination
  
  return(RMSE(predicted_ratings_g, train_edx_g$rating))
})

qplot(lambdas, rmses_g)  

lambda <- lambdas[which.min(rmses)]
lambda
# 0.5

min(rmses_g)
# 0.8530165

# Predictions with best tune lambda #######
# Here we make predictions over the test set, using best tune

# _______________________________________
# NOTE: This code could be directly applied using lambda = 0.5
# ________________________________________

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

reg_movie_user_myear_ryear_gender_effect_pred <- 
  test_edx_g %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_my, by = 'movie_year') %>%
  left_join(b_ry, by = 'rating_year') %>% 
  left_join(b_g, by = 'genres') %>% 
  group_by(userId, movieId) %>% 
  mutate(pred = mean(mu + b_i + b_u + b_my + b_ry + b_g)) %>%
  .$pred

# Here he change the previous code: final preds are the mean of all preds
# related to every movieId and userId combination

# Regularized Movie + User + Movie Year + Rating Year + Gender #####

reg_movie_user_myear_ryear_effect_gender_rmse <- 
  RMSE(reg_movie_user_myear_ryear_gender_effect_pred, 
       test_edx_g$rating)

reg_movie_user_myear_ryear_effect_gender_rmse
#__0.864636 orig model no gender #####
#__0.8631034 gender and mu orig #####
#__0.8631031 gender and mu g #####
#__0.8630654 gender and mean pred ######


# Why we use the mean of preds for movieId - UserId comb?
# Because when we separate genders by row, we can get several preds for
# the same movieId - userId combination:

revision_preds_g <- 
  test_edx_g %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_my, by = 'movie_year') %>%
  left_join(b_ry, by = 'rating_year') %>% 
  left_join(b_g, by = 'genres') %>% 
  mutate(pred = mu + b_i + b_u + b_my + b_ry + b_g) 


# For example, userId 325 have rated a lot of movies. If wee see the second one
# (Star Trek: Generation), we note that for every genre we get an slightly
# different rate. But we need give only one rate for that user, and that movie.

revision_preds_g %>% filter(userId == 325) %>% 
  select(userId, movieId, title, genres, rating, pred) %>%
  knitr::kable()

# So, before to make preds, we group by userId and movieId, and take the mean
# of the preds:

revision_preds_g <- 
  test_edx_g %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_my, by = 'movie_year') %>%
  left_join(b_ry, by = 'rating_year') %>% 
  left_join(b_g, by = 'genres') %>% 
  group_by(userId, movieId) %>% # <- Change
  mutate(pred = mean(mu + b_i + b_u + b_my + b_ry + b_g)) # <- Change (mean)

revision_preds_g %>% filter(userId == 59269 & genres == "Film-Noir") %>% 
  select(userId, movieId, title, genres, rating, pred) %>%
  knitr::kable()

# ___________________________________########
##### APPLYING THE MODEL OVER ENTIRE EDX_G SET ######
# ___________________________________########

mu <- mean(edx_g$rating) #       <- rmse = 0.8534774
# mu <- mean(train_edx$rating) # <- rmse = 0.8534776 

b_i <- edx_g %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

b_u <- edx_g %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

b_my <- edx_g %>% 
  left_join(b_i, by = 'movieId') %>% 
  left_join(b_u, by = 'userId') %>% 
  group_by(movie_year) %>% 
  summarise(b_my = sum(rating - b_i - b_u - mu)/(n()+lambda))

b_ry <- edx_g %>% 
  left_join(b_i, by = 'movieId') %>% 
  left_join(b_u, by = 'userId') %>% 
  left_join(b_my, by = 'movie_year') %>% 
  group_by(rating_year) %>% 
  summarise(b_ry = sum(rating - b_i - b_u - b_my - mu)/(n()+lambda))

b_g <- edx_g %>% 
  left_join(b_i, by = 'movieId') %>% 
  left_join(b_u, by = 'userId') %>% 
  left_join(b_my, by = 'movie_year') %>% 
  left_join(b_ry, by = 'rating_year') %>% 
  group_by(genres) %>% 
  summarise(b_g = sum(rating - b_i - b_u - b_my - b_ry - mu)/(n()+lambda))

edx_g_reg_movie_user_myear_ryear_gender_effect_pred <- 
  edx_g %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_my, by = 'movie_year') %>%
  left_join(b_ry, by = 'rating_year') %>% 
  left_join(b_g, by = 'genres') %>% 
  group_by(userId, movieId) %>% 
  mutate(pred = mean(mu + b_i + b_u + b_my + b_ry + b_g)) %>%
  .$pred

# Here he change the previous code: final preds are the mean of all preds
# related to every movieId and userId combination

str(edx_g_reg_movie_user_myear_ryear_gender_effect_pred)

# Regularized Movie + User + Movie Year + Rating Year + Gender #####

edx_g_reg_movie_user_myear_ryear_effect_gender_rmse <- 
  RMSE(edx_g_reg_movie_user_myear_ryear_gender_effect_pred, 
       edx_g$rating)

edx_g_reg_movie_user_myear_ryear_effect_gender_rmse
#__0.8534774########


# Adding the gender effect over the entire EDX data set improves the rmse 
# that we got with the previous model. 

# We will apply now the final model, train with the entire EDX data set,
# over the validation set


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

# Separation of genders by row, over the validation set ######

validation_g <- validation %>% separate_rows(genres, sep = "\\|") # It takes some time!

head(validation_g) %>% knitr::kable()

# Preds over validation set #####

validation_preds <- 
  validation_g %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_my, by = 'movie_year') %>%
  left_join(b_ry, by = 'rating_year') %>% 
  left_join(b_g, by = 'genres') %>% 
  group_by(userId, movieId) %>% 
  mutate(pred = mean(mu + b_i + b_u + b_my + b_ry + b_g)) %>%
  .$pred

# RMSE over validation set #####
validation_preds_rmse <- RMSE(validation_preds, validation_g$rating)
validation_preds_rmse
# __0.8627351 ###### : mu <- mean(edx_g$rating)
# __0.8627354 ###### : mu <- mean(train_edx$rating)

# Revision

validation_preds_rev <- 
  validation_g %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_my, by = 'movie_year') %>%
  left_join(b_ry, by = 'rating_year') %>% 
  left_join(b_g, by = 'genres') %>% 
  group_by(userId, movieId) %>% 
  mutate(pred = mean(mu + b_i + b_u + b_my + b_ry + b_g))


validation_preds_rev %>% filter(userId == 621) %>% 
  select(userId, movieId, title, genres, rating, pred) %>%
  knitr::kable()

validation_preds_rev %>% filter(userId == 621) %>% 
  select(userId, movieId, title, genres, rating, pred) %>%
  ggplot(aes(rating, pred)) +
  geom_point()

validation_preds_rev %>% 
  select(userId, movieId, title, genres, rating, pred) %>%
  ggplot(aes(rating, pred)) +
  geom_point()

# Final Table ######

final_table <- data.frame(model = c("mu", 
                                    "bi", 
                                    "bi_bu", 
                                    "reg_bi_bu", 
                                    "reg_bi_bu_bmy", 
                                    "reg_bi_bu_bmy_bry", 
                                    "reg_bi_bu_bmy_bry_b_g",
                                    "validation_final_model"), 
                          rmse = c(naive_rmse, 
                                   movie_effect_rmse,  
                                   movie_user_effect_rmse, 
                                   reg_movie_user_effect_rmse,  
                                   reg_movie_user_effect_myear_rmse,
                                   reg_movie_user_myear_ryear_effect_rmse,
                                   reg_movie_user_myear_ryear_effect_gender_rmse,
                                   validation_preds_rmse)) %>% 
  knitr::kable()

final_table