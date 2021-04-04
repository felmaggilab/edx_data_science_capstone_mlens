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

# Although not decided yet, this change also can be useful to add time effects 
# in our final model to enhanced the RMSE results (if it is necessary).

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
# EXPLORATORY DATA ANALYSIS #######
# ___________________________________########

# Ratings frecuency 
edx %>% ggplot(aes(rating)) +
  geom_density()

mu <- mean(edx$rating)
mu

# ___________________________________########
#### TEST SET AND TRAIN SET ########
# ___________________________________########

# Creation of test set y validation set from edx data.frame ######

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
##### TESTING BASIC MODELS ######
# ___________________________________########
# We will start testing very basic models (included on bibliography of the 
# course, just for basic testing process)

# _______No regularization_________ #####

# Naive ######

naive_pred <- mean(train_edx$rating)

str(naive_pred)

naive_rmse <- RMSE(naive_pred, test_edx$rating)
naive_rmse
#__1.060167 = 0 points #####

# Movie Effects ######

mu <- mean(train_edx$rating)

movie_avgs <- train_edx %>% 
  group_by(movieId) %>% 
  summarise(b_i = mean(rating-mu))

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

# Provisional table: 

data.frame(naive_rmse, movie_effect_rmse, movie_user_effect_rmse, 
           reg_movie_user_effect_rmse) %>% knitr::kable()

# Test Validation 1: must be removed from code #######
# Regularized movie + user effects

test_val_1 <- 
  validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(mu = mu, pred = mu + b_i + b_u)

head(test_val_1)

# 4 Films have b_i = NA, thus pred = NA
# If there are NAs, RMSE function doens't work, and return NA
test_val_1 %>%  filter(is.na(pred))

# To solve the problem, we will replace the 4 NAs with "mu"
# (the avg rating of all movies)
test_val_1$pred[is.na(test_val$pred)] <- mu

# Then we have remove all 4 NAs with mu
test_val_1 %>%  filter(is.na(pred))
test_val_1 %>%  filter(is.na(b_i))

# We can now extract predictions...
test_val_1_pred <-   test_val %>% .$pred

# ...and calculate RMSE over validation test
test_1 <- RMSE(test_val_pred, validation$rating)
test_1 
# __0.8652589 = 15 points ######

# ___________________________________########
##### ADDING GENDER EFFECT ######
# ___________________________________########

edx <- edx %>% 
  mutate(genres = as.factor(genres))

# Gender Effects ######


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

head(train_edx)

train_edx <- train_edx %>% 
  mutate(genres = as.factor(genres))

class(train_edx$genres)

test_edx <- test_edx %>% 
  mutate(genres = as.factor(genres))

class(test_edx$genres)

g_i <- train_edx %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  group_by(genres) %>% 
  summarise(g_i = mean(rating) - b_u - b_i - mu)

movie_user_effect_gender_pred <- 
  test_edx %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(g_i, by = "genres") %>% 
  mutate(pred = mu + b_i + b_u + g_i) %>% 
  .$pred

head(test_edx)
  


# ___________________________________########
# RECOMMENDER LAB ######
# ___________________________________########

# As spected, results with Basic Model are very far from goods RMSEs.
# We will try with recommenderlab package. It include several recommend. Alg.
# including those that are barely explained in the course (SDV, for example)

# Sources ######
# https://cran.r-project.org/web/packages/recommenderlab/vignettes/recommenderlab.pdf
# https://cran.r-project.org/web/packages/recommenderlab/recommenderlab.pdf
# https://rpubs.com/elias_alegria/intro_recommenderlab
# https://rpubs.com/umasrinivas/683551
# http://rstudio-pubs-static.s3.amazonaws.com/287685_258f4041cee643f9aaf7b2b654a82162.html
# https://rstudio-pubs-static.s3.amazonaws.com/400285_89f33b0f92f5474ea16e745eeaa5681a.html
# http://www.stats.ox.ac.uk/~sejdinov/teaching/atsml19/UBCF_IBCF.html
# https://rpubs.com/vamshigvk/whatsthenextmovie
# https://www.statworx.com/ch/blog/movie-recommendation-with-recommenderlab/
# https://rpubs.com/Xkong100/505398
# https://www.r-bloggers.com/2014/12/recommender-systems-101-a-step-by-step-practical-example-in-r/


# Features seleccion: userId, movieID, rating ####

edx_recom_df <- edx %>%  select(user = userId, item = movieId, rating = rating)

# Creation of matrix with recommenderlab structure #####

edx_recom_matrix <- edx_recom_df %>% 
  as("realRatingMatrix") #__as("realRatingMatrix") ####

dim(edx_recom_matrix)

# Visualization for first 30 users and first 30 ratings #####

edx_recom_matrix[1:30, 1:30] %>% getRatingMatrix #__getRatingMatrix ####

# Image of matrix: sparce visualization  #####

image(edx_recom_matrix[1:30, 1:30] %>% getRatingMatrix)

# ___________________________________########
# MATRIX NORMALIZATION ######
# ___________________________________########

# User Normalization (Rows) #####

# Center 
edx_recom_matrix %>%  getRatings %>% hist(main = "Ratings")

edx_recom_matrix_cent <- edx_recom_matrix %>% #__Method = Center #####
normalize

edx_recom_matrix_cent %>% getRatings %>% 
  hist(main = "Ratings normalized: center") 

# Z-score
edx_recom_matrix_z <- edx_recom_matrix %>% 
  normalize(method="Z-score") #__Method = Z-score #####

# edx_recom_matrix_z %>% getRatings %>% hist("Ratings normalized: Z-score") ERROR

# ___________________________________########
# CLEANING DATA ######
# ___________________________________########

# We will work with users that have ranked at least 10 movies, and with 
# movies ranked at least 20 times
# Private Note: depending on results, we will change this paramater.

rowCounts(edx_recom_matrix_cent) %>% as("matrix") %>% min
colCounts(edx_recom_matrix_cent) %>% as("matrix") %>% min

edx_recom_matrix_cent <- 
  edx_recom_matrix_cent[,colCounts(edx_recom_matrix_cent) >= 20]

rowCounts(edx_recom_matrix_cent) %>% as("matrix") %>% min
colCounts(edx_recom_matrix_cent) %>% as("matrix") %>% min

#__________________________________________________________________
#__________________________________________________________________

# ___________________________________########
# EVALUATION SCHEME LITTLE ######
# ___________________________________########

# See detais with ?evaluationScheme for further tunning

# You will waite for a while... Be patient! It takes long time.

eval_scheme_little <- evaluationScheme(edx_recom_matrix_cent[1:600], method = "split", train = 0.9, given = 5)

# ___________________________________########
# I TEST: CENTER LITTLE ######
# ___________________________________########

train_l <- eval_scheme_little %>% getData("train")
known_l <- eval_scheme_little %>% getData("known")
unknown_l <- eval_scheme_little %>% getData("unknown")

# Applying Models #####

#__Random ####
random_model_l <- Recommender(train_l[1:450], "RANDOM")
pred_ramdom_l <- predict(random_model_l, known_l, type = "ratings")
error_random_l <- rbind("random" = calcPredictionAccuracy(pred_ramdom_l, unknown_l))
error_random_l
# RMSE      MSE      MAE
# random 1.424282 2.028579 1.103045

pred_ramdom_l %>% as("data.frame") %>% head
str(pred_ramdom_l)

test <- pred_ramdom_l %>% as("data.frame")
class(test)

unique(test$user)

# COMPARING RESULTS with edx entire Matrix
# RMSE      MSE      MAE
# random 1.450548 2.104089 1.117895

#__UBCF ERROR ####
ubcf_model_l <- Recommender(train_l[1:450], "UBCF")
pred_ubcf_l <- predict(ubcf_model_l, known_l, type = "ratings")

# Error in h(simpleError(msg, call)) : 
# error in evaluating the argument 'x' in selecting a method for function 't':
# not-yet-implemented method for <dgCMatrix> %*% <list>

error_ubcf_l <- rbind("ubcf" = calcPredictionAccuracy(pred_ubcf_l, unknown_l))
error_ubcf_l

#__IBCF ERROR ####
ibcf_model_l <- Recommender(train_l[1:450], "IBCF")
pred_ibcf_l <- predict(ibcf_model_l, known_l, type = "ratings")
error_ibcf_l <- rbind("ubcf" = calcPredictionAccuracy(ibcf_model_l, unknown_l))
error_ibcf_l

dim(pred_ibcf_l)

# Error in (function (classes, fdef, mtable):
# unable to find an inherited method for function ‘calcPredictionAccuracy’ for 
# signature ‘"Recommender", "realRatingMatrix"                                                                                                                                      unable to find an inherited method for function ‘calcPredictionAccuracy’ for signature ‘"Recommender", "realRatingMatrix"’

#__SVD ####
svd_model_l <- Recommender(train_l[1:450], "SVD")
pred_svd_l <- predict(svd_model_l, known_l, type = "ratings")
error_svd_l <- rbind("svd" = calcPredictionAccuracy(pred_svd_l, unknown_l))
error_svd_l
# RMSE      MSE       MAE
# svd 1.108122 1.227935 0.8825007


test <- pred_svd_l %>% as("data.frame")
head(test)

edx_recom_df %>%  filter(user == 10 & item == 294)


#__ALS ####
als_mode_l <- Recommender(train_l, "ALS")
pred_als_l <- predict(als_mode_l, known_l, type = "ratings")
error_als_l <- rbind("als" = calcPredictionAccuracy(pred_als_l, unknown_l))
error_als_l
# RMSE      MSE       MAE
# als 1.006225 1.012488 0.7879996

#__HYBRID ERROR ######
hybrid_model_l <- Recommender(train_l[1:450], "HYBRID")

# Error in (function (..., weights = NULL)  : 
# No base recommender specified!

pred_hybrid_l <- predict(hybrid_model_l, known_l, type = "ratings")
error_hybrid_l <- rbind("hybrid" = calcPredictionAccuracy(hybrid_model_l, unknown_l))
error_hybrid_l
