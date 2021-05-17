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
if(!require(fastDummies)) install.packages("fastDummies", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(Hmisc)) install.packages("Hmisc", repos = "http://cran.us.r-project.org")

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
library(fastDummies)
library(corrplot)
library(Hmisc)

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

head(movielens) %>% knitr::kable()


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

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

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

# ___________________________________########
# DEALING WITH GENRES ####
# ___________________________________########

# Test #####

edx_little %>% 
  as_tibble()

edx_little_g  <- edx_little %>% 
  separate_rows(genres, sep = "\\|")

edx_little_g %>% 
  select(userId, movieId, rating, title, genres, rating_year, movie_year) %>% 
  knitr::kable()

# ___________________________________########
# APPLYING CHANGES TO ENTIRE DATA ######
# EDX DATA FRAME (except genres) #####
# ___________________________________########

# Adding rating_date and rating_year #####
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
edx %>% 
  select(userId, movieId, rating, title, genres, rating_year, movie_year) %>% 
  head() %>% 
  filter(movieId == 122 | movieId == 185 | movieId == 292 | movieId == 355) %>% 
  knitr::kable()

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

# Dim edx######

dim(edx)
# 9000055       9

# Number of movies #####

length(unique(edx$movieId)) 
# 10677

# Number of users #####
length(unique(edx$userId)) 
# 69878

# min and max ratings ######
min(edx$rating)
max(edx$rating)

# Matrix creation #######
head(edx)

edx_recom <- edx %>% 
  select(userId, movieId, rating)

head(edx_recom)

edx_ui <- edx_recom %>% as("realRatingMatrix")
edx_ui[1:100,1:100] %>% getRatingMatrix

# Matrix visualization #######
image(edx_ui[1:50,1:50])

matrix_edx_ui <- edx_ui %>%
  as("matrix")
image(matrix_edx_ui[1:50,1:50])

# Ratings frecuency  ######
edx %>% ggplot(aes(rating)) +
  geom_density(fill = "#0072B2") +
  labs(title = "Rating frecuency")

avg_rating <- mean(edx$rating) # Avg rating ######
avg_rating
# 3.512465 

sd_rating <- sd(edx$rating) # SD rating ######
sd_rating
# 1.060331

# Distribution of number of ratings by movie ####
edx %>% 
  group_by(movieId) %>% 
  mutate(n =  n()) %>% 
  select(n) %>% 
  unique() %>% 
  ggplot(aes(n)) +
  geom_histogram(bins = 100, color = "#999999", fill = "#0072B2") +
  scale_x_continuous(trans = "log2") +
  labs(title = "Distribuion of number of ratings by movie") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))

# Top 10 Most Rated Movies ####
edx %>% 
  group_by(title) %>% 
  mutate(movie_ratings =  n()) %>% 
  select(movieId, title, movie_ratings) %>% 
  arrange(desc(movie_ratings)) %>% 
  unique() %>% 
  head(10) %>% 
  knitr::kable()

# The 10 least-rated movies ######
edx %>% 
  group_by(title) %>% 
  mutate(movie_ratings =  n()) %>% 
  select(movieId, title, movie_ratings) %>% 
  arrange(desc(movie_ratings)) %>% 
  unique() %>% 
  tail(10) %>% 
  knitr::kable()
  
# Distribution of number of ratings by user #####
edx %>% 
  group_by(userId) %>% 
  mutate(n =  n()) %>% 
  select(n) %>% 
  unique() %>% 
  ggplot(aes(n)) +
  geom_histogram(bins = 100, color = "#999999", fill = "#0072B2") +
  scale_x_continuous(trans = "log10") +
  labs(title = "Distribution of number of ratings by user") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))

# Top 10 most active users #####
edx %>% 
  group_by(userId) %>% 
  mutate(user_ratings =  n()) %>% 
  select(userId, user_ratings) %>% 
  arrange(desc(user_ratings)) %>% 
  unique() %>% 
  head(10) %>% 
  knitr::kable()

# The 10 least active users ######
edx %>% 
  group_by(userId) %>% 
  mutate(user_ratings =  n()) %>% 
  select(userId, user_ratings) %>% 
  arrange(desc(user_ratings)) %>% 
  unique() %>% 
  tail(10) %>% 
  knitr::kable()

# Distribution of avg. ratings per movie ######
edx %>% 
  group_by(movieId) %>% 
  mutate(avg_rating = mean(rating)) %>% 
  ggplot(aes(avg_rating)) +
  geom_histogram(bins = 100, color = "#999999", fill = "#0072B2") +
  labs(title = "Distribution of avg. ratings per movie") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))

# The 10 highest rated films ####
# __Not regularized ####
edx %>% 
  group_by(movieId) %>% 
  mutate(avg_rating = mean(rating), movie_ratings = n()) %>% #Not normalized
  select(movieId, title, avg_rating, movie_ratings) %>% 
  arrange(desc(avg_rating)) %>% 
  unique() %>% 
  head(10) %>% 
  knitr::kable()

# __Regularized ####
edx %>% 
  group_by(movieId) %>% 
  mutate(avg_rating = (sum(rating))/(n()+lambda), movie_ratings = n()) %>% 
  select(movieId, title, avg_rating, movie_ratings) %>% 
  arrange(desc(avg_rating)) %>% 
  unique() %>% 
  head(10) %>% 
  knitr::kable()

# The 10 worst rated movies #####
# __Not regularized ####
edx %>% 
  group_by(movieId) %>% 
  mutate(avg_rating = mean(rating), movie_ratings = n()) %>% 
  select(movieId, title, avg_rating, movie_ratings) %>% 
  arrange(desc(avg_rating)) %>% 
  unique() %>% 
  tail(10) %>% 
  knitr::kable()

# __Regularized ####
edx %>% 
  group_by(movieId) %>% 
  mutate(avg_rating = (sum(rating))/(n()+lambda), movie_ratings = n()) %>% 
  select(movieId, title, avg_rating, movie_ratings) %>% 
  arrange(desc(avg_rating)) %>% 
  unique() %>% 
  tail(10) %>% 
  knitr::kable()

edx %>% 
  group_by(movieId) %>% 
  mutate(avg_rating = (sum(rating))/(n())) %>% 
  select(movieId, title, avg_rating) %>% 
  arrange(desc(avg_rating)) %>% 
  unique() %>% 
  tail(10) %>% 
  knitr::kable()

# Distribution of avg. ratings per user ######
edx %>% 
  group_by(userId) %>% 
  mutate(avg_rating = mean(rating)) %>% 
  ggplot(aes(avg_rating)) +
  geom_histogram(bins = 100, color = "#999999", fill = "#0072B2") +
  labs(title = "Distribution of avg. ratings per user") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))

# The 10 users with the highest average rating #####
# __Not regularized ####
edx %>% 
  group_by(userId) %>% 
  mutate(avg_rating = mean(rating), user_ratings = n()) %>% #Not normalized
  select(userId, avg_rating, user_ratings) %>% 
  arrange(desc(avg_rating)) %>% 
  unique() %>% 
  head(10) %>% 
  knitr::kable()

# __Regularized ####
edx %>% 
  group_by(userId) %>% 
  mutate(avg_rating = (sum(rating))/(n()+lambda), user_ratings = n()) %>% 
  select(userId, avg_rating, user_ratings) %>% 
  arrange(desc(avg_rating)) %>% 
  unique() %>% 
  head(10) %>% 
  knitr::kable()

# The 10 users with the lowest average rating #####
# __Not regularized ####
edx %>% 
  group_by(userId) %>% 
  mutate(avg_rating = mean(rating), user_ratings = n()) %>% #Not normalized
  select(userId, avg_rating, user_ratings) %>% 
  arrange(desc(avg_rating)) %>% 
  unique() %>% 
  tail(10) %>% 
  knitr::kable()

# __Regularized ####
edx %>% 
  group_by(userId) %>% 
  mutate(avg_rating = (sum(rating))/(n()+lambda), user_ratings = n()) %>% 
  select(userId, avg_rating, user_ratings) %>% 
  arrange(desc(avg_rating)) %>% 
  unique() %>% 
  tail(10) %>% 
  knitr::kable()

edx %>% arrange(movie_year)
# Films from 1915 to 2008

edx %>% arrange(rating_year)
# Ratings from 1995 to 2008

# Visualization of average ratings vs movie_year #####
edx %>% 
  group_by(movie_year) %>% 
  summarise(avg_rating = mean(rating)) %>% 
  ggplot(aes(movie_year, avg_rating)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Average ratings vs movie_year") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))

# Movie release year, movies rated, ratings, ratings per movie #####
edx %>% 
  group_by(movie_year) %>% 
  summarise(movie_year, movies_rated = length(unique(title)), ratings = n(),
            ratings_per_movie = round(ratings/movies_rated)) %>% 
  arrange(ratings) %>% 
  unique() %>% 
  head(10) %>% 
  knitr::kable()

edx %>% 
  group_by(movie_year) %>% 
  summarise(movie_year, movies_rated = length(unique(title)), ratings = n(),
            ratings_per_movie = round(ratings/movies_rated)) %>% 
  arrange(ratings_per_movie) %>% 
  unique() %>% 
  tail(10) %>% 
  knitr::kable()

# Movies per realease year ######
edx %>% 
  group_by(movie_year) %>% 
  summarise(movie_year, movies_rated = length(unique(title)), ratings = n(),
            ratings_per_movie = round(ratings/movies_rated)) %>% 
  arrange(ratings_per_movie) %>% 
  unique() %>% 
  ggplot(aes(movie_year, movies_rated)) +
  geom_line() +
  labs(title = "Movies per realease year") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))

# Number of movies per relase year  #####
edx %>%   
  group_by(movie_year) %>% 
  summarise(movie_year, movies = length(unique(title))) %>% 
  unique()

# Movies released in 1917 #####
edx %>% 
  filter(movie_year == "1917") %>% 
  group_by(title) %>% 
  summarise(avg_rating = mean(rating), title, ratings= n()) %>%
  unique() %>% 
  # head(10) %>% 
  knitr::kable()

# Movies released in 1940 #####
#__arrange by number of ratings####
edx %>% 
  filter(movie_year == "1940") %>% 
  group_by(title) %>% 
  summarise(avg_rating = mean(rating), title, ratings= n()) %>% 
  arrange(desc(ratings)) %>% 
  unique() %>% 
  head(10) %>% 
  knitr::kable()

#__arrange by avg_rating ####
edx %>% 
  filter(movie_year == "1940") %>% 
  group_by(title) %>% 
  summarise(avg_rating = mean(rating), title, ratings= n()) %>% 
  arrange(desc(avg_rating)) %>% 
  unique() %>% 
  head(10) %>% 
  knitr::kable()

# __Philadelphia Story, The (1940) ########
edx %>% 
  filter(title == "Philadelphia Story, The (1940)") %>% 
  summarise(rating = rating) %>% 
  ggplot(aes(rating)) +
  geom_histogram(color = "#999999", fill = "#0072B2")

# Movies released in 1995 #####
#__arrange by number of ratings####
edx %>% 
  filter(movie_year == "1995") %>% 
  group_by(title) %>% 
  summarise(avg_rating = mean(rating), title, ratings= n()) %>% 
  arrange(desc(ratings)) %>% 
  unique() %>% 
  head(10) %>%
  knitr::kable()

#__arrange by avg_rating####
edx %>% 
  filter(movie_year == "1995") %>% 
  group_by(title) %>% 
  summarise(avg_rating = mean(rating), title, ratings= n()) %>% 
  arrange(desc(avg_rating)) %>% 
  unique() %>% 
  head(10) %>%
  knitr::kable()

#__arrange by avg_rating: Regularized ####
edx %>% 
  filter(movie_year == "1995") %>% 
  group_by(title) %>% 
  summarise(avg_rating = (sum(rating))/(n()+lambda), title, ratings= n()) %>% 
  arrange(desc(avg_rating)) %>% 
  unique() %>% 
  head(10) %>%
  knitr::kable()

edx %>% 
  filter(movie_year == "1995") %>% 
  group_by(title) %>% 
  summarise(avg_rating = mean(rating), title, ratings= n()) %>% 
  arrange(desc(ratings)) %>% 
  unique() %>% 
  tail(10) %>%
  knitr::kable()

# __Batman Forever (1995) ########
edx %>% 
  filter(title == "Batman Forever (1995)") %>% 
  summarise(rating = rating) %>% 
  ggplot(aes(rating)) +
  geom_histogram(color = "#999999", fill = "#0072B2")

# Movies released in 2000 #####
edx %>% 
  filter(movie_year == "2000") %>% 
  group_by(title) %>% 
  summarise(title, avg_rating = mean(rating),title, ratings= n()) %>% 
  arrange(desc(ratings)) %>% 
  unique() %>% 
  head(10) %>%
  knitr::kable()

# There is a clear effect of the premiere year, on the average ratings. 
# This must be taken into account in our final model

# Visualization of average ratings vs rating_year #####
edx %>% 
  group_by(rating_year) %>% 
  summarise(rating_avgs = mean(rating)) %>% 
  ggplot(aes(rating_year, rating_avgs)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Average ratings vs rating_year") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))

# Movie rating year, movies rated, ratings, ratings per movie, avg_rating #####
edx %>% 
  group_by(rating_year) %>% 
  summarise(rating_year, movies_rated = length(unique(title)), ratings = n(),
            ratings_per_movie = round(ratings/movies_rated), 
            avg_rating = round(mean(rating),2)) %>% 
  # arrange(ratings) %>% 
  unique() %>% 
  knitr::kable()

# Movies rated in 1996 #####
edx %>% 
  filter(rating_year == "1996") %>% 
  group_by(title) %>% 
  summarise(avg_rating = mean(rating), title, ratings= n()) %>% 
  arrange(desc(ratings)) %>% 
  unique() %>% 
  head(15) %>%
  knitr::kable()

# Movies rated in 1999 #####
edx %>% 
  filter(rating_year == "1999") %>% 
  group_by(title) %>% 
  summarise(avg_rating = mean(rating), title, ratings= n()) %>% 
  arrange(desc(ratings)) %>% 
  unique() %>% 
  head(15) %>%
  knitr::kable()

# Movies rated in 2000 #####
edx %>% 
  filter(rating_year == "2000") %>% 
  group_by(title) %>% 
  summarise(avg_rating = mean(rating), title, ratings= n()) %>% 
  arrange(desc(ratings)) %>% 
  unique() %>% 
  head(15) %>%
  knitr::kable()

# Movies rated in 2008 #####
edx %>% 
  filter(rating_year == "2008") %>% 
  group_by(title) %>% 
  summarise(avg_rating = mean(rating), title, ratings= n()) %>% 
  arrange(desc(ratings)) %>% 
  unique() %>% 
  head(15) %>%
  knitr::kable()

# There is a certain effect of the rating year on the average ratings. 
# We will add this to the model, but the result on the rmse should not 
# be very pronounced.

# ___________________________________########
##### EXPLORING GENRE EFFECT  ######
##### APPROACH 1 ######
# ___________________________________########

# In order to use all the data provided in the data set, we will explore the
# genre effect. At this point, we are not sure if this approach will introduce
# noise to the model or, on the contrary, it will improve the RMSE

# To add genre effect, we will separate geres by row (this change add more)
# rows to the data sets (approach 1).

# Separation of genres by rows  #######
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

cor(genres_table$avg_rating, genres_table$ratings)

genres_selected_top <- genres_table %>% 
  filter(genres %in% c("Film-Noir", "Documentary", "War", "IMAX"))

genres_selected_middle <- genres_table %>% 
  filter(genres %in% c("Mystery", "Crime", "Drama", "Animation", 
                       "Western", "Musical", "Animation", "Children, Fantasy",
                       "Romance", "Adventure", "Thriller"))

genres_selected_bottom <- genres_table %>% 
  filter(genres %in% c("Horror", "Children", "Sci-Fi", "Action", "Comedy"))

genres_table %>% 
  ggplot(aes(ratings, avg_rating, color = genres)) +
  geom_point(aes(color = genres), show.legend = FALSE) +
  #geom_encircle(aes(x = ratings, y = avg_rating), 
                #data = genres_selected_top, 
                #color="red", 
                #size=2, 
                #expand=0.07) +
  #geom_encircle(aes(x = ratings, y = avg_rating), 
                #data = genres_selected_middle, 
                #color="red", 
                #size=2, 
                #expand=0.07) +
  #geom_encircle(aes(x = ratings, y = avg_rating), 
  #data = genres_selected_bottom, 
  #color="red", 
  #size=2, 
  #expand=0.07) +
  geom_text(aes(ratings, avg_rating, label = genres), nudge_y = -0.02) +
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
  scale_y_continuous(trans = "log10") +
  labs(title = "Movie year versus genders rating counts")


# Distributions of Ratings by Genres: NOT RUN! It crash R #
# edx_g %>% 
# filter(!genres == "(no genres listed)") %>% 
# ggplot(aes(genres, rating)) +
# geom_boxplot() +
# geom_jitter(width = 0.1, alpha = 0.2) +
# theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
# labs(title = "Distributions of Ratings by Genres")

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
  labs(title = "Distributions of Ratings by Genres. User 59269") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))


# Distributions of Ratings by Genres: Sample 10% obs. ######

n <- nrow(edx_g) * 0.1

set.seed(1970, sample.kind="Rounding")

sample_edx_g <- sample_n(edx_g, n)

# Comparison between edx avg gender rating and random sample ####

avg_genres_2 <- edx_g %>% 
  group_by(genres) %>% 
  summarise(avg_rating = mean(rating), sd = sd(rating)) %>% 
  arrange(desc(avg_rating))

avg_genres_sample <- sample_edx_g %>% 
  group_by(genres) %>% 
  summarise(avg_rating = mean(rating), sd = sd(rating)) %>% 
  arrange(desc(avg_rating))

edx_sample_comp <- avg_genres_2 %>% 
  left_join(avg_genres_sample, by = 'genres') %>% 
  filter(!genres == "(no genres listed)")

edx_sample_comp

# Distributions of Ratings by Genres: Ramdom Sample ######
sample_edx_g %>% 
  filter(!genres == "(no genres listed)") %>% 
  ggplot(aes(genres, rating)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Distributions of Ratings by Genres. Sample")

# Density avg rating by genres ######

# Order by avg_rating
edx_g$genres <- factor(edx_g$genres, levels = c("Film-Noir",
                                                "Documentary",
                                                "War",
                                                "IMAX",
                                                "Mystery",
                                                "Drama",
                                                "Crime",
                                                "Animation",
                                                "Musical",
                                                "Western",
                                                "Romance",
                                                "Thriller",
                                                "Fantasy",
                                                "Adventure",
                                                "Comedy",
                                                "Action",
                                                "Children",
                                                "Sci-Fi",
                                                "Horror"))

# __Grouped by movie #####
edx_g %>% 
  filter(!genres == "(no genres listed)") %>% 
  group_by(movieId) %>% 
  summarise(genres = genres, avg_rating = mean(rating)) %>% 
  ggplot(aes(avg_rating, fill = genres)) +
  geom_density(alpha = 0.2) +
  scale_y_continuous(breaks=NULL) + 
  labs(title = "Density avg rating by gender, grouped by movie")

# __Grouped by movie: Film Noir vs. Comedy - Density #####
edx_g %>% 
  filter(genres == "Film-Noir"
         | genres == "Comedy") %>%
  group_by(movieId) %>% 
  summarise(genres = genres, avg_rating = mean(rating)) %>% 
  ggplot(aes(avg_rating,  fill = genres)) +
  geom_density(alpha = 0.2) +
  labs(title = "Density of avg rating by gender, grouped by movie: Film-Noir vs. Comedy") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))

# __Grouped by movie: Film Noir vs. Comedy - Count #####
edx_g %>% 
  filter(genres == "Film-Noir"
         | genres == "Comedy") %>%
  group_by(movieId) %>% 
  summarise(genres = genres, avg_rating = mean(rating)) %>% 
  ggplot(aes(avg_rating, y = ..count..,  fill = genres)) +
  geom_density(alpha = 0.2) +
  labs(title = "Count of avg rating by gender, grouped by movie: Film-Noir vs. Comedy") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))

# __Grouped by movie: Documentary vs. Drama #####
edx_g %>% 
  filter(genres == "Documentary"
         | genres == "Drama") %>%
  group_by(movieId) %>% 
  summarise(genres = genres, avg_rating = mean(rating)) %>% 
  ggplot(aes(avg_rating, fill = genres)) +
  geom_density() +
  labs(title = "Density avg rating by gender, grouped by movie: Documentary vs. Drama") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
  facet_grid(genres ~ .)

# __Grouped by user #####
edx_g %>% 
  filter(!genres == "(no genres listed)") %>%
  group_by(userId) %>% 
  summarise(genres = genres, avg_rating = mean(rating)) %>% 
  ggplot(aes(avg_rating, y = ..count.., fill = genres)) +
  geom_density() +
  scale_y_continuous(breaks=NULL) + #Las borramos.
  #labs(title = "Density avg rating by gender, grouped by user") +
  facet_grid(genres ~ .)

  # ___________________________________########
  ##### Correlograms  ######
  # ___________________________________########
  
#Grouped by rating_year #######
  
genre_avg_rating_r_year <- edx_g %>% 
filter(!rating_year == "1995" & !genres == "(no genres listed)") %>% 
group_by(rating_year, genres) %>% 
summarise(avg_rating = mean(rating))
  
spread_edx_g_temp_r_y <- genre_avg_rating_r_year %>% 
    spread(genres, avg_rating)
  
spread_edx_g_temp_r_y %>% 
    knitr::kable()
  
spread_edx_g_r_y <- as.data.frame(spread_edx_g_temp_r_y) %>% 
select(Action,Adventure,Animation,Children,Comedy,Crime,
       Documentary,Drama,Fantasy,"Film-Noir",Horror,IMAX,
       Musical,Mystery,Romance,"Sci-Fi", Thriller, War, Western)
  
cor_mat_r_y <- cor(spread_edx_g_r_y, method = "pearson", use = "complete.obs")

corrplot(cor_mat_r_y, type = "upper", order = "alphabet", 
         tl.col = "black", tl.srt = 45)

cor_mat_r_y_2 <- rcorr(as.matrix(spread_edx_g_r_y))


#__alphabet order ######
corrplot(cor_mat_r_y_2$r, order="alphabet", 
         p.mat = cor_mat_r_y_2$P, sig.level = 0.05, insig = "blank", 
         tl.col = "black", tl.srt = 45)

#__hclust order ######
corrplot(cor_mat_r_y_2$r, order="hclust", 
         p.mat = cor_mat_r_y_2$P, sig.level = 0.05, insig = "blank", 
         tl.col = "black", tl.srt = 45)

#__FPC order ######
corrplot(cor_mat_r_y_2$r, order="FPC", 
         p.mat = cor_mat_r_y_2$P, sig.level = 0.05, insig = "blank", 
         tl.col = "black", tl.srt = 45)

# Grouped by movie_year #######
  
genre_avg_rating_m_year <- edx_g %>% 
    filter(!genres == "(no genres listed)") %>% 
    group_by(movie_year, genres) %>% 
    summarise(avg_rating = mean(rating))
  
spread_edx_g_temp_m_y <- genre_avg_rating_m_year %>% 
    spread(genres, avg_rating)
  
spread_edx_g_m_y <- as.data.frame(spread_edx_g_temp_m_y) %>% 
    select(Action,Adventure,Animation,Children,Comedy,Crime,
           Documentary,Drama,Fantasy,"Film-Noir",Horror,IMAX,
           Musical,Mystery,Romance,"Sci-Fi", Thriller, War, Western)
  
cor_mat_m_y <- cor(spread_edx_g_m_y, method = "pearson", use = "complete.obs")
  
corrplot(cor_mat_m_y, type = "upper", order = "alphabet", 
        tl.col = "black", tl.srt = 45)

cor_mat_m_y_2 <- rcorr(as.matrix(spread_edx_g_m_y))

#__alphabet order ######
corrplot(cor_mat_m_y_2$r, order="alphabet", 
         p.mat = cor_mat_m_y_2$P, sig.level = 0.05, insig = "blank",
         tl.col = "black", tl.srt = 45)

#__hclust order ######
corrplot(cor_mat_m_y_2$r, order="hclust", 
         p.mat = cor_mat_m_y_2$P, sig.level = 0.05, insig = "blank",
         tl.col = "black", tl.srt = 45)

#__FPC order ######
corrplot(cor_mat_m_y_2$r, order="FPC", 
         p.mat = cor_mat_m_y_2$P, sig.level = 0.05, insig = "blank",
         tl.col = "black", tl.srt = 45)

#Grouped by userId #######
  
genre_avg_rating_uid <- edx_g %>% 
    filter(!genres == "(no genres listed)") %>% 
    group_by(userId, genres) %>% 
    summarise(avg_rating = mean(rating))
  
spread_edx_g_temp_uid <- genre_avg_rating_uid %>% 
    spread(genres, avg_rating)
  
spread_edx_g_uid <- as.data.frame(spread_edx_g_temp_uid) %>% 
    select(Action,Adventure,Animation,Children,Comedy,Crime,
           Documentary,Drama,Fantasy,"Film-Noir",Horror,IMAX,
           Musical,Mystery,Romance,"Sci-Fi", Thriller, War, Western)
  
cor_mat_uid <- cor(spread_edx_g_uid, method = "pearson", use = "complete.obs")
  
corrplot(cor_mat_uid, type = "upper", order = "alphabet", 
           tl.col = "black", tl.srt = 45)

cor_mat_uid_2 <- rcorr(as.matrix(spread_edx_g_uid))

#__alphabet order ######
corrplot(cor_mat_uid_2$r, order="alphabet", 
         p.mat = cor_mat_uid_2$P, sig.level = 0.05, insig = "blank",
         tl.col = "black", tl.srt = 45)

#__hclust order ######
corrplot(cor_mat_uid_2$r, order="hclust", 
         p.mat = cor_mat_uid_2$P, sig.level = 0.05, insig = "blank",
         tl.col = "black", tl.srt = 45)

#__FPC order ######
corrplot(cor_mat_uid_2$r, order="FPC", 
         p.mat = cor_mat_uid_2$P, sig.level = 0.05, insig = "blank",
         tl.col = "black", tl.srt = 45)

#__Principal component analysis ######

#Data scaling
cor_mat_uid_2.sc <- scale(cor_mat_uid_2$r)
#Extraction of Principal Components
cor_mat_uid_2.pc <- prcomp(cor_mat_uid_2.sc)
#Judging the number of components
plot(cor_mat_uid_2.pc, type = "l")
  
# Grouped by movieId: not possible #####
  
genre_avg_rating_mid <- edx_g %>% 
    filter(!genres == "(no genres listed)") %>% 
    group_by(movieId, genres) %>% 
    summarise(avg_rating = mean(rating))
  
spread_edx_g_temp_mid <- genre_avg_rating_mid %>% 
    spread(genres, avg_rating)
  
spread_edx_g_mid <- as.data.frame(spread_edx_g_temp_mid) %>% 
    select(Action,Adventure,Animation,Children,Comedy,Crime,
           Documentary,Drama,Fantasy,"Film-Noir",Horror,IMAX,
           Musical,Mystery,Romance,"Sci-Fi", Thriller, War, Western)
  
cor_mat_mid <- cor(spread_edx_g_mid, method = "pearson", use = "complete.obs")
# Error in cor(spread_edx_g_mid, method = "pearson", use = "complete.obs") : 
# no complete element pairs
  

# ___________________________________########
##### RECOMMENDERLAB: dismissed ######
# ___________________________________########

# Creating Matrix #####

head(edx)

edx_recom <- edx %>% 
  select(userId, movieId, rating)

head(edx_recom)

edx_ui <- edx_recom %>% as("realRatingMatrix")
edx_ui[1:100,1:100] %>% getRatingMatrix

image(edx_ui[1:50,1:50])

# Cleaning the Matrix: At least 10 ratings #####

# Min ratings by user
rowCounts(edx_ui) %>% as("matrix") %>% min

# Min ratings by movie
colCounts(edx_ui) %>% as("matrix") %>% min

edx_ui <- edx_ui[,colCounts(edx_ui)>= 10]


# NOT RUN !!!!! it takes a lot of time!
# Eventually these attempts were dismissed
# ______________________________________

# Source: https://rpubs.com/elias_alegria/intro_recommenderlab 

# Evaluation shceme ####

# eval_scheme <- evaluationScheme(edx_ui, method = "split", train = 0.9, given = 5)

# Train and Test Sets ######

# train <- eval_scheme %>% getData("train")
# known <- eval_scheme %>% getData("known")
# unknown <- eval_scheme %>% getData("unknown")

# Trainning Models ######

# ubcf_model <- Recommender(train, "UBCF")
# ibcf_model <- Recommender(train, "IBCF")
# svd_model <- Recommender(train, "SVD")

# Predictions #####

# ubcf_pred <- predict(ubcf_model, known, type = "ratings")
# ibcf_pred <- predict(ibcf_model, known, type = "ratings")
# svd_pred <- predict(svd_model, known, type = "ratings")

# Error ######

# error_calc <- rbind("ubcf" = calcPredictionAccuracy(ubcf_pred, unknown),
              # "ibcf" = calcPredictionAccuracy(ibcf_pred, unknown),
              # "svd" = calcPredictionAccuracy(svd_pred, unknown))

# error_calc

# Using a little versión of edx #####
# Run only if you want test the package.

# set.seed(1970, sample.kind="Rounding")
# edx_ui_little <- edx_ui[sample(1:nrow(edx_ui), size=10000),]

# eval_scheme <- evaluationScheme(edx_ui_little, method = "split", train = 0.9, given = 5)

# train <- eval_scheme %>% getData("train")
# known <- eval_scheme %>% getData("known")
# eval_scheme %>% getData("unknown")

# ubcf_model <- Recommender(train, "UBCF")
# ibcf_model <- Recommender(train, "IBCF") # it takes time even with sample !
# scv_model <- Recommender(train, "SVD")

# ubcf_pred <- predict(ubcf_model, known, type = "ratings") # it takes time even with sample !
# ibcf_pred <- predict(ibcf_model, known, type = "ratings")
# svd_pred <- predict(svd_model, known, type = "ratings")

# error_calc <- rbind("ubcf" = calcPredictionAccuracy(ubcf_pred, unknown),
                    # "ibcf" = calcPredictionAccuracy(ibcf_pred, unknown),
                    # "svd" = calcPredictionAccuracy(svd_pred, unknown))

# error_calc

# This approach is dismissed

# ___________________________________########
##### TESTING BASIC MODELS ######
# ___________________________________########

# We will start with very basic models (included on bibliography of the 
# course, just for basic testing process, and see how rmse improves after each
# model improvement)

# No regularization  #####

# __Naive: mu ######

naive_pred <- mean(train_edx$rating)

str(naive_pred)

naive_rmse <- RMSE(naive_pred, test_edx$rating)
naive_rmse
#__1.060167 #####

# __Movie Effects: b_i ######

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

# __Movie User Effects: b_i + b_u #######

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

# Regularization ######

# Now: we will apply regularization to see if we get some improvements in our 
# RMSE

lambdas <- seq(0, 10, 0.25)

# __Lambda best-tune to Move Effects and User Efects Regularization #####
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

# __Predictions with best tune lambda #######
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

# __Regularized Movie + User Effects #####

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
##### ADDING GENRE EFFECT  ######
###### APPROACH 1  ######
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

reg_movie_user_myear_ryear_genre_effect_pred <- 
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

# Regularized Movie + User + Movie Year + Rating Year + Genre #####

reg_movie_user_myear_ryear_effect_genre_rmse <- 
  RMSE(reg_movie_user_myear_ryear_genre_effect_pred, 
       test_edx_g$rating)

length(reg_movie_user_myear_ryear_genre_effect_pred)
length(test_edx_g$rating)

reg_movie_user_myear_ryear_effect_genre_rmse
#__0.864636 orig model no genre #####
#__0.8631034 genre and mu orig #####
#__0.8631031 genre and mu g #####
#__0.8630654 genre and mean pred ######


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

revision_preds_g_mean <- 
  test_edx_g %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_my, by = 'movie_year') %>%
  left_join(b_ry, by = 'rating_year') %>% 
  left_join(b_g, by = 'genres') %>% 
  group_by(userId, movieId) %>% # <- Change
  mutate(pred = mean(mu + b_i + b_u + b_my + b_ry + b_g)) # <- Change (mean)

revision_preds_g_mean %>% filter(userId == 325) %>% 
  select(userId, movieId, title, genres, rating, pred) %>%
  knitr::kable()

# ___________________________________########
##### ADDING GENRE EFFECT ######
###### APPROACH 2  ######
# ___________________________________########

# In this second approach, instead of separating the genders into rows, 
# dummy variables are used to incorporate the genres into the model. 

# sum(xui * BetaK)

# In this case, no observations are added. The number of rows in the datasets #
# is not altered. Instead, new columns are added (as many as genres). 
# The values for each genre become 0 and 1. If a movie belongs to a specific 
# genre, the value of the column for that genre will be 1 for that movie

##### Adding dummy variables in train set #####

# replacing "|" by "," ("|" is not interpreted by dummy_columns funtion)
train_edx_genres <- train_edx %>% 
  mutate(genres = str_replace_all(genres, '\\|', ','))

# Creating dummy columns
train_edx_genres <- dummy_columns(train_edx_genres, select_columns = c("genres"), 
                                  split = ",",
                                  remove_first_dummy = TRUE)

# Replacing "-" by "_" in Fil-Noir and Sci-Fi 
train_edx_genres <- train_edx_genres %>% rename(genres_Film_Noir = "genres_Film-Noir",
                                                genres_Sci_Fi = "genres_Sci-Fi")

dim(train_edx_genres)

str(train_edx_genres)

train_edx_genres %>% 
  select(movieId, genres, genres_Action, genres_Adventure, genres_Sci_Fi) %>% 
  head() %>% 
  knitr::kable()

##### Adding dummy variables in test set #####

test_edx_genres <- test_edx %>% 
  mutate(genres = str_replace_all(genres, '\\|', ','))

test_edx_genres <- dummy_columns(test_edx_genres, select_columns = c("genres"), 
                                 split = ",",
                                 remove_first_dummy = TRUE)

test_edx_genres <- test_edx_genres %>% rename(genres_Film_Noir = "genres_Film-Noir",
                                              genres_Sci_Fi = "genres_Sci-Fi")

dim(test_edx_genres)

str(test_edx_genres)

# Constructing the model #####

# The idea is to calculate a weight (Beta) for each genre, and multiply that weight 
# by 1 if the film belongs to that genre (by zero otherwise)

#(Here we use lambda = 0.5 as best tune finded previously)

mu_genres <- mean(train_edx_genres$rating) ##__avg rating #####

b_i_genres <- train_edx_genres %>%
  group_by(movieId) %>%
  summarize(b_i_genres = sum(rating - mu_genres)/(n()+lambda)) # __b_i ####

b_u_genres <- train_edx_genres %>% # __b_u ######
  left_join(b_i_genres, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u_genres = sum(rating - b_i_genres - mu_genres)/(n()+lambda))

# __Calculating Betas for every genre ####

# The weight (Beta) of each genre is calculated as:
# rating - regularized avg genre rating - b_i - b_u 

Beta_Action <- 
  train_edx_genres %>% 
  left_join(b_i_genres, by = "movieId") %>%
  left_join(b_u_genres, by = "userId")%>%
  summarise(Beta_Action = mean(rating - sum(rating*genres_Action)/(sum(genres_Action)+lambda) - b_i_genres - b_u_genres))

Beta_Adventure <- 
  train_edx_genres %>% 
  left_join(b_i_genres, by = "movieId") %>%
  left_join(b_u_genres, by = "userId") %>%
  summarise(Beta_Adventure = mean(rating - sum(rating*genres_Adventure)/(sum(genres_Adventure)+lambda) - b_i_genres - b_u_genres))

Beta_Animation <- 
  train_edx_genres %>% 
  left_join(b_i_genres, by = "movieId") %>%
  left_join(b_u_genres, by = "userId") %>%
  summarise(Beta_Animation = mean(rating - sum(rating*genres_Animation)/(sum(genres_Animation)+lambda) - b_i_genres - b_u_genres))

Beta_Children <- 
  train_edx_genres %>% 
  left_join(b_i_genres, by = "movieId") %>%
  left_join(b_u_genres, by = "userId") %>%
  summarise(Beta_Children = mean(rating - sum(rating*genres_Children)/(sum(genres_Children)+lambda) - b_i_genres - b_u_genres))

Beta_Comedy <- 
  train_edx_genres %>% 
  left_join(b_i_genres, by = "movieId") %>%
  left_join(b_u_genres, by = "userId") %>%
  summarise(Beta_Comedy = mean(rating - sum(rating*genres_Comedy)/(sum(genres_Comedy)+lambda) - b_i_genres - b_u_genres))

Beta_Fantasy  <- 
  train_edx_genres %>% 
  left_join(b_i_genres, by = "movieId") %>%
  left_join(b_u_genres, by = "userId") %>%
  summarise(Beta_Fantasy = mean(rating - sum(rating*genres_Fantasy)/(sum(genres_Fantasy)+lambda) - b_i_genres - b_u_genres))

Beta_IMAX  <- 
  train_edx_genres %>% 
  left_join(b_i_genres, by = "movieId") %>%
  left_join(b_u_genres, by = "userId") %>%
  summarise(Beta_IMAX = mean(rating - sum(rating*genres_IMAX)/(sum(genres_IMAX)+lambda) - b_i_genres - b_u_genres))

Beta_Sci_Fi  <- 
  train_edx_genres %>% 
  left_join(b_i_genres, by = "movieId") %>%
  left_join(b_u_genres, by = "userId") %>%
  summarise(Beta_Sci_Fi = mean(rating - sum(rating*genres_Sci_Fi)/(sum(genres_Sci_Fi)+lambda) - b_i_genres - b_u_genres))

Beta_Drama  <- 
  train_edx_genres %>% 
  left_join(b_i_genres, by = "movieId") %>%
  left_join(b_u_genres, by = "userId") %>%
  summarise(Beta_Drama = mean(rating - sum(rating*genres_Drama)/(sum(genres_Drama)+lambda) - b_i_genres - b_u_genres))

Beta_Horror <- 
  train_edx_genres %>% 
  left_join(b_i_genres, by = "movieId") %>%
  left_join(b_u_genres, by = "userId") %>%
  summarise(Beta_Horror  = mean(rating - sum(rating*genres_Horror)/(sum(genres_Horror)+lambda) - b_i_genres - b_u_genres))

Beta_Mystery <- 
  train_edx_genres %>% 
  left_join(b_i_genres, by = "movieId") %>%
  left_join(b_u_genres, by = "userId") %>%
  summarise(Beta_Mystery  = mean(rating - sum(rating*genres_Mystery)/(sum(genres_Mystery)+lambda) - b_i_genres - b_u_genres))

Beta_Romance  <- 
  train_edx_genres %>% 
  left_join(b_i_genres, by = "movieId") %>%
  left_join(b_u_genres, by = "userId") %>%
  summarise(Beta_Romance = mean(rating - sum(rating*genres_Romance)/(sum(genres_Romance)+lambda) - b_i_genres - b_u_genres))

Beta_Thriller <- 
  train_edx_genres %>% 
  left_join(b_i_genres, by = "movieId") %>%
  left_join(b_u_genres, by = "userId") %>%
  summarise(Beta_Thriller = mean(rating - sum(rating*genres_Thriller)/(sum(genres_Thriller)+lambda) - b_i_genres - b_u_genres))

Beta_Crime  <- 
  train_edx_genres %>% 
  left_join(b_i_genres, by = "movieId") %>%
  left_join(b_u_genres, by = "userId") %>%
  summarise(Beta_Crime = mean(rating - sum(rating*genres_Crime)/(sum(genres_Crime)+lambda) - b_i_genres - b_u_genres))

Beta_War  <- 
  train_edx_genres %>% 
  left_join(b_i_genres, by = "movieId") %>%
  left_join(b_u_genres, by = "userId") %>%
  summarise(Beta_War = mean(rating - sum(rating*genres_War)/(sum(genres_War)+lambda) - b_i_genres - b_u_genres))

Beta_Western  <- 
  train_edx_genres %>% 
  left_join(b_i_genres, by = "movieId") %>%
  left_join(b_u_genres, by = "userId") %>%
  summarise(Beta_Western = mean(rating -sum(rating*genres_Western)/(sum(genres_Western)+lambda) - b_i_genres - b_u_genres))

Beta_Musical  <- 
  train_edx_genres %>% 
  left_join(b_i_genres, by = "movieId") %>%
  left_join(b_u_genres, by = "userId") %>%
  summarise(Beta_Musical = mean(rating - sum(rating*genres_Musical)/(sum(genres_Musical)+lambda) - b_i_genres - b_u_genres))

Beta_Documentary  <- 
  train_edx_genres %>% 
  left_join(b_i_genres, by = "movieId") %>%
  left_join(b_u_genres, by = "userId")  %>%
  summarise(Beta_Documentary = mean(rating - sum(rating*genres_Documentary)/(sum(genres_Documentary)+lambda) - b_i_genres - b_u_genres))

Beta_Film_Noir  <- 
  train_edx_genres %>% 
  left_join(b_i_genres, by = "movieId") %>%
  left_join(b_u_genres, by = "userId") %>%
  summarise(Beta_Film_Noir = mean(rating - sum(rating*genres_Film_Noir)/(sum(genres_Film_Noir)+lambda) - b_i_genres - b_u_genres))

# Calculating Sum(XuiK * BetaK), XuiK = 1 if genre = K
b_g_genres <- train_edx_genres %>% 
  left_join(b_i_genres, by = "movieId") %>%
  left_join(b_u_genres, by = "userId") %>%
  group_by(movieId) %>% 
  summarise(b_g_genres = (genres_Action * as.numeric(Beta_Action)) +
              (genres_Adventure * as.numeric(Beta_Adventure)) +
              (genres_Children * as.numeric(Beta_Children)) +
              (genres_Comedy * as.numeric(Beta_Comedy)) +
              (genres_Fantasy * as.numeric(Beta_Fantasy)) +
              (genres_IMAX * as.numeric(Beta_IMAX)) +
              (genres_Sci_Fi * as.numeric(Beta_Sci_Fi)) +
              (genres_Drama * as.numeric(Beta_Drama)) +
              (genres_Horror * as.numeric(Beta_Horror)) + 
              (genres_Mystery * as.numeric(Beta_Mystery)) +
              (genres_Romance * as.numeric(Beta_Romance)) +
              (genres_Thriller * as.numeric(Beta_Thriller)) +
              (genres_Crime * as.numeric(Beta_Crime)) +
              (genres_War * as.numeric(Beta_War)) +
              (genres_Western * as.numeric(Beta_Western)) +
              (genres_Musical * as.numeric(Beta_Musical)) +
              (genres_Documentary * as.numeric(Beta_Documentary)) +
              (genres_Film_Noir * as.numeric(Beta_Film_Noir))) %>% 
  unique()

train_predicted_ratings_genres <- 
  train_edx_genres %>% 
  left_join(b_i_genres, by = "movieId") %>%
  left_join(b_u_genres, by = "userId") %>% 
  left_join(b_g_genres, by = "movieId") %>%
  mutate(mu_genres = mu_genres, pred = mu_genres + b_i_genres + b_u_genres - b_g_genres) %>%
  .$pred

train_genres_rmse <- RMSE(predicted_ratings_genres, train_edx_genres$rating)
train_genres_rmse
# 0.8789393

# Predictions over the test set ####

test_predicted_ratings_genres <- 
  test_edx_genres %>% 
  left_join(b_i_genres, by = "movieId") %>%
  left_join(b_u_genres, by = "userId") %>% 
  left_join(b_g_genres, by = "movieId") %>%
  mutate(mu_genres = mu_genres, pred = mu_genres + b_i_genres + b_u_genres - b_g_genres) %>%
  .$pred

test_genres_rmse <- RMSE(test_predicted_ratings_genres, test_edx_genres$rating)
test_genres_rmse
# 0.8874411 #####

# In this case, when we add the genre to the model, we get worse preliminary 
# results than those obtained only with the movie and user effects.

# ___________________________________########
##### APPLYING FINAL MODEL OVER ENTIRE EDX_G SET ######
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

edx_g_reg_movie_user_myear_ryear_genre_effect_pred <- 
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

str(edx_g_reg_movie_user_myear_ryear_genre_effect_pred)

# Regularized Movie + User + Movie Year + Rating Year + Gender #####

edx_g_reg_movie_user_myear_ryear_effect_genre_rmse <- 
  RMSE(edx_g_reg_movie_user_myear_ryear_genre_effect_pred, 
       edx_g$rating)

edx_g_reg_movie_user_myear_ryear_effect_genre_rmse
#__0.8534774########


# Adding the gender effect over the entire EDX data set improves the rmse 
# that we got with the previous model. 

# We will apply now the final model, train with the entire EDX data set,
# over the validation set


# ___________________________________########
##### Cambiar por modelo sensillo APPLYING THE FINAL MODEL OVER VALIDATION SET  ######
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

validation_preds_rev %>% filter(movieId == 480) %>% 
  select(userId, movieId, title, genres, rating, pred) %>%
  knitr::kable()

validation_preds_rev %>% filter(userId == 621) %>% 
  select(userId, movieId, title, genres, rating, pred) %>%
  ggplot(aes(rating, pred)) +
  geom_point()

validation_preds_rev %>% 
  mutate(rating = as.factor(rating)) %>% 
  mutate(rating = factor(rating, 
                         levels = levels(rating)[c(9,8,10,3,7,2,4,6,5,1)])) %>% 
  select(userId, movieId, title, genres, rating, pred) %>%
  ggplot(aes(rating, pred)) +
  geom_boxplot() +
  geom_point() +
  geom_jitter(width = 0.1, alpha = 0.5)

validation_preds_rev %>% 
  mutate(rating = as.factor(rating)) %>% 
  mutate(rating = factor(rating, 
                         levels = levels(rating)[c(9,8,10,3,7,2,4,6,5,1)])) %>% 
  ggplot(aes(pred, fill = pred)) +
  geom_density()



class(validation_preds_rev$rating)


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


#_____ Validation  model movie, user, movie year, rating year________#####


validation_pred <- 
  validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_my, by = 'movie_year') %>%
  left_join(b_ry, by = 'rating_year') %>% 
  mutate(pred = mu + b_i + b_u + b_my + b_ry) %>%
  .$pred

# Regularized Movie + User + Movie Year + Rating Year #####

edx_reg_movie_user_myear_ryear_effect_rmse_val <- RMSE(validation_pred, 
                                                       validation$rating)
edx_reg_movie_user_myear_ryear_effect_rmse_val
# __0.8648047 #####

# ___________________________________########
  ##### EXPLAINING THE MODEL ######
# ___________________________________########

b_i
mean(b_i$b_i)
sd(b_i$b_i)

b_u
mean(b_u$b_u)
sd(b_u$b_u)

b_my
mean(b_my$b_my)
sd(b_my$b_my)

b_ry
mean(b_ry$b_ry)
sd(b_ry$b_ry)

b_g
mean(b_g$b_g)
sd(b_g$b_g)

cor(validation_preds_rev$pred, validation_preds_rev$rating)
# 0.5729347

film_noir <- validation_preds_rev %>% 
  filter(genres == "Film-Noir")

cor(film_noir$pred, film_noir$rating)

drama <- validation_preds_rev %>% 
  filter(genres == "Drama")

cor(drama$pred, drama$rating)

imax <- validation_preds_rev %>% 
  filter(genres == "IMAX")

cor(imax$pred, imax$rating)

validation_preds_rev %>%
  ggplot(aes(rating, pred, color = genres)) +
  geom_boxplot()



