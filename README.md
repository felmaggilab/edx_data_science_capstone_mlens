# edx_data_science_capstone_mlens
HarvardX - PH125.9x Capstone. Exercise 1

### Abstract

This report explains the process of building a linear model for movie ratings prediction, through which an RMSE of 0.8648047 is achieved. The final model takes into account movie effects, user effects, movie year effects, and rating year effects. 

During model development, we experimented with the genre effect in two different ways. The first approach was to separate the movie genres by row, and add them to the model in the same way that the movie, user, release year, and rating year effects were added. Since that generates as many new predictions as there are genres in the movie, the final prediction is calculated as an average (the sum of all predictions related to a user-movie combination, divided by the number of genres in the movie).

With this model we get an RMSE on the test data set of 0.8630654. This RMSE was by far the best of all those obtained on the test data set. However, it involves adding observations, and we prefer to present a final solution in which the number of observations is not altered.

The second approach was to add the gender effect using dummy variables with values 0 or 1 for each gender. In this way, no observations (rows) are added, but columns, in order to multiply the weight of each gender by the value of the dummy variable.

In this case, the RMSE obtained on the test set was 0.8922815. This result turned out to be worse than that obtained with a simple model that only took into account the film and user effects. Being a linear model, nothing, except changing the way in which the weights of each genre were calculated, was going to improve the result more than to eliminate the genre effect completely. After several tests, without finding an optimal way to calculate these weights, we decided to go for the simplest final model, which does not take into account the movie genres, but does not add observations in the original data sets.

Before adopting the linear approach, we performed tests with the _recommenderlab_ package. This approach was finally abandoned, among other reasons for its computational demands but, above all, for including techniques that we did not master at the time of development of our model.

***
