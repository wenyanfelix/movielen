###################################
# Create edx set and validation set
###################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1) # if using R 3.6.0: set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


#Exercises
#Q1
nrow(edx)
ncol(edx)

#q2
edx %>% filter(rating == 0) %>% nrow()
edx %>% filter(rating == 3) %>% tally()

#Q3
length(unique(edx$movieId))
n_distinct(edx$movieId)

#Q4
n_distinct(edx$userId)

#Q5
genres_q5 <- c("Drama", "Comedy", "Thriller", "Romance")

edx %>% separate_rows(genres, sep = "\\|") %>%
  filter(genres %in% genres_q5) %>%
  group_by(genres) %>%
  summarize(n = n())

#Q6
edx %>% group_by(movieId) %>%
  summarize(count = n(), title = first(title)) %>%
  arrange(desc(count))

edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

#Q7
edx %>% group_by(rating) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()


#summary of key index
total_mean_rating <- edx %>% summarize(rating = mean(rating)) %>% as.numeric()



#2019-06-05

#RMSE fuction
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#model 1
#predict by all movies' average

#average rating of all movies
mu_hat_1 <- mean(edx$rating)
rmse_1 <- RMSE(validation$rating, mu_hat_1)

#rmse_1 is about 1.061202
rmse_1

#store the result
rmse_results <- tibble(method = "Just the average", RMSE = rmse_1)



#model 2
#predict by all movies' by_group average
mu_hat_2 <- edx %>% group_by(movieId) %>% summarize(mean_by_group = mean(rating)) %>% summarize(m = mean(mean_by_group)) %>% pull()
rmse_2 <- RMSE(validation$rating, mu_hat_2)

#rmse_2 is about 1.108485 which is worse than rmse_1
#I take mu_hat_2 as the TRUE rating average of all movies, but it's performance is worse
#because we have to predict NOT once per movie, but many times per movie. So the rmse_1 works better.
rmse_2

#store the result
rmse_results <- bind_rows(rmse_results,
                          tibble(method = "Grouped average",
                                 RMSE = rmse_2))




#model 3
#Movie effects
#Since some movies are generally interesting and getting a higher rating, we should put this into the model.

#calculate the overall movie rating average
mu_3 <- mean(edx$rating)

#calculate the movie effect as b_i
movie_avgs_3 <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu_3))

#Use b_i to calculate the predicted rating
predicted_ratings_3 <- validation %>%
  select(movieId) %>%
  left_join(movie_avgs_3, by='movieId') %>%
  mutate(predict_rating = b_i + mu_3) %>%
  pull(predict_rating)

#calculate the rmse
rmse_3 <- RMSE(predicted_ratings_3, validation$rating)

#store rmse into results
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie Effect Model",  
                                     RMSE = rmse_3))
rmse_results


#model 4
#User effects
#Similar with the movie effects, users have their own criteria, we consider the user effects in this model.

#Calculate the user effect (b_u)
#since Y = mean + b_i + b_u
#b_u = Y - mean - b_i
user_avgs_4 <- edx %>%
  left_join(movie_avgs_3, by = "movieId") %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating - mu_3 - b_i))

predicted_ratings_4 <- validation %>%
  left_join(movie_avgs_3, by = "movieId") %>%
  left_join(user_avgs_4, by = "userId") %>%
  mutate(prediction_4 = mu_3 + b_i + b_u) %>%
  pull(prediction_4)



#calculate the rmse
rmse_4 <- RMSE(predicted_ratings_4, validation$rating)

#store rmse into results
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie Effect + User Effect",  
                                 RMSE = rmse_4))
rmse_results
