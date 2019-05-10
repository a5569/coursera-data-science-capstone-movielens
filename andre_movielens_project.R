# MovieLens - Movie rating model

# Loading the required libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# Downloading the data
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

# Reading the ratings file
ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

# Reading the movies file
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)

# Changing the column names
colnames(movies) <- c("movieId", "title", "genres")

# Adjusting the column types
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

# Creating the final dataset with both files
movielens <- left_join(ratings, movies, by = "movieId")

set.seed(1)

# Creating the splitting index
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)

# Creating the training dataset
training <- movielens[-test_index,]

temp <- movielens[test_index,]

# Creating the validation dataset
validation <- temp %>%
  semi_join(training, by = "movieId") %>%
  semi_join(training, by = "userId")

removed <- anti_join(temp, validation, by = c("userId", "movieId", "rating", "timestamp", "title", "genres"))
training <- rbind(training, removed)

# Removing temp datasets
rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Defining de RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Calculating the overall average rating
mu <- mean(training$rating)

# Calculating the movie average rating
movie_avgs <- training %>%
  group_by(movieId) %>%
  summarise(b_i=mean(rating - mu))

# Calculating the user average rating
user_avgs <- training %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u=mean(rating - mu - b_i))

# Calculating the ratings
predicted_rating <- validation %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

# Calulating the RMSE
rmse <- RMSE(predicted_rating, validation$rating)
rmse