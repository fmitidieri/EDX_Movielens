#EDX Capstone Final
#R versio 4.3.2
#Rstudio version 2023.12.0 Build 369
#Author: Fernando Athaide Mitidieri
##########################################################
# Create edx set, validation set (final hold-out test set)
# Note: this process could take a couple of minutes
##########################################################

#Veirfy if all packages are available, if not, they will be installed
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(svd)) install.packages("svd", repos = "http://cran.us.r-project.org")
if(!require(recommenderlab)) install.packages("recommenderlab", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")

#Once intalled the packages, we should load the libraries
library(tidyverse) # loaded version 1.3.0
library(caret) # loaded version 6.0-86
library(data.table) # loaded version 1.13.4
library(stringr) # loaded version 1.4.0
library(dplyr) # loaded version 1.0.2
library(svd) # loaded version 0.5
library(recommenderlab) # loaded version 0.2-6
library(ggplot2) # loaded version 3.3.2

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K/ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx_original <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx_original, by = "movieId") %>%
  semi_join(edx_original, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx_original <- rbind(edx_original, removed)

rm(dl, ratings, movies, test_index, temp, removed)
#Here ends the code provided by the staff. Now we have generated 
#edx and final_holdout_test sets.

##########################################################
# Pre-processing edx data set and creates a second 
# segmentation for training and validate the algorithms 
# before submit the final_holdout_test set 
##########################################################

#Analyzing the data we can observe that there is a unique 
#movieID for each title. 
str(edx_original)
head(edx_original)

#So, the first data transformation will be create a table to relate 
#movieID with title, this will be used to handle only Ids 
#as numerical data instead handle title that are strings.
df<-edx_original %>%
  select(movieId, title) %>%
  unique()
movieID_title<-df[order(df$movieId),]

#Checking if there is any movieId duplicated, as the result is TRUE it 
#means that really there is an unique movieId for each title, this 
#shows that we can use only one of them as features for the models
length(unique(movieID_title$movieId)) == nrow(movieID_title)

#This is the original edx data set preserved and work from here
#with a copy, because some data will be transformed 
edx <- edx_original
#Cleaning unnecessary variable
rm(df)

#Identify year of issuance from the title column for each movie 
#to be used as a new numerical feature
edx$year_title<-gsub(".*\\((.*)\\).*", "\\1", edx$title)

#So, as we already are using the movieId as the variable to identify 
#each movie and also we took the year from the title column, now 
#the title column will be deleted from edx data set
edx$title <- NULL

#In context, month and day do not add as much information and would 
#certainly causes increase in processing time, so the decision was made
#to keep only the year and transform the date column into a year column
edx$Year<-format(as.Date(as.POSIXct(edx$timestamp, origin="1970-01-01")), "%Y")

#And the timestamp column that will no longer be used was deleted
edx$timestamp <- NULL

#Now I will substitute the genre by a reference number. Again, process 
#numbers is easier than process strings. The idea is substitute each combination
#by a number, For each different combination of genre will be assigned a 
#unique number.
genresNumber<-movielens %>%
  select(genres) %>%
  unique()
genresNumber$genreId <- 1:nrow(genresNumber)

#Now that was generated the genreNumber table, these numbers will replace the 
#strings of characters of thr genre in the edx data set.
n<-length(genresNumber$genres)
for(i in 1:n){
  str<-genresNumber$genres[i]
  allRows<-grep(str, edx$genres, ignore.case = FALSE, perl = FALSE, value = FALSE,
                fixed = TRUE, useBytes = FALSE, invert = FALSE)
  edx$genres[allRows]<-genresNumber$genreId[i]
  num<-(i/n)*100
  print(num)
}

#verifying the types
glimpse(edx)

#Transforming all types in integers
edx$genres<-as.integer(edx$genres)
edx$rating<-as.integer(edx$rating)
edx$year_title<-as.integer(edx$year_title)
edx$Year<-as.integer(edx$year_title)

#Cleaning unnecessary variable
rm(i, n, str, allRows, num)

#Copying the edx to a work variable. This work variable will be 
#used to segregate the edx into two data set, one for training
# and the second to validate the results before apply on the 
#final_holdout_test data set
edx_aux <- edx

# The validation set will be 10% of the edx data, and the process to generate
#the two new data sets are practically the same from the beginning.
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = edx_aux$rating, times = 1, p = 0.1, list = FALSE)
edx <- edx_aux[-test_index,]  #0.90 = (1-p)
temp <- edx_aux[test_index,] 

# Make sure that all userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

#Cleaning unnecessary variable
rm(removed, temp, test_index)

#Saving training, test and validation sets to be used at RMD file. (AT MAC NOTEBOOK)
save(edx, file="ml-10M100K/edx.RData")
save(edx_original, file="ml-10M100K/edx_original.RData")
save(final_holdout_test, file="ml-10M100K/final_holdout_test.RData")
save(genresNumber, file="ml-10M100K/genresNumber.RData")
save(movieID_title, file="ml-10M100K/movieID_title.RData")
save(validation, file="ml-10M100K/validation.RData")
save(movielens, file="ml-10M100K/movielens.RData")

############################################################
#In case of crash if Studio R, load libraries and data sets
library(tidyverse) # loaded version 1.3.0
library(caret) # loaded version 6.0-86
library(data.table) # loaded version 1.13.4
library(stringr) # loaded version 1.4.0
library(dplyr) # loaded version 1.0.2
library(svd) # loaded version 0.5
library(recommenderlab) # loaded version 0.2-6
library(ggplot2)  # loaded version 3.3.2

#Loading training and validation sets. (AT MAC NOTEBOOK)
load("ml-10M100K/edx.RData")
load("ml-10M100K/edx_original.RData")
load("ml-10M100K/final_holdout_test.RData")
load("ml-10M100K/genresNumber.RData")
load("ml-10M100K/movieID_title.RData")
load("ml-10M100K/validation.RData")
load("ml-10M100K/movielens.RData")

# Let's Start with Explanatory Analysis using the RecommenderLab
# Michael Hahsler (2020). recommenderlab: Lab for Developing and
# Testing Recommender Algorithms. R package version 0.2-6.
# https://github.com/mhahsler/recommenderlab


#Let´s check how many combinations we could have in our dataset by finding
#the number of total users and the number of total movies

#Shows that movielens, originally has genres and titles as strings and for analises 
#purposes I removed titles and I'm just using movieId. Also I categorized the genre column.
#Year title was extracted from titles and simplify the timestamp do use only the 
#year of movie issue. 

#Therse two commands will show those transformations mentioned above.
glimpse(edx_original)
glimpse(edx)

#Now I will show how many users and movies are present
# in the edx data set. Remembering that edx dataset is the 
#originally generated by the given algorithm but also splited 
#in two new datasets. The results from now will be from 
#the training test used to develop this project.

n_user_n_movies<-edx %>% summarize(Users = n_distinct(userId),
                                   Movies = n_distinct(movieId))
n_user_n_movies

#Then now let's check how sparse are our data
rating_matrix <- edx %>%
  select(userId, movieId, rating) %>%
  spread(movieId, rating) %>%
  as.matrix()

#Use just in case some memory allocation error shows up
memory.limit(999999)

#Cleaning first column with userId. First, this column is saved to be used in the future
# And then the column is removed from rating_matrix
userIdList<-rating_matrix[1,]
rating_matrix<-rating_matrix[,-1]


#Just to make the life easier, I chose just a square matrix of 1000x1000 register
real_ratings<-as(rating_matrix[1:1000,1:1000],"realRatingMatrix")

#Visualization of a sample of data to confirm that it is a sparse matrix.
#This graphic takes time to be generated
figure1<-image(sample(real_ratings,1000), main = "Raw ratings")
real_ratings_full<-as(rating_matrix,"realRatingMatrix")

#Saving plot to be used in the report. Will be loaded into the RMD
#file. 
save(figure1, file="figures/figure1.RData")
figure1
dev.off()

real_ratingsx<-as(rating_matrix[1:50,1:50],"realRatingMatrix")
figure1x<-image(sample(real_ratingsx,50), main = "Raw ratings")

#Saving plot to be used in the report. Will be loaded into the RMD
#file. 
save(figure1x, file="figures/figure1x.RData")
figure1x
dev.off()

# Generating the histogram of ratings distribution
edx_mean<-mean(edx$rating)
figure2<-edx %>%
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.5, fill = "red", col = "white") +
  ggtitle("Rating Distribution") +
  labs(x="Ratings", y="Total") +
  geom_vline(xintercept = edx_mean,        # Add line for mean
             col = "black",
             lwd = 0.25)

#Saving plot to be used in the report. Will be loaded into the RMD
#file. 
save(figure2, file="figures/figure2.RData")
figure2
dev.off()

#Let's discover the users that most rated in the dataset
user_most_rated <- edx %>%
  group_by(userId) %>%
  summarize(count=n()) %>%
  top_n(10,count) %>%
  arrange(desc(count))

figure3<-user_most_rated %>% 
  ggplot(aes(x=reorder(userId, count), y=count)) +
  geom_bar(stat='identity', fill="red") + coord_flip(y=c(0, 7500)) +
  labs(x="", y="Number of ratings") +
  geom_text(aes(label= count), hjust=-0.1, size=3) +
  labs(title="Top Ten Users")

#Saving plot to be used in the report. Will be loaded into the RMD
#file. 
save(figure3, file="figures/figure3.RData")
figure3x
dev.off()

#Let's discover the ten most rated movies
most_rated <- edx %>%
  group_by(movieId) %>%
  summarize(count=n()) %>%
  top_n(10,count) %>%
  arrange(desc(count))

most_rated<-inner_join(most_rated,movieID_title,by="movieId")

figure4<-most_rated %>% 
  ggplot(aes(x=reorder(title, count), y=count)) +
  geom_bar(stat='identity', fill="red") + coord_flip(y=c(0, 35000)) +
  labs(x="", y="Number of ratings") +
  geom_text(aes(label= count), hjust=-0.1, size=3) +
  labs(title="Top Ten Movies Rated")

#Saving plot to be used in the report. Will be loaded into the RMD
#file. 
save(figure4, file="figures/figure4.RData")
figure3
dev.off()

#Let's find the ten most genre present in the dataset, remembering that 
# one movie can be classified in more than one genre
most_genre <- edx %>%
  group_by(genres) %>%
  summarize(count=n()) %>%
  top_n(10,count) %>%
  arrange(desc(count))

colnames(most_genre)<-c(x="genreId", y="count", z="genres")
most_genre<-inner_join(most_genre,genresNumber,by="genreId")
head(most_genre)

figure5<-most_genre %>% 
  ggplot(aes(x=reorder(genres,count), y=count))+
  geom_bar(stat='identity', fill="red") + coord_flip(y=c(0, 3500000)) +
  labs(x="", y="Total") +
  geom_text(aes(label= count), hjust=-0.1, size=3) +
  labs(title="Top ten genre rated")

#Saving plot to be used in the report. Will be loaded into the RMD
#file. 
save(figure5, file="figures/figure5.RData")
figure5
dev.off()

#And how is the rating distribution among the genres?
most_genre_2 <- edx %>%
  group_by(genres) %>%
  summarize(count=mean(rating)) %>%
  arrange(desc(count))

colnames(most_genre_2)<-c(x="genreId", y="count", z="genres")
most_genre_2<-inner_join(most_genre_2,genresNumber,by="genreId")

figure6<-most_genre_2 %>% 
    ggplot(aes(x=reorder(genres,count), y=count))+
    geom_bar(stat='identity', fill="red") + coord_flip(y=c(0, 5)) +
    labs(x="", y="Rating") +
    labs(title="Mean rating according genre") 

#Saving plot to be used in the report. Will be loaded into the RMD
#file. 
save(figure6, file="figures/figure6.RData")
figure6
dev.off()

#Let's analyse the behavior  of ratings related with the year title
year_distribuition <- edx %>%
  group_by(year_title) %>%
  summarize(count=n())

year_distribuition$year_title<-as.numeric(year_distribuition$year_title)

figure7<-year_distribuition %>% 
  ggplot(aes(year_title,count, col=count))+
  geom_line()+
  theme(legend.position = "none")+
  labs(x="Year", y="Total Ratings") +
  labs(title="Ratings per Year Launch")

#Saving plot to be used in the report. Will be loaded into the RMD
#file. 
save(figure7, file="figures/figure7.RData")
figure7
dev.off()

rm(most_genre, most_genre_2,most_rated,most_rated_2, userIdList)
rm(rating_matrix, real_ratings, real_ratings_full, real_ratingsx,year_distribuition)

##########################################################
# Algorithms analysis session
# In this session will be developed the algorithms 
# to predicts the rate according with the features 
# previously defined.
##########################################################

#Defining a RMSE function to calculate performance of models
RMSE<-function(true_ratings,predicted_ratings)
{sqrt(mean((true_ratings-predicted_ratings)^2))}

##########################################################
#First approach is calculate the naive mean
##########################################################
mu_hat<-mean(edx$rating)
mu_hat

#Each step of evolution of the algorithm will be 
#registered in a RMSE table to be used in the conclusion
#report
naive_rmse<-RMSE(mu_hat, validation$rating)
naive_rmse

#Creating a table to store all results
RMSE_Table <- data.frame("Mean", naive_rmse)
colnames(RMSE_Table)<-c(x="Method",y="RMSE")

##########################################################
#Second approach is include the movieId influence in the ratings 
##########################################################
mu <- mean(edx$rating)

#Calculating the influence of the film on the rating given in
#the training (edx) dataset.
#As a result, the coefficient b_i to be used in the
#equation for predictions. 
movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

#With the model previously generated and adding the movie bias coefficient
#will be generated the predicted ratings for the validation data set
predicted_ratings <- mu + validation %>%
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)
movie_bias<-RMSE(validation$rating, predicted_ratings)
movie_bias

RMSE_Table<-RMSE_Table %>% add_row(Method="Movie Bias", RMSE=movie_bias)

##########################################################
#Now will be included the influence of the user in the 
# ratings given. 
##########################################################
#Calculating the influence of the user on the rating given in
#the training (edx) dataset.
#As a result, is generated the coefficient b_u to be used in the
#equation for predictions. 
user_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

#With the model previously generated and adding the user bias coefficient
#will be generated the predicted ratings for the validation data set
predicted_ratings <- validation %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
user_bias<-RMSE(validation$rating,predicted_ratings)
user_bias

RMSE_Table<-RMSE_Table %>% add_row(Method="User Bias", RMSE=user_bias)

##########################################################
#The next influence to be considered will be
# year the the movie was launched, for this purpose will
# use the year title column created previously
##########################################################
#Calculating the influence of the title year on the rating given in
#the training (edx) dataset.
#As a result, is generated the coefficient b_y to be used in the
#equation for predictions. 
year_title_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(year_title) %>%
  summarize(b_y = mean(rating - mu - b_i - b_u))

#With the model previously generated and adding the year_title bias coefficient
#will be generated the predicted ratings for the validation data set
predicted_ratings <- validation %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_title_avgs, by='year_title') %>%
  mutate(pred = mu + b_i + b_u + b_y) %>%
  pull(pred)
year_title_bias<-RMSE(validation$rating, predicted_ratings)
year_title_bias

RMSE_Table<-RMSE_Table %>% add_row(Method="Year Title Bias", RMSE=year_title_bias)

##########################################################
#The next influence to be considered will be
# the year that the movie was rated
##########################################################
#Calculating the influence of the rating year on the rating given in
#the training (edx) dataset.
#As a result, is generated the coefficient b_Y to be used in the
#equation for predictions. 
year_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_title_avgs, by='year_title') %>%
  group_by(Year) %>%
  summarize(b_Y = mean(rating - mu - b_i - b_u - b_y))

#With the previously generated model and adding the bias coefficient of the year,
#predicted classifications will be generated for the validation data set
predicted_ratings <- validation %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_title_avgs, by='year_title') %>%
  left_join(year_avgs, by='Year') %>%
  mutate(pred = mu + b_i + b_u + b_y + b_Y) %>%
  pull(pred)
year_bias<-RMSE(validation$rating,predicted_ratings)
year_bias

RMSE_Table<-RMSE_Table %>% add_row(Method="Rating Year Bias", RMSE=year_bias)

##########################################################
#The next influence to be considered will be
#the genre that the movie was classified
##########################################################
#Calculating the influence of the genre on the rating given in
#the training (edx) dataset.
#As a result, is generated the coefficient b_g to be used in the
#equation for predictions. 
genre_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_title_avgs, by='year_title') %>%
  left_join(year_avgs, by='Year') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u - b_y - b_Y))

#With the previously generated model and adding the bias coefficient of the genres,
#predicted classifications will be generated for the validation data set
predicted_ratings <- validation %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_title_avgs, by='year_title') %>%
  left_join(year_avgs, by='Year') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_y + b_Y + b_g) %>%
  pull(pred)
genre_bias<-RMSE(validation$rating,predicted_ratings)
genre_bias

RMSE_Table<-RMSE_Table %>% add_row(Method="Genre Bias", RMSE=genre_bias)

#The model with the last coefficient added, presented a very good result 
#with the validation set. Now, let's try with the final_holdout_test set.
#This is the table with all RMSE calculatd for each model
RMSE_Table

##########################################################
#Apply the model in the final_holdout_test 
##########################################################
#First Step - pre-processing the final_holdout_test set for compatibility with 
#the process developed. Basically will be repeated the preprocess made on the 
#edx and validation sets: Use MovieId instead title title, create numerical 
#colunms with the year of rating and year of issuance of movie, and substitute 
#genres strings for codes. The main idea of all processes is use number instead
#strings. 

#Performing pre-processing on data
#Identify year of issuance for each movie
final_holdout_test_bkp <- final_holdout_test
final_holdout_test <- final_holdout_test_bkp

final_holdout_test$year_title<-gsub(".*\\((.*)\\).*", "\\1", final_holdout_test$title)

#So, now deleting title column from datasets
final_holdout_test$title <- NULL

#Transforming date column into year column
final_holdout_test$Year<-format(as.Date(as.POSIXct(final_holdout_test$timestamp, origin="1970-01-01")), "%Y")

#And deleting timestamp column that will not more longer used
final_holdout_test$timestamp <- NULL

#Now run over final_holdout_test
n<-length(genresNumber$genres)
for(i in 1:n){
  str<-genresNumber$genres[i]
  allRows<-grep(str, final_holdout_test$genres, ignore.case = FALSE, perl = FALSE, value = FALSE,
                fixed = TRUE, useBytes = FALSE, invert = FALSE)
  final_holdout_test$genres[allRows]<-genresNumber$genreId[i]
  num<-(i/n)*100
  print(num)
}
final_holdout_test$genres<-as.double(final_holdout_test$genres)


#Recovering the edx processed and complete, the intent is including validation 
#set in the edx set for the final assessment. The final_holdout_test will be used to 
#assess the model.

edx <- edx_aux 

#Now let's retraining the model with the entire edx data set
#first factor mu
mu <- mean(edx$rating)
#second factor b_i - movie bias
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))
#third factor b_u - user bias
user_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
#fourth factor b_y - year title bias
year_title_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(year_title) %>%
  summarize(b_y = mean(rating - mu - b_i - b_u))
#fifeth factor b_Y - year rating bias
year_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_title_avgs, by='year_title') %>%
  group_by(Year) %>%
  summarize(b_Y = mean(rating - mu - b_i - b_u - b_y))
#sixth factor b_g - genres bias
genre_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_title_avgs, by='year_title') %>%
  left_join(year_avgs, by='Year') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u - b_y - b_Y))

predicted_ratings <- final_holdout_test %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_title_avgs, by='year_title') %>%
  left_join(year_avgs, by='Year') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_y + b_Y + b_g) %>%
  pull(pred)

final_result <- RMSE(final_holdout_test$rating,predicted_ratings)
final_result

RMSE_Table<-RMSE_Table %>% add_row(Method="Final_Holdout_Test Result", RMSE=final_result)
RMSE_Table

save(RMSE_Table, file="ml-10M100K/RMSE_Table.RData")

#Cleaning unnecessary variable
rm(mu,movie_avgs, genre_avgs, user_avgs, year_avgs, year_title_avgs)
rm(i, n, str, allRows, num)








