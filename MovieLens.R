# ---
# title: "Report on MovieLens Project"
# author: "Pedro J. Llanos"
# date: "1/6/2020"
# #output: html_document 
# output: pdf_document
# #output: word_document
# #output: github_document
# ---

  
##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggcorrplot)) install.packages("ggcorrplot", repos = "http://cran.us.r-project.org")
if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")
if(!require(ggpmisc)) install.packages("ggpmisc", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(dslabs)
library(ggcorrplot)
library(GGally)
library(lubridate)
library(ggpmisc)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

#Takes a few seconds to process:
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

#Takes ~3 min to process:
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))
class(ratings)
names(ratings)

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

#The following line of code is given by edx:
# if using R 3.6 or earlier:
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                           title = as.character(title),
#                                           genres = as.character(genres))

#Modified the above line of code:
# if using R 3.6 or earlier:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

class(movies)
names(movies)
glimpse(movies)
head(movies)
tail(movies)
dim(movies)

#Not applicable
# if using R 4.0 or later:
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
#                                           title = as.character(title),
#                                           genres = as.character(genres))

#Combine ratings and movies data frames by movieId:
movielens <- left_join(ratings, movies, by = "movieId")
names(movielens)
head(movielens)
class(movielens)

#Some visualizations of the movielens data frame
#Group by title
top_movielens <- movielens %>%
  group_by(title) %>%
  summarize(rating=n()) %>% 
  top_n(20, rating) %>%
  arrange(desc(rating)) 
top_movielens
top_movielens %>% 
  mutate(title = reorder(title, rating)) %>%
  ggplot(aes(rating, title)) +
  geom_bar(stat="identity") +
  labs(x="Total Number of Ratings", y="Movie Title", title="Top 20 Movies Ratings", adj = 0.5) +
  theme(plot.title = element_text(hjust = 0.5)) 
ggsave('MovieTitles.bmp')

#Group by genres
top_movielens_genres <- movielens %>%
  group_by(genres) %>%
  summarize(rating=n()) %>% 
  arrange(desc(rating)) %>%
  top_n(20, rating) 
top_movielens_genres
top_movielens_genres %>% 
  mutate(genres = reorder(genres, rating)) %>%
  ggplot(aes(rating, genres)) +
  geom_bar(stat="identity") +
  labs(x="Total Number of Ratings", y="Movie Genre", title="Top 20 Movies Genres ") +
  theme(plot.title = element_text(hjust = 0.5)) 
ggsave('MovieGenres.bmp')

#Boxplot of top 20 movies 
movielens %>% filter(title %in% top_movielens$title) %>%
  mutate(title = reorder(title, rating, FUN = median)) %>%    # reorder
  ggplot(aes(rating, title, fill = genres)) +    # color by continent
  geom_boxplot() + 
  labs(x="Rating", y="Movie Title", title="Top 20 Movies Titles and their Rating by Genres") +
  theme(plot.title = element_text(hjust = 0.5)) 
ggsave('MoviesBoxPlot.bmp')

#or a much slower code for 10 top movies
TM <- as.data.frame(top_movielens)
TT <- TM$title
boxplot_movies <- movielens %>% group_by(title, rating) %>%
  filter(title== TT[1] | title== TT[2] | title== TT[3] | title== TT[4] | title== TT[5] | title== TT[6]| title== TT[7] | title== TT[8] | title== TT[9] | title== TT[10]) %>%    
  group_by(title) %>% 
  mutate(title = reorder(title, rating, FUN = median)) %>%    # reorder
  ggplot(aes(rating, title, fill = genres)) +    # color by continent
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + 
  labs(x="Rating", y="Movie Title", title="Top 10 Movies Titles and their Rating by Genres") +
  theme(plot.title = element_text(hjust = 0.5))  +  coord_flip()  #+ geom_jitter(width = 0.1, alpha =0.2)
boxplot_movies 
#ggsave('Movies10BoxPlot.bmp')

#Machine Learning
# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
#Define train set and test set:
edx <- movielens[-test_index,] #train set
temp <- movielens[test_index,] #test set

#There are so many unique genres:
unique(edx$genres)
#Correlation between movie variables
edxDF <- as.data.frame(edx)
corr_matrix <- cor(edxDF[sapply(edxDF, is.numeric)])
ggcorrplot(corr_matrix) + labs(title="Correlation Matrix between Movie Variables")
ggsave('MovieCorrelation1.bmp')
ggcorr(corr_matrix, nbreaks = 6, palette = "RdGy", label = TRUE, label_size = 3, label_color = "white") +
  labs(title="Correlation Matrix between Movie Variables")
ggsave('MovieCorrelation2.bmp')
 

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")
class(validation)
names(validation)

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)
head(edx)
names(edx)

#rm(dl, ratings, movies, test_index, temp, movielens, removed)

#Explore the data a bit more for userId, movieId, rating, timestamp, tittle and genres:
#for edx
summary(edx)
#for validation
summary(validation)

#Applying some material learned in Section 6 of ML Module 8: Model Fitting Recommnedation Systems
#Apply simple model to the train set:
mu_hat <- mean(edx$rating)
mu_hat
#Obtain a naive RMSE for test set:
naive_rmse <- RMSE(temp$rating, mu_hat)
naive_rmse
#Note this naive RMSE is almost identical to the validation rating:
naive_rmse <- RMSE(validation$rating, mu_hat)
naive_rmse
rmse_results <- tibble(method = "Just the average", 
                           RMSE = naive_rmse)
rmse_results

#===================Model 1===============================
#Analyze the "movieId" effect using the training set
#Average
mu <- mean(edx$rating) 
#Movie averages
movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
movie_avgs
#Plot the estimated movie averages
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 30, data = ., color = I("blue")) +
labs(x="Least Square Estimate, b_i", y="Movie Averages", title="Movie Effects") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave('Model1.bmp')

#Predict the ratings in this simple model:
predicted_ratings <- mu + validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i
#Obtain the RMSE for model 1:
model_1_rmse <- RMSE(predicted_ratings, validation$rating)
#Display the RMSE results for model 1:
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))
rmse_results
#Provide RMSE results for model 1 in the form of a table
rmse_results %>% knitr::kable()
#We see a decrease of about 11.73% in the RMSE
rmse_results$RMSE[1]-rmse_results$RMSE[2]

#====================Model 2==============================
#Next, we are going to analyze the "userId" effect
#using the training set:
edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "red") +
  labs(x="Least Square Estimate, b_u", y="Movie Averages", title="Movie User Effects") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave('Model2.bmp')
#User averages on the training set
user_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
user_avgs
#Obtain predicted ratings using the test set
predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred
#Obtain the RMSE for model 2:
model_2_rmse <- RMSE(predicted_ratings, validation$rating)
#Display the RMSE results for model 2:
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))
#Provide RMSE results for model 2 in the form of a table
rmse_results %>% knitr::kable()
#Using this second model, the RMSE was decreased by ~7.85% with respect model 2
rmse_results$RMSE[2]-rmse_results$RMSE[3]


#====================Model 3===========================
#Remember that there are other variables we can use to continue
#refining the RMSE. Some of these variables are "genres" and "timestamp"
#Here we will analyze the "genre effect"
names(edx)
edx %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

edx %>% 
  group_by(genres) %>% 
  summarize(b_g = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_g)) + 
  geom_histogram(bins = 30, color = "green") +
  labs(x="Least Square Estimate, b_u", y="Movie Averages", title="Movie Genres Effects") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave('Model3.bmp')
#User averages on the training set
genres_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u ))
genres_avgs
#Obtain predicted ratings using the test set
predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genres_avgs, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  .$pred
#Obtain the RMSE for model 3:
model_3_rmse <- RMSE(predicted_ratings, validation$rating)
#Display the RMSE results for model 3:
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User + Genres Effects Model",  
                                     RMSE = model_3_rmse ))
#Provide RMSE results for model 3 in the form of a table
rmse_results %>% knitr::kable()
#The RMSE slightly improved adding the genre factor by 0.004%
#with respect model 3
rmse_results$RMSE[3]-rmse_results$RMSE[4]

#====================Model 4=====================
#"Timestamp" to assess the effect
edx$timestamp <- as_datetime(edx$timestamp)
edx$timestamp <- as.Date(edx$timestamp)

edx %>% 
  mutate(date = round_date(timestamp, unit = "weeks")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth() +
  labs(x="Date (year) ", y="Movie Rating", title = "Average Ratings for Movies (1996-2008)") +
  scale_x_date(date_breaks = "18 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("MovieTimestamp.bmp")

#Test other specific polynomials
test.formula <- y ~ poly(x, 4, raw = TRUE)
edx %>% 
  mutate(date = round_date(timestamp, unit = "weeks")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth(method = 'lm', formula= test.formula ) +
  stat_poly_eq(formula= test.formula,
               aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~")), 
               parse = TRUE) +
  theme_bw() +
  labs(x="Date (year) ", y="Movie Rating", title = "Average Ratings for Movies (1996-2008)") +
  scale_x_date(date_breaks = "18 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  theme(plot.title = element_text(hjust = 0.5))
ggsave("MovieTimestampPol4.bmp")


#Takes 1 minute to process...
edx %>% 
  group_by(timestamp) %>% 
  summarize(b_w = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_w)) + 
  geom_histogram(bins = 30, color = "orange") +
  labs(x="Least Square Estimate, b_w", y="Movie Averages", title="Movie Timestamp Effects") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave('Model4.bmp')
#Obtain averages using training set (<1 minute to run)
timestamp_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genres_avgs, by='genres') %>%
  mutate(date = round_date(as_datetime(timestamp), unit = "week")) %>%
  group_by(date) %>%
  summarize(b_w = mean(rating - mu - b_i - b_u - b_g))
timestamp_avgs
#Obtain predicted ratings using the test set
predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genres_avgs, by='genres') %>%
  mutate(date = round_date(as_datetime(timestamp), unit = "week")) %>%
  left_join(timestamp_avgs, by='date') %>%
  mutate(pred = mu + b_i + b_u + b_g + b_w) %>%
  .$pred
#Obtain the RMSE for model 4:
model_4_rmse <- RMSE(predicted_ratings, validation$rating)
#Display the RMSE results for model 4:
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User + Genres + Week Effects Model",  
                                     RMSE = model_4_rmse ))
#Provide RMSE results for model 4 in the form of a table
rmse_results %>% knitr::kable()
#The RMSE has decreased 0.0009% with respect to model 4
rmse_results$RMSE[4]-rmse_results$RMSE[5]

#=====================Model 5: Regularization==================
#To improve our results, we will use regularization.
#Regularization constrains the total variability of 
#the effect sizes by penalizing large estimates that
#come from small sample sizes.

#Here are 10 of the largest mistakes that we made 
#when only using the movie
movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>% 
  select(title,  residual) %>% slice(1:10) %>% knitr::kable()
#Pick the top 10 best movies in the top worst movies
# based on the estimates of the movie effect.
movie_titles <- movielens %>% 
  filter(!is.na(title)) %>% #check for any NA title
  select(movieId, title) %>%
  distinct()
#Top 10 best movies
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  filter(!is.na(title)) %>% #check for any NA title
  select(title, b_i) %>% 
  slice(1:10) %>%  
  knitr::kable()
#Let's check how often they were rated in our training set.
edx %>% dplyr::count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>%  
  filter(!is.na(title)) %>% #check for any NA title 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()
#We can see that these movies are only rated between 1 and 4 times only.

#Top 10 worst movies
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  filter(!is.na(title)) %>% #check for any NA title
  select(title, b_i) %>% 
  slice(1:10) %>%  
  knitr::kable()
#Let's check how often they were rated in our training set.
edx %>% dplyr::count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  filter(!is.na(title)) %>% #check for any NA title
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()
#Note that 6 of the worst movies were rated less than 2 times while
#other were rated way more, one or two orders of magnitude more.

#Regularization permits us to penalize large estimates that
#come from small sample sizes. 
#Let's exclude movies with n<3 (number of ratings)
lambda <- 3
#Obtain the estimated regularized values
mu <- mean(edx$rating)
movie_reg_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 
#Plot regularized estimate vs. the least square estimates.
#we can also use tibble() instead of data_frame()
data_frame(original = movie_avgs$b_i, 
           regularized = movie_reg_avgs$b_i, 
           n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5) +
  geom_count(aes(color = ..n.., size = ..n..)) +
  labs(x="Least Square Estimate", y="Regularized Estimate", title="Relationship between Regularized \n and Least Square Estimates") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_abline(color="red")
ggsave('RegularizedEstimate.bmp')

#Let's now look at the top 10 best movies when using regularization
edx %>%
  dplyr::count(movieId) %>% 
  left_join(movie_reg_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

#and the top 10 worst movies using regularization
edx %>%
  dplyr::count(movieId) %>% 
  left_join(movie_reg_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable() 

#But lambda is a tunning parameter so we can use cross-validation. 
#In practice, we should be using full cross-validation 
#just on a training set without using the test set 
#until the final assessment.
lambdas <- seq(0, 10, 0.25)

mu <- mean(edx$rating)
just_the_sum <- edx %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())
rmses <- sapply(lambdas, function(l){
  predicted_ratings <- validation %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    .$pred
  return(RMSE(predicted_ratings, validation$rating))
})
qplot(lambdas, rmses)  + 
  labs(x="Lambdas", y="RMSES", title="Regularization (Movie Effects)") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave('RMSES_lambdas.bmp')
#Lambda that minimizes the RMSE
lambdas[which.min(rmses)]

#We can also use regularization to estimate the user 
#effect. This part takes over a minute to process.
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(edx$rating)
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  return(RMSE(predicted_ratings, validation$rating))
})
#Plot the RMSES vs. the Lambdas
qplot(lambdas, rmses) + 
  labs(x="Lambdas", y="RMSES", title="Regularization (Movie and Users Effects)") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave('RMSES_lambdas_Reg.bmp')
#Select the lambda that minimizes the RMSE
lambda <- lambdas[which.min(rmses)]
lambda
#RMSE results
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effects Model",  
                                     RMSE = min(rmses)))
#RMSE results in form of table
rmse_results %>% knitr::kable()

#The RMSE has decreased 0.00041% with respect to model 5
rmse_results$RMSE[5]-rmse_results$RMSE[6]
rmse_results$RMSE[1]-rmse_results$RMSE[6]

#Save the RMSE results as a file
write.table(rmse_results, file = "RMSE.csv",
            sep = "\t", row.names = F)


#Generating a model to estimate the probability for movie ratings:
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(movielens$rating, times = 1, p = 0.1, list = FALSE)
test_set <- movielens[test_index, ]
train_set <- movielens[-test_index, ]
#or
train_set <- movielens %>% slice(-test_index)
test_set <- movielens %>% slice(test_index)

# Estimating averages and standard deviations
params <- train_set %>%
  group_by(userId) %>%
  summarize(avg = mean(rating), sd = sd(rating))
params
# Estimating the prevalence
pi <- train_set %>% summarize(pi=mean(rating=="1")) %>% pull(pi)
pi
# Getting an actual rule
x <- test_set$rating
f0 <- dnorm(x, params$avg[3], params$sd[3])
f1 <- dnorm(x, params$avg[2], params$sd[2])
p_hat_bayes <- f1*pi / (f1*pi + f0*(1 - pi))
# Can we find a polynome that fits this function ?
model <- lm(p_hat_bayes ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) )
summary(model)
#Plot takes a couple of minutes to process
#Plot looks like a logistic regression estimate. 
plot(x,p_hat_bayes, xlab="Rating", ylab="Probability", main="Estimated Probability")
# For each value of x, we get the value of "y" estimated by the model, then add it to the plot
myPredict <- predict( model ) 
#Takes several minutes to process
ix <- sort(x,index.return=T)$ix
lines(x[ix], myPredict[ix], col=2, lwd=2 ) 
#Add pertinent features of the model to the plot
coeff <- round(model$coefficients , 2)
text(3, 0.3 , paste("Model : ",coeff[1] , " + " , coeff[2] , "*x"  , "+" , coeff[3] , "*x^2" , "+" , coeff[4] , "*x^3" , "+" , coeff[5] , "*x^4" , "+" , coeff[6] , "*x^5" , "\n\n" , "P-value adjusted = ",round(summary(model)$adj.r.squared,2)))


#Check evolution of ratings with time for specific movie genre 
#Select initial time
t0 <- edx$timestamp[1] # "1996-08-02"
#Test other specific polynomals
test.formula <- y ~ poly(x,9)
edx %>% filter(genres =="Drama", timestamp>t0) %>%
  mutate(timestamp = round_date(timestamp, unit = "years")) %>%
  group_by(timestamp) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(timestamp, rating)) +
  geom_point() +
  geom_smooth(method = 'lm', formula= test.formula, color='dark blue' ) +
  stat_poly_eq(formula= test.formula,
               aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~")), 
               parse = TRUE) +
  theme_bw() +
  labs(x="Date (year) ", y="Ratings", title = "Drama Ratings Evolution (1996-2008)") +
  scale_x_date(date_breaks = "6 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  theme(plot.title = element_text(hjust = 0.5))
ggsave('RatingEvol_Drama.bmp')

#Check evolution of ratings with time for specific movie title 
edx %>% filter(title =="Shawshank Redemption, The (1994)", timestamp>t0) %>%
  mutate(timestamp = round_date(timestamp, unit = "years")) %>%
  group_by(timestamp) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(timestamp, rating)) +
  geom_point() +
  geom_smooth(method = 'lm', formula= test.formula, color='dark blue' ) +
  stat_poly_eq(formula= test.formula,
               aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~")), 
               parse = TRUE) +
  theme_bw() +
  labs(x="Date (year) ", y="Ratings", title = "Shawshank Redemption Ratings Evolution (1996-2008)") +
  scale_x_date(date_breaks = "6 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  theme(plot.title = element_text(hjust = 0.5))
ggsave('RatingEvol_ShawshankRedemption.bmp')


