# loading packages
library(twitteR)
library(ROAuth)
library(tidyverse)
library(purrrlyr)
library(text2vec)
library(caret)
library(glmnet)
library(ggrepel)
library(png)
library(grid)

## the following code is copied over/modified from https://analyzecore.com/2017/02/08/twitter-sentiment-analysis-doc2vec/
### loading and preprocessing a training set of tweets
# function for converting some symbols
conv_fun <- function(x) iconv(x, "latin1", "ASCII", "")

##### loading classified tweets ######
# source: http://help.sentiment140.com/for-students/
# 0 - the polarity of the tweet (0 = negative, 4 = positive)
# 1 - the id of the tweet
# 2 - the date of the tweet
# 3 - the query. If there is no query, then this value is NO_QUERY.
# 4 - the user that tweeted
# 5 - the text of the tweet
setwd('./statnik13_wc/')

set.seed(2018)
percent_train <- .5
if(file.exists('training.1600000.processed.noemoticon.csv')) stop('download twitter data from https://docs.google.com/file/d/0B04GJPshIjmPRnZManQwWEdTZjg/edit')
tweets_classified <- read_csv('training.1600000.processed.noemoticon.csv', col_names = c('sentiment', 'id', 'date', 'query', 'user', 'text')) %>%
  .[sample(1:nrow(.),round(nrow(.)*percent_train)), ] %>%
  # converting some symbols
  dmap_at('text', conv_fun) %>%
  # replacing class values
  mutate(sentiment = ifelse(sentiment == 0, 0, 1))
# there are some tweets with NA ids that we replace with dummies
tweets_classified_na <- tweets_classified %>%
  filter(is.na(id) == TRUE) %>%
  mutate(id = c(1:n()))
tweets_classified <- tweets_classified %>%
  filter(!is.na(id)) %>%
  rbind(., tweets_classified_na)

# data splitting on train and test
set.seed(2340)
trainIndex <- createDataPartition(tweets_classified$sentiment, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)
tweets_train <- tweets_classified[trainIndex, ]
tweets_test <- tweets_classified[-trainIndex, ]

##### Vectorization #####
# define preprocessing function and tokenization function
prep_fun <- tolower
tok_fun <- word_tokenizer

it_train <- itoken(tweets_train$text, 
                   preprocessor = prep_fun, 
                   tokenizer = tok_fun,
                   ids = tweets_train$id,
                   progressbar = TRUE)
it_test <- itoken(tweets_test$text, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun,
                  ids = tweets_test$id,
                  progressbar = TRUE)

# creating vocabulary and document-term matrix
vocab <- create_vocabulary(it_train)
vectorizer <- vocab_vectorizer(vocab)
dtm_train <- create_dtm(it_train, vectorizer)
# define tf-idf model
tfidf <- TfIdf$new()
# fit the model to the train data and transform it with the fitted model
dtm_train_tfidf <- fit_transform(dtm_train, tfidf)
# apply pre-trained tf-idf transformation to test data
dtm_test_tfidf  <- create_dtm(it_test, vectorizer) %>% 
  transform(tfidf)

# train the model
t1 <- Sys.time()
glmnet_classifier <- cv.glmnet(x = dtm_train_tfidf,
                               y = tweets_train[['sentiment']], 
                               family = 'binomial', 
                               # L1 penalty
                               alpha = 1,
                               # interested in the area under ROC curve
                               type.measure = "auc",
                               # 5-fold cross-validation
                               nfolds = 5,
                               # high value is less accurate, but has faster training
                               thresh = 1e-3,
                               # again lower number of iterations for faster training
                               maxit = 1e3)
print(difftime(Sys.time(), t1, units = 'mins'))

plot(glmnet_classifier)
print(paste("max AUC =", round(max(glmnet_classifier$cvm), 4)))

preds <- predict(glmnet_classifier, dtm_test_tfidf, type = 'response')[ ,1]
auc(as.numeric(tweets_test$sentiment), preds)
# saveRDS(glmnet_classifier, file = 'model.rds')
### end copied over code

## scrape World Cup list comments
library(rvest)
library(lubridate)

wc_url <- 'https://www.sputnikmusic.com/list.php?listid=181739' # wc list url link
wc_obj <- wc_url %>%
  read_html(encoding = 'UTF-8')
comments <- wc_obj %>%
  html_node('div#c181739') %>%
  html_node('table') %>%
  html_children() # get all nodes that are dates, users, comments

comments_tbl <- map(c(1:3), ~comments %>%
  html_nodes(paste0('font[size="', .x, '"]')) %>%
    html_text() %>%
    as_tibble()) %>%
  bind_cols() %>%
  set_names(c('date', 'text', 'user')) %>%
  mutate(date = mdy(date),
         query = '',
         id = 1:n()) %>%
  dmap_at('text', conv_fun) # convert data into a tibble with same columns as tweet data
rm(comments, wc_obj)

# preprocessing and tokenization
it_tweets <- itoken(comments_tbl$text,
                    preprocessor = prep_fun,
                    tokenizer = tok_fun,
                    ids = comments_tbl$id,
                    progressbar = TRUE)

# creating vocabulary and document-term matrix
dtm_tweets <- create_dtm(it_tweets, vectorizer)

# transforming data with tf-idf
dtm_tweets_tfidf <- fit_transform(dtm_tweets, tfidf)

# predict probabilities of positiveness
preds_tweets <- predict(glmnet_classifier, dtm_tweets_tfidf, type = 'response')[ ,1]

# adding rates to initial dataset
comments_tbl$sentiment <- preds_tweets


## plot data
lower95 <- function(dat,n){
  a <- 1 + (dat*n)
  b <- 1 + n - (dat*n)
  x <- ((a/(a+b))-1.65*sqrt(a*b/(((a+b)^2)*(a+b+1))))
  return(x)
}
upper95 <- function(dat,n){
  a <- 1 + (dat*n)
  b <- 1 + n - (dat*n)
  x <- ((a/(a+b))+1.65*sqrt(a*b/(((a+b)^2)*(a+b+1))))
  return(x)
}


teams <- read_csv('WC_teams2018.csv') %>%
  mutate_at(vars(team), tolower) # read team names and regex
teams <- tibble(fils = dir('./flags/', '.png', full.names = T)) %>%
  mutate(team = fils %>% 
           str_split('/', simplify = T) %>% .[, ncol(.)] %>%
           str_replace('.png', '') %>%
           str_replace_all('-', ' ') %>%
           tolower(),
         img = map(fils, ~readPNG(.x) %>%
           rasterGrob(width=unit(.2,"inch"), 
                      height=unit(.2,"inch"), 
                      interpolate = F) %>%
           list())) %>%
  unnest() %>%
  right_join(teams) # add image data


detect_team <- function(text, teams){
  vec <- rep( FALSE, nrow(teams))
  for(i in 1:nrow(teams)){
    vec[i] <- str_detect(text, teams$regex[i])
  }
  return(teams$team[vec])
}
comments_tbl <- comments_tbl %>%
  mutate(team = map(tolower(text), 
                    ~ detect_team(.x, !!teams))) # detect teams in each comment
comments_tbl_teams <- comments_tbl %>% # create separate tibble for team related comments
  filter(map_dbl(team, length) == 1) %>% # filter to comments only containing one team
  unnest() %>%
  full_join(teams) # join comments tibble with teams tibble that contains images
dates_tbl <- comments_tbl %>%
  select(date, id) %>%
  group_by(date) %>%
  summarise_all(first) %>%
  mutate(label = paste('Day', 1:n())) # make tibble of unique dates for horizontal marks of the plot

comments_tbl_teams %>%
  group_by(team) %>%
  summarise(sentiment = mean(sentiment), n_comments = n()) %>%
  ungroup() %>%
  mutate(l95 = map2_dbl(sentiment, n_comments, lower95),
         l95_Rank = min_rank(desc(l95))) %>%
  select(l95_Rank, team, sentiment, n_comments) %>%
  arrange(l95_Rank) %>%
  knitr::kable('html', digits = 3, align = 'l') # team sentiment ranking

user_rank_tbl <- comments_tbl %>%
  group_by(user) %>%
  summarise(sentiment = mean(sentiment), n_comments = n()) %>%
  ungroup() %>%
  mutate(l95 = map2_dbl(sentiment, n_comments, lower95),
         u95 = map2_dbl(sentiment, n_comments, upper95),
         l95_Rank = min_rank(desc(l95)),
         u95_Rank = min_rank(u95)) # calculate uncertainty of sentiment per user


user_rank_tbl %>%
  select(l95_Rank, user, sentiment, n_comments) %>%
  arrange(l95_Rank) %>%
  head(20) %>%
  knitr::kable('html', digits = 3, align = 'l') # user positive sentiment ranking

user_rank_tbl %>%
  select(u95_Rank, user, sentiment, n_comments) %>%
  arrange(u95_Rank) %>%
  head(20) %>%
  knitr::kable('html', digits = 3, align = 'l') # user negative sentiment ranking

# plot 
wc_plt <- ggplot(comments_tbl, aes(id, sentiment)) +
  geom_point(alpha = .2) +
  theme(legend.position="none") +
  coord_cartesian(xlim = c(0,max(comments_tbl$id)+2)) +
  geom_vline(data = dates_tbl, aes(xintercept = id))  +
  mapply(function(xx, yy, ii) {
    x = comments_tbl_teams$img[[ii]]
    x$name = ii
    print(ii)
    annotation_custom(x, xmin=xx-.2, xmax=xx+.2, ymin=yy-.2, ymax=yy+.2)},
    comments_tbl_teams$id, 
    comments_tbl_teams$sentiment, 
    seq_len(nrow(comments_tbl_teams))) +
  geom_text(data = dates_tbl, mapping = aes(label = label, y = -.05), angle = 60, hjust = 0) +
  ggtitle('World Cup 2018 Sentiment',
          'In order of appearnce from left to right, sentiment of 1 is positive, 0 is negative')
wc_plt +
  ggsave('WC_Sentiment_plot.png', height = 6, width = 8)

