library(tidyverse)
library(stringr)
library(tidytext)
library(wordcloud)
library(rvest)
library(babynames)
## functions ----
fix_ratings <- function(x){
  x <- unclass(x[[1]]) %>% unlist(recursive=FALSE)
  names(x) <- gsub("ratings.",'',names(x))
  x <- as_tibble(x)
  return(x)
}

clean_reviews <- function(x, band, album)
{
  # fixes for â\u0080\u0093, â\u0080\u0099etc. need to be done
  x <- gsub("â\u0080\u0093", "--", x)
  x <- gsub("â\u0080\u0099|â\u0080\u0098", "'", x)
  x <- gsub("â\u0080\u009d|â\u0080\u009c", '"', x)
  x <- gsub("â\u0080|â\u0098\u0085|â\u0088\u0091|â\u0097\u008|â\u009a\u0084|â\u0088\u009e|â\u0099¡|â\u0084¢"," ", x)
  x <- gsub("â\u0089¥Ë\u009c|â\u0088\u0086Ë\u009a|â\u009d¤|â\u0098¯|â\u009c|â\u0088\u0086"," ",x)
  x <- gsub("â\u0097\u008a|â\u0099¢","o",x)
  x <- gsub("â\u0082¬","€",x)
  x <- gsub("â\u0081\u0084","/",x)
  x <- gsub("â\u0099¯","#",x)
  x <- gsub("â\u009e¢",'-',x)
  x <- gsub("â\u0086\u0092",'->',x)
  x <- gsub("?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)",'',x)
  if(grepl('â', x)) browser()
  x <- iconv(x, "UTF-8", "ASCII","") #%>% iconv(x,"latin1","ASCII")
  x <- tolower(x)
  album <- tolower(album)
  band <- tolower(band)
  x <- str_split(x,"function[(]d.*[)]") %>% unlist() %>%
    nth(1)
  x <- gsub("[[:space:]]", " ", x)
  # x <- gsub("<.*>", " ", x)
  x <- gsub("[.]", ' . ', x)
  #x <- gsub("[[].*[]]", " ", x)
  band <- gsub('[)]|[(]','',band)
  band <- gsub("[[].*[]]", "", band)
  band <- gsub('([?]|[*])','',band) %>% str_trim('both')
  album <- gsub('[)]|[(]','',album)
  album <- gsub("[[].*[]]", "", album)
  album <- gsub('([?]|[*])','',album) %>% str_trim('both')
  x <- gsub(paste0("(",band,'|',band, "+)"), " ", x)
  x <- gsub(paste0("(",album,'|',album, "+)"), " ", x)
  x <- gsub("[[:digit:]]"," ", x)
  x <- gsub("\"[^\"]+\"","",x)
  return(x)
}
fix_ratings <- function(x){
  x <- unclass(x[[1]]) %>% unlist(recursive=FALSE)
  names(x) <- gsub("ratings.",'',names(x))
  x <- as_tibble(x)
  return(x)
}
review_wordcloud <- function(dat, filename)
{
  filename <- gsub("%3F",'',filename)
  # from https://www.r-bloggers.com/word-cloud-in-r/ and
  # https://stackoverflow.com/questions/8399100/r-plot-size-and-resolution
  png(
    filename,
    width     = .5,
    height    = .5,
    units     = "in",
    res       = 1200,
    pointsize = 3
  )
  par(
    mar      = c(5, 5, 2, 2),
    xaxs     = "i",
    yaxs     = "i",
    cex.axis = 2,
    cex.lab  = 2
  )
  
  pal <- brewer.pal(9, "BuGn")
  pal <- pal[-(1:2)]
  dat %>% 
    with(wordcloud(word, 
                   tf_idf, 
                   scale = c(1.25,.4),
                   max.words = 50,
                   colors = pal, 
                   vfont=c("serif","bold")))
  dev.off()
}
writeLines_wspace <- function(x){
  writeLines(x)
  writeLines('')
  writeLines('------------------------------------------------------------------------------------------------------------------')
}

## stop words ----
extra_stops <- c('band','bands','album','albums','song',
                 'songs','music','review','summary',
                 'sputnik','sputnimusic','url','www')
stop_words <- bind_rows(stop_words,tibble(word=extra_stops,lexicon = 'custom'))#bind_rows(stop_words, 
last_names <- read_html('https://web.archive.org/web/20170715124521/https://names.mongabay.com/most_common_surnames.htm') %>% 
  html_table() %>% `[[`(1) %>%
  mutate(word = tolower(Surname), lexicon = 'custom') %>%
  select(word,lexicon)
babynames <- babynames %>% 
  filter(year>1950) %>%
  group_by(name) %>% 
  summarise(prop = sum(prop)) %>% 
  ungroup() %>%
  arrange(desc(prop)) %>% 
  mutate(word = tolower(name), lexicon = 'custom') %>%
  select(word, lexicon) %>%
  `[`(1:5000,)
stop_words <- bind_rows(stop_words, last_names, babynames)
## Script ----

fils <- dir('./data/','.', full.names = TRUE)

ratings_dat <- map_df(fils, read_rds)
ratings <- vector('list',nrow(ratings_dat))
for(i in 1:nrow(ratings_dat)) {
  ratings[[i]] <- fix_ratings(ratings_dat$ratings[i]) %>%
    mutate(staff = ratings_dat$user[i], rev_link = ratings_dat$rev_link[i])
}
ratings <- bind_rows(ratings)
dat <- ratings %>% 
  group_by(staff, albumlink) %>% 
  summarise(Mean = mean(Rating),
            count = n()) %>%
  ungroup() %>% group_by(staff) %>%
  summarise(Median = median(Mean, na.rm = TRUE),
            Mean = mean(Mean, na.rm = TRUE),
            count_mean = mean(count),
            count_median = median(count),
            rev_count = n()) %>%
  left_join(ratings_dat, by = c(staff="user")) %>% 
  group_by(staff) %>%
  summarise(mean_rating = mean(Mean, na.rm = TRUE),
            median_rating = median(Median, na.rm = TRUE),
            count_reviews = mean(rev_count),
            median_replies = median(replies,na.rm = TRUE), median_views = median(views,na.rm = TRUE),
            max_replies = max(replies,na.rm = TRUE), max_views = max(views, na.rm = TRUE),
            mean_rev_score = mean(as.numeric(score)),
            max_replies_album = paste(band[which.max(replies)],album[which.max(replies)], sep = "_"),
            max_views_album = paste(band[which.max(views)],album[which.max(views)], sep = "_")) %>%
  mutate(median_replies_rank = min_rank(desc(median_replies)), median_views_rank = min_rank(desc(median_views)))
rm(ratings)
tmp <- list()
for(i in unique(ratings_dat$user)){
  tmp[[i]] <- ratings_dat %>% filter(user == i) %>%
    mutate(text = pmap_chr(list(text,band,album), clean_reviews))
}
revs <- bind_rows(tmp)
rm(tmp)
revs <- revs %>% 
  select(user, rev_link, text) %>% 
  group_by(user) %>% unnest_tokens(word,text) %>% 
  ungroup() %>%
  filter(nchar(word) > 1 , !grepl("'s$",word)) %>%
  group_by(user) %>%
  count(user,word, sort = TRUE) %>%
  filter(n > 2) %>% 
  anti_join(stop_words, by = 'word') %>%
  anti_join(last_names,by = 'word') %>%
  anti_join(babynames, by = 'word') %>%
  bind_tf_idf(word, user, n) %>% 
  ungroup() %>% filter(idf != max(idf)) %>%
  # mutate(tf_idf = map2_dbl(tf,idf, ~ weighted.mean(c(.x,.y),c(5,1)))) %>% 
  group_by(user) %>%
  top_n(50) %>%
  arrange(user, -tf_idf) %>% 
  left_join(dat, by = c(user='staff')) %>%
  ungroup()

if(!dir.exists('./wordclouds/')) dir.create('./wordclouds/')
staff <- unique(revs$user)
# staff <- gsub("%3F",'',staff)
filenames <- paste0('./wordclouds/', staff,'.png')
walk2(staff, filenames, ~ review_wordcloud(dat = revs %>% filter(user == .x),.y))

tocopy <- with(dat,sprintf(paste0('User: <strong>%s</strong>, # of Reviews: %i',
                         "\nMean Review Rating by Reviewer: %.02f, Mean Rating of Album: %.02f",
                         "\nMedian Views of Reviews: %.00f (Rank: %i), Median Replies of Reviews: %.00f (Rank: %i)",
                         '<a href="http://www.sputnikmusic.com/blog/wp-content/uploads/%s.png"><img  src="http://www.sputnikmusic.com/blog/wp-content/uploads/%s.png" alt="%s" width="600" height="600" /></a>"'),
                  staff,as.integer(count_reviews),
                  mean_rev_score,mean_rating,
                  median_views, median_views_rank, median_replies,median_replies_rank,
                  max_views, max_views_album, 
                  max_replies,  max_replies_album,
                  staff,staff,staff))
                  

walk(tocopy,writeLines_wspace)