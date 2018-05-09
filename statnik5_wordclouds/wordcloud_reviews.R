library(tidyverse)
library(rvest)
library(tidytext)
library(stringr)
library(wordcloud)
library(ggraph)
library(igraph)
### functions----
read_comments <- function(url, sput = 'https://www.sputnikmusic.com/'){
  rev_url <- str_split(url,'/', simplify = TRUE) %>% 
    `[`(str_detect(.,'[:digit:]')) %>%
    paste0(sput,'album.php?reviewid=',.) 
  rev_page <- read_html(rev_url)
  reviewer <- rev_page %>% 
    html_nodes('a') %>% 
    html_attr('href') %>% 
    grep(pattern = 'user',x = ., value = TRUE) %>% 
    nth(1) %>%
    str_split('/',simplify = TRUE) %>%
    nth(3)
  band <- rev_page %>% 
    html_nodes('h1') %>% 
    html_nodes('a') %>% 
    html_text()
  album <- rev_page %>% 
    html_nodes('h1') %>% 
    html_nodes('span') %>% 
    html_text()
  comments <- list()
  users <- list()
  adder <- 1
  clean_comments <- function(rev_page){
    comments <- rev_page %>% 
      html_nodes('td.default') %>% 
      html_text()
    comments <- map_chr(comments, ~ gsub('Album Rating: [0-9][.][0-9]', '', .x) %>%
                          gsub('[[:space:]]', ' ', x = .) %>%
                          gsub(' [|] Sound Off', ' ', x = .) %>%
                          str_split('Digging: ', simplify = TRUE) %>%
                          nth(1))
  }
  clean_users <- function(rev_page){
    users <- rev_page %>% 
      html_nodes('td.comment') %>% 
      html_nodes('font.mediumtext') %>%
      html_text()
  }
  repeat{
    comments[[adder]] <- tibble(comments=clean_comments(rev_page))
    users[[adder]] <- tibble(users = clean_users(rev_page))
    if(nrow(users[[adder]])==0) break
    adder <- adder + 1
    rm(rev_page)
    rev_url_new <- paste0(rev_url,'&page=',adder)
    rev_page <- read_html(rev_url_new)
  }
  comments <- comments[-adder]
  users <- users[-adder]
  dat <- bind_cols(bind_rows(users),bind_rows(comments))
  dat$reviewer <- reviewer
  dat$url <- url
  dat$band <- band
  dat$album <- album
  return(dat)
}

analyze_comments <- function(dat){
  
  album <- tolower(dat$album[1])
  band <- tolower(dat$band[1])
  band <- gsub('[)]|[(]','',band)
  band <- gsub("[[].*[]]", "", band)
  band <- gsub('([?]|[*])','',band) %>% str_trim('both')
  album <- gsub('[)]|[(]','',album)
  album <- gsub("[[].*[]]", "", album)
  album <- gsub('([?]|[*])','',album) %>% str_trim('both')
  dat$comments <- gsub(paste0(band,'|',album), ' ', dat$comments, ignore.case = TRUE)
  wc_dat <- dat %>% 
    unnest_tokens(word,comments) %>%
    filter(!word %in% c('â', 'ã')) %>%
    filter(nchar(word) > 1) %>%
    count(word) %>% 
    anti_join(stop_words, by = 'word') %>%
    filter(n>2)
  
  ngram_stops <- c('message','edited')
  ngram_dat <- dat %>%
    unnest_tokens(bigram, comments,
                  token = 'ngrams', n = 2, collapse = FALSE) %>%
    count(bigram, sort = TRUE) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% ngram_stops[1], !word2 %in% ngram_stops[2]) %>%
    filter(!word1 %in% (stop_words %>% 
                           filter(lexicon == 'snowball') %>% 
                          '$'(word)),
           !word2 %in% (stop_words %>% 
                           filter(lexicon == 'snowball') %>% 
                          '$'(word))) %>% 
    filter(!grepl('ã',word1),  !grepl('ã',word2), 
           nchar(word1) > 1, nchar(word2) >1) %>% # figure this one out, still need to remove php, 1 char words
    filter(!grepl('www[.]|php|[.]com|http',word1), !grepl('www[.]|php|[.]com|http',word2)) %>%
    filter(n > 1)
  ngram_graph <- ngram_dat %>%
    top_n(100) %>%
    head(150) %>%
    graph_from_data_frame()
  
  
}

review_wordcloud <- function(dat, filename)
{
  set.seed(66811924) # whoever reads this, guess the significance
  # from https://www.r-bloggers.com/word-cloud-in-r/ and
  # https://stackoverflow.com/questions/8399100/r-plot-size-and-resolution
  png(
    filename,
    width     = .5,
    height    = .5,
    units     = "in",
    res       = 24000,
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
  pal <- pal[-(1:4)]
  dat %>% 
    with(wordcloud(word, 
                   n, 
                   scale = c(.55,.15)*1000/length(n),
                   max.words = 1000,
                   colors = pal, 
                   vfont=c("serif","bold")))
  dev.off()
}

review_network_plot <- function(dat, filename){
  set.seed(66811924) # whoever reads this, guess the significance
  a <- grid::arrow(type = "closed", 
                   length = unit(.085*100/min(c(100,nrow(ngram_dat))), 
                                 "inches"))
  
  ngram_ggraph <- ggraph(dat, layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                   arrow = a, end_cap = circle(.04, 'inches')) +
    geom_node_point(color = "lightgreen", size = 2.5) +
    geom_node_text(aes(label = name), size = 2.2, vjust = 1, hjust = 1) +
    theme_void()
  ggsave(filename, scale = .9, plot = ngram_ggraph, width = 12, height = 8, units = 'in')
}




review_wordcloud(dat, paste0(dat$band[1],'_',dat$album[1],'.png'))
review_network_plot(ngram_dat, paste0(band,'_',album,'_network.jpg'))
