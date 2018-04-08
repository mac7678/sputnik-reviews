library(tidyverse)
library(XML)
library(rvest)
library(lubridate)
library(wesanderson)
# library(RCurl)
# library(httr)
library(tidytext)

## Functions -------
scrape_comments <- function(sput_url){
  find_comment_pages <- function(rev_html, php_url){
    last_page <- rev_html %>% 
      html_node('div.pagination') %>% 
      html_nodes('a') %>% .[length(.)-1] %>% 
      html_text() %>% as.numeric()
    links <- paste0(php_url, 1:last_page)
    return(links)
  }
  sput <- 'https://www.sputnikmusic.com'
  rev_html <- tryCatch({rev_html <- read_html(sput_url, 
                                              encoding = 'UTF-8')},
                       error = function(er) {
                         rev_html <- read_html(sput_url)
                         warning('could not parse as UTF-8')
                         writeLines('could not parse as UTF-8')
                         return(rev_html)
                       })
  if(length(rev_html %>% html_nodes('div#leftColumn')) == 0) return(tibble())
  php_url <- rev_html %>%
    html_nodes('a') %>% 
    html_attr('href') %>% 
    str_subset('page=.*#comments$') %>% 
    unique() %>%
    .[1] %>%
    str_replace('&page=[1-9]#comments', '&page=')
  if(!is.na(php_url)) {
    php_url <- paste0(sput, php_url)
    links <- find_comment_pages(rev_html, php_url)
  } else {
    links <- sput_url
  }
  dat <- vector('list', length(links))
  for(link in links){
    rev_html <- tryCatch({rev_html <- read_html(link, 
                                                encoding = 'UTF-8')},
                         error = function(er) {
                           rev_html <- read_html(link)
                           warning('could not parse as UTF-8')
                           writeLines('could not parse as UTF-8')
                           return(rev_html)
                         })
    user <- rev_html %>%
      html_nodes('a[style="text-decoration:none;"]') %>%
      html_nodes('font.mediumtext') %>% 
      html_text()
    if(length(user) == 0) next
    date <- rev_html %>% 
      html_nodes('font.smalloffset') %>%
      html_nodes(xpath = './text()') %>%
      html_text() %>%
      .[seq(1, length(.), 2)] %>%
      mdy()
    comment <- rev_html %>%
      html_nodes('td.default') %>%
      html_text() %>%
      str_replace_all('^Album Rating: [0-5].[0-9]', '') %>%
      str_replace_all('^ \\| Sound Off', '') %>% 
      str_split('Digging: |This Message Edited On ', simplify = TRUE) %>% .[, 1]
    dat[[link]] <- tibble(sput_url, link, user, date, comment)
    # writeLines()
  }
  dat <- bind_rows(dat)
  return(dat)
}
lower95 <- function(dat,n){
  a <- 1 + (dat*n)
  b <- 1 + n - (dat*n)
  x <- ((a/(a+b)) - 1.65 * sqrt(a*b/(((a+b)^2) * (a+b+1))))
  if(!is.nan(x) & x <= 0) x <- 0
  if(!is.nan(x) & x >= 1) x <- 1
  return(x)
}

## Albums Scrape
sput <- 'https://www.sputnikmusic.com'
albums <- read_csv('./m_slash/metal_albums.csv') %>%
  mutate(albumlink = paste0(sput, albumlink))
contestants <- tibble(user = c('evilford','Demon of the Fall', 'ScuroFantasma', 'bgillesp'),
                      albums = c(list('https://www.sputnikmusic.com/review/52006/Death-Symbolic/'),
                                 list('https://www.sputnikmusic.com/review/36664/Immolation-Close-to-a-World-Below/'),
                                 list(c('https://www.sputnikmusic.com/review/10694/Slayer-Reign-in-Blood/', 
                                        'https://www.sputnikmusic.com/review/1556/Morbid-Angel-Altars-of-Madness/')),
                                 list(c('https://www.sputnikmusic.com/review/42577/Voivod-Killing-Technology/',
                                        'https://www.sputnikmusic.com/review/1884/Dismember-Like-an-Ever-Flowing-Stream/',
                                        'https://www.sputnikmusic.com/review/39291/Pestilence-Consuming-Impulse/'))
                      ))

dat <- vector('list', nrow(albums))
empty <- c()
dat_dir <- './m_slash/data/'
if(!dir.exists(dat_dir)) dir.create(dat_dir)

for(i in seq_along(albums$albumlink)){
  if(file.exists(paste0(dat_dir, albums$albumlink[i] %>% 
                        str_split('/', simplify = T) %>% 
                        .[length(.)-1] %>%
                        paste0('.csv')))) next
  
  tryCatch(tmp <- scrape_comments(albums$albumlink[i]))
  # {tmp <- scrape_comments(albums$albumlink[i])},
  #          error = {tmp <- tibble()},
  #          finally = print(albums$albumlink[i]))
  if(nrow(tmp) == 0) empty <- c(empty, albums$albumlink[i])
  write_csv(tmp,
            paste0(dat_dir, albums$albumlink[i] %>% 
                     str_split('/', simplify = T) %>% 
                     .[length(.)-1] %>%
                     paste0('.csv')))
  dat[[i]] <- tmp
  tmp <- tibble()
  writeLines(paste0(albums$albumlink[i], ' COMPLETED'))
}


fils <- dir(dat_dir, '.csv', full.names = T)
dat <- map_df(fils, read_csv)

# grab all of KILL's comments 
kill <- filter(dat, user == 'KILL') %>% 
  .$comment #%>% 
#str_replace_all('\\r\\n', ' ')
write(kill, './m_slash/kill_comments.txt')

# sample 10,000 comments
txt <- paste(dat$user, dat$comment, sep = ': ') #%>%
#str_replace_all('\\r\\n', ' ')
set.seed(159753)
txt <- sample(txt, 10000)
write(txt, './m_slash/m_slash_comments.txt')
rm(txt, kill)

# remove comment with most m/'s from each album
m_regex <- '\\\\m/|\\\\M/|m//|m/m/|M//|M/M/|^m/$|^M/$|(\\s|^)m/|(\\s|^)M/|/m/|/M/|\\\\m\\\\|\\\\M\\\\'
dat <- anti_join(dat, 
                 dat %>% 
                   mutate(comment = comment %>%
                            str_replace_all('(f|ht)tp\\S+\\s*|www\\S+\\s*', ' '),
                          x = str_count(comment, m_regex)) %>%
                   arrange(-x) %>%
                   group_by(sput_url) %>%
                   summarise_all(first)
) 

# metal by year and month
year_dat <- dat %>%
  mutate(month = month(date), 
         year = year(date), 
         comment = comment %>%
           str_replace_all('(f|ht)tp\\S+\\s*|www\\S+\\s*', ' '),
         x=str_count(comment, m_regex)) %>%
  group_by(year, month) %>%
  summarise(`m/'s` = sum(x, na.rm = T),
            comments = n()) %>%
  ungroup() %>%
  mutate(day = 1, 
         date = mdy(paste(month, day, year, sep = '-'))) %>%
  select(date, `m/'s`, comments) %>%
  mutate(`m/'s` = `m/'s`/sum(`m/'s`),
         comments = comments/sum(comments)
  ) %>% 
  gather(count, Percent_Scaled, `m/'s`:comments)

ggplot(year_dat, 
       aes(x = date, y = Percent_Scaled, color = count)) + 
  geom_line() + 
  geom_point() + 
  scale_x_date(date_breaks = "1 year", 
               date_labels = "%Y") +
  xlab('year') +
  ggtitle("Sputnikmusic total m/'s and comments per month from 2003 to 2018*",
          "*Among a non-representative sample of 593 metal albums") +
  ggsave('./m_slash/ms_across_years.jpg', height = 7, width = 10, dpi = 500)

# sorted album data
album_tbl <- dat %>%
  mutate(comment = comment %>%
           str_replace_all('(f|ht)tp\\S+\\s*|www\\S+\\s*', ' '),
         x=str_count(comment, m_regex),
         album = str_split(sput_url, '/', simplify = T) %>% .[, 6]) %>%
  select(album, everything(), -sput_url) %>% 
  group_by(album) %>%
  summarise(total_m = sum(x, na.rm = TRUE), 
            m_comments = sum(x>0, na.rm = TRUE),
            m_ratio = total_m/m_comments,
            total_comments = n(), 
            m_per_comment = total_m/total_comments, 
            m_per_comment_l95 = lower95(m_per_comment, total_comments)) %>%
  mutate(m_per_l95_rank = min_rank(desc(m_per_comment_l95))) %>% 
  left_join(albums %>%
              mutate(albumlink = str_split(albumlink, 
                                           '/', simplify = T) %>%
                       .[, 6]), 
            by = c(album = 'albumlink')) %>%
  gather(Gen, Genre, Genre1:Genre3) %>%
  select(-Gen) %>%
  filter(!is.na(Genre)) %>%
  mutate(Genre = factor(Genre)) %>%
  ungroup() %>%
  group_by(album) %>%
  summarise_all(first) %>%
  rename(`Primary Genre` = Genre) %>%
  arrange(m_per_l95_rank)
write_csv(album_tbl, './m_slash/album_data.csv')

ggplot(album_tbl[1:10, ],
       aes(x = fct_reorder(album, m_per_comment_l95, .desc = T), 
           y = m_per_comment_l95,
           fill = `Primary Genre`)) + 
  geom_bar(stat = 'identity') +
  geom_text(aes(label = paste(total_comments, "# of", "comm's", sep = '\r\n')), 
            vjust=1, color = 'white') +
  xlab('album') +
  ylab("m/'s per comment lower 95% CI bound") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11)) +
  ggtitle("Album Threads with most m/'s per comment*",
          "*From a sample of 593 album threads, all of which are the flagged review") +
  scale_fill_manual(values = c(wes_palette("BottleRocket"), wes_palette("Royal1"))) +
  ggsave('./m_slash/ms_albums.jpg', height = 7, width = 10)


# sorted user data
user_tbl <- dat %>%
  mutate(comment = comment %>%
           str_replace_all('(f|ht)tp\\S+\\s*|www\\S+\\s*', ' '),
         x=str_count(comment, m_regex)) %>%
  group_by(user) %>%
  summarise(total_m = sum(x, na.rm = TRUE), 
            m_comments = sum(x>0, na.rm = TRUE),
            m_ratio = total_m/m_comments,
            total_comments = n(), 
            m_per_comment = total_m/total_comments, 
            m_per_comment_l95 = lower95(m_per_comment, total_comments)) %>%
  filter(total_comments >= 100) %>%
  mutate(m_per_l95_rank = min_rank(desc(m_per_comment_l95))) %>% 
  arrange(m_per_l95_rank)

ggplot(user_tbl[1:20, ],
       aes(x = fct_reorder(user, m_per_comment_l95, .desc = T), 
           y = m_per_comment_l95)) + 
  geom_bar(stat = 'identity',
           fill = 'orange') +
  geom_text(aes(label = paste(total_comments, "# of", "comm's", sep = '\r\n')),vjust=1) +
  xlab('user') +
  ylab("m/'s per comment lower 95% CI bound") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14)) +
  ggtitle("Users* with most m/'s per comment after 95% CI adjustment",
          "*with 100 or more comments") +
  ggsave('./m_slash/ms_users.jpg', height = 6, width = 12)

# genre data sorted
genre_tbl <- dat %>%
  left_join(albums, by = c(sput_url = 'albumlink')) %>%
  gather(Gen, Genre, Genre1:Genre3) %>%
  select(-Gen) %>%
  filter(!is.na(Genre)) %>%
  mutate(comment = comment %>%
           str_replace_all('(f|ht)tp\\S+\\s*|www\\S+\\s*', ' '),
         x=str_count(comment, m_regex)) %>%
  group_by(Genre) %>%
  summarise(total_m = sum(x, na.rm = TRUE), 
            m_comments = sum(x>0, na.rm = TRUE),
            m_ratio = total_m/m_comments,
            total_comments = n(), 
            m_per_comment = total_m/total_comments, 
            m_per_comment_l95 = lower95(m_per_comment, total_comments),
            unique_albums = length(unique(sput_url))) %>%
  filter(unique_albums >= 10) %>%
  mutate(m_per_l95_rank = min_rank(desc(m_per_comment_l95))) %>% 
  arrange(m_per_l95_rank) 
write_csv(genre_tbl, './m_slash/genre_data.csv')

ggplot(genre_tbl[1:20, ],
       aes(x = fct_reorder(Genre, m_per_comment_l95, .desc = T), 
           y = m_per_comment_l95)) + 
  geom_bar(stat = 'identity',
           fill = 'red') +
  geom_text(aes(label = paste(total_comments, "# of", "comm's", sep = '\r\n')),vjust=1) +
  xlab('genre') +
  ylab("m/'s per comment lower 95% CI bound") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14)) +
  ggtitle("Genres* with most m/'s per comment",
          paste0("*From a sample of 593 album threads, For genres with 10 or more unique albums, all comments from the flagged review \r\n",
                 "Albums can have up to 3 Genres")) +
  ggsave('./m_slash/ms_genre.jpg', height = 8, width = 12)

# m/'s over expected
xM_tbl <- dat %>%
  mutate(comment = comment %>%
           str_replace_all('(f|ht)tp\\S+\\s*|www\\S+\\s*', ' '),
         x=str_count(comment, m_regex),
         album = str_split(sput_url, '/', simplify = T) %>% .[, 6]) %>%
  select(album, everything(), -sput_url) %>%
  left_join(dat %>%
              mutate(comment = comment %>%
                       str_replace_all('(f|ht)tp\\S+\\s*|www\\S+\\s*', ' '),
                     x=str_count(comment, m_regex)) %>%
              group_by(user) %>%
              summarise(total_m = sum(x, na.rm = TRUE), 
                        m_comments = sum(x>0, na.rm = TRUE),
                        m_ratio = total_m/m_comments,
                        total_comments = n(), 
                        m_per_comment_user = total_m/total_comments) %>%
              select(user, m_per_comment_user)) %>%
  group_by(album) %>%
  summarise(total_m = sum(x, na.rm = TRUE), 
            m_comments = sum(x>0, na.rm = TRUE),
            m_ratio = total_m/m_comments,
            total_comments = n(), 
            m_per_comment = total_m/total_comments, 
            m_per_comment_l95 = lower95(m_per_comment, total_comments),
            m_per_comment_user = mean(m_per_comment_user, na.rm = T),
            avg_date = mean(date)) %>%
  mutate(m_over_xM = (m_per_comment-m_per_comment_user)*total_comments,
         m_over_xM_rank = min_rank(desc(m_over_xM))) %>% 
  left_join(albums %>%
              mutate(albumlink = str_split(albumlink, 
                                           '/', simplify = T) %>%
                       .[, 6]), 
            by = c(album = 'albumlink')) %>%
  gather(Gen, Genre, Genre1:Genre3) %>%
  select(-Gen) %>%
  filter(!is.na(Genre)) %>%
  ungroup() %>%
  group_by(album) %>%
  summarise_all(first) %>%
  rename(`Primary Genre` = Genre) %>%
  arrange(m_over_xM_rank)
write_csv(xM_tbl, './m_slash/xM_data.csv')

ggplot(xM_tbl[c(1:20), ],
       aes(x = fct_reorder(album, m_over_xM, .desc = T), 
           y = m_over_xM,
           fill =  `Primary Genre`)) + 
  geom_bar(stat = 'identity') +
  geom_text(aes(label = paste(total_comments, "# of", "comm's", sep = '\r\n')),
            vjust=1,
            color = 'white') +
  xlab('album') +
  ylab("m/'s over expected") +
  theme(axis.text.x = element_text(angle = 75, hjust = 1, size = 12)) +
  ggtitle("Album Threads with MOST m/'s over Expected") + 
  scale_fill_manual(values = c(wes_palette("Moonrise1"), wes_palette("Royal1"),  wes_palette("Royal2"))) +
  # facet_wrap(~Type , scale="free", nrow = 2) +
  ggsave('./m_slash/ms_over_expected_most.jpg', height = 8, width = 12)
ggplot(tail(xM_tbl, 20),
       aes(x = fct_reorder(album, m_over_xM, .desc = T), 
           y = m_over_xM,
           fill = `Primary Genre`)) + 
  geom_bar(stat = 'identity') +
  geom_text(aes(label = paste(total_comments, "# of", "comm's", sep = '\r\n')),
            vjust=0) +
  scale_fill_manual(values = c(wes_palette("Royal1"), wes_palette("Moonrise1"),  wes_palette("Royal2"))) +
  xlab('album') +
  ylab("m/'s over expected") +
  theme(axis.text.x = element_text(angle = 75, hjust = 1, size = 12)) +
  ggtitle("Album Threads with LEAST m/'s over Expected") +
  ggsave('./m_slash/ms_over_expected_least.jpg', height = 8, width = 12)

# contest winner

contestants <- contestants %>% unnest()
contest <- list()
for(i in seq_along(contestants$albums)){
  tryCatch(tmp <- scrape_comments(contestants$albums[i]))
  contest[[i]] <- tmp
  tmp <- tibble()
  writeLines(paste0(contestants$albums[i], ' COMPLETED'))
}
contest_tbl <- bind_rows(contest)
contest_tbl <- contest_tbl %>%
  mutate(comment = comment %>%
           str_replace_all('(f|ht)tp\\S+\\s*|www\\S+\\s*', ' '),
         x=str_count(comment, m_regex),
         album = str_split(sput_url, '/', simplify = T) %>% .[, 6]) %>%
  select(album, everything(), -sput_url) %>% 
  group_by(album) %>%
  summarise(total_m = sum(x, na.rm = TRUE), 
            m_comments = sum(x>0, na.rm = TRUE),
            m_ratio = total_m/m_comments,
            total_comments = n(), 
            m_per_comment = total_m/total_comments, 
            m_per_comment_l95 = lower95(m_per_comment, total_comments)) %>%
  mutate(m_per_l95_rank = min_rank(desc(m_per_comment_l95))) %>%
  arrange(m_per_l95_rank) %>%
  left_join(contestants %>% mutate(album = str_split(albums, '/', simplify = T) %>% .[, 6]))
knitr::kable(contest_tbl %>%
               select(album, 
                      m_per_comment_l95,
                      m_per_l95_rank, user) %>%
               mutate(m_per_comment_l95 = round(m_per_comment_l95, 3)),
            format = 'html')
