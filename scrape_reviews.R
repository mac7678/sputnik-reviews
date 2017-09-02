library(tidyverse)
library(XML)
library(rvest)
library(tidytext)
### functions ----
strsplit_last <- Vectorize(function(x, split = "/") tail(unlist(strsplit(x,split)),1))
find_rev_tab <- function(staff_link)
{
  x <- grep("/user_reviews.php?",getHTMLLinks(staff_link),value=TRUE)[1]
  return(x)
}
find_reviews <- function(staff_link)
{
  x <- grep("/review/",getHTMLLinks(staff_link),value=TRUE)
  return(x)
}
findSputURL_soundoffpage <- function(sputurl){ # find soundoffpage
  links <- getHTMLLinks(sputurl)
  if(!is.na(links[grep('/soundoff',links)[1]])){
    links <- links[grep('/soundoff',links)[1]]
    links <- paste0('http://www.sputnikmusic.com',links)
  }else{
    links <- sputurl
  }
  return(links)
}
findSputURL_band <- function(sputurl){ # find band page
  links <- getHTMLLinks(sputurl)
  if(!is.na(links[grep('bands/',links)[1]])){
    links <- links[grep('bands/',links)[1]]
    links <- paste0('http://www.sputnikmusic.com/',links)
  }else{
    links <- sputurl
  }
  
  return(links)
}

find_album <- function(tmp){
  newtmp <- list()
  if(any(names(tmp)=='font') & any(names(tmp)=='.attrs')){
    if(regexpr('/album/',tmp$.attrs)>0){
      newtmp <- tmp
    }else{
      newtmp <- NULL
    }
  }else{
    newtmp <- NULL
  }
  return(newtmp)
}
scrape_band <- function(band_page){
  links <- xpathSApply(band_page, "//b")
  band <- xmlToList(links[[1]]) # get band name
  return(band)
}


scrape_soundoff <- function(obj,link,alldat=list(),getuser_metrics = TRUE){ # function to scrape data 
  if(!any(class(obj) %in% c("HTMLInternalDocument", "HTMLInternalDocument",
                            "XMLInternalDocument",  "XMLAbstractDocument")) && is.character(obj)){
    link <- obj
    if(!grepl('/soundoff.php',obj)) stop('Character string provided is not a soundoff page')
    obj <- htmlParse(obj)
  }
  if(!any(class(obj) %in% c("HTMLInternalDocument", "HTMLInternalDocument",
                            "XMLInternalDocument",  "XMLAbstractDocument")) && !is.character(obj)) {
    stop(paste0('input is not an html object or a character.',
                'if calling this function directly,',
                ' ensure that your input is a character ',
                'string that is the name of sputnikmusic soundoff page.'))
  }
  user_links <- grep('/user/',getHTMLLinks(obj),value = TRUE)
  links <- getHTMLLinks(obj)
  if(length(grep('/best/albums/',links))>1){ # if theirs more than one link to best albums
    # ... that means the release year is in the second link
    release.year <- as.numeric(tail(unlist(strsplit(tail(grep('/best/albums/',links,value = TRUE),1),'/')),1))
  }else{
    release.year <- as.numeric(tail(unlist(strsplit(unlist(lapply(xpathSApply(obj, "//b"),xmlToList)[[2]]),'/')),1))
  }
  dat<-readHTMLTable(obj,which = 1)
  if(any(grepl('http:/',user_links))) user_links <- user_links[-grep('http:/',user_links)]
  dat$V2 <- as.character(dat$V2)
  names(dat)[1] <- 'Rating'
  dat <- dat[!is.na(dat$V2),]
  dat <- dat[c(1,2)]
  dat$Rating <- as.numeric(substr(dat$Rating,1,3))
  dat <- dat[!is.na(dat$Rating),]
  dat <- dat[2:nrow(dat),]
  # for (i in 1:length(dat$V2))
  split_rating <- function(dat){
    tmpstr <- strsplit(x = dat, split = ' | ',fixed = TRUE)
    out <- data.frame(user = tmpstr[[1]][1],date = tmpstr[[1]][2])
    return(out)
  }
  dat <- data.frame(dat,bind_rows(lapply(dat$V2,split_rating)))
  user_links <- user_links[dat$Rating>0]
  if(getuser_metrics){
    user_stats <- bind_rows(lapply(user_links,userstat,alldat = alldat))
  } else {
    user_stats <- data.frame()
  }
  dat <- dat[dat$Rating>0,]
  dat <- dat[, c(1,3,4)]
  dat <- map_df(dat,~ if(is.factor(.x)) as.character(.x) else .x)
  sputdate <- function(dates){
    if(!is.character(dates)) stop('Not readable')
    s<-c()
    for (i in 1:length(dates))
    {
      tmpdate <- unlist(strsplit(dates[i], ' '))
      if (length(tmpdate)==1){
        s[i] <- NA}
      else
      {
        mo <- grep(strsplit(tmpdate, ' ')[1],month.name)
        da <- as.numeric(substr(tmpdate[2],1,nchar(tmpdate[2])-2))
        ye <- paste0('20',tmpdate[3])
        s[i] <- paste(ye,mo,da,sep = "/")
      }
    }
    s <- as.Date(s,"%Y/%m/%d")
    return(s)
  }
  dat$date <- sputdate(dat$date)
  trim.trailing <- function (x) sub("\\s+$", "", x)
  dat$user <- trim.trailing(dat$user)
  dat$userlinks <- substr(user_links,7,nchar(user_links))
  dat$albumlink <- link
  dat <- data.frame(release.year,dat)
  if(getuser_metrics) dat <- data.frame(dat,user_stats)
  dat <- dat[order(dat$date,decreasing = TRUE),]
  return(dat)
}

scrape_review <- function(rev_link, session)
{
  rev <- session %>% jump_to(rev_link)
  text <- rev %>% html_node('#leftColumn') %>% html_text()
  album <- rev %>% html_node('span') %>% html_text()
  badbands <- "/bands//29826/"
  if(map_lgl(badbands, ~grepl(.x,findSputURL_band(rev_link))) %>% any()){
    band <- NA_character_
  } else {
    band <- findSputURL_band(rev_link) %>% htmlParse() %>% 
      scrape_band()
  }
  score <- rev %>% html_nodes('span') %>% html_text() %>% `[`(2)
  rev_date <- rev %>% html_nodes("span") %>% 
    html_text() %>% `[`(4) %>% 
    strsplit("[|]") %>% unlist()
  replies <- rev_date[2] %>% 
    strsplit(' ') %>% unlist() %>% `[`(3) %>%
    as.numeric()
  views <- rev_date[3] %>% 
    strsplit(' ') %>% unlist() %>% `[`(2) %>% 
    gsub(pattern = ",",replacement='') %>%
    as.numeric()
  rev_date <- gsub(',','',rev_date[1]) %>% strsplit(' ') %>% unlist()
  # following from https://stackoverflow.com/questions/14543627/extracting-numbers-from-vectors-of-strings
  rev_date[2] <- gsub("([0-9]+).*$", "\\1", rev_date[2]) 
  rev_date <- paste0(rev_date, collapse = ' ') %>% as.Date("%B %d %Y")
  release_date <- rev %>% html_nodes("p") %>% 
    html_text() %>% `[`(1) %>%
    strsplit("Release Date") %>% 
    unlist() %>% `[`(2) %>% 
    strsplit(' ') %>% unlist() %>% `[`(2)
  ratings <- findSputURL_soundoffpage(rev_link) %>% 
    scrape_soundoff(getuser_metrics = FALSE)
  
  x <- tibble(band, album, 
              release_date, rev_date, rev_link, 
              score, replies, views,
              text, ratings = as.list(ratings)) %>% 
    nest(ratings) %>% rename(ratings = data)
  writeLines(paste0(rev_link," COMPLETED"))
  return(x)
}

sput <- "http://www.sputnikmusic.com"
if(!dir.exists('./data/')) dir.create('./data/') # make folder for data
users <- c("acadhdemy")
do_staff <- TRUE # set to true if you want scrape all staff
if(do_staff)
{
  staff_links <- paste0(sput, "/", "staff.php")
  staff_links <- getHTMLLinks(staff_links)
  staff_links <- paste0(sput,
                        grep("/user/", staff_links, value = TRUE)
  )
  staff_links <- c(staff_links,paste0(sput,"/user/",users))
} else {
  staff_links <- paste0(sput,"/user/",users)
}
staff_meta <- tibble(users = strsplit_last(staff_links), staff_links = staff_links)
all_revs <- list()
session <- html_session("http://www.sputnikmusic.com/login.php") 
# from: https://stackoverflow.com/questions/35108711/r-using-rvest-to-scrape-a-password-protected-website-without-logging-in-at-eac
form <- html_form(session)[[2]]
filled <- form %>% set_values(`uname` = as.character(readline("Username?")), 
                              `passwd` = as.character(readline("Password?")) )
session <- submit_form(session,filled)
for(i in seq_along(staff_links))
{
  tmp_staff <- staff_links[i]
  link <- paste0(sput, find_rev_tab(tmp_staff))
  tmp_reviews <- paste0(sput, unique(find_reviews(link)))
  if(tmp_reviews[1] == sput) next
  revs <- bind_rows(map(tmp_reviews, scrape_review, session = session))
  revs <- revs %>% 
    mutate(user = staff_meta$users[i]) %>% as_tibble() %>%
    '['(, c(11,1:10))
  saveRDS(revs, paste0('./data/',staff_meta$users[i],'.rds'))
  writeLines(paste0("Completed ", staff_meta$users[i]))
  # all_revs[[staff_meta$users[i]]] <- revs
}
