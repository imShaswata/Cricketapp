######  Dataset of all Indian Cricketers is obained through  Web Scrapping 
######  The website http://india.crictotal.com/profile contains all the info that we are going to use  

# load libraries ----------------------------------------------------------
library(data.table,quietly = T)   # For easy and fast data wrangling
library(tidyverse,quietly = T)    # For easy pipe-lining methods
library(rvest,quietly = T )       # For webpage harvesting/web scrapping 

#setwd("C:/Users/Asus/Desktop/cricket/data_processing")

# simulate html session and navigate to requirred urls -----------------------------------------------
sesn        <- html_session("http://india.crictotal.com/profile/")
url_test    <- sesn %>% jump_to('?type=1')      # url of all test cricketer info
url_odi     <- sesn %>% jump_to('?type=2')      # url of all odi  cricketer info
url_t20     <- sesn %>% jump_to('?type=3')      # url of all t20  cricketer info


# Web Scrap: TEST Cricketers ----------------------------------------------------------------------------
master_page_test         <- url_test %>% read_html %>% html_nodes('.schedule-tbl')        # Master page,consisting of all test players' list    
master_hyplink_test      <- master_page_test %>% html_nodes('td:nth-child(2) a')  %>% 
                                                                 html_attr('href')        # Master link,consisting individual biodata of all test players

master_tab_test  <- as.data.table((master_page_test %>% html_table(fill=T)))              # Master table of all test cricketers

##### bio-data tab ####                                                                                          
bio_test <- lapply(master_hyplink_test,
                   function(x) x %>% read_html %>% html_nodes(".player-txt"))             # extracts out biodata for each test player form master links

bio_test       <- lapply(bio_test,function(x) as.character(x))                            # in text format
bio_test       <- lapply(bio_test,function(x) str_split(x,"<strong>"))                    # split the string

for(i in 1:length(bio_test)){
  bio_test[[i]] <- unlist(bio_test[[i]])                                                  # simplify the list structure
}

bio_filtered_test <- list()

for (i in 1:length(bio_test)) {
  bio_filtered_test[[i]] <- gsub("<[^>]*>|\\t|\\n|-->","",bio_test[[i]])                  # scrap out html notations 
  bio_filtered_test[[i]] <- bio_filtered_test[[i]][-1]
  }

biodata_test <- data.table()                                                              # Indiv player's biodata will be populated in biodata_test
biodata_test <- biodata_test[,`:=`(c("Full Name","Date of Birth","Major Team", 
                                     "Playing Roll","Batting Style","Bowling Style","International Debut"),
                                   as.character(rep(NA,length(bio_filtered_test))))]


## populate biodata_test with indiv biodata info
xx <- c()
for(i in 1:length(bio_filtered_test)){
  for (j in 1:length(bio_filtered_test[[i]])){
    xx <- unlist(strsplit(bio_filtered_test[[i]][j],":"))[1]
    if(xx %in% names(biodata_test)){
      biodata_test[i,xx]=unlist(strsplit(bio_filtered_test[[i]][j],":"))[2]
    }
    
  }
}
names(biodata_test) <- gsub(" ","_",names(biodata_test))


## extract place of birth from Date_of_Birth column
dob <- trimws(biodata_test$Date_of_Birth)
temp <- c()
for(i in 1:length(dob)){
  temp[i] <- unlist(strsplit(dob[i],","))
  }
temp <- lapply(dob,function(x)trimws(unlist(strsplit(x,","))))    # temp contains DOB string in splitted format


place_of_birth <- c()                                             # place_of_birth takes out the strings containing places of birth 
for(i in 1:length(temp)){
  place_of_birth[i]=paste0(temp[[i]][3:length(temp[[i]])],collapse = ",")
}

place_of_birth <- as.data.frame(place_of_birth)
biodata_test[,"place_of_birth" := place_of_birth]
saveRDS(biodata_test,"biodata_test.rds")



##### detailed career statistics tab #######

# extracts out detailed-career-stat of each test player form master link
stat_table_test <- lapply(master_hyplink_test,
                          function(x) x %>% 
                            read_html %>% 
                            html_nodes(".player-tbl") %>% 
                            html_table(fill=T))                   

names(stat_table_test) = master_tab_test$X2
saveRDS(stat_table_test,"stat_table_test.rds")





# Web Scrap: ODI Cricketers ----------------------------------------------------------------------------
master_page_odi         <- url_odi %>% read_html %>% html_nodes('.schedule-tbl')        # Master page,consisting of all odi players' list    
master_hyplink_odi      <- master_page_odi %>% html_nodes('td:nth-child(2) a')  %>% 
  html_attr('href')        # Master link,consisting individual biodata of all odi players

master_tab_odi  <- as.data.table((master_page_odi %>% html_table(fill=T)))              # Master table of all odi cricketers

##### bio-data tab ####                                                                                          
bio_odi <- lapply(master_hyplink_odi,
                   function(x) x %>% read_html %>% html_nodes(".player-txt"))             # extracts out biodata for each odi player form master links

bio_odi       <- lapply(bio_odi,function(x) as.character(x))                            # in text format
bio_odi       <- lapply(bio_odi,function(x) str_split(x,"<strong>"))                    # split the string

for(i in 1:length(bio_odi)){
  bio_odi[[i]] <- unlist(bio_odi[[i]])                                                  # simplify the list structure
}

bio_filtered_odi <- list()

for (i in 1:length(bio_odi)) {
  bio_filtered_odi[[i]] <- gsub("<[^>]*>|\\t|\\n|-->","",bio_odi[[i]])                  # scrap out html notations 
  bio_filtered_odi[[i]] <- bio_filtered_odi[[i]][-1]
}

biodata_odi <- data.table()                                                              # Indiv player's biodata will be populated in biodata_odi
biodata_odi <- biodata_odi[,`:=`(c("Full Name","Date of Birth","Major Team", 
                                     "Playing Roll","Batting Style","Bowling Style","International Debut"),
                                   as.character(rep(NA,length(bio_filtered_odi))))]


## populate biodata_odi with indiv biodata info
xx <- c()
for(i in 1:length(bio_filtered_odi)){
  for (j in 1:length(bio_filtered_odi[[i]])){
    xx <- unlist(strsplit(bio_filtered_odi[[i]][j],":"))[1]
    if(xx %in% names(biodata_odi)){
      biodata_odi[i,xx]=unlist(strsplit(bio_filtered_odi[[i]][j],":"))[2]
    }
    
  }
}
names(biodata_odi) <- gsub(" ","_",names(biodata_odi))


## extract place of birth from Date_of_Birth column
dob <- trimws(biodata_odi$Date_of_Birth)
temp <- c()
for(i in 1:length(dob)){
  temp[i] <- unlist(strsplit(dob[i],","))
}
temp <- lapply(dob,function(x)trimws(unlist(strsplit(x,","))))    # temp contains DOB string in splitted format


place_of_birth <- c()                                             # place_of_birth takes out the strings containing places of birth 
for(i in 1:length(temp)){
  place_of_birth[i]=paste0(temp[[i]][3:length(temp[[i]])],collapse = ",")
}

place_of_birth <- as.data.frame(place_of_birth)
biodata_odi[,"place_of_birth" := place_of_birth]
saveRDS(biodata_odi,"biodata_odi.rds")



##### detailed career statistics tab #######

# extracts out detailed-career-stat of each odi player form master link
stat_table_odi <- lapply(master_hyplink_odi,
                          function(x) x %>% 
                            read_html %>% 
                            html_nodes(".player-tbl") %>% 
                            html_table(fill=T))                   

names(stat_table_odi) = master_tab_odi$X2
saveRDS(stat_table_odi,"stat_table_odi.rds")


# Web Scrap: T20 Cricketers ----------------------------------------------------------------------------
master_page_t20         <- url_t20 %>% read_html %>% html_nodes('.schedule-tbl')        # Master page,consisting of all t20 players' list    
master_hyplink_t20      <- master_page_t20 %>% html_nodes('td:nth-child(2) a')  %>% 
  html_attr('href')        # Master link,consisting individual biodata of all t20 players

master_tab_t20  <- as.data.table((master_page_t20 %>% html_table(fill=T)))              # Master table of all t20 cricketers

##### bio-data tab ####                                                                                          
bio_t20 <- lapply(master_hyplink_t20,
                   function(x) x %>% read_html %>% html_nodes(".player-txt"))           # extracts out biodata for each t20 player form master links

bio_t20       <- lapply(bio_t20,function(x) as.character(x))                            # in text format
bio_t20       <- lapply(bio_t20,function(x) str_split(x,"<strong>"))                    # split the string

for(i in 1:length(bio_t20)){
  bio_t20[[i]] <- unlist(bio_t20[[i]])                                                  # simplify the list structure
}

bio_filtered_t20 <- list()

for (i in 1:length(bio_t20)) {
  bio_filtered_t20[[i]] <- gsub("<[^>]*>|\\t|\\n|-->","",bio_t20[[i]])                  # scrap out html notations 
  bio_filtered_t20[[i]] <- bio_filtered_t20[[i]][-1]
}

biodata_t20 <- data.table()                                                              # Indiv player's biodata will be populated in biodata_t20
biodata_t20 <- biodata_t20[,`:=`(c("Full Name","Date of Birth","Major Team", 
                                     "Playing Roll","Batting Style","Bowling Style","International Debut"),
                                   as.character(rep(NA,length(bio_filtered_t20))))]


## populate biodata_t20 with indiv biodata info
xx <- c()
for(i in 1:length(bio_filtered_t20)){
  for (j in 1:length(bio_filtered_t20[[i]])){
    xx <- unlist(strsplit(bio_filtered_t20[[i]][j],":"))[1]
    if(xx %in% names(biodata_t20)){
      biodata_t20[i,xx]=unlist(strsplit(bio_filtered_t20[[i]][j],":"))[2]
    }
    
  }
}
names(biodata_t20) <- gsub(" ","_",names(biodata_t20))


## extract place of birth from Date_of_Birth column
dob <- trimws(biodata_t20$Date_of_Birth)
temp <- c()
for(i in 1:length(dob)){
  temp[i] <- unlist(strsplit(dob[i],","))
}
temp <- lapply(dob,function(x)trimws(unlist(strsplit(x,","))))    # temp contains DOB string in splitted format


place_of_birth <- c()                                             # place_of_birth takes out the strings containing places of birth 
for(i in 1:length(temp)){
  place_of_birth[i]=paste0(temp[[i]][3:length(temp[[i]])],collapse = ",")
}

place_of_birth <- as.data.frame(place_of_birth)
biodata_t20[,"place_of_birth" := place_of_birth]
saveRDS(biodata_t20,"biodata_t20.rds")



##### detailed career statistics tab #######

# extracts out detailed-career-stat of each t20 player form master link
stat_table_t20 <- lapply(master_hyplink_t20,
                          function(x) x %>% 
                            read_html %>% 
                            html_nodes(".player-tbl") %>% 
                            html_table(fill=T))                   

names(stat_table_t20) = master_tab_t20$X2
saveRDS(stat_table_t20,"stat_table_t20.rds")
