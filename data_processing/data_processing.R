


##############       Web scrapped Data is processed through some data cleaning/filtering   ################
## Manual modification of Players' name, Birth place, correction of State of Birth is done and 
## The lattitude longitude of the place_of_birth is obtained using Google API
## Finally Processed data is used in the App


# Loading packages --------------------------------------------------------

library(data.table)     # For easy and fast data wrangling
library(ggmap)          # To fetch lat-long info from place_of_birth
register_google(key = "***********************",write = TRUE)



# Load the previously saved web scrapped datasets ------------------------

stat_table_test <- readRDS("./web_scrapped_data/stat_table_test.rds")
stat_table_odi  <- readRDS("./web_scrapped_data/stat_table_odi.rds") 
stat_table_t20  <- readRDS("./web_scrapped_data/stat_table_t20.rds") 


biodata_test <- readRDS("./web_scrapped_data/biodata_test.rds")
biodata_odi  <- readRDS("./web_scrapped_data/biodata_odi.rds")
biodata_t20  <- readRDS("./web_scrapped_data/biodata_t20.rds")



# Data Processing : Test Cricketers ---------------------------------------

## remove whitespaces from all entries
biodata_test[,c(names(biodata_test)):= lapply(.SD,function(x) trimws(x)),
             .SDcols = c(names(biodata_test))]

## processing the some columns to have desired form
biodata_test[,`:=`(International_Debut = as.numeric(International_Debut),
                   place_of_birth      = gsub(",India","",place_of_birth),
                   DOB                 = substring(Date_of_Birth,1,12))]


## Following modifications are apllied due to incomplete/old/wrong/missing info in the dataset
biodata_test[Full_Name == "Rabindra Ramanarayan Singh", place_of_birth :="Princes Town,Trinidad and Tobago"]

biodata_test[,place_of_birth := 
               lapply(place_of_birth,
                      function(x) ifelse(x %in% c("Ahmedabad","Porbandar","Saurashtra","Navagam-Khed,Saurashtra"),
                                         paste0(x,",Gujarat"),
                                  ifelse(x %in% c("Bangalore","Mysore",'Puttur,Mysore','Bhadravati,Mysore','Mangalore,Mysore'),
                                         paste0(x,",Karnataka"),
                                  ifelse(x %in% c("Madras (now Chennai)","Chennai","Madras","Morappakam","Mamandur,Madras"),
                                         paste0(x,",Tamil Nadu"),
                                  ifelse(x %in% c("Shaktigarh","Calcutta (now Kolkata),West Bengal"),
                                         paste0(unlist(strsplit(x," "))[1],",Bengal"),
                                  ifelse(x %in% c("Ujjain,MP"),
                                         paste0(unlist(strsplit(x,","))[1],",Madhya Pradesh"),
                                  ifelse(x %in% c("Jamshedpur,Bihar","Ranchi,Bihar","Bokaro,Bihar","Ranchi,Bihar (Now Jharkhand)"),
                                         paste0(unlist(strsplit(x,","))[1],",Jharkhand"),
                                  ifelse(x %in% "Pataudi,Punjab",
                                         paste0(unlist(strsplit(x,","))[1],",Haryana"),
                                  ifelse(x %in% "Chandigarh,Punjab",
                                         unlist(strsplit(x,","))[1],
                                  ifelse(x %in% c("Karachi,Sind","Sind (now in Pakistan)","Punjab (Now in Pakistan)",
                                                  "Lahore,Punjab (Now in Pakistan)","Karachi,Sind (now in Pakistan)"),
                                         paste0(unlist(strsplit(x,","))[1],",Pakistan"),
                                  ifelse(x %in% c("Sind (now in Pakistan)","Punjab (Now in Pakistan)"),
                                         paste0(unlist(strsplit(x," "))[1],",Pakistan"),
                                  ifelse(x %in% "Comilla,West Bengal",
                                         paste0(unlist(strsplit(x,","))[1],",Bangladesh"),
                                         x))))))))))))]
                                                         

biodata_test[,place_of_birth := as.character(place_of_birth)]


## Split Place of birth to create State of birth column
biodata_test$state_of_birth <-  as.character(lapply(biodata_test$place_of_birth,
                                                    function(x) tail(unlist(strsplit(x,split = "," )),1)))
## Rename changed name of states
biodata_test[,state_of_birth := ifelse(state_of_birth %in% c("Orrisa","Orissa"),"Odisha",state_of_birth)]

##  fetch lat long of places using api key
#lat_long_data_test <- setDT(geocode(biodata_test$place_of_birth))
#saveRDS(lat_long_data_test,"lat_long_data_test_data.rds")
biodata_test_new <- cbind(biodata_test,lat_long_data_test)

## add count column N to denote number of players having same birth_of_place 
biodata_test_new[,N:=.N,by=.(place_of_birth)]

## deviate lat-long values of same place_of_birth a bit to place unique markers in the map 
set.seed(160320)
biodata_test_new[,":="(lat1 =ifelse(N>1 & place_of_birth %in% "Bombay (now Mumbai),Maharashtra",
                                    lat+runif(N,0.028827,0.222669),
                                    ifelse(N>1 & place_of_birth %in% c("Madras (now Chennai),Tamil Nadu","Madras (now Chennai)"),
                                           lat+runif(N,-0.1,-0.05),
                                           ifelse(N>1 & place_of_birth %in% "Bhavnagar,Gujarat",
                                                  lat+runif(N,-0.03,0.03),
                                                  ifelse(N>1 & !place_of_birth %in% c("Bombay (now Mumbai),Maharashtra","Madras (now Chennai),Tamil Nadu", "Madras (now Chennai)","Bhavnagar,Gujarat"),
                                                         lat+runif(N,-0.17309,0.17309),lat)))),
                       
                       lon1 =ifelse(N>1 & place_of_birth %in% "Bombay (now Mumbai),Maharashtra",
                                    lon+runif(N,-0.04041,0.04041),
                                    ifelse(N>1 & place_of_birth %in% c("Madras (now Chennai),Tamil Nadu", "Madras (now Chennai)"),
                                           lon+runif(N,-0.1,-0.03),
                                           ifelse(N>1 & place_of_birth %in% "Bhavnagar,Gujarat",
                                                  lon+runif(N,-0.05,0.023),
                                                  ifelse(N>1 & !place_of_birth %in% c("Bombay (now Mumbai),Maharashtra","Madras (now Chennai),Tamil Nadu","Madras (now Chennai)","Bhavnagar,Gujarat"),
                                                         lon+runif(N,-0.21325,0.21325),lon)))))]



saveRDS(biodata_test_new,"biodata_test_new.rds")



# Data Processing : ODI Cricketers ----------------------------------------

## remove whitespaces from all entries
biodata_odi[,c(names(biodata_odi)):= lapply(.SD,function(x) trimws(x)),
             .SDcols = c(names(biodata_odi))]

## processing the some columns to have desired form
biodata_odi[,`:=`(International_Debut = as.numeric(International_Debut),
                  place_of_birth      = gsub(",India","",place_of_birth),
                  DOB                 = substring(Date_of_Birth,1,12))]


## Following modifications are apllied due to incomplete/old/wrong/missing info in the dataset
biodata_odi[Full_Name == "Rabindra Ramanarayan Singh", place_of_birth :="Princes Town,Trinidad and Tobago"]

biodata_odi[,place_of_birth := 
               lapply(place_of_birth,
                      function(x) ifelse(x %in% c("Ahmedabad","Porbandar","Saurashtra","Navagam-Khed,Saurashtra"),
                                         paste0(x,",Gujarat"),
                                  ifelse(x %in% c("Bangalore","Mysore",'Bhadravati,Mysore','Mangalore,Mysore'),
                                         paste0(x,",Karnataka"),
                                  ifelse(x %in% c("Madras (now Chennai)","Chennai"),
                                         paste0(x,",Tamil Nadu"),
                                  ifelse(x %in% c("Secunderabad,Hyderabad","Hyderabad"),
                                         paste0(x,",Andhra Pradesh"),
                                  ifelse(x %in% c("Shaktigarh","Calcutta (now Kolkata),West Bengal","Medinipur,Calcutta (now Kolkata),Benga"),
                                         paste0(unlist(strsplit(x," "))[1],",Bengal"),
                                  ifelse(x %in% c("Jamshedpur,Bihar","Ranchi,Bihar","Bokaro,Bihar","Ranchi,Bihar (Now Jharkhand)"),
                                         paste0(unlist(strsplit(x,","))[1],",Jharkhand"),
                                  ifelse(x %in% "Mumbai",
                                         paste0(x,",Maharashtra"),
                                  ifelse(x %in% "Ujjain,MP",
                                         paste0(unlist(strsplit(x,","))[1],",Madhya Pradesh"),
                                  ifelse(x %in% "Kowdiar,Trivandrum",
                                         paste0(x,",Kerala"),
                                  ifelse(x %in% "Nainital,Uttaranchal",
                                         paste0(unlist(strsplit(x,","))[1],",Uttarakhand"),
                                  ifelse(x %in% "Jalandhar",
                                         paste0(x,",Punjab"),
                                  ifelse(x %in% c("Sirsa","Rohtak"),
                                         paste0(unlist(strsplit(x,","))[1],",Haryana"),
                                  ifelse(x %in% "Chandigarh,Punjab",
                                         unlist(strsplit(x,","))[1],
                                         x))))))))))))))]


biodata_odi[,place_of_birth := as.character(place_of_birth)]


## Split Place of birth to create State of birth column
biodata_odi$state_of_birth <-  as.character(lapply(biodata_odi$place_of_birth,
                                                    function(x) tail(unlist(strsplit(x,split = "," )),1)))
## Rename changed name of states
biodata_odi[,state_of_birth := ifelse(state_of_birth %in% c("Orrisa","Orissa"),"Odisha",state_of_birth)]


##  fetch lat long of places using api key
#lat_long_data_odi <- setDT(geocode(biodata_odi$place_of_birth))
#saveRDS(lat_long_data_odi,"lat_long_data_odi.rds")
biodata_odi_new <- cbind(biodata_odi,lat_long_data_odi)

## add count column N to denote number of players having same birth_of_place(same lat-long values)
biodata_odi_new[,N:=.N,by=.(place_of_birth)]

## deviate lat-long values of same place_of_birth a bit to place unique markers in the map
biodata_odi_new[,":="(lat1 =ifelse(N>1 & place_of_birth %in% "Bombay (now Mumbai),Maharashtra",
                                   lat+runif(N,0.028827,0.222669),
                                   ifelse(N>1 & place_of_birth %in% c("Madras (now Chennai),Tamil Nadu",
                                                                      "Madras (now Chennai)"),
                                          lat+runif(N,-0.1,-0.05),
                                          ifelse(N>1 & place_of_birth %in% "Bhavnagar,Gujarat",
                                                 lat+runif(N,-0.03,0.03),
                                                 ifelse(N>1 & !place_of_birth %in% c("Bombay (now Mumbai),Maharashtra",
                                                                                     "Madras (now Chennai),Tamil Nadu",
                                                                                     "Madras (now Chennai)",
                                                                                     "Bhavnagar,Gujarat"),
                                                        lat+runif(N,-0.17309,0.17309),lat)))),
                      
                      lon1 =ifelse(N>1 & place_of_birth %in% "Bombay (now Mumbai),Maharashtra",
                                   lon+runif(N,-0.04041,0.04041),
                                   ifelse(N>1 & place_of_birth %in% c("Madras (now Chennai),Tamil Nadu",
                                                                      "Madras (now Chennai)"),
                                          lon+runif(N,-0.1,-0.03),
                                          ifelse(N>1 & place_of_birth %in% "Bhavnagar,Gujarat",
                                                 lon+runif(N,-0.05,0.023),
                                                 ifelse(N>1 & !place_of_birth %in% c("Bombay (now Mumbai),Maharashtra",
                                                                                     "Madras (now Chennai),Tamil Nadu",
                                                                                     "Madras (now Chennai)",
                                                                                     "Bhavnagar,Gujarat"),
                                                        lon+runif(N,-0.21325,0.21325),lon)))))]
saveRDS(biodata_odi_new,"biodata_odi_new.rds")



# Data Processing : T20 Cricketers ----------------------------------------

## remove whitespaces from all entries
biodata_t20[,c(names(biodata_t20)):= lapply(.SD,function(x) trimws(x)),
            .SDcols = c(names(biodata_t20))]

## processing the some columns to have desired form
biodata_t20[,`:=`(International_Debut = as.numeric(International_Debut),
                  place_of_birth      = gsub(",India","",place_of_birth),
                  DOB                 = substring(Date_of_Birth,1,12))]

## Following modifications are apllied due to incomplete/old/wrong/missing info in the dataset
biodata_t20[,place_of_birth := 
              lapply(place_of_birth,
                     function(x) ifelse(x %in% c("Ahmedabad","Porbandar","Navagam-Khed,Saurashtra"),
                                        paste0(x,",Gujarat"),
                                 ifelse(x %in% 	"Chennai",
                                        paste0(x,",Tamil Nadu"),
                                 ifelse(x %in% c("Medinipur,Calcutta (now Kolkata),Benga"),
                                        paste0(unlist(strsplit(x," "))[1],",Bengal"),
                                 ifelse(x %in% "Ranchi,Bihar (Now Jharkhand)",
                                        paste0(unlist(strsplit(x,","))[1],",Jharkhand"),
                                 ifelse(x %in% "Mumbai",
                                        paste0(x,",Maharashtra"),
                                 ifelse(x %in% "Ujjain,MP",
                                        paste0(unlist(strsplit(x,","))[1],",Madhya Pradesh"),
                                 ifelse(x %in% "Hyderabad",
                                        paste0(x,",Andhra Pradesh"),
                                 ifelse(x %in% "Pulluvila,Vizhinjam,Trivandrum",
                                        paste0(x,",Kerala"),
                                 ifelse(x %in% "Nainital,Uttaranchal",
                                        paste0(unlist(strsplit(x,","))[1],",Uttarakhand"),
                                 ifelse(x %in% "Jalandhar",
                                        paste0(x,",Punjab"),
                                 ifelse(x %in% c("Sirsa","Rohtak"),
                                        paste0(unlist(strsplit(x,","))[1],",Haryana"),
                                 ifelse(x %in% "Chandigarh,Punjab",
                                        unlist(strsplit(x,","))[1],
                                        x)))))))))))))]

biodata_t20[,place_of_birth := as.character(place_of_birth)]

## Split Place of birth to create State of birth column
biodata_t20$state_of_birth <-  as.character(lapply(biodata_t20$place_of_birth,
                                                   function(x) tail(unlist(strsplit(x,split = "," )),1)))

## Rename changed name of states
biodata_t20[,state_of_birth := ifelse(state_of_birth %in% c("Orrisa","Orissa"),"Odisha",state_of_birth)]


##  fetch lat long of places using api key
#lat_long_data_t20 <- setDT(geocode(biodata_t20$place_of_birth))
#saveRDS(lat_long_data_t20,"lat_long_data_t20.rds")
biodata_t20_new <- cbind(biodata_t20,lat_long_data_t20)


## add count column N to denote number of players having same birth_of_place(same lat-long values)
biodata_t20_new[,N:=.N,by=.(place_of_birth)]

## deviate lat-long values of same place_of_birth a bit to place unique markers in the map 
biodata_t20_new[,":="(lat1 =ifelse(N>1 & place_of_birth %in% "Bombay (now Mumbai),Maharashtra",
                                   lat+runif(N,0.028827,0.222669),
                                   ifelse(N>1 & place_of_birth %in% c("Madras (now Chennai),Tamil Nadu",
                                                                      "Madras (now Chennai)"),
                                          lat+runif(N,-0.1,0.1),
                                          ifelse(N>1 & place_of_birth %in% "Bhavnagar,Gujarat",
                                                 lat+runif(N,-0.03,0.03),
                                                 ifelse(N>1 & !place_of_birth %in% c("Bombay (now Mumbai),Maharashtra",
                                                                                     "Madras (now Chennai),Tamil Nadu",
                                                                                     "Madras (now Chennai)",
                                                                                     "Bhavnagar,Gujarat"),
                                                        lat+runif(N,-0.17309,0.17309),lat)))),
                      
                      lon1 =ifelse(N>1 & place_of_birth %in% "Bombay (now Mumbai),Maharashtra",
                                   lon+runif(N,-0.04041,0.04041),
                                   ifelse(N>1 & place_of_birth %in% c("Madras (now Chennai),Tamil Nadu",
                                                                      "Madras (now Chennai)"),
                                          lon+runif(N,-0.1,-0.03),
                                          ifelse(N>1 & place_of_birth %in% "Bhavnagar,Gujarat",
                                                 lon+runif(N,-0.05,0.023),
                                                 ifelse(N>1 & !place_of_birth %in% c("Bombay (now Mumbai),Maharashtra",
                                                                                     "Madras (now Chennai),Tamil Nadu",
                                                                                     "Madras (now Chennai)",
                                                                                     "Bhavnagar,Gujarat"),
                                                        lon+runif(N,-0.21325,0.21325),lon)))))]

saveRDS(biodata_t20_new,"biodata_t20_new.rds")



