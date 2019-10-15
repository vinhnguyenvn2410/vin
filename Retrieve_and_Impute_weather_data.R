library(dplyr)
library(tidyverse)
library(rvest)
library(zoo)
# I used 2 methods of retrieving the data to double check their accuracy
# Both methods yields similar data

#Method 1: shorter and 'easier' with data cleaning
#loading the packages
library(riem)
library(dplyr)
library(imputeTS)
#connect to network and retrieve data
riem_networks()
riem_stations("CA_ASOS")
df1 = riem_measures("LAX", date_start = "2014-01-01")

# Data cleaning with the na_ma from the imputeTS package
# data cleaning using na_ma where i chose k=1 and 'linear' which performs simple
# average of the data 
summary(df1)
df2= df1%>%
  select(-c(metar,skyc1,skyc2,skyc3,skyc4,wxcodes,
            ice_accretion_1hr,ice_accretion_3hr,
            ice_accretion_6hr,peak_wind_time,gust,skyl1,skyl2,skyl3,skyl4,
            p01i,peak_wind_gust,peak_wind_drct))%>%
  mutate_at(.vars = vars(tmpf,dwpf,relh,drct,sknt,alti,mslp,
                         vsby,feel),
          .funs = list(~ na_ma(.,k=1,weighting = 'linear')))

head(df2)
#write cleaned data to txt file
write.table(df2,"LAX_temperature.txt",sep=",",row.names=FALSE)

# Method 2: longer but didn't improve in terms of missing data much 
# so i didn't clean this one
# This script downloads data 
# from Iowa State university as a text file in your local machine.
# https://mesonet.agron.iastate.edu/request/download.phtml?network=CA_ASOS


iem.wd <- "C:/Users/vinhnguyenvn2401/Downloads/Data" #download location
date1 <- ISOdate(2014,1,1) #start date in year, month, day format
date2 <- ISOdate(2019,10,10) #end date in year, month, day format
user.network <- c("ASOS")
user.state <- c("CA") #state
user.faaid <- c("LAX") #site FAA identifier - leave empty and a list will print for your reference
#################

library(jsonlite)
library(RCurl)
library(lubridate)
library(stringr)

#create subdirectories
download.wd <- str_c(iem.wd, user.network, user.faaid, sep="/")
if(user.faaid != "") {dir.create(download.wd, recursive=T)}
setwd(download.wd)

service <- "https://mesonet.agron.iastate.edu/cgi-bin/request/asos.py?"
service <- str_c(service, "data=all&tz=Etc/UTC&format=comma&latlon=yes&", sep="")
service <- str_c(service, "year1=", year(date1), "&month1=", month(date1), "&day1=", mday(date1), "&", sep="")
service <- str_c(service, "year2=", year(date2), "&month2=", month(date2), "&day2=", mday(date2), "&", sep="")

states <- c("AK AL AR AZ CA CO CT DE FL GA ")
states <- str_c(states,"HI IA ID IL IN KS KY LA MA MD ")
states <- str_c(states,"ME MI MN MO MS MT NC ND NE NH ")
states <- str_c(states,"NJ NM NV NY OH OK OR PA RI SC ") 
states <- str_c(states,"SD TN TX UT VA VT WA WI WV WY")

states <- unlist(strsplit(states, " "))

networks <- "AWOS"
for (i in 1:length(states)) {
  networks[i+1] <- str_c(states[i], "_ASOS", sep="")
}

if (user.network == "ASOS"){
  networks <- networks[which(networks %in% str_c(user.state, "_", user.network))]
} else {
  networks <- subset(networks %in% str_c(user.network))
}

for (network in networks){
  #get metadata
  uri <- str_c("https://mesonet.agron.iastate.edu/geojson/network/", network, ".geojson", sep="")
  
  data <- url(uri)
  jdict <- fromJSON(data)
  
  for (i in 1:nrow(jdict$features)){
    site <- jdict$features[i,]
    faaid <- site$properties$sid
    if (faaid == user.faaid) {
      sitename <- site$properties$sname
      uri <- str_c(service, "station=", faaid)
      print(str_c("Network:", network, "Downloading:", sitename, faaid, sep=" "))
      data <- url(uri)
      #print(data) #uncomment to print metadata
      datestring1 <- format(date1, "%Y%m%d")
      datestring2 <- format(date2, "%Y%m%d")
      outfn <- str_c(network, "_", faaid, "_", datestring1, "_to_", datestring2, sep="")
      download.file(uri, str_c(outfn, ".txt"), "auto")
    } 
    if (user.faaid == "" & i == 1) {
      print(data.frame(jdict$features$properties[c("sname", "sid")]))
    }
  }
}

library(data.table)
d = read.table("CA_ASOS_LAX_20140101_to_20191010.txt", 
               sep=",",header = TRUE, 
                # col.names=c('station','valid','lon','lat','tmpf','dwpf','relh','drct','sknt','p01i','alti','mslp'
               #             ,'vsby','gust','skyc1','skyc2','skyc3',
                          # 'skyc4','skyl1','skyl2','skyl3','skyl4','wxcodes','ice_accretion_1hr','ice_accretion_3hr','ice_accretion_6hr','peak_wind_gust','peak_wind_drct','peak_wind_time','feel','metar'), 
               fill=FALSE, 
               strip.white=TRUE)

# Cleaning
#checking the data set. 
summary(d)

# As specified from the website, the missing values will be encoded 'M'
#dropping irrelevant columns
d= d%>%
  select(c(station,valid,tmpf,dwpf))
glimpse(d)

#replace specific encoding with NA
df <- na_if(d, 'M') 

#replace NA with previous row values
df= df%>%
  mutate(tmpf = na.locf(tmpf))%>%
  mutate(dwpf = na.locf(dwpf))

