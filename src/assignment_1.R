library(tidyverse)
library(tidyr)
library(ggthemes)
library(lubridate)

# Now that we have learned how to munge (manipulate) data
# and plot it, we will work on using these skills in new ways

#matt was here
####-----Reading in Data and Stacking it ----- ####
#Reading in files
files <- list.files('data',full.names=T)


#Read in individual data files
ndmi <- read_csv(files[1]) %>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndmi')


ndsi <- read_csv(files[2]) %>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndsi')

ndvi <- read_csv(files[3])%>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndvi')

# Stack as a tidy dataset
full_long <- rbind(ndvi,ndmi,ndsi) %>%
  gather(key='site',value='value',-DateTime,-data) %>%
  filter(!is.na(value))

##### Question 1 #####
#1 What is the correlation between NDVI and NDMI? - here I want you to
#convert the full_long dataset in to a wide dataset using the 
#function "spread" and then make a plot that shows the correlation as a
# function of if the site was burned or not

## Your code here
?spread
full_wide <- spread(full_long, key = "data", value = "value")%>%
  filter_if(is.numeric,all_vars(!is.na(.)))%>%
  mutate(month = month(DateTime),
         year = year(DateTime))

?filter
head(full_wide)
summer_only <- filter(full_wide, month %in% c(6,7,8,9))

ggplot(summer_only, aes(x=ndmi, y=ndvi, color = site))+
  geom_point()+
  theme_few()

## End Code for Question 1 -----------


#### Question 2 ####
#2) What is the correlation between average NDSI (normalized 
# snow index) for January - April and average NDVI for June-August?
#In other words, does the previous year's snow cover influence vegetation
# growth for the following summer? 


## Your code here
WinterNDSI <- filter(full_wide, month %in% c(1,2,3,4)) %>%
  group_by(year, site) %>%
  summarize(meanNDSI = mean(ndsi))
head(WinterNDSI)

SummerNDVI <- filter(full_wide, month %in% c(6,7,8)) %>%
  group_by(year, site) %>%
  summarize(meanNDVI = mean(ndvi))
head(SummerNDVI)

#Merge Summer and Winter data
NDSI_NDVI <- merge(x= WinterNDSI, y= SummerNDVI)

#Plot the annual meanNDVI and meanNDSI for burned and unburned sites

ggplot(NDSI_NDVI, aes(x= year, y=meanNDVI, color = site))+
  geom_line()+
  geom_line(aes(y=meanNDSI))+
  ylab("Index Value")+
  theme_bw()+
  facet_wrap(~site)

#try to put into a facet wrap
#?facet_wrap

## End code for question 2 -----------------


###### Question 3 ####
#How is the snow effect from question 2 different between pre- and post-burn
# and burned and unburned? 

#Add data column "condition" based on year (2004) display string "post" or "pre", not really implemented in code
q3 <- mutate(NDSI_NDVI, condition = ifelse(year >= 2004, "post", "pre"))

ggplot(q3, aes (x=meanNDSI, y=meanNDVI, color = condition))+
  geom_line()+
  theme_bw()
  

ggplot(q3, aes(x=meanNDSI, y=meanNDVI, color = site))+
  geom_line()+
  theme_bw()
  #facet_wrap(~site)


## End code for question 3

###### Question 4 #####
#What month is the greenest month on average? Does this change in the burned
# plots after the fire? 

# Get monthly average NDVI over whole time period
monthNDVI <- group_by(full_wide, month) %>%
  summarize(meanmoNDVI = mean(ndvi))

#Plot monthly average NDVI including burned and unburned sites and whole timestep before
ggplot(monthNDVI, aes(x = month, y = meanmoNDVI, label=month))+
  geom_point()+
  theme_bw()+
  geom_text(hjust=-1,vjust=1)
  
#Select data after the fire, group by month and site, and summarize average NDVI at site and month
monthNDVIburned <- filter(full_wide, year>2004)%>%
  group_by(month, site) %>%
  summarize(mean_mo_site = mean(ndvi))

#Plot burned and unburned monthly average NDVI after the fire
ggplot(monthNDVIburned, aes(x = month, y = mean_mo_site, color = site, label = month))+
  geom_point()+
  geom_text(hjust=-1,vjust=1)+
  theme_bw()+
  ylab("AVG NDVI")
#Before the fire September was the "greenest" and after the fire august became the average greenest month
#Need to learn how to label a max/min data point


## End code for question 4

##### Question 5 ####
#What month is the snowiest on average?

#Group full_wide data by month and summarize average monthly NDSI
full_long_NDSI <- group_by(full_wide, month)%>%
  summarize(mean_mo_NDSI  = mean(ndsi))

#Plot average monthly NDSI over the entire time step with months labeled on figure
ggplot(full_long_NDSI, aes(x=month, y=mean_mo_NDSI, label = month))+
  geom_point()+
  geom_text(hjust=-1,vjust=1)+
  ylab("AVG NDSI")
#Snowiest month = January closely followed by February - still learning how to only label max's and/or min's

#Thanks Dr. Ross, REALLY enjoying this class despite getting clobbered, but I'm beginning to see a photon
#at the end of a long tunnel. Have a great weekend!
## End code for question 5
