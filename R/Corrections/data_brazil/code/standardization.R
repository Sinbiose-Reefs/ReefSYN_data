# 1. Packages ----

library(tidyverse)

# 2. Standardize data ----

## 2.1 Dataset XII (https://ipt.iobis.org/wsaobis/resource?r=reefsyn_abrolhos_benthos) ----

data_event <- read.delim("./data/XII/event.txt")

data_occurence <- read.delim("./data/XII/occurrence.txt")

data_measurement <- read.delim("./data/XII/measurementorfacts.txt")


data_all <- left_join(data_measurement, data_event) %>% 
  left_join(., data_occurence) %>% # Problem on the join with many-to-many relationship
  group_by(eventDate, year, eventID, decimalLatitude, decimalLongitude,
           locality, minimumDepthInMeters, maximumDepthInMeters) %>% 
  summarise(total = sum(measurementValue)) %>% 
  ungroup()

hist(data_all$total) # Problem: the sum is not equal to 100 for most sampling units

## 2.2 Dataset XIII (https://ipt.iobis.org/wsaobis/resource?r=reefsyn_abrolhos_benthos_spatial) ----

#data_event <- read.delim("./data/XIV/event.txt")
data_event <- read.csv("./data/XIII/event_core.csv")

#data_occurence <- read.delim("./data/XIV/occurrence.txt")
data_occurence <- read.csv("./data/XIII/DF_occ.csv")

#data_measurement <- read.delim("./data/XIV/measurementorfacts.txt")
data_measurement <- read.csv("./data/XIII/DF_eMOF.csv")


# match
data_all <- left_join(data_measurement, data_event) %>% 
  left_join(., data_occurence) %>% # Problem on the join with many-to-many relationship
  # I don't know what's the left join is doing to produce these NAs
  group_by(eventID, locality, scientificNameAccepted) %>%
  
  # I would use mean here because we have subunits within the videoplots which we could not differentiate
  # As far I could see this is the explanation for covers above 1.
  # 
  summarize(total = sum(measurementValue),
            eventDate = mean(as.Date(eventDate),na.rm=T), 
            year = mean(year,na.rm=T), 
            decimalLatitude = mean(decimalLatitude,na.rm=T), 
            decimalLongitude = mean(decimalLongitude,na.rm=T),
            minimumDepthInMeters = mean(minimumDepthInMeters,na.rm=T), 
            maximumDepthInMeters = mean(minimumDepthInMeters,na.rm=T))

# ALL: now this seems ok, rigth?
hist(data_all$total) # Problem: the sum is not equal to 100 for most sampling units


## 2.3 Dataset XIV (https://ipt.iobis.org/wsaobis/resource?r=sisbiotamar_bentos) ----

#data_event <- read.delim("./data/XIV/event.txt")
data_event <- read.csv("./data/XIV/event_core.csv")

#data_occurence <- read.delim("./data/XIV/occurrence.txt")
data_occurence <- read.csv("./data/XIV/DF_occ.csv")

#data_measurement <- read.delim("./data/XIV/measurementorfacts.txt")
data_measurement <- read.csv("./data/XIV/DF_eMOF.csv")

data_all <- left_join(data_measurement, data_event) %>% 
  left_join(., data_occurence) %>% # Problem on the join with many-to-many relationship
  
  # I don't know what's the left join is doing to produce these NAs
  group_by(eventID, locality, scientificNameAccepted) %>%
  
  # I would use mean here because we have subunits within the videoplots which we could not differentiate
  # As far I could see this is the explanation for covers above 1.
  # 
  summarize(total = mean(measurementValue), # using the average across sub units
            eventDate = mean(as.Date(eventDate),na.rm=T), 
            year = mean(year,na.rm=T), 
            decimalLatitude = mean(decimalLatitude,na.rm=T), 
            decimalLongitude = mean(decimalLongitude,na.rm=T),
            minimumDepthinMeters = mean(minimumDepthinMeters,na.rm=T), 
            maximumDepthInMeters = mean(minimumDepthinMeters,na.rm=T))

# ALL: now this seems ok, rigth?
hist(data_all$total) # Problem: the sum is not equal to 100 for most sampling units



## 2.4 Dataset XV (https://ipt.iobis.org/wsaobis/resource?r=peld-iloc_benthic-community) ----

data_event <- read.delim("./data/XV/event.txt")

data_occurence <- read.delim("./data/XV/occurrence.txt")

data_measurement <- read.delim("./data/XV/measurementorfact.txt")

data_all <- left_join(data_occurence, data_event) %>% 
  left_join(., data_measurement) %>% # Problem on the join with many-to-many relationship
  group_by(eventDate, eventID, decimalLatitude, decimalLongitude,
           locality) %>% 
  summarise(total = sum(measurementValue)) %>% 
  ungroup()

hist(data_all$total) # Problem: the sum is not equal to 100 for most sampling units

## 2.5 Dataset XVI (https://ipt.iobis.org/wsaobis/resource?r=reefsyn_benthosrn) ----

data_event <- read.delim("./data/XVI/event.txt")

data_occurence <- read.delim("./data/XVI/occurrence.txt")

data_measurement <- read.delim("./data/XVI/measurementorfacts.txt")

data_all <- left_join(data_occurence, data_event) %>% 
  left_join(., data_measurement) %>% # Problem on the join with many-to-many relationship
  group_by(eventDate, eventID, decimalLatitude, decimalLongitude,
           locality) %>% 
  summarise(total = sum(measurementValue)) %>% 
  ungroup()

hist(data_all$total) # Problem: the sum is not equal to 100 for most sampling units
