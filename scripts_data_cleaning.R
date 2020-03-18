#load tidyverse

library(tidyverse)

# Question 1:For each station, how many days have a minimum temperature,
# a maximum temperature and a rainfall measurement recorded?

#open file

bom_data <- read_csv("raw_data/BOM_data.csv")
bom_stations <- read_csv("raw_data/BOM_stations.csv")

#view files
view(bom_data)
view(bom_stations)

#separate min and max temps

temp_min_max<- separate(bom_data, Temp_min_max, into = c("min", "max"), sep = "/")

#filter out all rows that have min, max and rainfall data

cutdown_min_max_rainfall <- filter(temp_min_max, min !="-", max!="-", Rainfall !="-")

#group per station

perstation_cutdown_min_max_rainfall <- group_by(cutdown_min_max_rainfall, Station_number)

#summarise per station number that have min, max and rainfall data
#Finish Question 1:

Bom_data_number <- summarise(perstation_cutdown_min_max_rainfall,
                             days_recorded_min_max_rainfall=n())


#__________________________________________________  


# Question 2: Which month saw the lowest average daily temperature difference?

#view files
view(bom_data)

#separate min and max temps

temp_min_max<- separate(bom_data, Temp_min_max, into = c("min", "max"), sep = "/")

#from Bill to get numeric
temp_min_max %>% 
  mutate(min = as.numeric(min)) %>% 
  mutate(max = as.numeric(max)) %>% 
  mutate(Rainfall = as.numeric(Rainfall))-> BOM_Data

#substract max and min into new column
temp_difference <- mutate(BOM_Data, temp_difference = max - min)


#remove all NA, need to get all NA out of the temp_difference

temp_difference_minus_NA <- temp_difference %>% filter(temp_difference != "NA")

                             
#group per month average

Average_difference <- temp_difference_minus_NA %>% group_by(Month) %>% 
  summarise(mean_difference = mean(temp_difference))


#rearrange in order    
 
arrange(Average_difference,mean_difference)   

# Answer is month 6

#____________________________________________________________________________

#Q3: Which state saw the lowest average daily temperature difference?

# open second file

# open file

bom_stations <- read_csv("raw_data/BOM_stations.csv")

#view files

view(bom_stations)

#make long file by gather

bom_station_long <- gather(bom_stations, key = "station_id", value = "value", 2:21)

#spread

bom_stations_new <- spread(bom_station_long, key = info, value = value)

#combine with file from yesterday

bom_data <- read_csv("raw_data/BOM_data.csv")

view(bom_data)

bom_stations_new_2 <- rename(bom_stations_new, Station_number = station_id)

#make sure variables are same

bom_stations_3 <- bom_stations_new_2 %>% 
  mutate(Station_number = as.numeric(Station_number)) 
  
temp_difference_minus_NA %>% 
  mutate(Station_number = as.numeric(Station_number)) 

#combine
combined_bom <- full_join(temp_difference_minus_NA , bom_stations_3)

#group per state
#group per month average

Average_difference <- combined_bom %>% group_by(Month) %>% group_by(state) %>% 
  summarise(mean_difference = mean(temp_difference))


#rearrange in order    

arrange(Average_difference,mean_difference)   


#ANSWER Q3: QLD
#________________________________________________________________________________

#Q4: Does the westmost (lowest longitude) or eastmost (highest longitude)
#weather station in our dataset have a higher average solar exposure?

#columns needing same numeric values
combined_bom_num <- combined_bom %>% 
  mutate(Solar_exposure = as.numeric(Solar_exposure))


#combined gives NA, need taking out of the file
combined_bom_minus_NA <- combined_bom_num %>% filter(Solar_exposure != "NA")

#group by longitude

combined_bom_by_lat <- combined_bom_minus_NA %>%  group_by(lon)

#summarise

av_sol_exp <-  summarise(combined_bom_by_lat, mean_solar_exp = mean(Solar_exposure))


#ANSWER Eastmost (153.4661, mean solar exp is 19.5 vs 19.1 for westmost)



 





