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

                             
#group per month (file names need to change with above extra line for NA)

per_month_temp_diff <- temp_difference_minus_NA %>% group_by(Month) %>% 
  arrange(temp_difference)




