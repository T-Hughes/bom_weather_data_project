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


   


# Question 2: Which month saw the lowest average daily temperature difference?



