#load tidyverse

library(tidyverse)

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

