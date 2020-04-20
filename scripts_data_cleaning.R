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

#would like to have table with just min and max longitude, instead of 19 rows
#use filter and min OR max

Q4 <- filter(av_sol_exp, lon == min (lon) | lon == max(lon))

#______________________________________________________________
# added ggplot challenge
#Q1: For the Perth Station (ID9225), produce 3 scatter plots showing the relationship
# between the max temp and each other measurement recorded (min temp, rainfall and
# solar exp)

perth <- filter(combined_bom_minus_NA, Station_number==9225)

perth %>% 
  mutate(min = as.numeric(min)) %>% 
  mutate(max = as.numeric(max)) %>% 
  mutate(Rainfall = as.numeric(Rainfall)) %>% 
  mutate(Solar_exposure = as.numeric(Solar_exposure)) -> perth2

perth2


graph1 <- perth2 %>% 
  ggplot(mapping = aes(x = max, y = min)) +
  geom_point(alpha = 0.2)+
  labs(title = "Max and min temperatures at Perth Station ID9225", 
       x = "max", 
       y = "min",
       subtitle = "source: BOM meterological data")

graph2 <- perth2 %>% 
  ggplot(mapping = aes(x = max, y = Rainfall)) +
  geom_point(alpha = 0.2)+
  labs(title = "Max and Rainfall at Perth Station ID9225", 
       x = "max", 
       y = "Rainfall",
       subtitle = "source: BOM meterological data")

graph3 <- perth2 %>% 
  ggplot(mapping = aes(x = max, y = Solar_exposure)) +
  geom_point(alpha = 0.2)+
  labs(title = "Max and solar exposure at Perth Station ID9225", 
       x = "max", 
       y = "Solar Exposure",
       subtitle = "source: BOM meterological data")

#Q2:Display these 4 measurements for the Perth station in a single 
# scatterplot by using additional aestetics mappings

graph4 <- perth2 %>% 
  ggplot(mapping = aes(x = max, y = min, size = Rainfall, colour = Solar_exposure)) +
  geom_point(alpha = 0.2)+
  labs(title = "Max and min temperatures, rainfall and solar
       exposure at Perth station ID9225", 
       x = "max", 
       y = "min",
       subtitle = "source: BOM meterological data")

# above graph4 worked, but I find it hard to read, so play around with it

perth2 %>% 
  ggplot(mapping = aes(x = max, y = min, size = Rainfall, colour = Solar_exposure)) +
  geom_point(alpha = 0.2)  +
  scale_color_gradient(low="blue", high="red")
#only slightly better, discuss with Aswin


#Q3:Take the four plots you have produced in Q1 and Q2 and save them as a multipanel figure

library(cowplot)

four_plots <- plot_grid(graph1, graph2, graph3, graph4)
# worked ok, but in a real plot you would not have titles on the individual
# plots first, so ideally would have worked without titles, then add one title 
# to the four plots together.
four_plots_resized <- plot_grid(graph1, graph2, graph3, graph4, rel_heights = c(1,2), 
          rel_widths = c(1,2))



ggsave(filename = "analysed_data/perth.png" , 
       plot = four_plots_resized,
       width = 12, height = 10, dpi = 300, units = "cm")
# still does not work for last graph for titles
ggsave(filename = "analysed_data/perth2.png" , 
       plot = four_plots_resized,
       width = 20, height = 15, dpi = 300, units = "cm")

#Q4:Using the entire BOM dataset calculate the average monthly rainfall for each station
#Produce a lineplot to visualise this data and the state each station is in.
rainfall_per_station_per_month <- combined_bom %>% 
  filter(Rainfall != "NA") %>% 
  select(state, Station_number, Month, Rainfall) %>% 
  group_by(state, Station_number, Month) %>% 
  summarise(mean_rainfall = mean(Rainfall)) %>% 
  arrange(Month) %>% 
  mutate(Month = as.factor(Month))


  
av_rainfall <- rainfall_per_station_per_month %>% ggplot(aes(x = Month, y = mean_rainfall, 
  colour = state , group = Station_number))+
  geom_line()+
  labs(title = "Average Monthly rainfall", 
       x = "Month", 
       y = "Average rainfall",
       subtitle = "source: BOM meterological data")

#weird average rainfall per month, is it per day?



ggsave(filename = "analysed_data/av_rainfall_per_month.png" , 
       plot = av_rainfall,
       width = 20, height = 15, dpi = 300, units = "cm")

#playing around with graph

rainfall_per_station_per_month %>% ggplot(aes(x = Month, y = mean_rainfall, 
  colour = state , group = Station_number))+
  geom_line()+
  geom_point(colour = "black") +
  labs(title = "Average Monthly rainfall", 
       x = "Month", 
       y = "Average rainfall",
       subtitle = "source: BOM meterological data")


 # not working
# geom_errorbar(aes(ymin=len-sd, ymax=len+sd), width=.2,
  #              position=position_dodge(0.05))



