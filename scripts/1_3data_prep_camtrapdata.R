# Packages to load####
#Data manipulation
library(readxl)
library(tidyverse)
library(lubridate)

# Load data ####

data0 <- read_excel("data/camtrapdata_clean.xlsx")
plot_coords <- read_excel("data/site_coordinates.xlsx")
# Manipulate data to get datasets needed##
data1 <- data0 %>% 
  rename("plot_id"="Station")
data1 <- merge(plot_coords, data1, by = "plot_id")

# Get the camera log to calculate sampling effort
cameralog <- read_excel("data/cameralog.xlsx", sheet = "cameralog_fixed") %>% 
  mutate( date_set = ymd(date_set),
          end = ymd(date_set)+ 35, #Put the end date as 35 days from the date set/carcass placement
          Problem1_from = as.numeric(Problem1_from
          ),
          Problem2_from = as.numeric(Problem2_from),
          Problem3_from = as.numeric(Problem3_from),
          Problem4_from = as.numeric(Problem4_from),
          Problem1_to = as.numeric(Problem1_to),
          Problem2_to = as.numeric(Problem2_to),
          Problem3_to = as.numeric(Problem3_to),
          Problem4_to = as.numeric(Problem4_to),
  ) %>% 
  mutate(Problem1_from = as.Date.numeric(Problem1_from, origin = "1899-12-30"),
         Problem2_from = as.Date.numeric(Problem2_from, origin = "1899-12-30"),
         Problem3_from = as.Date.numeric(Problem3_from, origin = "1899-12-30"),
         Problem4_from = as.Date.numeric(Problem4_from, origin = "1899-12-30"),
         Problem1_to = as.Date.numeric(Problem1_to, origin = "1899-12-30"),
         Problem2_to = as.Date.numeric(Problem2_to, origin = "1899-12-30"),
         Problem3_to = as.Date.numeric(Problem3_to, origin = "1899-12-30"),
         Problem4_to = as.Date.numeric(Problem4_to, origin = "1899-12-30"),
         
  )

# Make sure that the problems are relevant to my sampling time, i.e. 35 days from carcass placement
cameralog1 <- cameralog %>% 
  mutate(Problem1_from = case_when(Problem1_from < end ~ Problem1_from),
         Problem2_from = case_when(Problem2_from < end ~ Problem2_from),
         Problem3_from = case_when(Problem3_from < end ~ Problem3_from),
         Problem4_from = case_when(Problem4_from < end ~ Problem4_from))

cameralog <- cameralog1 %>% 
  mutate(p4 = as.numeric(difftime(Problem4_to,Problem4_from, units = "days")), 
         p3 = as.numeric(difftime(Problem3_to, Problem3_from, units = "days")), 
         p2 = as.numeric(difftime(Problem2_to, Problem2_from, units = "days")), 
         p1 = as.numeric(difftime(Problem1_to, Problem1_from, units = "days"))) %>% 
  mutate_at(c("p1", "p2", "p3", "p4"), ~replace(., is.na(.), 0)) %>% 
  mutate(days_inactive = p1 + p2 + p3 + p4,
         days_active = as.numeric(difftime(end, date_set, units = "days")-days_inactive)) %>% 
  dplyr::select(Station, days_active) %>% 
  rename(plot_id = Station)

data1 <- merge(cameralog, data1, by = "plot_id")

#Select all the data within 35 days of carcass placement (that is also when all other sampling was carried out)
data1 <- data1 %>% 
  mutate(days_since_placement = difftime(date(DateTimeOriginal), date_set, units = "days")) %>% 
  filter(days_since_placement < 35)

# Nest dataset by species
data1 <-  data1 %>% 
  dplyr::select(-treatment) %>% 
  group_by(Species) %>% 
  nest()

# # Count all event numbers
# data1v <- data1%>% 
#   mutate(
#     visit_events = map(data, ~length(which(.x$visit_time != "NA")))
#   )

# data1 <- data1 %>% 
# #Add dotplots to see how the data is distributed
# mutate(dotplots = map(data, ~dotchart(x = as.numeric(.x$visit_time),
#                                        groups = factor(.x$Behaviour),
#                                        xlab= "Time spent at carcasses (Seconds)",
#                                        ylab = "Order of observations",
#                                        main = paste(Species))))# %>%
# Erase dotplot column after having a look at the plots
# dplyr::select(-dotplots)

# Eliminate all the species which have too few records (less than 60) to be able to be analysed
data2 <- data1 %>% 
  filter(Species != "Blue-tongued lizard",
         Species != "Copperhead",
         Species != "Crimson rosella",
         Species != "Emu",
         Species != "Galah",
         Species != "Pied currawong",
         Species != "Red deer",
         Species != "Small mammal",
         Species != "White-winged chough",
         Species != "Willie wagtail",
         #the following 4 could be grouped with other herbivore/non scavengers potentially
         Species != "Australian magpie",
         Species != "Kookaburra",
         Species != "Macropod",
         Species != "Rabbit"
  )
# # select data for herbivores/non-scavengers####
# other_animals <- data2 %>% 
#   filter(Species != "Corvid",
#          Species != "Dingo",
#          Species != "Red fox",
#          Species != "Brushtail possum",
#          Species != "Wedge-tailed eagle",
#          Species != "Little eagle"
#       )  %>% 
#   unnest(cols = c(data)) %>% 
#   mutate(visit_time = as.numeric(case_when(Behaviour == "Not feeding"~ visit_time,
#                                            TRUE ~ "0"))) %>% 
#   group_by(Species) %>% 
#   nest() %>% 
#   mutate(tot_visittime = map(data,~sum(.x$visit_time)),
#          visit_events = map(data, ~length(which(.x$visit_time != 0)))
#   )
# 
# other_animals1 <- other_animals %>% 
#   select(-tot_visittime, -visit_events) %>% 
#   unnest(cols = c(data)) %>% 
#   rename("indep_event" = "group")
# writexl::write_xlsx(other_animals1, path = "data/non_scav_data.xlsx")

# select data for scavengers only ####
scav_data <- data2 %>% 
  filter(Species != "Eastern grey kangaroo",
         Species != "Fallow deer",
         Species != "Sambar",
         Species != "Red-necked wallaby",
         Species != "Swamp wallaby",
         Species != "Common wombat",
         Species != "Horse"
  )

scav_data <- scav_data %>% 
  unnest(cols = c(data)) %>%
  #Separate out the Feeding time from the visit only time for each event
  mutate(feed_time = as.numeric(case_when(Behaviour == "Feeding" ~ visit_time)),
         visit_time = as.numeric(case_when(Behaviour == "Not feeding"~ visit_time))) 

scav_data1 <- scav_data %>% 
  group_by(Species) %>% 
  nest() %>% 
  #Calculate the total feeding times, visit times, scavenging event number, visit event number
  mutate(tot_feedtime = map(data, ~sum(which(.x$feed_time != "NA"))),
         tot_visittime = map(data,~sum(which(.x$visit_time !="NA"))),
         scav_events = map(data, ~length(which(.x$feed_time != "NA"))),
         visit_events = map(data, ~length(which(.x$visit_time != "NA"))),
         mean_n = map(data, ~round(mean(as.numeric(.x$N)), digits = 2))
  ) %>% 
  filter(Species != "Wedge-tailed eagle",
         Species != "Little eagle") %>% 
  unnest(cols = c(data)) %>% 
  ungroup()

# totals <- as.data.frame(cbind(scav_data1$Species, scav_data1$tot_feedtime,
#                           scav_data1$tot_visittime, scav_data1$scav_events,
#                           scav_data1$visit_events, scav_data1$mean_n)) %>%
#   rename( "Species"= "V1" ,
#           "tot_feedtime"= "V2" ,
#           "tot_visittime"= "V3" ,
#            "scav_events"= "V4",
#           "visit_events"= "V5" ,
# "mean_n"= "V6" )
# writexl::write_xlsx(totals, "data/totals.xlsx")

#Data for analysis####
scav_data2 <- scav_data1 %>% 
  dplyr::select(-(18:22), -days_since_placement)

data_feed <- scav_data2 %>% 
  filter(feed_time != "NA") %>% 
  dplyr::select(-Behaviour, -visit_time) %>% 
  mutate(feed_time = as.numeric(feed_time),
         MoonPhase = NULL,
         AmbientTemperature = NULL,
         N = as.numeric(N))
writexl::write_xlsx(data_feed, "data/data_feed.xlsx")
data_visit <- scav_data2 %>% 
  filter(visit_time != "NA") %>% 
  dplyr::select(-Behaviour, -feed_time) %>% 
  mutate(visit_time = as.numeric(visit_time),
         MoonPhase = NULL,
         AmbientTemperature = NULL,
         N = as.numeric(N))
writexl::write_xlsx(data_visit, "data/data_visit.xlsx")
data_group <- scav_data2 %>% 
  dplyr::select(-Behaviour, -feed_time, -visit_time) %>% 
  mutate(MoonPhase = NULL,
         AmbientTemperature = NULL,
         N = as.numeric(N))
writexl::write_xlsx(data_group, "data/data_group.xlsx")

# now the data is ready for analysis
# Declutter environment
rm(plot_coords, cameralog, data1, scav_data, scav_data1, scav_data2, data2)
