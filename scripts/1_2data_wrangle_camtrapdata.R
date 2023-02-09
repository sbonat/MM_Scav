# Camera trap code

#Packages to load####
library(readxl)
library(tidyverse)
library(lubridate)

# Data manipulation from raw data ####
data0 <- read_excel("C:/Users/sbon6737/Desktop/Rprojects/camtrapdata_merged.xlsx")
data1 <- data0 %>%
  rename_with(~gsub("metadata_", "", .x, fixed = TRUE)) %>% 
  dplyr::select(Station, DateTimeOriginal, MoonPhase, AmbientTemperature,
         Species, Species1N, Behaviour1, Behaviour2, 
         Species2, Species2N, Species3, Species3N) %>%
  #This section will substitute the names for the MM cameras, because these need to be merged together
  mutate( Station = gsub("CRMMNIL1", "CRMMNIL", Station),
          Station = gsub("CRMMNIL2", "CRMMNIL", Station),
          Station = gsub("CRMMHE1", "CRMMHE", Station),
          Station = gsub("CRMMHE2", "CRMMHE", Station),
          Station = gsub("MSMMNIL1", "MSMMNIL", Station),
          Station = gsub("MSMMNIL2", "MSMMNIL", Station),
          Station = gsub("MSMMHE1", "MSMMHE", Station),
          Station = gsub("MSMMHE2", "MSMMHE", Station),
          Station = gsub("WPMMNIL1", "WPMMNIL", Station),
          Station = gsub("WPMMNIL2", "WPMMNIL", Station),
          Station = gsub("WPMMHE1", "WPMMHE", Station),
          Station = gsub("WPMMHE2", "WPMMHE", Station)) %>% 
  #Order the dataset according to this
  arrange(Station, Species, DateTimeOriginal) %>% 
  group_by(Station, Species) %>% 
  #Following bit of code calculates deltatime similarly to camtrapR, I just need to redo it because of what I just did above
  mutate( DateTimeOriginal = as.POSIXct(DateTimeOriginal,format="%d/%m/%Y %H:%M:%S",tz="Australia/Brisbane" ),
          deltatime = DateTimeOriginal - lag(DateTimeOriginal),
          deltatimesecs = as.numeric(deltatime, units = "secs"),
          deltatimesecs = replace_na(deltatimesecs, 0),
          deltatimemin = ceiling(as.numeric(deltatime, units = 'mins')), #ceiling rounds up to the nearest whole number
          deltatimemin = replace_na(deltatimemin, 0)) %>% 
          dplyr::select(-deltatime) %>% 
          ungroup() %>% 
          mutate(
          Species1_N1_B1 = paste(Species, Species1N, Behaviour1, sep = "_"),
          Species2_N2_B2 = paste(Species2, Species2N, Behaviour2, sep = "_"),
          Species3_N3 = paste(Species3, Species3N, sep = "_"),
          Species1 = NULL, Species2 = NULL, Species3 = NULL,
          Species1N = NULL, Species2N = NULL, Species3N = NULL, Behaviour2 = NULL) %>% 
  pivot_longer(cols = c(Species1_N1_B1, Species2_N2_B2, Species3_N3),
               names_to = "Speciesord_group",
               values_to = "Species_N"
  ) %>%
  filter(Species_N != "NA_NA_NA",
         Species_N != "NA_NA") %>%
  dplyr::select(-Speciesord_group, -Behaviour1) %>% 
  separate(col = Species_N, sep = "_", into = c("Species", "N", "Behaviour1"))
  
#Add the dates when cameras were set
data1 <- data1%>% 
  mutate(date_set = case_when(Station == "CRSCNIL"~ "2021-01-08",
                              Station == "CRCNIL" ~ "2020-12-16",
                              Station == "CRMMHE" ~ "2020-12-08",
                              Station == "CRMMNIL" ~ "2020-12-15",
                              Station == "CRSCHE" ~ "2020-12-16",
                              Station == "CRCHE" ~ "2020-12-11",
                              Station == "MSCHE"| Station == "MSCNIL"~ "2021-11-18",
                              Station == "MSMMHE"| Station == "MSMMNIL" ~ "2021-11-16",
                              Station == "MSSCHE" ~ "2021-11-20",
                              Station == "MSSCNIL" ~ "2021-11-21",
                              Station == "WPCHE"| Station == "WPCNIL" ~ "2020-12-21",
                              Station == "WPSCHE" ~ "2021-01-19",
                              Station == "WPSCNIL" ~ "2021-01-25",
                              Station == "WPMMHE"| Station == "WPMMNIL" ~ "2020-12-17",
                              TRUE ~ "NA"), .after = Station)
# nonscav_list <- c("Bird", "Common wombat", "Emu", "Fallow deer", 
#              "Red-necked wallaby", "Horse", "Rabbit", "Sambar",
#              "Kookaburra", "Macropod", "Unknown deer", "Crimson rosella",
#              "Eastern grey kangaroo", "White-winged chough", "Galah", "Red deer", "Willie wagtail",
#              "Pied currawong", "Australian magpie", "Macropod", "Swamp wallaby")
# scav_list <- c("Brushtail possum", "Corvid", "Dingo", "Red fox", "Wedge-tailed eagle", "Domestic dog",
#                "Little eagle")

# Tidy up the behaviour column. This is due to how photos were tagged initially.
data1 <- data1 %>% mutate(Behaviour = case_when(
    #Tidy up the non-scavenger species
    (Species ==  "Bird"| Species == "Common wombat"|Species == "Emu"|Species == "Fallow deer"|
    Species =="Red-necked wallaby"|Species == "Horse"|Species == "Rabbit"|Species == "Sambar"|
    Species =="Kookaburra"|Species == "Macropod"|Species == "Unknown deer"|Species == "Crimson rosella"|
    Species =="Eastern grey kangaroo"|Species == "White-winged chough"|Species == "Galah"|Species == "Red deer"|Species == "Willie wagtail"|
    Species =="Pied currawong"|Species == "Australian magpie"|Species == "Macropod"|Species == "Swamp wallaby")
    & (Behaviour1 == "NA") ~ "Not feeding",

    (Species ==  "Bird"| Species == "Common wombat"|Species == "Emu"|Species == "Fallow deer"|
    Species =="Red-necked wallaby"|Species == "Horse"|Species == "Rabbit"|Species == "Sambar"|
    Species =="Kookaburra"|Species == "Macropod"|Species == "Unknown deer"|Species == "Crimson rosella"|
    Species =="Eastern grey kangaroo"|Species == "White-winged chough"|Species == "Galah"|Species == "Red deer"|Species == "Willie wagtail"|
    Species =="Pied currawong"|Species == "Australian magpie"|Species == "Macropod"|Species == "Swamp wallaby")
    & (Behaviour1 == "Not Feeding" | Behaviour1 == "Not feeding") ~ "Not feeding",

    Species ==  "Bird"| Species == "Common wombat"|Species == "Emu"|Species == "Fallow deer"|
    Species =="Red-necked wallaby"|Species == "Horse"|Species == "Rabbit"|Species == "Sambar"|
    Species =="Kookaburra"|Species == "Macropod"|Species == "Unknown deer"|Species == "Crimson rosella"|
    Species =="Eastern grey kangaroo"|Species == "White-winged chough"|Species == "Galah"|Species == "Red deer"|Species == "Willie wagtail"|
    Species =="Pied currawong"|Species == "Australian magpie"|Species == "Macropod"|Species == "Swamp wallaby"
    & Behaviour1 == "Feeding" ~ "Feeding",
    
    # Tidy up for scavengers
    (Species =="Brushtail possum" | Species == "Corvid"| Species == "Dingo"| 
    Species == "Red fox"| Species == "Wedge-tailed eagle"| Species == "Domestic dog"|
    Species =="Little eagle") & (Behaviour1 == "Not Feeding"| Behaviour1 == "Not feeding") ~ "Not feeding",
    
    (Species =="Brushtail possum"| Species == "Corvid"| Species == "Dingo"| 
    Species == "Red fox"| Species == "Wedge-tailed eagle"| Species == "Domestic dog"|
    Species =="Little eagle") & (Behaviour1 == "NA") ~ "Feeding",
    
    (Species =="Brushtail possum"| Species == "Corvid"| Species == "Dingo"| 
    Species == "Red fox"| Species == "Wedge-tailed eagle"| Species == "Domestic dog"|
    Species =="Little eagle") & (Behaviour1 == "Feeding") ~ "Feeding"
    ),
    # fix column N. Due to how photos were tagged initially, any NAs are a n = 1
    N = gsub("NA", "1", N)
    ) %>% 
  dplyr::select(-Behaviour1) %>% 
  #Make sure there's no mistakes in the Behaviour column for the control sites
  mutate(Behaviour = case_when((Station == "MSCNIL"| Station == "MSCHE" | Station =="CRCHE" | Station == "CRCNIL" |
                                  Station == "WPCNIL"| Station == "WPCHE") ~ "Not feeding",
                               TRUE ~ Behaviour)) 

data1 <- data1%>% 
  #filter out irrelevant records
  filter(
    Species != "Human",
    Species != "NA",
    Species != "Unidentifiable",
    Species != "Domestic dog",
    Species != "Unknown deer",
    Species != "Bird"
  )

# Histogram photos to choose event interval
hist_events <- cut(data1$deltatimemin, c(seq(0, 30, 1), Inf))
hist_events1 <- subset(hist_events, hist_events != "(0,1]")

# Histogram with all photos
hist(as.numeric(hist_events), breaks=0:31, xaxt='n', xlab="Minutes between images", ylab='Number of images',
     col=1, border=0, cex.axis=0.8, las=1, main="")
axis(1, at=0:31, labels=c(seq(0,30,1), '>30'), cex.axis=0.8)
box()

# Histogram without photos between 0-1 min


png("figures/camtrap_data_event_interval.png",
       width = 900,
       height = 500,
    bg = "white")
hist(as.numeric(hist_events1), breaks=0:31, xaxt='n', xlab="Minutes between images", ylab='Number of images',
     col=1, border=0, cex.axis=0.8, las=1, main="")
axis(1, at=0:31, labels=c(seq(0,30,1), '>30'), cex.axis=0.8)
box()
dev.off()

#New section: calculate independent events
#Calculate independent events using deltatimesecs and the cumulative sum function
#Currently deltatimesecs is 300sec (i.e. 5 min) Double check with others if this is appropriate
data1 <-  data1 %>% mutate(group = cumsum(ifelse(deltatimesecs>300,1,0)))


#Calculate maximum and minimum for each group
maxmintime <- data1 %>% 
  group_by(Station,Species,group) %>% 
  #get the max (endtime) and min (starttime) for each group
  summarise(Maxtime = max(DateTimeOriginal), Mintime=min(DateTimeOriginal)) %>% 
  #Calculate the difference in time between the start and end of each event, by using difftime
  mutate(visit_time = as.numeric(difftime(Maxtime, Mintime, units = "secs")),
         visit_time = gsub("0", "1", visit_time)) %>% 
  #then select the relevant columns, and ungroup dataset
  dplyr::select(Station, Species, group, visit_time) %>% 
  ungroup()

#Merge the two dataframes, and then filter to keep only the single events
data2 <- merge.data.frame(x = data1, y = maxmintime, by = c("Station", "Species", "group")) %>% 
  #Tidy up group size column
  mutate(N = gsub("Fallow deer", "1", N),
         Species = gsub("1", "Fallow deer", Species),
         N = as.numeric(N)
  ) %>% 
  #Group by independent event
  group_by(Station, Species, group) %>%
  #Calculate the max group size for each independent event
  #Fix up possum data as well, one Mt brushtail record that is a mistake. Put as brushtail p.
  mutate(Nmax = max(N),
         Species = gsub("Mountain brushtail possum", "Brushtail possum", Species))%>%
  #Then, pick the right behaviour from the intervals. When there is both feeding and not feeding in one group,
  #The group should be chosen as "Feeding".
  mutate(Behaviour = gsub('\\s+', '', Behaviour)) %>% 
  mutate(Behaviour1 = case_when(Behaviour == "Feeding" ~ 2,
                                TRUE ~ 1)
  ) %>% 
  ungroup()
#Calculate the mean of the behaviour column in a separate df
behaviour <- data2 %>% 
  group_by(Station, Species, group) %>% 
  summarise_at(vars(Behaviour1),
               list(b1 = mean))
 #Then merge it back to the original df and use case_when to sub values
data3 <- merge.data.frame(x = data2, y = behaviour, by = c("Station", "Species", "group"))%>% 
  dplyr::select(-Behaviour, -Behaviour1) %>% 
  mutate(Behaviour = case_when(
    b1 > 1 ~ "Feeding", # If the mean is more than one, it means that there was at least one feeding in the group
    b1 == 1 ~ "Not feeding" # If the mean is equal to one, then it means there were only ones in the group
  )) %>% 
  dplyr::select(-b1) %>% 
  group_by(Station, Species, group) %>% 
  filter(row_number()== 1) %>%  #keep only first row for each group
  ungroup() %>%
  #eliminate unnecessary columns
  dplyr::select(-deltatimesecs, -deltatimemin, -N) %>% 
  #rename Nmax column to N, as it is the group size for each event
  rename("N" = "Nmax")

#Clear the environment a little
data1 <- data3
rm(data2, data3, maxmintime, behaviour, hist_events, hist_events1, data0, cameralog1)

writexl::write_xlsx(data1, path = "data/camtrapdata_clean.xlsx") #uncomment if you need to update clean spreadsheet
