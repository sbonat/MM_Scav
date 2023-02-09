#Packages to load####

# Loading and tidying data
library(readxl)
library(tidyverse)
library(lubridate)
library(broom)
library(broom.helpers)

#Plotting
library(ggplot2)
library(ggpubr)

#Load data####
data_fly <- read_excel("data/invert_counts.xlsx", sheet = "flies")
data_beetle <- read_excel("data/invert_counts.xlsx", sheet = "beetles")
data_camtrap <- read_excel("data/camtrapdata_clean.xlsx")


#Data prep####

#Pool together the head and back pitfall traps by adding them together

#Fly data: abundance

flies <- data_fly %>% 
  group_by(site, type, treatment, sample_time, headback) %>% 
  nest() %>%
  mutate(data = map(data, ~mutate(.x,
                                  carc_id = seq(1:n())
  ))) %>% 
  unnest(cols = c(data)) %>% 
  ungroup() %>% 
  group_by(site, type, treatment, sample_time, carc_id) %>% 
  nest() %>% 
  mutate(data = map(data, ~mutate(.x,
                                  n_ea = sum(fly_n))), #add together the head and back traps for each carcass
         data = map(data, ~filter(.x, row_number()==1))
  ) %>% 
  unnest(cols = c(data)) %>% 
  ungroup() %>% 
  select(-fly_n, -headback) %>% 
  group_by(site, type, treatment, sample_time) %>% 
  nest() %>% 
  mutate(data = map(data, ~mutate(.x,
                                  n = round(mean(n_ea)))), #take a mean of fly_n for each treatment and sampling time
         data = map(data, ~filter(.x, row_number()==1))
  ) %>% 
  unnest(cols = c(data)) %>% 
  ungroup() %>% 
  select(-n_ea)%>% 
  mutate(type = case_when(str_detect(type, "MM") ~ "Mass mortality",
                          str_detect(type, "SC") ~ "Single carcass",
                          str_detect(type, "C") ~ "Control"),
         site = case_when(str_detect(site, "CR") ~ "Site 1",
                          str_detect(site, "WP") ~ "Site 2",
                          str_detect(site, "MS") ~ "Site 3"),
         treatment = case_when(str_detect(treatment, "HE") ~ "Open 2",
                               str_detect(treatment, "Nil") ~ "Open 1",
                               str_detect(treatment, "VE") ~ "Vertebrate exclusion",
                               str_detect(treatment, "IE") ~ "Invertebrate exclusion"), 
         
         ) %>% 
  filter(type != "Control",
         sample_time != "NA")

#Beetle data: abundance
beetles <- data_beetle %>% 
  group_by(site, type, treatment, sample_time, headback) %>% 
  nest() %>%
  mutate(data = map(data, ~mutate(.x,
                                  carc_id = seq(1:n())
  ))) %>% 
  unnest(cols = c(data)) %>% 
  ungroup() %>% 
  group_by(site, type, treatment, sample_time, carc_id) %>% 
  nest() %>% 
  mutate(data = map(data, ~mutate(.x,
                                  n_ea = sum(beetle_n))), #add together the head and back traps for each carcass
         data = map(data, ~filter(.x, row_number()==1))
  ) %>% 
  unnest(cols = c(data)) %>% 
  ungroup() %>% 
  select(-beetle_n, -headback) %>% 
  group_by(site, type, treatment, sample_time) %>% 
  nest() %>% 
  mutate(data = map(data, ~mutate(.x,
                                  n = round(mean(n_ea)))), #take a mean of fly_n for each treatment and sampling time
         data = map(data, ~filter(.x, row_number()==1))
  ) %>% 
  unnest(cols = c(data)) %>% 
  ungroup() %>% 
  select(-n_ea)%>% 
  mutate(type = case_when(str_detect(type, "MM") ~ "Mass mortality",
                          str_detect(type, "SC") ~ "Single carcass",
                          str_detect(type, "C") ~ "Control"),
         site = case_when(str_detect(site, "CR") ~ "Site 1",
                          str_detect(site, "WP") ~ "Site 2",
                          str_detect(site, "MS") ~ "Site 3"),
         treatment = case_when(str_detect(treatment, "HE") ~ "Open 2",
                               str_detect(treatment, "Nil") ~ "Open 1",
                               str_detect(treatment, "VE") ~ "Vertebrate exclusion",
                               str_detect(treatment, "IE") ~ "Invertebrate exclusion"), 
         
         )%>% 
  filter(type != "Control",
         sample_time != "NA")

#Camera trap data

#First, calculate the weeks from start for each plot
verts <- data_camtrap %>% 
  rename("plot_id" = "Station",
         "species" = "Species",
         "indep_event" = "group",
         "behaviour" = "Behaviour",
         "f_time" = "visit_time") %>% 
  mutate(behaviour = case_when(
    #Tidy up the non-scavenger species
    species ==  "Bird"| species == "Common wombat"|species == "Emu"|species == "Fallow deer"|
       species =="Red-necked wallaby"|species == "Horse"|species == "Rabbit"|species == "Sambar"|
       species =="Kookaburra"|species == "Macropod"|species == "Unknown deer"|species == "Crimson rosella"|
       species =="Eastern grey kangaroo"|species == "White-winged chough"|species == "Galah"|species == "Red deer"|species == "Willie wagtail"|
       species =="Pied currawong"|species == "Australian magpie"|species == "Macropod"|species == "Swamp wallaby"
     ~ "Not feeding",
    TRUE ~ behaviour)) %>% 
  
  filter(behaviour == "Feeding") %>% 
  dplyr::select(plot_id, indep_event, date_set, DateTimeOriginal, species, N, f_time) %>% 
  group_by(plot_id) %>%
  nest() %>% 
    mutate(data = map(data, ~mutate(.x,
                                    #calculate days since carcass placement
                                    days = as.numeric(round(difftime(DateTimeOriginal, date_set, 
                                                    units = "days"))))
                                      ),
           #this is the fourth week after carcass placement, filtering by days > 0 cause there's two mistakes
           data = map(data, ~filter(.x, days < 35 & days >0)) 
          ) %>% 
  unnest(cols = c(data)) %>% 
  ungroup()

#get the data for time to first arrival for each camera
first_arrival <- verts %>% 
  arrange(DateTimeOriginal) %>% 
  group_by(plot_id) %>%
  filter(row_number()==1)

# Then, proceed to select feeding events for:
# T1 (day 0 to day 3), T2 (day 7 to 14), and T3 (day 23 to 29)
# These were the times when carcasses were sampled for insects

verts <- verts %>% 
  mutate(sample_time = case_when(days >= 0 & days <=3 ~ "T1",
                                 days >= 7 & days <= 14 ~ "T2",
                                 days >= 23 & days <= 29 ~ "T3",
                                 TRUE ~ "NA")
         ) %>% 
  filter(sample_time != "NA") %>%  #Exclude all the values that are not in those sampling periods
  group_by(plot_id, species, sample_time) %>% 
  mutate(event_n = length(sample_time), #get the number of events for each week (= to the number of rows)
         f_time = as.numeric(f_time),
         total_f = sum(f_time)) %>% #get the total feeding time 
  filter(row_number()==1) %>%  #keep first row for each group
  dplyr::select(plot_id, species, sample_time, event_n, total_f)  #these columns are necessary

#add info about site, type, treatment  
verts <- verts %>% 
mutate(type = case_when(str_detect(plot_id, "MM") ~ "Mass mortality",
                        str_detect(plot_id, "SC") ~ "Single carcass"),
       site = case_when(str_detect(plot_id, "CR") ~ "Site 1",
                        str_detect(plot_id, "WP") ~ "Site 2",
                        str_detect(plot_id, "MS") ~ "Site 3"),
       treatment = case_when(str_detect(plot_id, "HE") ~ "Open 2",
                             str_detect(plot_id, "NIL") ~ "Open 1"), .after = plot_id,
       
       total_f = ceiling(total_f/60)
       ) %>%  #put the feeding time in min
       ungroup()
rm(data_beetle, data_biomass, data_camtrap, data_fly)

#Time to first arrival plot
plot_arrival <- ggplot(data = first_arrival,
                       mapping = aes(plot_id,
                                     days,
                                     fill = species
                       ))+
                geom_col()+
                scale_fill_manual(values = c("coral1", "steelblue2", "grey"))+
                labs(x = "Plot ID",
                     y = "Days from carcass placement",
                     title = "",
                     fill = "Species")+
                theme_classic()+
                theme(axis.text.x=element_text(angle=90),
                      legend.position = c(.95, .95),
                      legend.justification = c("right", "top"),
                      legend.box.just = "right",
                      legend.margin = margin(3, 3, 3, 3),
                      legend.box.background = element_rect(fill = "white", colour = "black"))
plot_arrival

ggsave("figures/plot_detection.png", plot_arrival,
       width = 7,
       height = 5)

rm(plot_arrival, first_arrival)

dev.off()


#beetle and fly datasets have one observation per plot repeated three times (T1, T2, T3)
#vertebrate data has more observations per plot as there were different species at different sites

writexl::write_xlsx(beetles, "data/beetle_n_analysis.xlsx")
writexl::write_xlsx(flies, "data/fly_n_analysis.xlsx")
writexl::write_xlsx(verts, "data/vert.xlsx")

rm(beetles, flies, verts, data_beetle, data_fly, verts)
