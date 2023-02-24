library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(readxl)

#Load data####
data_b <- read_excel("data/beetle_n_analysis.xlsx") %>% 
  mutate(site = as.factor(site),
         type = as.factor(type),
         treatment = gsub("Open 1", "Open", treatment), #pool Open 1 and Open 2 together
         treatment = gsub("Open 2", "Open", treatment),
         treatment = as.factor(treatment),
         sample_time = gsub("T1", "Week 0", sample_time),
         sample_time = gsub("T2", "Week 2", sample_time),
         sample_time = gsub("T3", "Week 4", sample_time),
         sample_time = as.factor(sample_time)
  ) %>% 
  dplyr::select(-carc_id) %>% 
  dplyr::filter(treatment != "Invertebrate exclusion") #Not relevant for this section

data_f <- read_excel("data/fly_n_analysis.xlsx") %>% 
  mutate(site = as.factor(site),
         type = as.factor(type),
         treatment = gsub("Open 1", "Open", treatment), #pool Open 1 and Open 2 together
         treatment = gsub("Open 2", "Open", treatment),
         treatment = as.factor(treatment),
         sample_time = gsub("T1", "Week 0", sample_time),
         sample_time = gsub("T2", "Week 2", sample_time),
         sample_time = gsub("T3", "Week 4", sample_time),
         sample_time = as.factor(sample_time)) %>% 
  dplyr::select(-carc_id)%>% 
  dplyr::filter(treatment != "Invertebrate exclusion") #Not relevant for this section

data_v <- read_excel("data/vert.xlsx") %>% 
  mutate(site = as.factor(site),
         type = as.factor(type),
         treatment = gsub("Open 1", "Open", treatment), #pool Open 1 and Open 2 together
         treatment = gsub("Open 2", "Open", treatment),
         treatment = as.factor(treatment),
         sample_time = gsub("T1", "Week 0", sample_time),
         sample_time = gsub("T2", "Week 2", sample_time),
         sample_time = gsub("T3", "Week 4", sample_time),
         sample_time = as.factor(sample_time),
         species = as.factor(species)) %>% 
  dplyr::select(-plot_id, -treatment, -total_f)


scav_n_avg_overall <- verts %>% 
  group_by(site,type) %>% 
  summarise(sum_scavev = sum(N)) %>% 
  group_by(type) %>% 
  summarise(mean = mean(sum_scavev))

#Get event numbers for different groups

#Plots including invert exclusiont treatment####
#Get data in right format
n_flies <- data_f %>% 
  group_by(type, treatment, sample_time) %>% 
  summarise(mean_n = round(mean(n)),
            se = round(sd(n)/sqrt(length(n)))) %>% 
  ungroup() %>% 
  mutate(treat_week = paste(treatment, sample_time, sep = ", "))

barplot_fly <- ggplot(n_flies,
                         mapping = aes(treat_week, mean_n,
                                       fill = type,
                                       ))+
  geom_col(position = position_dodge2(preserve = "single"), col = "black")+
  geom_errorbar(aes(ymin = mean_n - se, ymax = mean_n + se), position = position_dodge2(width = 0.2, padding = 0.8))+
  labs(title = "",
       y = "Mean fly number (n)",
       x = "",
       fill = "Carcass treatment",
       # col = "Exclusion treatment"
       )+
  coord_flip()+
  theme_classic()
barplot_fly

#Get beetle abundances for mass mortality vs single carcasses
#First, load beetle abundance data
#Get data in right format
n_beetle <- data_b %>% 
  group_by(type, treatment, sample_time) %>% 
  summarise(mean_n = round(mean(n)),
            se = round(sd(n)/sqrt(length(n)))) %>% 
  ungroup() %>% 
  mutate(treat_week = paste(treatment, sample_time, sep = ", "))

barplot_beetle <- ggplot(n_beetle,
                      mapping = aes(treat_week, as.numeric(mean_n),
                                    fill = type,
                      ))+
  geom_col(position = position_dodge2(preserve = "single"), col = "black")+
  geom_errorbar(aes(ymin = mean_n - se, ymax = mean_n + se), position = position_dodge2(width = 0.2, padding = 0.8))+
  labs(title = "",
       y = "Mean beetle number (n)",
       x = "",
       fill = "Carcass treatment",
       # col = "Exclusion treatment"
  )+
  coord_flip()+
  theme_classic()
barplot_beetle

#Plots excluding invert exclusion treatment####
#Get data in right format
n_flies1 <- data_f %>% 
  filter(treatment != "Invertebrate exclusion") %>% 
  group_by(type, treatment, sample_time) %>% 
  summarise(mean_n = round(mean(n)),
            se = round(sd(n)/sqrt(length(n)))) %>% 
  ungroup() %>% 
  mutate(treat_week = paste(treatment, sample_time, sep = ", "))

barplot_fly1 <- ggplot(n_flies1,
                      mapping = aes(treat_week, mean_n,
                                    fill = type,
                      ))+
  geom_col(position = position_dodge2(preserve = "single"), col = "black")+
  geom_errorbar(aes(ymin = mean_n - se, ymax = mean_n + se), position = position_dodge2(width = 0.2, padding = 0.8))+
  labs(title = "",
       y = "Mean fly number (n)",
       x = "",
       fill = "Carcass treatment",
       # col = "Exclusion treatment"
  )+
  coord_flip()+
  theme_classic()
barplot_fly1

#Get beetle abundances for mass mortality vs single carcasses
#First, load beetle abundance data
#Get data in right format
n_beetle1 <- data_b %>% 
  filter(treatment != "Invertebrate exclusion") %>% 
  group_by(type, treatment, sample_time) %>% 
  summarise(mean_n = round(mean(n)),
            se = round(sd(n)/sqrt(length(n)))) %>% 
  ungroup() %>% 
  mutate(treat_week = paste(treatment, sample_time, sep = ", "))

barplot_beetle1 <- ggplot(n_beetle1,
                         mapping = aes(treat_week, as.numeric(mean_n),
                                       fill = type,
                         ))+
  geom_col(position = position_dodge2(preserve = "single"), col = "black")+
  geom_errorbar(aes(ymin = mean_n - se, ymax = mean_n + se), position = position_dodge2(width = 0.2, padding = 0.8))+
  labs(title = "",
       y = "Mean beetle number (n)",
       x = "",
       fill = "Carcass treatment",
       # col = "Exclusion treatment"
  )+
  coord_flip()+
  theme_classic()
barplot_beetle1

#Vertebrate scavenging event n####
#Then Look at vertebrate scavenger event number
#Get data in right format
n_vert <- data_v %>% 
  mutate(event_n = as.numeric(event_n)) %>% 
  group_by(type, species, sample_time) %>% 
  summarise(mean_n = round(mean(event_n)),
            se = round(sd(event_n)/sqrt(length(event_n)))) %>% 
  ungroup() %>% 
  mutate(treat_week = paste(species, sample_time, sep = ", "))

barplot_vert <- ggplot(n_vert,
                         mapping = aes(treat_week, as.numeric(mean_n),
                                       fill = type,
                         ))+
  geom_col(position = position_dodge2(preserve = "single"), col = "black")+
  geom_errorbar(aes(ymin = mean_n - se, ymax = mean_n + se), position = position_dodge2(width = 0.2, padding = 0.8))+
  labs(title = "",
       y = "Mean scavenging event number (n)",
       x = "",
       fill = "Carcass treatment",
       # col = "Exclusion treatment"
  )+
  coord_flip()+
  theme_classic()
barplot_vert

#Saving all plots####

ggsave("figures/barplot_flyn1.png", barplot_fly1,
       width = 9,
       height = 5)
ggsave("figures/barplot_beetlen1.png", barplot_beetle1,
       width = 9,
       height = 5)
ggsave("figures/barplot_vert_ev.png", barplot_vert,
       width = 9,
       height = 5)
ggsave("figures/barplot_flyn.png", barplot_fly,
       width = 9,
       height = 5)
ggsave("figures/barplot_beetlen.png", barplot_beetle,
       width = 9,
       height = 5)


#Calculations for general averages and abundances####
#beetles
gen_avg_b <- data_b %>% 
  group_by(type) %>% 
  summarise(mean_n = round(mean(n)), #average overall beetle number
            total_n = sum(n) #total overall beetle numbers
  )

#Calculate how many times mm is bigger than sc
comparison_mm_sc_b <-  round(1529/680, digits = 1) #result 2.2
comparison_mm_sc_b <-  round(57/25, digits = 1) #result 2.3

#see difference in values among exclusions for beetles
gen_avg_b_exc <- data_b %>% 
  group_by(treatment, type) %>% 
  summarise(mean_n = round(mean(n)), #average overall beetle number
            total_n = sum(n) #total overall beetle numbers
  )

#fly
gen_avg_f <- data_f %>% 
  group_by(type) %>% 
  summarise(mean_n = round(mean(n)), #average overall fly number
            total_n = sum(n) #total fly numbers
  ) 


#Calculate how many times mm is bigger than sc
comparison_mm_sc_f <-  round(1733/499, digits = 1) #result 3.5
comparison_mm_sc_f <-  round(64/18, digits = 1) #result 3.6


#see difference in values among exclusions for flies
gen_avg_f_exc <- data_f %>% 
  group_by(treatment, type) %>% 
  summarise(mean_n = round(mean(n)), #average overall fly number
            total_n = sum(n) #total fly numbers
  ) 


#Vertebrates
gen_avg_v <- data_v %>% 
  group_by(type) %>% 
  summarise(mean_ev = round(mean(event_n)), #average overall scavenging event number
            total_ev = sum(event_n) #total scavenging event numbers
  ) 
#Calculate how many times mm is bigger than sc
comparison_mm_sc_v <-  round(1200/194, digits = 1) #result 6.2
comparison_mm_sc_v <-  round(39/8, digits = 1) #result 4.9
