library(ggplot2)
library(tidyverse)
library(RColorBrewer)

scav_n_avg_overall <- verts %>% 
  group_by(site,type) %>% 
  summarise(sum_scavev = sum(N)) %>% 
  group_by(type) %>% 
  summarise(mean = mean(sum_scavev))

#Get event numbers for different groups
#First, load the data_f spreadsheet

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

#Then Look at vertebrate scavenger event number

n_vert <- data_v %>% 
  mutate(event_n = as.numeric(event_n)) %>% 
  group_by(type, species, sample_time) %>% 
  summarise(mean_n = round(mean(event_n)),
            se = round(sd(event_n)/sqrt(length(event_n)))) %>% 
  ungroup() %>% 
  mutate(treat_week = paste(species, sample_time, sep = ", "))

#overall average, and total scavenging events for Week 0, Week2, Week4
gen_avg <- data_v %>% 
  group_by(type) %>% 
  summarise(mean_ev = round(mean(event_n)), #average overall scavenging event number
            total_ev = sum(event_n) #total scavenging event numbers
            )

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

ggsave("figures/barplot_vert_ev.png", barplot_vert,
       width = 9,
       height = 5)
ggsave("figures/barplot_flyn.png", barplot_fly,
       width = 9,
       height = 5)
ggsave("figures/barplot_beetlen.png", barplot_beetle,
       width = 9,
       height = 5)
