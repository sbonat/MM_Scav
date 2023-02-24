# Packages to load####
#Data manipulation or exploration
library(readxl)
library(tidyverse)
library(lubridate)
library(fitdistrplus) #check data distributions
library(broom) #tidying model results
library(broom.helpers) #tidying model results
library(broom.mixed) #tidying model results

#Graphics packages
library(ggplot2)
library(ggpubr)

#Stats packages
library(lares)
library(lme4)
library(nlme)
library(MASS)
library(MuMIn)
library(optimx) #optimizers
library(nloptr) #optimizers

#Model validation tools
library(predictmeans)
library(emmeans)

# Load data ####

#Since I am having issues with models, it is best to focus on SC and MM plots
#Control plots do not have much data for scavengers anyway, so best to exclude
#So, when loading data, filter != "Control"

data_feed <- read_excel("data/data_feed.xlsx") %>% 
  dplyr::select(-N) %>% 
  rename("indep_event" = "group",
         "species" = "Species") %>% 
  filter(type != "Control") %>% 
  mutate(site = as.factor(site),
         plot_id = as.factor(plot_id),
         type = as.factor(type),
         species = as.factor(species),
         date = as.factor(date(DateTimeOriginal))
  )

data_group <- read_excel("data/data_group.xlsx")%>% 
  rename("indep_event" = "group",
         "species" = "Species") %>% 
  filter(type != "Control")%>% 
  mutate(site = as.factor(site),
         plot_id = as.factor(plot_id),
         type = as.factor(type),
         species = as.factor(species),
         date = as.factor(date(DateTimeOriginal)),
         .after = DateTimeOriginal
         )


data_visit <- read_excel("data/data_visit.xlsx")%>% 
  dplyr::select(-N) %>% 
  rename("indep_event" = "group",
         "species" = "Species") %>% 
  filter(type != "Control")%>% 
  mutate(site = as.factor(site),
         plot_id = as.factor(plot_id),
         type = as.factor(type),
         species = as.factor(species),
         date = as.factor(date(DateTimeOriginal)),
         .after = DateTimeOriginal
         )


# DATA EXPLORATION: Time series plots ####
# Feed time
timeseries1 <- ggplot(data_feed,
         mapping = aes(x = week(DateTimeOriginal),
         y = feed_time,
         col = species))+
        geom_point()+
        labs(x = "Week of the Year",
         y = "Time spent scavenging (Min)",
         title = "Feeding time",
         col = "Species")

# Group size
timeseries2 <- ggplot(data_group,
                      mapping = aes(x = week(DateTimeOriginal),
                                    y = N,
                                    col = species))+
  geom_point()+
  labs(x = "Week of the Year",
       y = "N",
       title = "Group size",
       col = "Species")

# Visit time
timeseries3 <- ggplot(data_visit,
                      mapping = aes(x = week(DateTimeOriginal),
                                    y = visit_time,
                                    col = species))+
  geom_point()+
  labs(x = "Week of the Year",
       y = "Time spent scavenging (Min)",
       title = "Visiting time",
       col = "Species")

timeseries <- ggarrange(timeseries1, timeseries2, timeseries3,
                        labels = c("A", "B", "C"))

timeseries


ggsave("figures/timeseries_byspecies.png", plot = timeseries,
       width = 10,
       height = 8)
rm(timeseries1, timeseries2, timeseries3, timeseries)


# DATA EXPLORATION: Boxplots ####
feed_boxplot <- ggplot(data = data_feed,
         mapping = aes(x= site, y= feed_time, fill = type))+
         geom_boxplot()+
         labs(x = "",
              y = "Time (min)",
              title = "Feeding time by site and Carcass treatment")



group_boxplot <- ggplot(data = data_group,
                        mapping = aes(x= site, y= N, fill = type))+
  geom_boxplot()+
  labs(x = "",
       y = "N",
       title = "Group size by site and Carcass treatment")



visit_boxplot <- ggplot(data = data_visit,
                       mapping = aes(x= site, y= visit_time, fill = type))+
  geom_boxplot()+
  labs(x = "",
       y = "Time (min)",
       title = "Visit time by site and Carcass treatment")


boxplots <- ggarrange(feed_boxplot, visit_boxplot, group_boxplot,
          labels = c("A", "B", "C"))

ggsave("figures/exploratory_plots/boxplots.png",
       boxplots,
       width = 8,
       height = 6)

#We can expect issues with homogeneity of variance

# Data EXPLORATION: Histograms####

par(mfrow = c(2,2))
hist(data_feed$feed_time,
     xlab = "Time spent feeding (sec)",
     main = "")
hist(data_visit$visit_time,
     xlab = "Time visiting only (sec)",
     main = "")
hist(data_group$N,
     xlab = "Group size (n)",
     main = "")

dev.off()

#Once plots are saved, tidy up environment
rm(feed_boxplot, visit_boxplot, group_boxplot, boxplots)


#check correlation structures####
corr_dataf1 <- data_feed %>% 
  dplyr::select(species, site, type, feed_time)
corr_cross(corr_dataf1, max_pvalue = 0.05, top = 10)

corr_datav1 <- data_visit %>% 
  dplyr::select(species, site, type, visit_time)
corr_cross(corr_datav1, max_pvalue = 0.05, top = 10)

corr_datag1 <- data_group %>% 
  dplyr::select(species, site, type, N)
corr_cross(corr_datag1, max_pvalue = 0.05, top = 10)

#Unsurprisingly, species is correlated with site

rm(corr_dataf1, corr_datag1, corr_datav1)

#Check distribution. Likely negative binomial for all data

#Visit time data
d1_v <- fitdist(data_visit$visit_time, distr = "pois", method = "mle")
plot(d1_v)
d2_v <- fitdist(data_visit$visit_time, distr = "nbinom", method = "mle")
plot(d2_v) #neg binom distribution fits data better

#Feeding time data
d1_f <- fitdist(data_feed$feed_time, distr = "pois", method = "mle")
plot(d1_f)
d2_f <- fitdist(data_feed$feed_time, distr = "nbinom", method = "mle")
plot(d2_f) #neg binom distribution fits data really well

#Group size data
d1_g <- fitdist(data_group$N, distr = "pois", method = "mle")
plot(d1_g)
d2_g <- fitdist(data_group$N, distr = "nbinom", method = "mle")
plot(d2_g) #negative binomial distribution fits data a little better

rm(d1_f, d1_g, d1_v, d2_f, d2_g, d2_v)

# Model formulas ####

# Feed time
ff1 <- feed_time ~ type + species + type*species + offset(log(days_active))

# Group size
fg1 <- N ~ type + species + type*species + offset(log(days_active))

# Visit_time
fv1 <- visit_time ~ type + species + type*species + offset(log(days_active))

#Negative binomial general models####
mf1 <- glm.nb(ff1,
          data = data_feed)

mg1 <- glm.nb(fg1,
          data = data_group)

mv1 <- glm.nb(fv1,
             data = data_visit)

#See the difference between poisson models and nb where the distribution fit wasn't as clear

mg2 <- glm(fg1,
          family = poisson,
          data = data_group)

AICc(mg1, mg2) 
#nb fits better
rm(mg2)

#Validation plots ####
residplot(mf1, newwd = F) # overdispersed, normality of residuals not great
residplot(mg1, newwd = F) # normality of residuals not great
residplot(mv1, newwd = F) # normality of residuals not great

#Adding random factors ####

# First I am going to try adding a random factor for site to see if this helps
# then I'll add date to account for temporal autocorrelation 

#Run negative binomial GLMM (glmer() function)

#Site as random
glmerf1 <- glmer.nb(feed_time ~ type + species + type*species + offset(log(days_active)) + 
                      (1| site),
                    control = glmerControl(optimizer = "bobyqa"),
                    data = data_feed
                    )

glmerg1 <- glmer.nb(N ~ type + species + type*species + offset(log(days_active)) + 
                      (1| site),
                    control = glmerControl(optimizer = "bobyqa"),
                    data = data_group
                    )

glmerv1 <- glmer.nb(visit_time ~ type + species + type*species + offset(log(days_active)) +  
                      (1| site),
                    control = glmerControl(optimizer = "bobyqa"),
                    data = data_visit
                    )

#Date as random
glmerf2 <- glmer.nb(feed_time ~ type + species + type*species + offset(log(days_active)) + 
                      (1| date),
                    control = glmerControl(optimizer = "bobyqa"),
                    data = data_feed
                    )

glmerg2 <- glmer.nb(N ~ type + species + type*species + offset(log(days_active)) + 
                      (1| date),
                    control = glmerControl(optimizer="bobyqa"),
                    data = data_group) 

glmerv2 <- glmer.nb(visit_time ~ type + species + type*species + offset(log(days_active)) +  
                      (1| date),
                    control = glmerControl(optimizer = "bobyqa"),
                    data = data_visit
                    )

#Date nested within site as random

glmerf3 <- glmer.nb(feed_time ~ type + species + type*species + offset(log(days_active)) + 
                      (1| site/date),
                    control = glmerControl(optimizer = "Nelder_Mead"),
                    data = data_feed
                    )

glmerg3 <- glmer.nb(N ~ type + species + type*species + offset(log(days_active)) + 
                      (1| site/date),
                    control = glmerControl(optimizer="bobyqa"),
                    data = data_group
                    )

glmerv3 <- glmer.nb(visit_time ~ type + species + type*species + offset(log(days_active)) +  
                      (1| site/date),
                    control = glmerControl(optimizer = "bobyqa"),#works with bobyqa, not nloptwrap
                    data = data_visit
                    )#model has a singular fit


#Date and site cross correlated, not nested
glmerf4 <- glmer.nb(feed_time ~ type + species + type*species + offset(log(days_active)) + 
                      (1| site) + (1|date),
                    control = glmerControl(optimizer = "Nelder_Mead"), 
                    data = data_feed
                    )

glmerg4 <- glmer.nb(N ~ type + species +type*species + offset(log(days_active)) + 
                      (1| site) +(1|date),
                    control = glmerControl(optimizer="bobyqa"),
                    data = data_group
                    )

glmerv4 <- glmer.nb(visit_time ~ type + species + type*species + offset(log(days_active)) +  
                      (1| site) +(1|date),
                    control = glmerControl(optimizer = "bobyqa"),
                    data = data_visit
                    ) #model has singular fit

#Now, choose the best random structure based on AIC
AICc(mf1, glmerf1, glmerf2, glmerf3, glmerf4) #feed time model: glmerf3 is the optimal random structure (1|site/date)
AICc(mg1, glmerg1, glmerg2, glmerg3, glmerg4) #group size model: glmerg3 is the optimal random structure (1|site/date)
AICc(mv1, glmerv1, glmerv2, glmerv3, glmerv4) #visit time model: glmerv3 has the best AIC, however model has singular fit
                                              #next best is glmerv2: (1|date) as random


#Check residuals
residplot(glmerf3, newwd = F) #residuals look OK
acf(resid(glmerf3, type = "pearson")) #Some autocorrelation, but not extremely large, to be expected
#Compare with glmerf2 where date is the only random
residplot(glmerf2, newwd = F) #residuals look OK
acf(resid(glmerf2, type = "pearson")) #better in glmerf3


residplot(glmerg3, newwd = F) #residuals look OK, a bit of weird but overall not too bad
acf(resid(glmerg3, type = "pearson")) #Autocorrelation larger, however this is to be expected
#Compare with glmerf2 where date is the only random
residplot(glmerg2, newwd = F) #residuals look better in glmerg3
acf(resid(glmerg2, type = "pearson")) #better in glmerg3


residplot(glmerv2, newwd = F) #residuals look OK
acf(resid(glmerv2, type = "pearson"))#Autocorrelation plot great

rm(mf1, glmerf1, glmerf2, glmerf3, glmerf4,  
   mg1, glmerg1, glmerg2, glmerg3, glmerg4, 
   mv1, glmerv1, glmerv2, glmerv3, glmerv4)

#Model selection####

#Feed time
ff1 <- feed_time ~ type + species + type*species + offset(log(days_active)) + (1| site/date)
ff2 <- feed_time ~  type + species + offset(log(days_active)) + (1| site/date)
ff3 <- feed_time ~  type + offset(log(days_active)) + (1| site/date)
ff4 <- feed_time ~  species + offset(log(days_active)) + (1| site/date)

f_listf <- c(ff1, ff2, ff3, ff4)

models_f <- map(f_listf, ~glmer.nb(.x,
                                   control = glmerControl(optimizer = "Nelder_Mead"),
                                   data = data_feed
                                    ))
sel_f <- model.sel(models_f)

#Select models with delta <2. Only one model
#Top model is m1, the full model
residplot(models_f[[1]], newwd = F)

# Group size
fg1 <- N ~ type + species + type*species + offset(log(days_active)) + (1| site/date)
fg2 <- N ~  type + species + offset(log(days_active)) + (1| site/date)
fg3 <- N ~  type + offset(log(days_active)) + (1| site/date)
fg4 <- N ~  species + offset(log(days_active)) + (1| site/date)

f_listg <- c(fg1, fg2, fg3, fg4)

models_g <- map(f_listg, ~glmer.nb(.x,
                                   control = glmerControl(optimizer = "bobyqa"), #works with bobyqa, not nloptwrap
                                   data = data_group
                  ))

sel_g <-  model.sel(models_g)

#Select models with delta <2. Only one model
#the best model is the full model
residplot(models_g[[1]], newwd = F)

# Visit_time
fv1 <- visit_time ~ type + species + type*species + offset(log(days_active)) + (1| date)
fv2 <- visit_time ~  type + species + offset(log(days_active)) + (1| date)
fv3 <- visit_time ~  type + offset(log(days_active)) + (1| date)
fv4 <- visit_time ~  species + offset(log(days_active)) + (1| date)

f_listv <- c(fv1, fv2, fv3, fv4)

models_v <- map(f_listv, ~glmer.nb(.x,
                                   control = glmerControl(optimizer = "bobyqa"), #works with bobyqa, not nloptwrap
                                   data = data_visit
                  ))

sel_v <- model.sel(models_v)
#Select models with delta <2. Only one model
#top model is m2, without interactions

residplot(models_v[[2]], newwd = F)

#Tidy up environment
rm(models_f, models_g, models_v, 
   f_listf, f_listg, f_listv,
   sel_f, sel_g, sel_v,
   ff1, ff2, ff3, ff4,
   fg1, fg2, fg3, fg4,
   fv1, fv2, fv3, fv4)

# Final models ####
#Re-run models, then get values for plots

m_feed <- glmer.nb(feed_time ~  type + species + offset(log(days_active)) + (1| site/date),
          control = glmerControl(optimizer = "Nelder_Mead"), 
          data = data_feed)

m_group <- glmer.nb(N ~ type + species + type*species + offset(log(days_active)) + (1| site/date),
                    control = glmerControl(optimizer= "bobyqa"), 
                    data = data_group)

m_visit <- glmer.nb(visit_time ~ type + species + offset(log(days_active)) + (1| date),
                    control = glmerControl(optimizer = "bobyqa"), #works with bobyqa, not nloptwrap
                    data = data_visit)


# Plots####
plot_data_feed <- m_feed %>% tidy_plus_plus(
                                        exponentiate = F, #data was not log transformed
                                        variable_labels = c("(Intercept)" = "Intercept",
                                                            "species" = "Species",
                                                            "type" = "Carcass treatment")
                                                )

plot_data_feed <- plot_data_feed %>%
  filter(effect == "fixed") %>% 
  mutate(
    #A column to tell if the value was positive or negative. Remember here we have Post/pre ratio
    pos_neg = case_when(estimate > 0 ~ 1, # positive value
                        estimate < 0 ~ -1, # negative value
                        estimate == 0 ~ 0) # reference value
  ) %>% 
  filter(term != "(Intercept)")

#Check if estimates need to be exponentiated?
plot_feed <- plot_data_feed %>% 
  ggplot(mapping = aes(reorder(label, estimate), estimate,
                       col = pos_neg))+
  geom_point()+
  coord_flip()+
  geom_abline(intercept = 0, #reference line
              slope = 0,
              linetype = 2,
              colour = "black")+
  labs(x = "",
       y = "Estimate",
       title = "Vertebrate scavenger feeding time")+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1)+
  theme_classic()+
  theme(legend.position="none")


# ggsave("figures/models/m1_feed.png", plot_feed,
#        height = 6,
#        width = 8)

plot_data_group <- m_group %>% tidy_plus_plus(
  exponentiate = F, #data was not log transformed
  variable_labels = c("(Intercept)" = "Intercept",
                      "species" = "Species",
                      "type" = "Carcass treatment")
) 

plot_data_group <- plot_data_group %>%
  filter(effect == "fixed") %>% 
  mutate(
    #A column to tell if the value was positive or negative. Remember here we have Post/pre ratio
    pos_neg = case_when(estimate > 0 ~ 1, # positive value
                        estimate < 0 ~ -1, # negative value
                        estimate == 0 ~ 0) # reference value
  ) %>% 
  filter(term != "(Intercept)")

#Check if estimates need to be exponentiated?
plot_group <- plot_data_group %>% 
  ggplot(mapping = aes(reorder(label, estimate), estimate,
                       col = pos_neg))+
  geom_point()+
  coord_flip()+
  geom_abline(intercept = 0, #reference line
              slope = 0,
              linetype = 2,
              colour = "black")+
  labs(x = "",
       y = "Estimate",
       title = "Vertebrate scavenger group size")+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1)+
  theme_classic()+
  theme(legend.position="none")


plot_data_visit <- m_visit %>% tidy_plus_plus(
  exponentiate = F, #data was not log transformed
  variable_labels = c("(Intercept)" = "Intercept",
                      "species" = "Species",
                      "type" = "Carcass treatment")
) 

plot_data_visit <- plot_data_visit %>%
  filter(effect == "fixed") %>% 
  mutate(
    #A column to tell if the value was positive or negative. Remember here we have Post/pre ratio
    pos_neg = case_when(estimate > 0 ~ 1, # positive value
                        estimate < 0 ~ -1, # negative value
                        estimate == 0 ~ 0) # reference value
  ) %>% 
  filter(term != "(Intercept)")

#Check if estimates need to be exponentiated?
plot_visit <- plot_data_visit %>% 
  ggplot(mapping = aes(reorder(label, estimate), estimate,
                       col = pos_neg))+
  geom_point()+
  coord_flip()+
  geom_abline(intercept = 0, #reference line
              slope = 0,
              linetype = 2,
              colour = "black")+
  labs(x = "",
       y = "Estimate",
       title = "Vertebrate scavenger visiting time")+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1)+
  theme_classic()+
  theme(legend.position="none")


# ggsave("figures/models/m3_visit.png", plot_visit,
#        height = 6,
#        width = 8)

plot_models <- ggarrange(plot_feed, plot_group, plot_visit,
                         ncol = 2,
                         nrow = 2,
                         align = "v")

ggsave("figures/models.png", plot_models,
       width = 14,
       height = 9)

# Model results ####
m_feed_est <- m_feed %>% tidy_plus_plus() %>% 
  dplyr::select(var_label, label, n_obs, estimate, std.error, conf.low, conf.high) %>% 
  mutate(across(.cols = c(4:7), ~round(.x, digits = 3))) %>% 
  rename("Variable" = "var_label",
         "Variable level" = "label",
         "Estimate" = "estimate", 
         "Number of observations" = "n_obs",
         "SE" = "std.error",
         "Lower CI (95%)" = "conf.low",
         "Upper CI (95%)" = "conf.high")
#writexl::write_xlsx(m_feed_est, "results/m_feed.xlsx")

m_group_est <- m_group %>% tidy_plus_plus() %>%
  dplyr::select(var_label, label, n_obs, estimate, std.error, conf.low, conf.high) %>% 
  mutate(across(.cols = c(4:7), ~round(.x, digits = 3))
         ) %>% 
  rename("Variable" = "var_label",
         "Variable level" = "label",
         "Estimate" = "estimate", 
         "Number of observations" = "n_obs",
         "SE" = "std.error",
         "Lower CI (95%)" = "conf.low",
         "Upper CI (95%)" = "conf.high")

#writexl::write_xlsx(m_group_est, "results/m_group.xlsx")

m_visit_est <- m_visit %>% tidy_plus_plus() %>% 
  dplyr::select(var_label, label, n_obs, estimate, std.error, conf.low, conf.high) %>% 
  mutate(across(.cols = c(4:7), ~round(.x, digits = 3))) %>% 
  rename("Variable" = "var_label",
         "Variable level" = "label",
         "Estimate" = "estimate", 
         "Number of observations" = "n_obs",
         "SE" = "std.error",
         "Lower CI (95%)" = "conf.low",
         "Upper CI (95%)" = "conf.high")
#writexl::write_xlsx(m_visit_est, "results/m_visit.xlsx")

#Estimated marginal means####

emmeans_feed <- as.data.frame(emmeans(m_feed, ~ type|species, type = "response")) %>% 
  mutate(across(c(3,4, 6, 7), ~round(.x, digits = 1))) %>% 
  mutate(lower.SE = response - SE,
         upper.SE = response + SE,
         resp_var = "Scavenging time (min)")

#writexl::write_xlsx(emmeans_feed, "results/emmeans_feed.xlsx")

# emmeans_feed_plot <- emmeans_feed %>% 
#   ggplot(mapping = aes(type, response
#                        ))+
#   geom_point()+
#   labs(x = "",
#        y = "Estimated time (min)",
#        title = "Scavenging time with Standard Error bars")+
#   geom_errorbar(aes(ymin = lower.SE, ymax = upper.SE), width = 0.1)+
#   theme_bw()+
#   theme(legend.position="none")+
#   facet_wrap(vars(species))+
#   geom_text(label = emmeans_feed$response,
#             nudge_x = 0.2,
#             size = 3)

# emmeans_feed_plot 
# 
# ggsave("figures/emmeans_feed.png", emmeans_feed_plot,
#        width = 8,
#        height = 6)

emmeans_group <- as.data.frame(emmeans(m_group, ~ type|species, type = "response")) %>% 
  mutate(across(c(3,4, 6, 7), ~round(.x, digits = 1))) %>% 
  mutate(lower.SE = response - SE,
         upper.SE = response + SE,
         resp_var = "Group size (n)")

#writexl::write_xlsx(emmeans_group, "results/emmeans_group.xlsx")

# emmeans_group_plot <- emmeans_group %>% 
#   ggplot(mapping = aes(type, response
#   ))+
#   geom_point()+
#   labs(x = "",
#        y = "Estimated group size (n)",
#        title = "Group size with Standard Error bars")+
#   geom_errorbar(aes(ymin = lower.SE, ymax = upper.SE), width = 0.1)+
#   theme_bw()+
#   theme(legend.position="none")+
#   facet_wrap(vars(species))+
#   geom_text(label = emmeans_group$response,
#             nudge_x = 0.15,
#             size = 3)
# emmeans_group_plot 
#
# ggsave("figures/emmeans_group.png", emmeans_group_plot,
#        width = 8,
#        height = 6)

emmeans_visit <- as.data.frame(emmeans(m_visit, ~ type|species, type = "response")) %>% 
  mutate(across(c(3,4, 6, 7), ~round(.x, digits = 1))) %>% 
  mutate(lower.SE = response - SE,
         upper.SE = response + SE,
         resp_var = "Visitation only (min)")

#writexl::write_xlsx(emmeans_visit, "results/emmeansvisit.xlsx")

# emmeans_visit_plot <- emmeans_visit %>% 
#   ggplot(mapping = aes(type, response
#   ))+
#   geom_point()+
#   labs(x = "",
#        y = "Estimated time (min)",
#        title = "Visit time with Standard Error bars")+
#   geom_errorbar(aes(ymin = lower.SE, ymax = upper.SE), width = 0.1)+
#   theme_bw()+
#   theme(legend.position="none")+
#   facet_wrap(vars(species))+
#   geom_text(label = emmeans_visit$response,
#             nudge_x = 0.2,
#             size = 3)

# emmeans_visit_plot 
# 
# ggsave("figures/emmeans_visit.png", emmeans_visit_plot,
#        width = 8,
#        height = 6)

emmeans_all <- rbind(emmeans_feed, emmeans_visit, emmeans_group)

emmeans_plot <- emmeans_all %>% 
  ggplot(aes(type, response
    ))+
    geom_point()+
    labs(x = "Carcass treatment",
         y = "Estimated mean",
         title = "")+
    geom_errorbar(aes(ymin = lower.SE, ymax = upper.SE), width = 0.1)+
    theme_classic()+
    theme(legend.position="none",
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 10),
          strip.text.x = element_text(size = 12),
          strip.text.y = element_text(size = 12),
          panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))+
    facet_grid(resp_var ~species)+
    geom_text(label = emmeans_all$response,
              nudge_x = 0.3,
              size = 4)
emmeans_plot


ggsave("figures/emmeans_plot.png", emmeans_plot,
       height = 7,
       width = 10)
