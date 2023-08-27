#Packages to load ####
# Loading and tidying data
library(readxl)
library(tidyverse)
library(lubridate)

#Plotting
library(ggplot2)
library(ggpubr)
library(factoextra)

#Factor analysis
library(FactoMineR) #Factor analysis of mixed-type data
library(vegan) #adonis function for permanova

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


#Permanova for beetle data####
#Need to remove one n = 0 observation in Site 2 SC IE 
data_b1 <- data_b%>% 
  filter(n!=0)

permanova_b <- with(data_b1, adonis2(n ~ type + treatment + sample_time +type*treatment + type*sample_time + treatment*sample_time, 
                                    data = data_b1, 
                                    method = "gower",
                                    permutations = 9999,
                                    sqrt.dist = T,
                                    na.action = na.fail,
                                    strata = site #this is a grouping variable (i.e. similar to random factor)
                    ))

permanova_b

permanova_b <- permanova_b %>% 
  tibble::rownames_to_column() %>% 
  rename("Variable" = "rowname") 

permanova_b <- permanova_b %>% 
    mutate(across(.cols = 3:6, ~round(.x, digits = 3)))

writexl::write_xlsx(permanova_b, "results/permanova_b.xlsx")


#Permanova for fly data####

permanova_f <- with(data_f, adonis2(n ~ type + treatment + sample_time +type*treatment + type*sample_time + treatment*sample_time, 
                                    data = data_f, 
                                    method = "gower",
                                    sqrt.dist = T,
                                    permutations = 9999,
                                    na.action = na.omit,
                                    strata = site
                    ))

permanova_f <- permanova_f %>% 
  rownames_to_column() %>% 
  rename("Variable" = "rowname")

permanova_f <- permanova_f%>% 
  mutate(across(.cols = 3:6, ~round(.x, digits = 3)))
permanova_f

writexl::write_xlsx(permanova_f, "results/permanova_f.xlsx")

#Permanova for vertebrate data####
permanova_v <- with(data_v, adonis2(event_n ~ type + sample_time + species + type*sample_time + species*sample_time + species*type, 
                                    data = data_v, 
                                    method = "gower",
                                    sqrt.dist = T,
                                    permutations = 9999,
                                    na.action = na.omit,
                                    strata = site
                    ))

permanova_v <- permanova_v %>% 
  rownames_to_column() %>% 
  rename("Variable" = "rowname")

permanova_v <- permanova_v%>% 
  mutate(across(.cols = 3:6, ~round(.x, digits = 3)))

writexl::write_xlsx(permanova_v, "results/permanova_v.xlsx")

#Beetles####

#Use FAMD() from FactoMineR. Factor analysis of mixed data
famd_b <- FAMD(data_b, graph = F)
summary(famd_b)

percent_explained_b <- as.data.frame(famd_b$eig) %>% 
  dplyr::select(2,3) %>% 
  mutate_all(~round(.x, digits = 1)) %>% 
  rownames_to_column() %>% 
  rename("Component" = "rowname",
         "percent_explained" = "percentage of variance",
         "percent_cumulative" = "cumulative percentage of variance") %>% 
  mutate(Component = case_when(str_detect(Component, "comp") ~ paste("Axis", 1:n())))

#scree plot, to show how much variance is explained by each axis
famd_b_scree <- fviz_screeplot(famd_b)
rm(famd_b_scree)

# coord	: coordinates of indiiduals/variables. So, correlation to axis
# cos2	: cos2 values representing the quality of representation on the factor map.
# contrib	: contributions of individuals / variables to the principal components. 

#Contribution to PC axes (i.e. variance explained?)
famd_res_b_n <- get_famd_var(famd_b, element = "quanti.var")
quanti.vars.b <- as.data.frame(famd_res_b_n$contrib)
famd_res_b_other <- get_famd_var(famd_b, element = "quali.var")
quali.vars.b <-  as.data.frame(famd_res_b_other$contrib)
#Put all the values in one df
contrib_b <- rbind(quanti.vars.b, quali.vars.b) %>% 
  mutate_all(~round(.x, digits = 3)) %>% 
  rownames_to_column()%>% 
  rename("Variable" = "rowname")
writexl::write_xlsx(contrib_b, "results/famd_contrib_b.xlsx")


#Coordinates on PC axes (i.e. correlation values)
quanti.coord.b <- as.data.frame(famd_res_b_n$coord)
quali.coord.b <-  as.data.frame(famd_res_b_other$coord)
#Put all the values in one df
coord_b <- rbind(quanti.coord.b, quali.coord.b) %>% 
  mutate_all(~round(.x, digits = 3)) %>% 
  rownames_to_column()%>% 
  rename("Variable" = "rowname") 

colnames(coord_b)[2:6] <- paste(percent_explained_b$Component, " (", percent_explained_b$percent_explained, "%)", sep = "")

writexl::write_xlsx(coord_b, "results/famd_coord_b.xlsx")


#Plot Axis1 and Axis2
plot_b <- fviz_famd(famd_b, axes = c(1,2), 
                    geom= "text",
                    repel = T,
                    habillage = c("sample_time"),
                    addEllipses = T, ellipse.type = "confidence",
                    legend.title = "Sampling period",
                    # alpha.ind = 0.7, #transparency
                    labelsize = 6,
                    col.ind = "black",
                    col.quali.var = "darkblue")+
  labs(title = "BEETLES",
       x = "Axis 1 (22.9%)",
       y = "Axis 2 (14.3%)"
       ) +
  theme_bw()+
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.box.background = element_rect(fill = "white", colour = "black", linewidth = 1),
        legend.title =element_text(size=16),
        legend.text=element_text(size=14),
        axis.title = element_text(size = 16))


#Flies####
#Use FAMD() from FactoMineR. Factor analysis of mixed data
famd_f <- FAMD(data_f, graph = F)
summary(famd_f)

#extract %var explained by each axis
percent_explained_f <- as.data.frame(famd_f$eig) %>% 
  dplyr::select(2,3) %>% 
  mutate_all(~round(.x, digits = 1)) %>% 
  rownames_to_column() %>% 
  rename("Component" = "rowname",
         "percent_explained" = "percentage of variance",
         "percent_cumulative" = "cumulative percentage of variance") %>% 
  mutate(Component = case_when(str_detect(Component, "comp") ~ paste("Axis", 1:n())))

#scree plot, to show how much variance is explained by each axis
famd_f_scree <- fviz_screeplot(famd_f)


rm(famd_f_scree)

#Contribution to PC axes (i.e. variance explained?)
famd_res_f_n <- get_famd_var(famd_f, element = "quanti.var")
quanti.vars.f <- as.data.frame(famd_res_f_n$contrib)
famd_res_f_other <- get_famd_var(famd_f, element = "quali.var")
quali.vars.f <-  as.data.frame(famd_res_f_other$contrib)
#Put all the values in one df
contrib_f <- rbind(quanti.vars.f, quali.vars.f) %>% 
  mutate_all(~round(.x, digits = 3)) %>% 
  rownames_to_column()%>% 
  rename("Variable" = "rowname") 
writexl::write_xlsx(contrib_f, "results/famd_contrib_f.xlsx")
#Coordinates on PC axes (i.e. correlation values)
quanti.coord.f <- as.data.frame(famd_res_f_n$coord)
quali.coord.f <-  as.data.frame(famd_res_f_other$coord)
#Put all the values in one df
coord_f <- rbind(quanti.coord.f, quali.coord.f) %>% 
  mutate_all(~round(.x, digits = 3)) %>% 
  rownames_to_column()%>% 
  rename("Variable" = "rowname") 

colnames(coord_f)[2:6] <- paste(percent_explained_f$Component, " (", percent_explained_f$percent_explained, "%)", sep = "")


writexl::write_xlsx(coord_f, "results/famd_coord_f.xlsx")

#Plot Axis1 and Axis2
plot_f <- fviz_famd(famd_f, axes = c(1,2), 
                     geom= "text", 
                     repel = T,
                     habillage = c("sample_time"),
                     legend.title = "Sampling period",
                     addEllipses = T, ellipse.type = "confidence",
                     # alpha.ind = 0.7, #transparency
                     labelsize = 6,
                     col.ind = "black",
                     col.quali.var = "darkblue")+
  labs(title = "FLIES",
       x = "Axis 1 (17.4%)",
       y = "Axis 2 (14.3%)"
       )+
  theme_bw()+
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.box.background = element_rect(fill = "white", colour = "black", linewidth = 1),
        legend.title =element_text(size=16),
        legend.text=element_text(size=14),
        axis.title = element_text(size = 16))

#Vertebrates####
#Use FAMD() from FactoMineR. Factor analysis of mixed data
famd_v <- FAMD(data_v, graph = F)
summary(famd_v)
percent_explained_v <- as.data.frame(famd_v$eig) %>% 
  dplyr::select(2,3) %>% 
  mutate_all(~round(.x, digits = 1)) %>% 
  rownames_to_column() %>% 
  rename("Component" = "rowname",
         "percent_explained" = "percentage of variance",
         "percent_cumulative" = "cumulative percentage of variance") %>% 
  mutate(Component = case_when(str_detect(Component, "comp") ~ paste("Axis", 1:n())))


#scree plot, to show how much variance is explained by each axis
famd_v_scree <- fviz_screeplot(famd_v)

rm(famd_v_scree)


#Contribution to PC axes (i.e. variance explained?)
famd_res_v_n <- get_famd_var(famd_v, element = "quanti.var")
quanti.vars.v <- as.data.frame(famd_res_v_n$contrib)
famd_res_v_other <- get_famd_var(famd_v, element = "quali.var")
quali.vars.v <-  as.data.frame(famd_res_v_other$contrib)
#Put all the values in one df
contrib_v <- rbind(quanti.vars.v, quali.vars.v) %>% 
  mutate_all(~round(.x, digits = 3)) %>% 
  rownames_to_column()%>% 
  rename("Variable" = "rowname") 
writexl::write_xlsx(contrib_v, "results/famd_contrib_v.xlsx")

#Coordinates on PC axes (i.e. correlation values)
quanti.coord.v <- as.data.frame(famd_res_v_n$coord)
quali.coord.v <-  as.data.frame(famd_res_v_other$coord)
#Put all the values in one df
coord_v <- rbind(quanti.coord.v, quali.coord.v) %>% 
  mutate_all(~round(.x, digits = 3)) %>% 
  rownames_to_column()%>% 
  rename("Variable" = "rowname") 

colnames(coord_v)[2:6] <- paste(percent_explained_v$Component, " (", percent_explained_v$percent_explained, "%)", sep = "")


writexl::write_xlsx(coord_v, "results/famd_coord_v.xlsx")

#Plot Axis1 and Axis2
plot_v1 <- fviz_famd(famd_v, 
                     axes = c(1,2), 
                    geom= "text", 
                    repel = T,
                    habillage = c("type"),
                    legend.title = "Carcass treatment",
                    addEllipses = T, ellipse.type = "confidence",
                    # alpha.ind = 0.7, #transparency
                    labelsize = 6,
                    # fontsize = 4,
                    col.ind = "black",
                    col.quali.var = "darkblue")+
                    labs(title = "",
                         x = "Axis 1 (18.3%)",
                         y = "Axis 2 (17.0%)"
                    )+
  theme_bw()+
  theme(legend.position = c(.95, 0.2),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.box.background = element_rect(fill = "white", colour = "black", linewidth = 1),
        legend.title =element_text(size=16),
        legend.text=element_text(size=14),
        axis.title = element_text(size = 16))

plot_v2 <- fviz_famd(famd_v, axes = c(1,2), 
                    geom= "text", 
                    repel = T,
                    habillage = c("species"),
                    legend.title = "Species",
                    addEllipses = T, ellipse.type = "confidence",
                    # alpha.ind = 0.7, #transparency
                    labelsize = 6,
                    # fontsize = 4,
                    col.ind = "black",
                    col.quali.var = "darkblue")+
                    labs(title = "",
                         x = "Axis 1 (18.3%)",
                         y = "Axis 2 (17.0%)"
                    )+
  theme_bw()+
  theme(legend.position = c(.95, .2),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.box.background = element_rect(fill = "white", colour = "black", linewidth = 1),
        legend.text=element_text(size=14),
        legend.title = element_text(size = 16),
        axis.title = element_text(size = 16),
        )


#Final plots####
plot_v <- ggarrange(plot_v1, plot_v2,
                    labels = "AUTO",
                    nrow = 2, 
                    ncol = 1)


ggsave("figures/famd_verts.png",
       plot = plot_v,
       width = 10,
       height = 12,
       bg = "white"
      )

famd_plots_invert <- ggarrange(plot_b, plot_f,
                               labels = "AUTO",
                                 ncol = 1,
                                 nrow = 2,
                                 align = "v")

ggsave("figures/famd_plots_invert.png", 
       plot = famd_plots_invert,
       width = 10,
       height = 12,
       bg = "white"
       )


rm(quanti.coord.f , quali.coord.f, quali.vars.f, quanti.vars.f, famd_res_f_n, famd_res_f_other,
   quanti.coord.b , quali.coord.b, quali.vars.b, quanti.vars.b, famd_res_b_n, famd_res_b_other,
   quanti.coord.v , quali.coord.v, quali.vars.v, quanti.vars.v, famd_res_v_n, famd_res_v_other,
   percent_explained_b, percent_explained_f, percent_explained_v,
   plot_v1, plot_v2)
