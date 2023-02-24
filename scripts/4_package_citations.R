library(knitr)

knitr::write_bib(c("readxl", "tidyverse", "lubridate", "ggplot2", 
                   "ggpubr", "factoextra", "FactoMineR", "vegan",
                   "broom", "broom.mixed", "broom.helpers", "lares",
                   "lme4", "nlme", "optimx", "nloptr", "fitdistrplus",
                   "camtrapR", "performance", "predictmeans","MuMIn", "emmeans"), file = "paper/Rpackages.bib")