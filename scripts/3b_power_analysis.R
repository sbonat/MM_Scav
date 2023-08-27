library(simr)




m_feed <- glmer.nb(feed_time ~  type + species + offset(log(days_active)) + (1| site/date),
                   control = glmerControl(optimizer = "Nelder_Mead"), 
                   data = data_feed)

m_group <- glmer.nb(N ~ type + species + type*species + offset(log(days_active)) + (1| site/date),
                    control = glmerControl(optimizer= "bobyqa"), 
                    data = data_group)

m_visit <- glmer.nb(visit_time ~ type + species + offset(log(days_active)) + (1| date),
                    control = glmerControl(optimizer = "bobyqa"), #works with bobyqa, not nloptwrap
                    data = data_visit)