library(tidyverse)
library("ipumsr")


usa_file <- ipums_example("data/usa_00002.xml")
usa_data <- read_ipums_micro("data/usa_00002.xml")
write_rds(usa_data,"data/individual_data_2021_rds")

#가설을 세우자 SPEKENG=영어 사용 유형, RACED=

view(head(usa_data, 100))

Usa_data_H<- usa_data %>% group_by(SPEAKENG, RACE) %>% 
  filter(INCTOT>0, INCTOT<9999999, EDUCD!=999, RACE==1| RACE==2 | RACE==3 |RACE==4 |
         RACE==5) %>%  
  summarise(EDUCD_ratio=weighted.mean(EDUCD>=101, w=PERWT),
            W_POP=sum(PERWT)) %>% view()

Usa_data_H$SPEAKENG <- as.character(Usa_data_H$SPEAKENG)
Usa_data_H$RACE <- as.factor(Usa_data_H$RACE)

Usa_data_H %>% 
  ggplot() + geom_point(aes(x=SPEAKENG, y=EDUCD_ratio, group=RACE, color=RACE,
                            size=W_POP)) +
  geom_line(aes(x=SPEAKENG, y=EDUCD_ratio, group=RACE, color=RACE)) +
  labs(x="", y="More than Bachelor's degree", color="RACE", 
       title = "University graduation rate according to English level", 
       caption = "Source: CENSUS data 2021") +
  scale_size(guide=NULL)+
  theme_bw()

usa_data %>% group_by(RACED) %>% summarise(RACED_POP=sum(PERWT)) %>% view()
