library(tidyverse)
library("ipumsr")
usa_file <- ipums_example("data/usa_00003.xml")
data <- read_ipums_micro("data/usa_00003.xml")
write_rds(data,"data/census_img_2019.rds")

view(head(data))

#Q1  Install “ipumsr” package if you have not. Check the labels of BPL by running the following codes: “ipums_val_labels(data$BPL)”. What are the values for “Canada”, “China”, and “Germany” in the BPL variable? 
view(ipums_val_labels(data$BPL))
#Canada:150 , China:500, Germany:453


#Q2 We define an individual as “foreign born” if the value of his/her BPL is 150 or higher. List 
#the 5 origin countries from which the most immigrants have immigrated to the U.S. (You must provide 
#the names of the countries, not the continents or regions.) 
population_by_country <- data %>% filter(MET2013!=0,OCC!=0, 0<INCTOT, INCTOT<9999999, BPL>=150) %>% group_by(BPL) %>% 
  summarize(Population=sum(PERWT)) %>% arrange(desc(Population)) %>% view()
#200:Mexico, 210:Central America, 521:India, 300:SOUTH AMERICA, 260:West Indies


#Q3 Prove the 5 MSAs with the highest “foreign born” shares. 
population_by_MSAs <- data %>% filter(MET2013!=0,OCC!=0, 0<INCTOT, INCTOT<9999999, BPL>=150) %>% group_by(MET2013) %>% 
  summarize(Population=sum(PERWT)) %>% arrange(desc(Population))
#35620:	New York-Newark-Jersey City NY-NJ-PA, 
#31080:Los Angeles-Long Beach-Anaheim, CA, 
#33100:Miami-Fort Lauderdale-West Palm Beach, FL, 
#16980:Chicago-Naperville-Elgin, IL-IN-WI
#26420:Houston-The Woodlands-Sugar Land, TX


#Q4 Generate data using the following codes: 
Q4 <- data %>% filter(EDUCD!=999) %>% 
  mutate(fb=if_else(BPL>=150, "Foreign born", "Native born"), 
         cdegree=if_else(EDUCD<=100, "No Bachelor's degree", 
                         if_else(EDUCD==101, "Bachelor's degree", "Graduate degree")) %>% 
           factor(levels=c("No Bachelor's degree", "Bachelor's degree", "Graduate degree")))
Q4 %>% ggplot() + geom_bar(aes(fb, fill=cdegree,weight=PERWT),position="fill") +
  labs(title="Educational attainment of foreign and native borns") +
  theme(plot.title = element_text(size=10)) +
  scale_y_continuous(name=NULL, labels=scales::label_comma(scale=100, suffix="%")) +
  scale_x_discrete(name=NULL) +
  scale_fill_discrete(name=NULL) +
  theme(legend.position = "bottom", legend.text = element_text(size=7)) +
  theme(axis.text.y = element_text(size=8)) +
  theme(axis.text.x = element_text(size=8)) +
  coord_flip()

#Q5 After removing individuals who didn’t report incomes or made negative income from 
#“data”, calculate average wages for “foreign born” and “native born”. 
Q5<- data %>% filter(EDUCD!=999, INCTOT>0, INCTOT<9999999) %>% 
  mutate(fb=if_else(BPL>=150, "Foreign born", "Native born"))
Q5 %>% group_by(fb) %>% summarise(AVG_wage_fb=weighted.mean(INCTOT, w=PERWT))
#1 Foreign born  50193. 2 Native born 49558.

#Q6 After removing individuals whose OCC value is 0, find the 3 most common occupations 
#for “foreign born” and “native born”. You should provide occupation names, not OCC codes.
data %>% filter(OCC!=0, MET2013!=0, 0<INCTOT, INCTOT<9999999, EDUCD!=999) %>% 
  group_by(OCC) %>% summarise(Pop_OCC=sum(PERWT)) %>% arrange(desc(Pop_OCC)) %>% view()
#440:Other managers, 4720:Cashiers, 4760:	Retail salespersons

#Q7. (15 points) Generate data using the following codes: 
Q7 <- data %>% 
  filter(EDUCD!=999, INCTOT!=9999999 & INCTOT>0) %>% 
  mutate(fb=if_else(BPL>=150, "Foreign born", "Native born"), 
         cdegree=if_else(EDUCD>=101, 1, 0)) %>% 
  group_by(MET2013, fb) %>% 
  summarise(bachelor_sh=weighted.mean(cdegree, w=PERWT), 
            avg_wage=weighted.mean(INCTOT, w=PERWT), pop=sum(PERWT)) %>% 
  filter(pop>1000)

Q7 %>% ggplot(aes(x=bachelor_sh,y=avg_wage, color=fb)) + 
  geom_point(aes(shape=fb)) +
  geom_smooth() +
  labs(title="College premium:foreign born vs native born", y="Avg.Income", x=
         "Share of residents with bachelor's degree or higher") +
  theme(plot.title = element_text(size=10)) +
  scale_x_continuous(labels=scales::label_comma(scale=100, suffix="%")) +
  scale_y_continuous(labels=scales::label_comma(scale=0.001, prefix="$", suffix="K")) +
  scale_color_discrete(name=NULL) +
  theme(legend.position = c(0.15,0.9)) +
  theme(axis.title.x = element_text(size=8)) +
  theme(axis.text.x = element_text(size=8)) +
  theme(axis.title.y = element_text(size=8)) +
  theme(axis.text.y = element_text(size=8)) +
  theme(legend.text = element_text(size=8)) +
  guides(shape = FALSE)

#Q8. (10 points) By editing your codes on Q4, generate the following figure. (We will check your codes. 
#You don’t have to answer this question in your summary.) 
Q8 <- data %>% filter(EDUCD!=999) %>% 
  mutate(fb=if_else(BPL==500, "China", if_else(BPL==453, "German", "others")), 
         cdegree=if_else(EDUCD<=100, "No Bachelor's degree", 
                         if_else(EDUCD==101, "Bachelor's degree", "Graduate degree")) %>% 
           factor(levels=c("No Bachelor's degree", "Bachelor's degree", "Graduate degree")))

Q8 %>% filter(fb != "others") %>%
  ggplot() + geom_bar(aes(fb, fill=cdegree,weight=PERWT),position="fill") +
  labs(title="Educational attainment of foreign and native borns") +
  theme(plot.title = element_text(size=10)) +
  scale_y_continuous(name=NULL, labels=scales::label_comma(scale=100, suffix="%")) +
  scale_x_discrete(name=NULL) +
  scale_fill_discrete(name=NULL) +
  theme(legend.position = "bottom", legend.text = element_text(size=7)) +
  theme(axis.text.y = element_text(size=8)) +
  theme(axis.text.x = element_text(size=8)) +
  coord_flip()


#Q9 By editing your codes on Q5, calculate average wages for immigrants from China and 
# immigrants from Germany. 

Q9<- data %>% filter(EDUCD!=999, INCTOT>0, INCTOT<9999999) %>% 
  mutate(fb=if_else(BPL==500, "China", if_else(BPL==453, "Germany", "others")))

Q9 %>% filter(fb!="others") %>%  group_by(fb) %>% summarise(AVG_wage_fb=weighted.mean(INCTOT, w=PERWT))
#1 China       64304. 2 Germany      60085.

#Q10 By editing your codes on Q6, find the 3 most common occupations for immigrants from 
#China and immigrants from Germany. 
Q10<- data %>% filter(OCC!=0, MET2013!=0, 0<INCTOT, INCTOT<9999999, EDUCD!=999) %>% 
  mutate(fb=if_else(BPL==500, "China", if_else(BPL==453, "German", "others")))

Q10 %>% filter(fb!="others") %>% 
  group_by(BPL, OCC) %>% summarise(Pop_OCC=sum(PERWT))%>%
  arrange(desc(Pop_OCC)) %>% view()
#China: 1021:Software developers, 2205:Postsecondary teachers, 440:	Other managers
#German: 440: Other managers 4760:	Retail salespersons 10:Chief executives and legislators

#Q11 By editing your codes on Q7, generate the following figure.
Q11 <- data %>% 
  filter(EDUCD!=999, INCTOT!=9999999 & INCTOT>0) %>% 
  mutate(fb=if_else(BPL==500, "China", if_else(BPL==453, "Germany", "others")), 
         cdegree=if_else(EDUCD>=101, 1, 0)) %>% 
  group_by(MET2013, fb) %>% 
  summarise(bachelor_sh=weighted.mean(cdegree, w=PERWT), 
            avg_wage=weighted.mean(INCTOT, w=PERWT), pop=sum(PERWT)) %>% 
  filter(pop>1000)

Q11 %>% filter(fb!="others") %>%  ggplot(aes(x=bachelor_sh,y=avg_wage, color=fb)) + 
  geom_point(aes(shape=fb)) +
  geom_smooth() +
  labs(title="College premium:foreign born vs native born", y="Avg.Income", x=
         "Share of residents with bachelor's degree or higher") +
  theme(plot.title = element_text(size=10)) +
  scale_x_continuous(labels=scales::label_comma(scale=100, suffix="%")) +
  scale_y_continuous(labels=scales::label_comma(scale=0.001, prefix="$", suffix="K")) +
  scale_color_discrete(name=NULL) +
  theme(legend.position = c(0.15,0.9)) +
  theme(axis.title.x = element_text(size=8)) +
  theme(axis.text.x = element_text(size=8)) +
  theme(axis.title.y = element_text(size=8)) +
  theme(axis.text.y = element_text(size=8)) +
  theme(legend.text = element_text(size=8)) +
  guides(shape = FALSE)
