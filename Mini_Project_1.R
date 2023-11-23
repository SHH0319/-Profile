#Q1 Load the data to R and clean the data set as in Task of Dplyr lecture notes (page 14). Assign the dataset to an object named as “sample”. 
R codes
library(tidyverse)
library("ipumsr")
hmda <- read.csv("data/hmda_2014_ca_all-records_codes.csv")
temp <- hmda %>% filter(loan_purpose==1, property_type==1, 
                        action_taken==1| action_taken==3,loan_amount_000s<1000)
temp1 <- arrange(temp, desc(loan_amount_000s)) %>% 
  mutate(ap=if_else(action_taken==1,1,0))
sample <- select(temp1, 'applicant_race_1', 'ap', 'loan_amount_000s',
                 'denial_reason_1', 'applicant_income_000s')

#Q2. Calculate loan approval probability for each race as in the class task.
R codes
sample %>% group_by(applicant_race_1) %>% 
  summarise(loan_appproval_probability=mean(ap))

#Q3. Visualize your outcome in Q2 as in the class task (Hint: geom_col())
R codes
loan_approval_mean <- sample %>% group_by(applicant_race_1) %>% 
  summarise(loan_appproval_probability=mean(ap))
ggplot(loan_approval_mean, aes(applicant_race_1,loan_appproval_probability))+
  geom_col() + ggsave("temp/loan_approval_mean.png")
#Q4. Run the following codes. There are 6 operations including 4 pipes and 2 plus. Please explain all of them. 
R codes
sample %>% filter(!is.na(denial_reason_1)) %>% 
  count(denial_reason_1) %>% mutate(share=n/sum(n) )%>% 
  ggplot(aes(denial_reason_1, share)) + 
  geom_bar(stat="identity") + scale_x_discrete(limits=c(1:9))

1.	The first pipe of “filter(!is.na(denial_reason_1)) selects rows only if denial_reason_1 is not missing.”
2.	The Second pipe of “count(denial_reason_1)” counts the number of rows by each 9 denial reason.
3.	The Third pipe of “mutate(share=n/sum(n))” makes a new column that calculates the proportion of the number of each 9 denial reason compared to the total number of the rows. 
4.	The fourth pipe of “ggplot(aes(denial_reason_1, share))” makes a graph that x-axis shows denial_reason_1 and y-axis shows share
5.	The first plus of “geom_bar(stat="identity")” specifies the type of graph as a bar graph with the y-axis height as the data value.
6.	The Second plus of “scale_x_discrete(limits=c(1:9))” specifies to show the sequence of X-axis groups as column 1:9  within the graph.

#Q5. Run Q4 for “Black or African American”. Repeat for “White”.  What’s the main difference between two groups?
R codes
Black or African American
sample %>% filter(!is.na(denial_reason_1), applicant_race_1==3) %>% 
  count(denial_reason_1) %>% mutate(share=n/sum(n) )%>%
  ggplot(aes(denial_reason_1, share)) + 
  geom_bar(stat="identity") + scale_x_discrete(limits=c(1:9))

White
sample %>% filter(!is.na(denial_reason_1), applicant_race_1==5) %>% 
  count(denial_reason_1) %>% mutate(share=n/sum(n) )%>%
  ggplot(aes(denial_reason_1, share)) + 
  geom_bar(stat="identity") + scale_x_discrete(limits=c(1:9))


PART 2

Q1. Load the downloaded data, remove observations in “Not in identifiable area”. Then, calculate average ages and populations for MSAs.
R codes
library(tidyverse)
library("ipumsr")

usa_file <- ipums_example("data/usa_00001.xml")
usa_data <- read_ipums_micro("data/usa_00001.xml")
write_rds(usa_data,"data/census_pop_2019.rds")

usa_data %>% filter(MET2013!=0) %>%
  group_by(MET2013) %>% 
  summarise(average_age=mean(AGE), population=sum(PERWT)) %>%
  view()

Q2. Choose cities with more than 1 million population. Show the 10 youngest MSAs.
R codes
average_age_and_pop <- usa_data %>% filter(MET2013!=0) %>%
  group_by(MET2013) %>% 
  summarise(average_age=mean(AGE), population=sum(PERWT))

average_age_and_pop %>% filter(population>1000000) %>% arrange(average_age) %>% view()


Q3. Calculate population by AGE. Then, draw a diagram with x-axis = AGE and y-axis=population by AGE. For plotting the diagram, you can just modify “data”, “X”, and “Y” from the following codes: “ggplot(data, aex(X, Y)) + geom_point() + geom_line() + scale_x_discrete()”
R codes
population_by_age <- usa_data %>% filter(MET2013!=0) %>% group_by(AGE) %>% 
  summarize(Population=sum(PERWT)) %>% 
  ggplot(aes(x=AGE, y=Population)) + geom_point() + geom_line()
population_by_age

Q4. Isolate your sample with individuals whose AGE is between 31 and 55 and OCC is not N/A (“OCC !=0”). Then, create a new variable showing age groups as “31-35”, “36-40”, “41-45”, “46-50”, “51-55” using “if_else()” function. Show the 3 occupations with the most people by the age groups. You don’t have to provide OCC descriptions. If you are curious about what those occupations are, visit https://usa.ipums.org/usa/volii/occ2018.shtml.
R codes
Q4_usa_data <- usa_data %>% filter(MET2013!=0, AGE>=31, AGE<=55, OCC!=0)
Q4_usa_data_age_groups <- Q4_usa_data %>% mutate(age_groups=if_else(AGE>=31 & AGE<=35,"31-35", 
                                                                    if_else(AGE >= 36 & AGE <= 40, "36-40",
                                                                            if_else(AGE >= 41 & AGE <= 45, "41-45",
                                                                                    if_else(AGE >= 46 & AGE <= 50, "46-50",
                                                                                            if_else(AGE >= 51 & AGE <= 55, "51-55", "Other"))))))

Q4_arrange <- Q4_usa_data_age_groups %>% group_by(age_groups, OCC)%>%
  summarise(pop = n()) %>% 
  arrange(desc(pop)) 

top3_occ <- Q4_arrange %>%
  group_by(age_groups) %>%
  top_n(3, wt = pop) %>%
  arrange(age_groups) %>% 
  view()

Q5. For the age group we defined in Q4, show the 3 occupations with the highest incomes.
R codes
Q5_arrange <- Q4_usa_data_age_groups %>% filter(0 < INCTOT , INCTOT < 9999999)%>% 
  group_by(age_groups, OCC)%>%
  summarise(income = weighted.mean(INCTOT, w=PERWT)) %>% 
  arrange(desc(income))

top3_occ <- Q5_arrange %>%
  group_by(age_groups) %>%
  top_n(3, wt = income) %>%
  arrange(age_groups) %>% 
  view()
