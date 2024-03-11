library(tidyverse)
library(dplyr)
library(ipumsr)


ddi <- read_ipums_ddi("data/usa_00007.xml")
data <- read_ipums_micro(ddi)

write_rds(data, "data/census07.rds")
head(data)


#clean_data <- data %>% select(YEAR,REGION, STATEFIP, MET2013, CITY, OWNERSHPD, PERWT, RACED, BPLD, INCTOT, OCC)
clean_data <- data %>% select(YEAR,REGION, STATEFIP, CITY, OWNERSHPD, BPLD, PERWT)
view(head(clean_data,40))

#limit birthplace to areas outside of the US, to determine Latin American immigrants
#remove N/A of ownership
filter_data <- clean_data %>% filter( BPLD == 26092 |
  (BPLD >= 30000 & BPLD <= 30091) |
  (BPLD >= 20000 & BPLD <= 26044),
  OWNERSHPD != 00, STATEFIP != 99 & STATEFIP != 97)

#change variable types 
summary_data <- filter_data %>% 
  mutate(StateLive = as_factor(STATEFIP),
         BirhtPlace = as_factor(BPLD),
         Ownership = as_factor(OWNERSHPD),
         Region = as_factor(REGION))

#across US Latin Immigrants pop > 100
g_data <- summary_data %>%
    group_by(BirhtPlace, Ownership, StateLive, Region, YEAR) %>% 
    summarize(count = n()) %>% 
    filter(Ownership == "Owned free and clear", count >100) %>% 
    select(BirhtPlace,Region,StateLive,Ownership, YEAR, count)

#ggplot - general owming a house in the US 
ggplot(g_data, aes(x = YEAR, y = count)) +
  geom_bar(stat = "identity", aes(fill = BirhtPlace))+
  labs(title = "Number of Latin American Immigrants Owning a House Over the Years",
       x = "Year",
       y = "Count")


grouped_data <- summary_data %>%
  group_by(BirhtPlace, YEAR) %>% 
  summarize(count = n()) %>% 
  filter(count>2000)

view(grouped_data)

#ggplot - general owming a house in the US 
ggplot(grouped_data, aes(x = YEAR, y = count)) +
  geom_bar(stat = "identity", aes(fill = BirhtPlace), colour = "black")+
  labs(title = "Number of Latin American Immigrants Owning a House Over the Years",
       x = "Year",
       y = "Count")


ggplot(grouped_data, aes(x = YEAR, y = count)) +
  geom_col(position = position_dodge(), aes(fill = BirhtPlace))+
  labs(title = "Number of Latin American Immigrants Owning a House Over the Years",
       x = "Year",
       y = "Count")

#######
#Latin Immigrants in US by region whose owership is own free and clear
df2 <- summary_data %>% 
  filter(Ownership == "Owned free and clear") %>% 
  group_by(Region, YEAR)%>%
  summarize(ImmigrantCount = n())

df2

#ggplot - general owming a house in the US 
ggplot(df2, aes(x = YEAR, y = ImmigrantCount)) +
  geom_line(aes(color=Region))+ geom_point(aes(color=Region))


#######
library(tidyverse)
library(dplyr)
library(ipumsr)

ddi <- read_ipums_ddi("data/usa_00007.xml")
data <- read_ipums_micro(ddi)
write_rds(data, "data/census07.rds")

#select variables to use 
clean_data <- data %>% select(YEAR,REGION, STATEFIP, CITY, OWNERSHPD, BPLD, PERWT)

#limit birthplace to Latin American countries 
#remove N/A of ownership
filter_data <- clean_data %>% filter( BPLD == 26092 |
                                        (BPLD >= 30000 & BPLD <= 30091) |
                                        (BPLD >= 20000 & BPLD <= 26044),
                                      OWNERSHPD != 00, STATEFIP != 99 & STATEFIP != 97)

#change variable types from numbers to str
summary_data <- filter_data %>% 
  mutate(StateLive = as_factor(STATEFIP),
         BirhtPlace = as_factor(BPLD),
         Ownership = as_factor(OWNERSHPD),
         Region = as_factor(REGION))

#Latin Immigrants in US by state whose ownership is own free and clear, population is > 500
df3 <- summary_data %>% 
  filter(Ownership == "Owned free and clear") %>% 
  group_by(StateLive, YEAR)%>%
  summarize(ImmigrantCount = n()) %>% 
  filter(ImmigrantCount >= 500)

#ggplot - owning a house in the US 
ggplot(df3, aes(x = YEAR, y = ImmigrantCount)) +
  geom_line(aes(color=StateLive))+ geom_point(aes(color=StateLive))+
  labs(color= "States",title = "Latin American Homeowners in Different American States",
       caption = "Source: e American Community Survey data")+
  scale_x_continuous(expand=c(0,0), limits =c(2006,2022),breaks=c(2006:2021),name="Years")+
  scale_y_continuous(name="Population")+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5))

#expand=c(0,0), limits =c(0,10000)

##################
# Group data by state and calculate homeownership rates
homeownership_rates <- filter_data %>%
  group_by(REGION, YEAR) %>%
  summarize(HomeownershipRate = sum(OWNERSHPD == 12) / n()) %>%
  ungroup()
homeownership_rates


homeownership_rates <- homeownership_rates %>%
  left_join(filter_data, by = "REGION")

# Rename state codes with state names for clarity
ddd <- homeownership_rates %>% 
  mutate(StateLive = as_factor(STATEFIP),
         BirhtPlace = as_factor(BPLD),
         Ownership = as_factor(OWNERSHPD),
         Region = as_factor(REGION))


# Create a bar chart to visualize homeownership rates
ggplot(ddd, aes(x = Region, y = HomeownershipRate)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Homeownership Rates of Latin American Immigrants by State",
       x = "State",
       y = "Homeownership Rate") +
  theme_minimal()


  