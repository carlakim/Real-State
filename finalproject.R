library(ipumsr)
library(tidyverse)
library(dplyr)
library(ggmap)
library(sf)
library(tidycensus)
library(stringr)

ddi <- read_ipums_ddi("data/usa_00013.xml")
data <- read_ipums_micro(ddi)
write_rds(data, "data/usa_00013.rds")


#CLEANING DATA
proj <- data %>%
  filter(HHINCOME != 9999999, AGE %in% c(016:067), INCWELFR != 99999, 
         MET2013 != 00000, PERNUM %in% c(1:4), YEAR==2011, POVERTY!=000)


#SHARE OF WELFARE RECIPIENTS BY MET
proj %>% mutate(gotwelfare=if_else(INCWELFR>0, 1, 0)) %>% 
  group_by(MET2013) %>% 
  summarize(welfarep=weighted.mean(gotwelfare, w=HHWT)) %>% 
  arrange(desc(welfarep)) 


#SHARE OF WELFARE RECIPIENTS BY STATEFIP
x<- proj %>% mutate(gotwelfareS=if_else(INCWELFR>0, 1, 0)) %>% 
  group_by(STATEFIP) %>% 
  summarize(welfarepS=weighted.mean(gotwelfareS, w=HHWT)) %>% 
  mutate(welfarepS=welfarepS) %>% 
  arrange(desc(welfarepS))

view(x)

projs <- left_join(proj, x, by = join_by(STATEFIP == STATEFIP))
head(projs)

###########1
#MERGING DATASETS FOR MAP
var <- load_variables(2011, "acs5", cache=TRUE)


usa <- get_acs(geography = "state",
               variables = c(pop="B00001_001",
                             hh="B00002_001"),
               output="wide", year = 2011, geometry=TRUE)

usa <- usa %>% filter(!NAME %in% c("Hawaii", "Puerto Rico","Guam", "Alaska"))

usa$GEOID <- as.integer(usa$GEOID)

jj <- left_join(usa,x, by=join_by(GEOID == STATEFIP))

view(jj)

#removeNA row
#df <- na.omit(jj)

ggplot() +
  geom_sf(aes(fill=welfarepS, geometry=geometry), jj, color="white") +
  scale_fill_viridis_c(option="C") +
  theme_void() + labs(fill="",
                      title="Share of Welfare Recipients across the US",
                      caption="Source: American Community Survey and US Census Bureau")+
  theme(plot.title = element_text(hjust=0.5))




#####2
buy_data1 <- proj %>% filter(OWNERSHP!=0) %>% 
  mutate(imp=factor(if_else(POVERTY<=100, "Impoverished", "Not Poverished")), 
         buy=if_else(OWNERSHP==1, 1, 0)) %>% 
  select(STATEFIP, imp, buy, HHWT)

#OWNS A HOUSE Impoverished x Not impoverished 
buy_data1 <- buy_data1 %>% group_by(STATEFIP, imp) %>% 
  summarise(buyshare=weighted.mean(buy, w=HHWT)) %>% 
  select(STATEFIP, buyshare, imp)

usa_buy <- left_join(usa, buy_data1, by=join_by(GEOID==STATEFIP))

usa_buyNA<- na.omit(usa_buy)

ggplot(usa_buyNA) +
  geom_sf(aes(fill=buyshare, geometry=geometry), color="white") +
  scale_fill_viridis_c() +
  theme(plot.title = element_text(hjust = 0.5, size=15))+
  labs(fill=NULL, title=" Share of Home Ownership by State and Poverty Status",
       caption="Source: American Community Survey and US Census Bureau")+
  theme_void() +
  facet_wrap(~imp)


##### 3
beans <- proj %>%
  mutate(beans=if_else(POVERTY<=100, "At or Below the Poverty Line",
                       if_else(POVERTY<=500, "Making Up to 4x Above the Poverty Line",
                               if_else(POVERTY==501, "Making More Than 5x the Poverty Line", NA))),
         beans=factor(beans, levels=c("At or Below the Poverty Line",
                                      "Making Up to 4x Above the Poverty Line",
                                      "Making More Than 5x the Poverty Line")),
         buy=if_else(OWNERSHP==1, 1, 0)) %>%
           select(STATEFIP, buy, HHWT, beans)

bins <- beans %>% group_by(STATEFIP, beans) %>% 
  summarise(ownershare=weighted.mean(buy, w=HHWT)) %>% 
  select(STATEFIP, ownershare, beans)        

bingbong <- left_join(usa, bins, by=join_by(GEOID==STATEFIP))

bingbongNA<- na.omit(bingbong)

ggplot(bingbongNA) +
  geom_sf(aes(fill=ownershare, geometry=geometry), color="white") +
  scale_fill_viridis_c() +
  theme_void() + facet_wrap(~beans)+
  labs(fill="", title="Home Ownership Across the US by Poverty Bins",
       caption="Source: American Community Survey and US Census Bureau") +
  theme(plot.title = element_text(hjust = 0.5, size=15),
        legend.title= element_blank(), legend.position = "bottom",
        plot.caption = element_text(hjust = 0.1))
####4
proj %>% 
  mutate(index=if_else(POVERTY<=100, "At or Below the Poverty Line",
                       if_else(POVERTY<=500, "Making Up to 4x Above the Poverty Line",
                               if_else(POVERTY==501, "Making more than 5x the Poverty Line", NA))),
         index=factor(index, levels=c("At or Below the Poverty Line",
                                      "Making Up to 4x Above the Poverty Line",
                                      "Making more than 5x the Poverty Line"))) %>% 
  na.omit(index) %>% count(index) %>% 
  mutate(popshare=n/sum(n)) %>%
  ggplot() + geom_col(aes(x=index, y=popshare), fill="skyblue") + theme_bw() +
  scale_y_continuous(labels=scales::label_comma(suffix="%", scale=100)) +
  labs(x=NULL, y=NULL, title="Share of US Population by Income Range")

ggsave("temp/groupproj4.png")

#####5
#Top 5 states/states with highest and lowest homeownership rate→ homeownership by poverty bins
name_merge<- left_join(buy_data1, bins, by=join_by(STATEFIP==STATEFIP))

haha <- left_join(usa,name_merge, by=join_by(GEOID == STATEFIP))

gg <- haha %>% 
  filter(GEOID == 06 | GEOID == 12 | GEOID == 48 | GEOID == 26 | GEOID == 13 | GEOID == 16)

ggplot(gg)+
  geom_col(aes(x = NAME, y = ownershare, fill = beans), position = "dodge")+
  scale_x_discrete(limits = c("California","Texas","Florida","Michigan","Georgia", "Idaho"))+
  scale_y_continuous(labels=scales::label_comma(suffix="%", scale=100)) +
  labs(x=NULL, y=NULL, fill="Bins",
       title = "Share of Home Ownership by States with Varying Real State Markets", caption = "Source: American Community Survey and US Census Bureau")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size=20),
        legend.title= element_blank(), legend.position = "bottom",
        plot.caption = element_text(hjust = 0.1, size=14),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 15))
######6
ga_acs <- get_acs(geography = "county", 
                  variables = c(pop="B00001_001",hh="B00002_001"), 
                  state="GA", output="wide", year = 2011, geometry=TRUE)

ga_county <- ga_acs %>% filter(GEOID==13063 | GEOID==13067 |
                                 GEOID==13089 | GEOID==13121 | GEOID==13135) %>% 
  mutate(area=st_area(geometry) %>% as.numeric() *0.1^6) %>% 
  select(GEOID, NAME, area, geometry)

ga_proj <- proj %>% filter(COUNTYFIP==13063 | COUNTYFIP==13067 |
                             COUNTYFIP==13089 | COUNTYFIP==13121 | COUNTYFIP==13135) %>% 
  mutate(index=if_else(POVERTY<=100, "At or Below the Poverty Line",
                       if_else(POVERTY<=500, "Making Up to 4x Above the Poverty Line",
                               if_else(POVERTY==501, "Making More Than 5x the Poverty Line", NA))),
         index=factor(index, levels=c("At or Below the Poverty Line",
                                      "Making Up to 4x Above the Poverty Line",
                                      "Making More Than 5x the Poverty Line"))) %>% 
  na.omit(index) %>% filter(OWNERSHP==1) %>% 
  group_by(COUNTYFIP, index) %>% summarize(pop=sum(PERWT))

ga_county$GEOID <- as.integer(ga_county$GEOID)

ga_merge <- left_join(ga_county, ga_proj, by=join_by(GEOID==COUNTYFIP))

ga_merge <- ga_merge %>% 
  mutate(pop_density=pop/area)

ga_merge$NAME <- ga_merge$NAME %>% str_remove_all("County, Georgia")

ga_merge$group <- cut_interval(ga_merge$pop_density*0.1^3, 7)

ggplot(ga_acs) + geom_sf(aes(fill=pop_density), data=ga_merge, color="white") +
  geom_sf_label(aes(label=NAME), data=ga_merge, color="black") +
  scale_fill_viridis_c(option="C") + theme_void() + facet_wrap(~index) +
  labs(fill=NULL, 
       caption="Source: American Community Survey and US Census Bureau",
       title="Population Density of Home Owners in Metro Atlanta") +
  theme(plot.title = element_text(hjust = 0.5, size=15),
        legend.title= element_blank(), legend.position = "bottom",
        plot.caption = element_text(hjust = 0.1))




#############
ga_new <- proj %>% filter(GEOID==13063 | GEOID==13067 |
                            GEOID==13089 | GEOID==13121 | GEOID==13135) %>% 
  mutate(index=if_else(POVERTY<=100, "At or Below the Poverty Line",
                       if_else(POVERTY<=500, "Making Up to 4x Above the Poverty Line",
                               if_else(POVERTY==501, "Making More Than 5x the Poverty Line", NA))),
         index=factor(index, levels=c("At or Below the Poverty Line",
                                      "Making Up to 4x Above the Poverty Line",
                                      "Making More Than 5x the Poverty Line"))) %>% 
  na.omit(index) %>% filter(index=="At or Below the Poverty Line") %>% 
  group_by(GEOID) %>% summarize(top_pop=sum(PERWT)) %>% 
  mutate(popshare=top_pop/sum(top_pop))

ga_new$GEOID <- as.character(ga_new$GEOID)

yyy <- left_join(ga_new,ga_acs, by=join_by(GEOID == GEOID))




ggmap(ga_map) +
  geom_sf(aes(fill=popshare), data=yyy, color="white", alpha=0.6,
          inherit.aes=FALSE) +
  geom_sf_label(aes(label=NAME), data=yyy, color="black",
                inherit.aes=FALSE) +
  scale_fill_viridis_c(option="C") + theme_void() +
  labs(x=NULL, y=NULL, fill=NULL, 
       caption="Source: American Community Survey and US Census Bureau",
       title="Population of Impoverished Home Owners in Metro Atlanta") +
  theme(plot.title = element_text(hjust = 0.5, size=20),
        legend.title= element_blank(), legend.position = "bottom",
        plot.caption = element_text(hjust = 0.05, size=14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 8))




####FINAL
ga_acs <- get_acs(geography = "county", 
                  variables = c(pop="B00001_001",hh="B00002_001"), 
                  state="GA", output="wide", year = 2011, geometry=TRUE)

ga_county <- ga_acs %>% filter(GEOID==13063 | GEOID==13067 |
                                 GEOID==13089 | GEOID==13121 | GEOID==13135) %>% 
  mutate(area=st_area(geometry) %>% as.numeric() *0.1^6) %>% 
  select(GEOID, NAME, area, geometry)

proj$GEOID <- as.integer(proj$GEOID)

ga_proj <- proj %>% filter(GEOID==13063 | GEOID==13067 |
                             GEOID==13089 | GEOID==13121 | GEOID==13135) %>% 
  mutate(index=if_else(POVERTY<=100, "At or Below the Poverty Line",
                       if_else(POVERTY<=500, "Making Up to 4x Above the Poverty Line",
                               if_else(POVERTY==501, "Making More Than 5x the Poverty Line", NA))),
         index=factor(index, levels=c("At or Below the Poverty Line",
                                      "Making Up to 4x Above the Poverty Line",
                                      "Making More Than 5x the Poverty Line"))) %>% 
  na.omit(index) %>% filter(OWNERSHP==1) %>% 
  group_by(GEOID, index) %>% summarize(pop=sum(PERWT))

ga_county$GEOID <- as.integer(ga_county$GEOID)

ga_merge <- left_join(x=ga_county, y=ga_proj, by=join_by("GEOID"))

ga_merge <- ga_merge %>% 
  group_by(GEOID) %>% 
  mutate(shares = pop/sum(pop))

ga_merge

ga_merge$NAME <- ga_merge$NAME %>% str_remove_all("County, Georgia")

ga_merge$group <- cut_interval(ga_merge$shares,7)

ggplot(ga_acs) + geom_sf(aes(fill=shares), data=ga_merge, color="white") +
  geom_sf_label(aes(label=NAME), data=ga_merge, color="black") +
  scale_fill_viridis_c(option="C") + theme_void() + facet_wrap(~index) +
  labs(fill=NULL, 
       caption="Source: American Community Survey and US Census Bureau",
       title="Population Share of Home Owners in Metro Atlanta") +
  theme(plot.title = element_text(hjust = 0.5, size=20),
        legend.title= element_blank(), legend.position = "bottom",
        plot.caption = element_text(hjust = 0.1, size=14),
        strip.text = element_text(size = 12))




