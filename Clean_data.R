rm(list=ls())

library(nnet)
library(readxl)
library(foreign)
library(haven)
library(tidyverse)
library(dplyr)
library(pracma)
library(patchwork)
library(ggplot2)

#------------------------------------------------------------------------------#
# Load all the raw data
#------------------------------------------------------------------------------#

load(file = "Raw_data/gender_birth_new.Rda")

data2010 <- read_dta("Raw_data/cfps2010adult.dta")
data2012 <- read_dta("Raw_data/cfps2012adult.dta")
data2014 <- read_dta("Raw_data/cfps2014adult.dta")
data2016 <- read_dta("Raw_data/cfps2016adult.dta")
data2018 <- read_dta("Raw_data/cfps2018adult.dta")
data2020 <- read_dta("Raw_data/cfps2020adult.dta")

hh2014 <- read_dta("Raw_data/person2014.dta")
hh2018 <- read_dta("Raw_data/person2018.dta")

famecon2014 <- read_dta("Raw_data/cfps2014famecon.dta")
famecon2018 <- read_dta("Raw_data/cfps2018famecon.dta")

job_ttl     <- read_dta("Raw_data/job_ttl_main.dta")
house_price <- read_excel("Raw_data/house_price.xlsx")
child       <- read_dta("Raw_data/child_raw.dta")


#------------------------------------------------------------------------------#
# get ethnic code
#------------------------------------------------------------------------------#

minzu2010 <- data2010 %>% select(pid, qa5code) %>%
  mutate(han = case_when(qa5code==1 ~ 1,
                         qa5code>1 ~ 0)) %>% select(-qa5code)

minzu2012 <- data2012 %>% select(pid, qa701code) %>%
  mutate(han = case_when(qa701code==1 ~ 1,
                         qa701code>1 ~ 0)) %>% select(-qa701code)

minzu2014 <- data2014 %>% select(pid, qa701code) %>%
  mutate(han = case_when(qa701code==1 ~ 1,
                         qa701code>1 ~ 0)) %>% select(-qa701code)

minzu2016 <- data2016 %>% select(pid, pa701code) %>%
  mutate(han = case_when(pa701code==1 ~ 1,
                         pa701code>1 & pa701code<79 ~ 0)) %>% select(-pa701code)

minzu2018 <- data2018 %>% select(pid, qa701code) %>%
  mutate(han = case_when(qa701code==1 ~ 1,
                         qa701code>1 & qa701code<79 ~ 0)) %>% select(-qa701code)

minzu2020 <- data2020 %>% select(pid, qa701code) %>%
  mutate(han = case_when(qa701code==1 ~ 1,
                         qa701code>1 & qa701code<79 ~ 0)) %>% select(-qa701code)

minzu_ttl <- rbind(minzu2010, minzu2012, minzu2014, minzu2016, minzu2018, minzu2020) %>% 
  arrange(pid) %>% filter(!is.na(han)) %>% group_by(pid) %>% filter(row_number()==1) %>% ungroup()
minzu_ttl$pid <- as.numeric(minzu_ttl$pid)
mean(minzu_ttl$han) # 0.912648

#------------------------------------------------------------------------------#
# get household id
#------------------------------------------------------------------------------#

fid14 <- hh2014 %>% select(fid, pid) %>% mutate(year=2014)
fid18 <- hh2018 %>% select(fid, pid) %>% mutate(year=2018)

fid_ttl <- rbind(fid14, fid18)

#------------------------------------------------------------------------------#
# get education code
#------------------------------------------------------------------------------#

educ <- rbind(data2014 %>% select(pid, cfps2014edu) %>% rename(educ=cfps2014edu) %>%
                mutate(year=2014),
              data2018 %>% select(pid, cfps2018edu) %>% rename(educ=cfps2018edu) %>%
                mutate(year=2018)) %>%
  mutate(educ1 = case_when(educ<4 ~ "Less than High School",
                           educ==4 ~ "High School Graduate",
                           educ>=5 ~ "College and Higher")) %>%
  select(-educ)

#------------------------------------------------------------------------------#
# get employment code
#------------------------------------------------------------------------------#

employer <- rbind(data2014 %>% select(pid, qg2, jobclass) %>% mutate(year=2014),
                  data2018 %>% select(pid, qg2, jobclass) %>% mutate(year=2018))

employment <- rbind(data2014 %>% select(pid, employ2014) %>% 
                      mutate(year=2014,
                             employed = case_when(employ2014==1 ~ 1,
                                                  employ2014==0|employ2014==3 ~ 0)) %>%
                      select(-employ2014),
                    data2018 %>% select(pid, employ) %>% 
                      mutate(year=2018,
                             employed = case_when(employ==1 ~ 1,
                                                  employ==0|employ==3 ~ 0)) %>%
                      select(-employ))


#------------------------------------------------------------------------------#
# get province code
#------------------------------------------------------------------------------#

prov_data <-  rbind(data2010 %>% select(pid, provcd) %>% rename(prov=provcd) %>% mutate(year=2010),
                    data2012 %>% select(pid, provcd) %>% rename(prov=provcd) %>% mutate(year=2012),
                    data2014 %>% select(pid, provcd14) %>% rename(prov=provcd14) %>% mutate(year=2014),
                    data2016 %>% select(pid, provcd16) %>% rename(prov=provcd16) %>% mutate(year=2016),
                    data2018 %>% select(pid, provcd18) %>% rename(prov=provcd18) %>% mutate(year=2018),
                    data2020 %>% select(pid, provcd20) %>% rename(prov=provcd20) %>% mutate(year=2020))

prov_data$prov <- as.numeric(prov_data$prov)


#------------------------------------------------------------------------------#
# get urban status
#------------------------------------------------------------------------------#

urban_data <- rbind(data2010 %>% select(pid, urban) %>% mutate(year=2010),
                    data2012 %>% select(pid, urban12) %>% rename(urban=urban12) %>% mutate(year=2012),
                    data2014 %>% select(pid, urban14) %>% rename(urban=urban14) %>% mutate(year=2014),
                    data2016 %>% select(pid, urban16) %>% rename(urban=urban16) %>% mutate(year=2016),
                    data2018 %>% select(pid, urban18) %>% rename(urban=urban18) %>% mutate(year=2018),
                    data2020 %>% select(pid, urban20) %>% rename(urban=urban20) %>% mutate(year=2020)) %>%
  arrange(pid, year)

urban_data$urban <- as.numeric(urban_data$urban)

urban_easy <- urban_data %>% filter(urban>=0) %>% group_by(pid) %>% filter(row_number()==1) %>% ungroup() %>%
  select(pid, urban) %>% rename(urban2=urban)

#------------------------------------------------------------------------------#
# get housing price for year 2014 and 2018
#------------------------------------------------------------------------------#

housing_price <- pivot_longer(house_price,
                              cols = c(y2014, y2018),
                              names_to = "year",
                              values_to = "hprice") %>%
  mutate(year = case_when(year=="y2014" ~ 2014,
                          year=="y2018" ~ 2018))

#------------------------------------------------------------------------------#
# clean ideal number of children for all people
#------------------------------------------------------------------------------#

ideal14 <- data2014 %>% select(pid, qm501) %>% 
  rename(ideal=qm501) %>% mutate(year=2014) %>% filter(!is.na(ideal) & ideal>=0)
ideal18 <- data2018 %>% select(pid, qka202) %>% 
  rename(ideal=qka202) %>% mutate(year=2018) %>% filter(!is.na(ideal) & ideal>=0)

ideal14$ideal <- as.numeric(ideal14$ideal)
ideal18$ideal <- as.numeric(ideal18$ideal)

# available ideal number of children for all people
ideal_ttl <- rbind(ideal14, ideal18) %>% arrange(pid, year)
# merge with gender and birthy
ideal_ttl2 <- left_join(ideal_ttl, gender_birthy, by="pid")

length(unique(ideal_ttl2$pid)) # 41930


#------------------------------------------------------------------------------#
# get ideal number of children for interested couples
# age = 20 - 50 in 2014 and age = 20 - 50 in 2018
#------------------------------------------------------------------------------#

ideal_ttl3 <- ideal_ttl2 %>% mutate(age = year-birthy) %>%
  filter((gender==0 & age>=20 & age<=50 & year==2014) | (gender==0 & age>=20 & age<=50 & year==2018)) %>%
  left_join(data2014 %>% select(pid, qea0) %>% mutate(mar14=as.numeric(qea0)) %>%
              mutate(year=2014) %>% select(-qea0), by=c("pid","year")) %>%
  left_join(data2018 %>% select(pid, qea0) %>% mutate(mar18=as.numeric(qea0)) %>%
              mutate(year=2018) %>% select(-qea0), by=c("pid","year")) %>%
  mutate(mar=case_when(year==2014 ~ mar14, year==2018 ~ mar18)) %>% select(-mar14, -mar18) %>%
  # keep married women only
  filter(mar==2)
  
length(unique(ideal_ttl3$pid)) # 9862

table(ideal_ttl3$year) # 2014: 7644; 2018: 6738
table(ideal_ttl3$ideal)/sum(table(ideal_ttl3$ideal))

#------------------------------------------------------------------------------#
# get the number of children they have
#------------------------------------------------------------------------------#

# for people in time2010, get their children and corresponding birthy_c
household <- left_join(ideal_ttl3 %>% select(pid),child %>% select(pid,pid_c,birthy_c) %>%
                         rename(birthy_c2=birthy_c), by="pid", multiple = "all") %>%
  left_join(gender_birthy %>% rename(pid_c=pid, gender_c=gender, birthy_c=birthy), by="pid_c") %>%
  # use birthy_c in gender_birthy first; if lacking, use birthy after pid_c in famconf data
  mutate(birthy_c3 = case_when(!is.na(birthy_c) ~ birthy_c,
                               is.na(birthy_c) & !is.na(birthy_c2) ~ birthy_c2)) %>%
  select(-birthy_c,-birthy_c2) %>% rename(birthy_c=birthy_c3) %>%
  group_by(pid, pid_c) %>% filter(row_number()==1) %>% ungroup()

# delete those pid that have at least one missing birthy_c for children
temp <- household %>% filter(!is.na(pid_c) & is.na(birthy_c))
household2 <- household %>% filter(!pid %in% temp$pid) %>%
  left_join(gender_birthy, by="pid")

length(unique(ideal_ttl3$pid)) # 9862
length(unique(household2$pid)) # 9828
# 9828 people have no missing child birth year

# calculate number of children pid has in 2014 and 2018
household3 <- household2 %>% mutate(child14=as.numeric(birthy_c<=2014),
                                    child18=as.numeric(birthy_c<=2018)) %>%
  group_by(pid) %>% mutate(childnum14 = sum(child14, na.rm = TRUE),
                           childnum18 = sum(child18, na.rm = TRUE)) %>%
  filter(row_number()==1) %>% ungroup() %>% select(pid, childnum14, childnum18)

child_num <- gather(household3, year, N, childnum14:childnum18, factor_key=TRUE) %>%
  mutate(year = case_when(year=="childnum14" ~ 2014,
                          year=="childnum18" ~ 2018)) %>% arrange(pid, year)

# combine base data with number of children
ideal_ttl4 <- left_join(ideal_ttl3, child_num, by=c("pid","year")) %>%
  filter(N<=3 & !is.na(N))

length(unique(ideal_ttl4$pid))
# 9590

#------------------------------------------------------------------------------#
# Get the final data set
# set all greater than 2 to be 2
# use aggregate income, child educational costs, and 
# provincial-level housing price
#------------------------------------------------------------------------------#
prov_list=c(11,12,13,14,21,22,23,31,32,33,34,35,36,37,
            41,42,43,44,45,50,51,52,53,61,62)

data <- left_join(ideal_ttl4, prov_data, by=c("pid","year")) %>%
  # urban status
  left_join(urban_data, by=c("pid","year")) %>%
  left_join(urban_easy, by="pid") %>%
  mutate(urban3 = case_when(urban>=0 ~ urban,
                            urban<0 ~ urban2)) %>% select(-urban,-urban2) %>%
  rename(urban=urban3) %>% filter(!is.na(urban)) %>%
  # province
  mutate(prov = case_when(prov>0 ~ prov)) %>% filter(!is.na(prov) & prov %in% prov_list) %>%
  # ethnic
  left_join(minzu_ttl, by="pid") %>%
  filter(!is.na(han)) %>%
  # education
  left_join(educ, by=c("pid", "year")) %>% filter(!is.na(educ1)) %>%
  # employment
  left_join(employment, by=c("pid", "year")) %>% filter(employed>=0|is.na(employed)) %>%
  left_join(employer, by=c("pid", "year")) %>%
  mutate(employer = case_when(qg2==1|qg2==2|qg2==3 ~ "government",
                              qg2==5 ~ "foreign",
                              qg2==4|qg2==7|qg2==8 ~ "private",
                              qg2==6|qg2==9|qg2==77 ~ "other"),
         jobclass = case_when(jobclass==4|jobclass==5|jobclass==3 ~ "employed", # 农业打工算受雇不算农业
                              jobclass==1 ~ "agriculture",
                              jobclass==2 ~ "self-employed")) %>%
  # delete those that have jobs but with missing job class info
  filter(!(employed==1 & is.na(jobclass))) %>%
  # delete those that are employed but with missing employer info
  filter(!(jobclass=="employed" & is.na(employer))|is.na(jobclass)) %>% select(-qg2) %>%
  # change those who are not employed now to unemployed
  mutate(jobclass=case_when(employed==1 ~ jobclass, employed==0 ~ "unemployed"),
         employer=case_when(employed==1 ~ employer)) %>%
  # housing price
  left_join(housing_price, by=c("prov","year"))

table(data$ideal)/sum(table(data$ideal))
table(data$N)/sum(table(data$N))

data <- data %>%
  # keep N=1,2,3 and ideal=1,2,3 only
  filter(N>0 & N<=3 & ideal>0 & ideal<=3) %>%
  # set all greater than 2 to be 2
  mutate(N=case_when(N>0 & N<3 ~ N,
                     N==3 ~ 2),
         ideal=case_when(ideal>0 & ideal<3 ~ ideal,
                         ideal==3 ~ 2))

dim(data)[1]
# 12646
length(unique(data$pid))
# 8763

save(data, file = "Working_data/data_final.RData")

# EOF #


