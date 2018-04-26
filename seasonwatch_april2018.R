rm(list = ls())

library(tidyverse)
library(lubridate)

dat <- read.csv('user_tree_observations.csv',stringsAsFactors = F)

dat <- dat %>% select(date,is_leaf_mature,is_leaf_fresh,is_flower_bud,is_fruit_ripe,is_fruit_unripe,is_flower_open,user_tree_id,user_id)

treeid <- read.csv('user_tree_table.csv', stringsAsFactors = F) %>% select(user_tree_id,tree_id)

spcid <- read.csv('species_master.csv', stringsAsFactors =F ) %>% select(species_id,species_scientific_name)

trees <- read.csv('trees.csv', stringsAsFactors = F) %>% select(tree_Id,species_id, tree_location_id)

loc <- read.csv('location_master.csv', stringsAsFactors = F) %>% select(tree_location_id,state_id)

dat$date <- as.Date(dat$date)

dat <- dat %>% filter(date > '2010-01-01', date< '2018-01-01')

dat <- dat[!duplicated(dat[,c(1,8)]),]

dat <- left_join(dat,treeid)

dat <- left_join(dat,trees, by = c("tree_id"="tree_Id"))
dat <- left_join(dat,spcid)
dat <- left_join(dat,loc)


spcs_table <- dat %>% group_by(species_scientific_name) %>% summarise(n_distinct(user_tree_id))

dat <- dat %>% filter(state_id == 18, species_scientific_name %in% c('Artocarpus heterophyllus','Cassia fistula','Mangifera indica','Tamarindus indica','Phyllanthus emblica'))

dat$year <- year(dat$date)

tree_per_year <- dat %>% group_by(species_scientific_name,year) %>% summarise(n_tree = n_distinct(user_tree_id))

#  


val_ind <- dat %>% 
  group_by(species_scientific_name,year,user_tree_id) %>% 
  summarise(nobs = n_distinct(date)) %>% 
  filter(nobs >= 20) %>% 
  group_by(species_scientific_name,year) %>% 
  mutate(n_tree = n_distinct(user_tree_id)) %>%
  filter(n_tree>=10)

valdat <- inner_join(dat,val_ind[,2:3])

colnames(valdat)

valdat1 <- gather(valdat[,c(-9:-12,-14)],'phenophase','value',2:7)

valdat1$value[valdat1$value == -1] <- NA

valdat1$value[valdat1$value == 2] <- 1

valdat1$week <- week(valdat1$date)

valdat1 <- valdat1 %>% group_by(species_scientific_name,phenophase,user_tree_id,week) %>% mutate(value = mean(value,na.rm = T)) %>% filter(date == min(date)) %>% ungroup()

phenprop <- valdat1 %>% group_by(species_scientific_name,phenophase,year,week) %>% summarise(nobs = n(), prop = sum(value,na.rm = T)/nobs*100)

allweek <- expand.grid(species_scientific_name = unique(phenprop$species_scientific_name), phenophase = unique(phenprop$phenophase), year = 2015:2017,week = 1:53)

phenprop <- left_join(allweek,phenprop)

phenprop$datep <- ymd(paste(phenprop$year,'01','01',sep = '-')) + weeks(phenprop$week)

phenprop$datep <- as.Date(phenprop$datep)

#phenprop1 <- phenprop %>% filter(nobs > 10)

#phenprop1 <- left_join(allweek,phenprop1)


ggplot(data = phenprop) + geom_line(aes(x = datep,y = nobs),color = 'red', lty = 2) + geom_line(aes(x = datep,y = prop)) + facet_grid(phenophase~species_scientific_name)+ theme_bw()

####################

user_week <- valdat1 %>% group_by(species_scientific_name,year,user_tree_id) %>% summarise(weeksm = n_distinct(week))

valdat1 <- valdat1 %>% arrange(species_scientific_name,phenophase,date, user_tree_id)

weekrun <- valdat1 %>% group_by(species_scientific_name,phenophase,year,user_tree_id) %>% mutate(diff = c(1,diff(week)))%>% arrange(species_scientific_name,phenophase,year, user_tree_id) %>% do(longest_run = max(rle(.$diff)$lengths))

weekrun$longest_run <- unlist(weekrun$longest_run)

ggplot(weekrun) + geom_histogram(aes(longest_run)) + facet_grid(phenophase~species_scientific_name)

######################

fortdat <- valdat1 

fortdat$day <- day(fortdat$date)
fortdat$day <- ifelse(fortdat$day >14, 15,1)
fortdat$fortnight <- as.Date(paste(year(fortdat$date),month(fortdat$date),fortdat$day, sep = '-'))

fortdat <- fortdat %>% group_by(species_scientific_name,phenophase,user_tree_id,fortnight) %>% summarise(value = mean(value,na.rm = T))

fortdat$day <- day(fortdat$fortnight)

fortdat$fort_no <- ifelse(fortdat$day == 1, 1,2)

fortdat$fort_no <- (month(fortdat$fortnight)-1)*2 + fortdat$fort_no

fortdat$year <- year(fortdat$fortnight)

n_fort <- fortdat %>% group_by(species_scientific_name,phenophase,year,user_tree_id) %>% summarise(n_forts = n_distinct(fortnight))

ggplot(n_fort) + geom_histogram(aes(n_forts)) + facet_grid(phenophase~species_scientific_name)

fortrun <- fortdat %>% group_by(species_scientific_name,phenophase,user_tree_id) %>% mutate(diff = c(1,diff(fort_no)))%>% arrange(species_scientific_name,phenophase, user_tree_id) %>% do(longest_run = max(rle(.$diff)$lengths))

fortrun$longest_run <- unlist(fortrun$longest_run)

ggplot(fortrun) + geom_histogram(aes(longest_run)) + facet_grid(phenophase~species_scientific_name)

t <- valdat1 %>% filter(species_scientific_name == "Artocarpus heterophyllus",year == 2012, user_tree_id == 2055,phenophase == 'is_flower_bud')




sp_year_tree <- val_ind %>% group_by(species_scientific_name, year) %>% slice(1)
