#install.packages('RMySQL')
##install.packages("dplyr")
#-------------------------------- INIT -------------------------------------#
require(RMySQL) #if already installed
library(dplyr)
#-------- CSV reading --------------------#

under_nour <- read.csv("undernourished.csv")
dispo_alim <- read.csv("dispo_alim.csv")
cereal <- read.csv("Cereal.csv")
pop <- read.csv("Population.csv")
head(pop)
head(under_nour)
head(dispo_alim)
head(cereal)
#-------- BDD connexion -------------------#
db_fao <- dbConnect(RMySQL::MySQL(), host = "localhost",dbname="fao",user = "root", password = "")

#----------------- Questions -------------------------------------------------#

## Q.1 : Taille de la population mondiale ?
## SQL Query 
dbGetQuery(db_fao, "SELECT Year, SUM(value) FROM population group by Year;")
##R Query
sum_pop_by_year <-  pop %>% group_by(Year) %>% summarise(total = sum(Value))
sum_pop_by_year

##Q 2.2 Comment mesure-t-on la disponibilité alimentaire ?
head(dispo_alim)
# Voici comment nous avons obtenu le fichier actuel de la dispo_alim
#df %>% mutate(dispo_alim_tonnes = production + import_quantity - export_quantity + stock_varaition)
#df %>% mutate(dispo_alim_hum = sum(dispo_alim_tonnes) - feed - seed - processing - other_uses - losses)
#df %>% mutate(dispo_alim_ann_perhab = (dispo_alim_hum/Value)*1000)
#df %>% mutate(dispo_alim_day_perhab = (dispo_alim_ann_perhab/365)*1000)
#df %>% mutate(dispo_alim_kcal_p_j = (dispo_alim_ann_perhab*food_supply_quantity_kgcapitayr)/100)
#df %>% mutate(dispo_prot = (dispo_alim_ann_perhab*protein_supply_quantity_gcapitaday)/100)
#df %>% mutate(dispo_mat_gr = (dispo_alim_ann_perhab*fat_supply_quantity_gcapitaday)/100)

#Q2.3 Top 20 des aliments
top_20 <- dispo_alim %>% select(item, dispo_alim_kcal_p_j, dispo_prot) %>%  group_by(item) %>% 
  summarise(ratio_kcal = mean(dispo_alim_kcal_p_j)) %>% arrange(desc(ratio_kcal)) %>% head(20)
top_20
#Q 2.4 Dispo mondiale En végétaux uniquement
dispo_alim %>% filter(origin == "vegetal" & year == "2018") %>% group_by(origin) %>% summarise(dispo = sum(dispo_alim_tonnes))
dispo_alim %>% filter(origin == "vegetal" & year == "2018") %>% group_by(origin) %>% summarise(dispo = sum(dispo_alim_kcal_p_j))
## Q.3.1 Proportion de la pop en sous-nutrition ?
head(under_nour)
under_nour[is.na(under_nour)] <- 0
by_area <- under_nour %>% group_by(Year) %>% summarise(total = ((sum(Value) * 100000)*100)/7800000000)
by_area

## Q.3.2
decade2000_2002 <- under_nour %>% filter(Year == "2000-2002" & Value != 0) %>% 
  group_by(Area.Code, Area) %>% summarise(res = max(Value))  %>% arrange(desc(res)) %>% head(5)
decade2017_2019 <- under_nour %>% filter(Year == "2017-2019"& Value != 0) %>% 
  group_by(Area.Code, Area) %>% summarise(res = max(Value)) %>%  arrange(desc(res))%>% head(5)

decade2000_2002
decade2017_2019 

#------------------------------------ Query SQL ---------------------------------------

#SELECT country, MAX(dispo_alim_tonnes) FROM `dispo_alim` GROUP BY country
#SELECT country, MAX(`dispo_alim_kcal_p_j`) FROM `dispo_alim` GROUP BY country
#SELECT country, MAX(`dispo_prot`) FROM `dispo_alim` GROUP BY country

#SELECT country, year, MIN(dispo_alim_tonnes) FROM `dispo_alim` GROUP BY year
#SELECT country, year, MIN(dispo_alim_kcal_p_j) FROM `dispo_alim` GROUP BY year
#SELECT country, year, MIN(dispo_prot) FROM `dispo_alim` GROUP BY year


#select year, country, SUM(loses) from equilibre_prod group by year

#SELECT Area, SUM(Value) from undernourished where Year = "2017-2019"  GROUP BY Area LIMIT 10
