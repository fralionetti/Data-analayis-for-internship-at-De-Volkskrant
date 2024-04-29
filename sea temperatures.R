library(tidyverse)
library(knitr)
library(scales)
install.packages('janitor')
library(janitor)
library(jsonlite)


zeewater<-fromJSON("https://climatereanalyzer.org/clim/sst_daily/json/oisst2.1_world2_sst_day.json") %>% 
  unnest()
head(zeewater)

zeewatergrafiek<-zeewater %>% 
  group_by(name) %>%
  mutate(date = 
           seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day")) %>% 
  # mutate(dagmaand=format(as.character(date), "%d-%m")) %>% 
  mutate(datanieuw=case_when(!leap_year(as.numeric(name))&month(date)>2~lag(data),TRUE~data)) %>% 
  filter(!(!leap_year(as.numeric(name))&day(date)==29&month(date)==2)) %>% 
  pivot_wider(id_cols = date, names_from=name, values_from = datanieuw) %>% 
  arrange(date) %>% 
  mutate(dagmaand=format(date,"%d-%m")) %>% 
  select(dagmaand,  everything(), -date)
head(zeewatergrafiek)

zeewatergrafiek %>% write_csv("C:/Users/lionet000/Documents/climate change/sea level.csv")


artic_seaice_extent <- read_csv("C:/Users/lionet000/Downloads/arctic-sea-ice-extent.csv")
print(artic_seaice_extent)
 
artic_seaice_extent <- artic_seaice_extent %>% 
  select('1981-2010 Median', '2020', '2021', '2022', '2023', '2024', '1979', 'Date')
head(artic_seaice_extent)

artic_seaice_extent %>% write_csv("C:/Users/lionet000/Documents/climate change/Artic Sea Ice extent2.csv")

antartic_seaice_extent <- read_csv("C:/Users/lionet000/Downloads/arctic-sea-ice-extent (3).csv")
print(antartic_seaice_extent)

antartic_seaice_extent <- antartic_seaice_extent %>% 
  select('1981-2010 Median', '2020', '2021', '2022 (Record minimum)', '2023', '2024', '1979', 'Date')
head(antartic_seaice_extent)

antartic_seaice_extent %>% write_csv("C:/Users/lionet000/Documents/climate change/Antartic Sea Ice extent.csv")


