library(tidyverse)
library(knitr)
library(scales)
library(janitor)
library(lubridate)

# Neerslagtekort vanaf 1906
# https://climexp.knmi.nl/data/int_nl.dat
tekort<-read.table("https://climexp.knmi.nl/data/int_nl.dat", 
                   header=FALSE, skip=11) 
print(tekort)

#Looking at the data until 2022
historischtekortbewerkt <- tekort %>% 
  mutate(Datum = parse_date(as.character(V1), "%Y%m%d"), tekort = V2) %>% 
  select(-V1, -V2) %>%
  filter(year(Datum) <= 2022) %>%  # Filter data up to 2022
  mutate(maand_dag = format(Datum, "%m-%d")) %>% 
  group_by(maand_dag) %>% 
  summarise(
    median_1906_202 = median(tekort, na.rm = TRUE),
    max_1906_2022 = max(tekort, na.rm = TRUE)
  )

# Keep 2023 values in the original tekort dataset
tekort_2023 <- tekort %>% 
  mutate(Datum = parse_date(as.character(V1), "%Y%m%d"), tekort = V2) %>% 
  filter(year(Datum) == 2023) %>% 
  mutate(maand_dag = format(Datum, "%m-%d")) %>% 
  select(maand_dag, tekort)
print(tekort_2023)

#join the data together
drought_NL <- right_join(historischtekortbewerkt, tekort_2023, by = "maand_dag")
print(drought_NL)

drought_NL %>% write_csv("C:/Users/lionet000/Documents/climate change/Dorught in the Netherlands.csv")
drought_NL %>% write.table( "clipboard", sep="\t", dec=",", row.names=FALSE, col.names=TRUE)
