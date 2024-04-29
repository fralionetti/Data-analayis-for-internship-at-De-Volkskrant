#STEP 1: reading the document and dysplaying it

#Installing the necessary packages
install.packages(c("tidyverse", "readxl"))
library(tidyverse)
library(readxl)
EXTERNALfuelsubsidiestemplate2023new_1_ <- read_excel("EXTERNALfuelsubsidiestemplate2023new (1).xlsx")

#Specifying the file path

excel_file <- "C:/Users/lionet000/Downloads/external-fuel-subsidies-template-2023-10-02 (1).xlsx"

#Specifying the sheet name I want to read
sheet_name <- "data"

#Reading only the specified sheet
IMF_data <- read_excel(excel_file, sheet = sheet_name)

#Displaying the first few rows of the tibble
head(IMF_data)

#STEP 2: keeping only the necessary columns


#selection the tidyverse-way, and I have given them names I understand
IMF_subdata<-IMF_data %>% 
  select(countryname, countrycode, region, year, Global_warming=mit.sub.glow.tot.all.1, mit_exrate, mit.pop.mn, mit.gdp.pre.lvl.1,
         Explicit_subsidies=mit.sub.texp.tot.all.1, Implicit_subsidies=mit.sub.timp.tot.all.1, Total_subsidies=mit.sub.tot.tot.all.1)
head(IMF_subdata)

example_NL<-IMF_subdata %>% 
  filter(countryname=="Netherlands", year==2022)
head(example_NL)

#hmmm 2 rows with the same data

count_year_country<-IMF_data %>% 
  count(countryname, year)
head(count_year_country)
# the same for every country/year - 
# I looked in the data and it is because there are different scenario's 
# Don't know if this matters for these numbers though, something to check

IMF_subdata_distinct<-IMF_subdata %>% 
  distinct() # only unique rows
head(IMF_subdata_distinct)


# count again
count_year_country2<-IMF_subdata_distinct %>% 
  count(countryname, year)
head(count_year_country2)
# now it is fine, so the scenario is not relevant

#The climate change variable is in millions, while the GDP one is in billions. So, I create a new climate change variable in billions
IMF_newdata <- IMF_subdata_distinct %>%
  mutate(climatechange_billions = Global_warming / 1000)

#I check that it worked. It seems correct
head(IMF_newdata$climatechange_billions)
example_NL2<-IMF_newdata %>% 
  filter(countryname=="Netherlands", year==2022)
print(example_NL2$climatechange_billions)

#creating a new variable: climate change as as a share of GDP (in billions)
IMF_newdata <- IMF_newdata %>%
  mutate(climatechange_GDP = (climatechange_billions / mit.gdp.pre.lvl.1) * 100)


#I check that it worked with a few examples. The results are a little different from the IMF ones: in the IMF dataset the value is 1 for the Netherland and Italy in 2022. It could be that the IMF rounds the result to 1 where they are > 0.5 and < 1
head(IMF_newdata$climatechange_GDP)

example_NL3<-IMF_newdata %>% 
  filter(countryname=="Netherlands", year==2022)
print(example_NL3$climatechange_GDP)

example_IT<-IMF_newdata %>% 
  filter(countryname=="Italy", year==2022)
print(example_IT$climatechange_GDP)

#Now, I create the explicit subsidies variable as a share of GDP. Again, I first need to transform the explicit subsidies variables in billions.
IMF_newdata <- IMF_newdata %>%
  mutate(explicitsubidies_billions = Explicit_subsidies / 1000)

#I check that it worked
head(IMF_newdata$explicitsubidies_billions)

example_NL4<-IMF_newdata %>% 
  filter(countryname=="Netherlands", year==2022)
print(example_NL4$explicitsubidies_billions)

#I create the new variable: explicit subsidies as a share of GDP
IMF_newdata <- IMF_newdata %>%
  mutate(explicitsubidies_GDP = (explicitsubidies_billions / mit.gdp.pre.lvl.1) * 100)

#I check that it worked
head(IMF_newdata$explicitsubidies_GDP)

example_NL5<-IMF_newdata %>% 
  filter(countryname=="Netherlands", year==2022)
print(example_NL5$explicitsubidies_GDP)

#Now, I also create the implicit subsidies variable a a share of GDP. 

IMF_newdata <- IMF_newdata %>%
  mutate(implicitsubidies_billions = Implicit_subsidies / 1000)

#I check that it worked
head(IMF_newdata$implicitsubidies_billions)

example_NL5<-IMF_newdata %>% 
  filter(countryname=="Netherlands", year==2022)
print(example_NL5$implicitsubidies_billions)

IMF_newdata <- IMF_newdata %>%
  mutate(implicitsubidies_GDP = (implicitsubidies_billions / mit.gdp.pre.lvl.1) * 100)

#I check that it worked
head(IMF_newdata$implicitsubidies_GDP)

example_NL6<-IMF_newdata %>% 
  filter(countryname=="Netherlands", year==2022)
print(example_NL6$implicitsubidies_GDP)

#Finally, I do the same with the total subsidies variable

IMF_newdata <- IMF_newdata %>%
  mutate(totalsubidies_billions = Total_subsidies / 1000)

#I check that it worked
head(IMF_newdata$totalsubidies_billions)

example_NL7<-IMF_newdata %>% 
  filter(countryname=="Netherlands", year==2022)
print(example_NL7$totalsubidies_billions)

IMF_newdata <- IMF_newdata %>%
  mutate(totalsubidies_GDP = (totalsubidies_billions / mit.gdp.pre.lvl.1) * 100)

#I check that it worked
head(IMF_newdata$totalsubidies_GDP)

example_NL8<-IMF_newdata %>% 
  filter(countryname=="Netherlands", year==2022)
print(example_NL8$totalsubidies_GDP)

head(IMF_newdata)

