library(tidyverse)

#covid <- read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv")
gdp <- read.csv(file = 'gdp.csv', header = FALSE)

country_names = c()
data_from = c()
latest_value = c()
change_3_months = c()
change_12_months = c()

#Get Country names 
range = seq(1,dim(gdp)[1],by = 6)
for (i in range){
  country_names = append(country_names, gdp[i,1])
}

#Get datafrom 
range = seq(2,dim(gdp)[1],by = 6)
for (i in range){
  data_from = append(data_from, gdp[i,1])
}

#Get latest value  
range = seq(3,dim(gdp)[1],by = 6)
for (i in range){
  latest_value = append(latest_value, gdp[i,1])
}

#Get 3 month change  
range = seq(4,dim(gdp)[1],by = 6)
for (i in range){
  change_3_months = append(change_3_months, gdp[i,1])
}

#Get 12 month change  
range = seq(5,dim(gdp)[1],by = 6)
for (i in range){
  change_12_months = append(change_12_months, gdp[i,1])
}

#Merge into a tibble 
gdp <- tibble(Countries=country_names,
              Latest_data_from = data_from,
              Latest_value=latest_value,
              Change_3_months=change_3_months,
              Change_12_months= change_12_months)
