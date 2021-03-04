library(tidyverse)

original_covid_data <- read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv") #DO NOT MODIFY  THIS VARIABLE
covid <- read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv")
gdp <- read.csv(file = 'gdp.csv', header = FALSE)


#DATA PREPROCESSIG 

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


#Check which countries from gdp are not in covid dataset 
gdp.distinct.country = gdp %>% distinct(Countries)
covid.distinct.country = covid %>% distinct(Country)

indexes = gdp.distinct.country$Countries %in% covid.distinct.country$Country
not_found = gdp.distinct.country$Countries[!indexes]

#Make sure they are not written as something else
#For this we are checking for a substring of each not found country in the covid dataset 
for (country in not_found){
  for (i in 1:dim(covid.distinct.country)[1]){
    subs = substr(country,1,3)
    if (grepl(subs, covid.distinct.country[i,1]$Country, fixed = TRUE)){
      cat(country, ":", covid.distinct.country[i,1]$Country, "\n")
    }
  }
}


#Change the names of gdp coutries to match the covid countries dataset
i = which(gdp$Countries == 'Bolivia')
gdp$Countries[i] = "Bolivia (Plurinational State of)"

i = which(gdp$Countries == 'Brunei')
gdp$Countries[i] = "Brunei Darussalam"

i = which(gdp$Countries == 'Iran')
gdp$Countries[i] = "Iran (Islamic Republic of)"

i = which(gdp$Countries == 'Moldova')
gdp$Countries[i] = "Republic of Moldova"

i = which(gdp$Countries == 'Palestine')
gdp$Countries[i] = "occupied Palestinian territory, including east Jerusalem"

i = which(gdp$Countries == 'Russia')
gdp$Countries[i] = "Russian Federation"

i = which(gdp$Countries == 'Tanzania')
gdp$Countries[i] = "United Republic of Tanzania"

i = which(gdp$Countries == 'United Kingdom')
gdp$Countries[i] = "The United Kingdom"

i = which(gdp$Countries == 'Vietnam')
gdp$Countries[i] = "Viet Nam"

i = which(gdp$Countries == 'USA')
gdp$Countries[i] = "United States of America"

####Other names have to "other" names and this other name can be found on google and mapped to the covid dataset 
i = which(gdp$Countries == 'Burma (Myanmar)')
gdp$Countries[i] = "Myanmar"

i = which(gdp$Countries == 'Cape Verde')
gdp$Countries[i] = "Cabo Verde"

i = which(gdp$Countries == 'South Korea')
gdp$Countries[i] = "Republic of Korea"




#Sanity check  
gdp.distinct.country = gdp %>% distinct(Countries)
covid.distinct.country = covid %>% distinct(Country)

indexes2 = gdp.distinct.country$Countries %in% covid.distinct.country$Country
not_found2 = gdp.distinct.country$Countries[!indexes2]
cat("From all the countries in the gdp dataset only these were not found in the covid dataset: ",not_found2,"\n")

#FILTER THE COUNTRIES IN GDP THAT ALSO BELONG TO COVID DATASET
gdp = gdp %>% filter(Countries != not_found2)
gdp = gdp[-c(which(gdp$Countries == "Macao")),] #Had to remove manually

#Get rows of interest from covid dataset 
row_index = covid$Country %in% gdp$Countries
covid = covid[row_index,]


##############################################################
covid <- covid %>% arrange(Date_reported)


get.quarter.info = function(covid){
  covid_quarter = tibble(quarter = "dmmy", country = "dmmy", starting_cases = -1, ending_cases = -1, total_cases = -1,
                         starting_deaths = -1, ending_deaths = -1, total_deaths = -1)
  
  all_countries = covid %>% distinct(Country)
  for (country_name in all_countries){
    one_country <- covid %>% filter(Country == country_name)
    Q1 = subset(one_country,Date_reported >= "2020-01-01" & Date_reported <= "2020-03-31")
    Q2 = subset(one_country,Date_reported >= "2020-04-01" & Date_reported <= "2020-06-30")
    Q3 = subset(one_country,Date_reported >= "2020-07-01" & Date_reported <= "2020-09-30")
    Q4 = subset(one_country,Date_reported >= "2020-10-01" & Date_reported <= "2020-12-31")
    
    Q1min =  Q1 %>% filter(Date_reported == min(Q1$Date_reported))
    Q1max =  Q1 %>% filter(Date_reported == max(Q1$Date_reported))
    
    Q2min =  Q2 %>% filter(Date_reported == min(Q2$Date_reported))
    Q2max =  Q2 %>% filter(Date_reported == max(Q2$Date_reported))
    
    Q3min =  Q3 %>% filter(Date_reported == min(Q3$Date_reported))
    Q3max =  Q3 %>% filter(Date_reported == max(Q3$Date_reported))
    
    Q4min =  Q4 %>% filter(Date_reported == min(Q4$Date_reported))
    Q4max =  Q4 %>% filter(Date_reported == max(Q4$Date_reported))
    
    covid_quarter = covid_quarter %>% add_row(quarter="Q1 / 2020", country = country_name,
                                              starting_cases= Q1min$Cumulative_cases, ending_cases = Q1max$Cumulative_cases, total_cases = Q1max$Cumulative_cases - Q1min$Cumulative_cases,
                                              starting_deaths= Q1min$Cumulative_deaths, ending_deaths = Q1max$Cumulative_deaths, total_deaths = Q1max$Cumulative_deaths - Q1min$Cumulative_deaths)
    
    covid_quarter = covid_quarter %>% add_row(quarter="Q2 / 2020", country = country_name,
                                              starting_cases= Q2min$Cumulative_cases, ending_cases = Q2max$Cumulative_cases, total_cases = Q2max$Cumulative_cases - Q2min$Cumulative_cases,
                                              starting_deaths= Q2min$Cumulative_deaths, ending_deaths = Q2max$Cumulative_deaths, total_deaths = Q2max$Cumulative_deaths - Q2min$Cumulative_deaths)
    
    covid_quarter = covid_quarter %>% add_row(quarter="Q3 / 2020", country = country_name,
                                              starting_cases= Q3min$Cumulative_cases, ending_cases = Q3max$Cumulative_cases, total_cases = Q3max$Cumulative_cases - Q3min$Cumulative_cases,
                                              starting_deaths= Q3min$Cumulative_deaths, ending_deaths = Q3max$Cumulative_deaths, total_deaths = Q3max$Cumulative_deaths - Q3min$Cumulative_deaths)
    
    covid_quarter = covid_quarter %>% add_row(quarter="Q4 / 2020", country = country_name,
                                              starting_cases= Q4min$Cumulative_cases, ending_cases = Q4max$Cumulative_cases, total_cases = Q4max$Cumulative_cases - Q4min$Cumulative_cases,
                                              starting_deaths= Q4min$Cumulative_deaths, ending_deaths = Q4max$Cumulative_deaths, total_deaths = Q4max$Cumulative_deaths - Q4min$Cumulative_deaths)
  }
  
  return(covid_quarter[-1,]) #Return all but first row because that was a dummy row
}

Qcovid = get.quarter.info(covid = covid)

######## USE 'gdp' AND 'Qcovid' DATAFRAMES 

copy_Qcovid = Qcovid
covid_gdp = data.frame(gdp[1:4])
covid_gdp = covid_gdp %>% filter(Latest_data_from == "Q3 / 2020") #data from 2020 only
covid_gdp = data.frame(merge(copy_Qcovid, covid_gdp, by.x = c("country", "quarter"), by.y = c("Countries", "Latest_data_from")))
covid_gdp$case_increase = unlist(c(covid_gdp[4] - covid_gdp[3]))
covid_gdp = data.frame(covid_gdp[,c(1, 9:11)])

population = read_csv('https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_TotalPopulationBySex.csv')
population = population %>% filter(Time == "2019")
population = data.frame(population[,c(2,9)])
i = which(population$Location == "United Kingdom")
population$Location[i] = "The United Kingdom"
i = which(population$Location == "State of Palestine")
population$Location[i] = "occupied Palestinian territory, including east Jerusalem"
covid_gdp = merge(population, covid_gdp, by.x = "Location", by.y = "country")

covid_gdp$prp_case_increase = unlist(c(covid_gdp[5] / covid_gdp[2]))
covid_gdp$gdp_per_capita = as.numeric(unlist(covid_gdp[3])) / covid_gdp[2]
#covid_gdp = data.frame(covid_gdp[,c(1,4,6,7)])

######## 
#`covid_gpa`:
# Change_3_months - %GDP change from Q2
# prp_Case_increase - proportion of the population infected Q3
# gdp_per_capita - total GDP per country population

