library(tidyverse)
original_covid_data = read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv")

################### Average Number of New Cases Per Quarter ###################
covid <- original_covid_data %>% arrange(Date_reported)


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
quarter1 = Qcovid %>% filter(quarter == "Q1 / 2020")
quarter2 = Qcovid %>% filter(quarter == "Q2 / 2020")
quarter3 = Qcovid %>% filter(quarter == "Q3 / 2020")
quarter4 = Qcovid %>% filter(quarter == "Q4 / 2020")


mean_cases_per_q = tibble(quarter = c(1,2,3, 4),
                          means = c(mean(quarter1$total_cases),
                                    mean(quarter2$total_cases),
                                    mean(quarter3$total_cases),
                                    mean(quarter4$total_cases)))

ggplot(data=mean_cases_per_q,aes(x=quarter, y=means)) + 
  geom_line()+
  geom_point()+
  xlab("Quarter (ie. 1:Q1)")+
  ylab("Number of Cases") +
  ggtitle("Average Number of New Cases per Quarter")

summary(quarter3$total_cases)


################################################################################ 