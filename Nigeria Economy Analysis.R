# An Exploratory analysis in the Nigeria Economy
# Loading Required Packages
library(readxl)
library(tidyverse)
library(lubridate)
library(skimr)
library(plotly)
library(corrplot)

# Loading the dataset in
Nigeria_econ <- read_excel("C:/Users/LOLADE/Desktop/Dataset/Nigeria Economy.xlsx")

# Exploratory Analysis
view(Nigeria_econ)
str(Nigeria_econ)
skim_without_charts(Nigeria_econ)
summary(Nigeria_econ)

# Creating some plots to do a better Exploratory Analysis
Nig_cor <- cor(Nigeria_econ) # Creating a correlation matrix of c(-1.1)
corrplot(Nig_cor, type = "upper") # Creating the correlation Plot
names(Nigeria_econ)

##### Analysis on the Inflation Rate in the Economy per year #####
Nigeria_econ %>% 
  select(Year,`Inflation rate`) %>% 
  arrange(desc(`Inflation rate`)) %>% 
  print(n = 5)
# The Inflation was highest in the year 1995 in the economy with the inflation hitting 72.8 percent
# There was aa massive decline in the inflation rate from 1995 to 2000

plot_ly(x = ~Year, y = ~`Inflation rate`, data = Nigeria_econ) %>% 
  add_lines() %>% 
  layout(
    title = "Inflation Rate Per Year(1990-2023) in the Nigeria Economy",
    xaxis = list(
      title = "Year"),
    yaxis = list(
      title = "Inflation Rate")
) # Plotting the graph of the Inflation rate per year

##### Analysis for the rate of Government debt per year #####
Nigeria_econ %>% 
  select(Year,`Government debt`) %>% 
  arrange(desc(`Government debt`)) %>% 
  print(n = 5)
# The Government debt was highest in the year 1991 with the Government debt rate hitting 75 percent,
# and it was lowest in the year 2008 with the debt rate hitting 0.073 percent
# There is a steady increase in the inflation rate from 2008 till 2023

plot_ly(x = ~Year, y = ~`Government debt`, data = Nigeria_econ,name = "scatter") %>% 
  add_lines() %>% layout(
    title = "Government Debt Per Year(1990-2023) in the Nigeria Economy",
    xaxis = list(
      title = "Year"),
    yaxis = list(
      title = "Government Debt")
) # Plotting the Government debt rate per year

##### Analysis on the Umemployment rate in the Economy per year #####
Nigeria_econ %>% 
  select(Year,Unemployment) %>% 
  arrange(desc(Unemployment)) %>% 
  print(n = 5)
# The Inflation was highest in the year 2023 in the economy with the inflation hitting 40.6 percent
# There was a massive spike in the unemployment rate starting from the year 2019 till 2023 and this could have happened due
# to the global pandemic
plot_ly(x = ~Year, y = ~Unemployment, data = Nigeria_econ) %>% 
  add_lines() %>% 
  layout(
    title = "Unemployment Rate Per Year(1990-2023) in the Nigeria Economy",
    xaxis = list(
      title = "Year"),
    yaxis = list(
      title = "Unemployment Rate")
) # Plotting the graph of the Inflation rate per year


