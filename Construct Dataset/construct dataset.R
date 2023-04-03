library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(stringi)
library(readxl)
library(tidyverse)
library(rstudioapi)


#set working directory
setwd(dirname(getActiveDocumentContext()$path)) 

input_path = paste0(getwd(), "/Input/")
output_path = paste0(getwd(), "/Output/")

#load raw data
pwt = read_excel(paste0(input_path, "pwt1001.xlsx"), sheet = "Data")
wdi = read_excel(paste0(input_path, "wdi.xlsx"), sheet = "Data", na = "..")
euro = read_excel(paste0(input_path, "eurostat.xlsx"), sheet = "Sheet 1")

#make vector of countries
countries = c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", 
              "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", 
              "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Norway", "Poland", "Portugal", "Romania",
              "Slovakia", "Slovenia", "Spain", "Sweden", "United Kingdom")

#rename countries in wdi data
wdi$`Country Name`[which(wdi$`Country Name` == "Czechia")] = "Czech Republic" 
wdi$`Country Name`[which(wdi$`Country Name` == "Slovak Republic")] = "Slovakia" 

#rename countries in pwt
colnames(euro)[1] = "country"
euro$country[which(euro$country == "Czechia")] = "Czech Republic"
euro$country[which(euro$country == "Slovak Republic")] = "Slovakia" 
euro$country[which(euro$country == "Germany (until 1990 former territory of the FRG)")] = "Germany"

###Energy
energy = data.frame(year = seq(2000, 2019), country = rep(countries, each = length(seq(2000, 2019))) , eneruse = NA )

for (i in countries) {
  vec = as.numeric(euro[which(euro$country == i), seq(2, 40, by = 2)])
  
  energy$eneruse[which(energy$country == i)] = vec
  
} 


##Capital
capital = data.frame(year = seq(2000, 2019), country = rep(countries, each = length(seq(2000, 2019))) , capital = NA )

for (i in countries) {
  vec = as.numeric(pwt$cn[which(pwt$country == i & pwt$year %in% seq(2000, 2019))])
  
  capital$capital[which(capital$country == i)] = vec
  
} 

##gdp
gdp = data.frame(year = seq(2000, 2019), country = rep(countries, each = length(seq(2000, 2019))) , gdp = NA )

for (i in countries) {
  vec = as.numeric(pwt$cgdpo[which(pwt$country == i & pwt$year %in% seq(2000, 2019))])
  
  gdp$gdp[which(gdp$country == i)] = vec
  
} 

##Rest from WDI
rest = data.frame(year = seq(2000, 2019), country = rep(countries, each = length(seq(2000, 2019))),
                  labor = NA, pop = NA, ghg = NA, co2 = NA, gdp = NA, renew = NA, urban = NA, indgdp = NA)

#loop to fill data frame
for (i in countries) {
  for (j in seq(1, 8)) {
    
    vec = as.numeric(wdi[which(wdi$`Country Name` == i),][j,][,8:27])
    
    rest[which(rest$country == i), 2+j] = vec
    
  }
}

#delete gdp from rest
rest = rest[,-c(7)]

####merge dataframes

first = data.frame(country = rep(countries, each = length(seq(2000, 2019))), year = rep(seq(2000, 2019), times = length(countries))) #create data frame with years

df_list = list(first, gdp, energy, capital, rest) #list of dframes


dt = df_list %>% reduce(left_join, by= c("country", "year"))

save(dt, file = paste0(output_path, "dataset_final.rda"))
