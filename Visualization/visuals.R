library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(stringi)
library(readxl)
library(tidyverse)
library(plm)
library(modelr)
library(stargazer)
library(ggplot2)
library(esquisse)
library(rgdal)
library(RColorBrewer)
library(broom)
library(viridis)
library(hrbrthemes)
library(ggpubr)
library(fmsb)
library(xtable)
library(maptools)
library(rstudioapi)

#set working directory
setwd(dirname(getActiveDocumentContext()$path)) 

input_path = paste0(getwd(), "/Input/")
output_path = paste0(getwd(), "/Output/")


load(paste0(getwd(), "/Input/sfa_efficiencies.rda"))
load(paste0(getwd(), "/Input/sfa_efficiencies_half.rda"))
load(paste0(getwd(), "/Input/dea_efficiencies_ghg.rda"))
load(paste0(getwd(), "/Input/dea_efficiencies_ener.rda"))
load(paste0(getwd(), "/Input/descriptive_data.rda"))

####Change format of SFA efficiencies for paper tables
eff.ener.sfa = data.frame(matrix(ncol = 22, nrow = 31))
names(eff.ener.sfa) = c("country", seq(from=2000, to=2019), "average")
eff.ener.sfa$country = unique(eff.sfa$country)[1:31] 

eff.ghg.sfa = data.frame(matrix(ncol = 22, nrow = 31))
names(eff.ghg.sfa) = c("country", seq(from=2000, to=2019), "average")
eff.ghg.sfa$country = unique(eff.sfa$country)[1:31] 

countries = unique(eff.sfa$country)[1:31]



for (i in 2000:2019) {
  
  vec1 = eff.sfa$enercobb[which(eff.sfa$year == i & eff.sfa$country %in% countries)]
  vec2 = eff.sfa$ghgcobb[which(eff.sfa$year == i & eff.sfa$country %in% countries)]
  
  eff.ener.sfa[[as.character(i)]] = as.numeric(vec1)
  eff.ghg.sfa[[as.character(i)]] = as.numeric(vec2)
  
}

#add row means
eff.ghg.sfa$average = as.numeric(rowMeans(eff.ghg.sfa[,2:21]))
eff.ener.sfa$average = as.numeric(rowMeans(eff.ener.sfa[,2:21]))



####Choropleth map

#load map
europe_full = readOGR("https://raw.githubusercontent.com/leakyMirror/map-of-europe/master/GeoJSON/europe.geojson")


#identify countries
countries = unique(eff.sfa$country)[1:30]


europe = europe_full[europe_full@data$NAME %in% countries, ] 

#fortify data
eu_fort = tidy(europe, region = "NAME")

#create dataframes with needed data
#energy
data.ener = data.frame(matrix(ncol = 6, nrow = 30))
names(data.ener) = c("country", "val00", "val04", "val08", "val12", "val16")

data.ener$val00 = apply(eff.ener.sfa[1:30,2:5], 1, mean)
data.ener$val04 = apply(eff.ener.sfa[1:30,5:9], 1, mean)
data.ener$val08 =  apply( eff.ener.sfa[1:30,10:13], 1, mean)
data.ener$val12 =  apply( eff.ener.sfa[1:30,14:17], 1, mean)
data.ener$val16 = apply(eff.ener.sfa[1:30,18:21], 1, mean)

# data.ener$val98 = eff.ener.sfa[1:30,2]
# data.ener$val02 = eff.ener.sfa[1:30,6]
# data.ener$val06 = eff.ener.sfa[1:30,10]
# data.ener$val10 = eff.ener.sfa[1:30,14]
# data.ener$val14 = eff.ener.sfa[1:30,18]
# data.ener$val17 = eff.ener.sfa[1:30,21]
data.ener$country = unique(eff.ener.sfa[1:30,1])

for (i in 2:6) {
  data.ener[,i] = as.numeric(data.ener[, i])
  
}

data.ener$group00 = cut(data.ener$val00, breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
data.ener$group04 = cut(data.ener$val04, breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
data.ener$group08 = cut(data.ener$val08, breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
data.ener$group12 = cut(data.ener$val12, breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
data.ener$group16 = cut(data.ener$val16, breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9, 1))


#ghg
data.ghg = data.frame(matrix(ncol = 6, nrow = 30))
names(data.ghg) = c("country", "val00", "val04", "val08", "val12", "val16")

data.ghg$val00 = apply(eff.ghg.sfa[1:30,2:5], 1, mean)
data.ghg$val04 = apply(eff.ghg.sfa[1:30,5:9], 1, mean)
data.ghg$val08 =  apply( eff.ghg.sfa[1:30,10:13], 1, mean)
data.ghg$val12 =  apply( eff.ghg.sfa[1:30,14:17], 1, mean)
data.ghg$val16 = apply(eff.ghg.sfa[1:30,18:21], 1, mean)
#data.ghg$val17 = eff.ghg.sfa[1:30,21]
data.ghg$country = unique(eff.ghg.sfa[1:30,1])

for (i in 2:6) {
  data.ghg[,i] = as.numeric(data.ghg[, i])
  
}

data.ghg$group00 = cut(data.ghg$val00, breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
data.ghg$group04 = cut(data.ghg$val04, breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
data.ghg$group08 = cut(data.ghg$val08, breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
data.ghg$group12 = cut(data.ghg$val12, breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
data.ghg$group16 = cut(data.ghg$val16, breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9, 1))

#merge geo and efficiency data
eu_fort.ghg = eu_fort %>%
  left_join(. , data.ghg, by=c("id"="country"))

eu_fort.ener = eu_fort %>%
  left_join(. , data.ener, by=c("id"="country"))

#define colors

scalecolors = c("#600000", "#fb2929", "#fa7f26", "#fff31a", "#69dc23", "#00900a")

#scalecolors = c("dark red", "red", "orange", "yellow", "green", "dark green")


#ghg plots
p1.g = ggplot() +
  geom_polygon(data = eu_fort.ghg, aes(fill = group00, x = long, y = lat, group = group) , size=0, alpha=0.9, color = rgb(0,0,0,0.3)) +
  theme_void() +
  scale_fill_manual(guide= "none", breaks = levels(eu_fort.ghg$group00), values = scalecolors, labels = c("< 0.5", "0.5-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "> 0.9"))+
  labs(
    title = "2000-2003",
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    
    plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -2, t = 0.4, l = 0, r = 0, unit = "cm")),
    plot.margin = unit(c(b = 0, t = 0, l = 0, r = -1), "cm"),
  ) +
  coord_map()

p2.g <- ggplot() +
  geom_polygon(data = eu_fort.ghg, aes(fill = group04, x = long, y = lat, group = group) , size=0, alpha=0.9, color = rgb(0,0,0,0.3)) +
  theme_void() +
  scale_fill_manual(guide= "none", breaks = levels(eu_fort.ghg$group04), values = scalecolors, labels = c("< 0.5", "0.5-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "> 0.9"))+
  labs(
    title = "2004-2007",
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    
    plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -2, t = 0.4, l = 0, r = 0, unit = "cm")),
    plot.margin = unit(c(b = 0, t = 0, l = 0, r = -1), "cm"),
  ) +
  coord_map()

p3.g <- ggplot() +
  geom_polygon(data = eu_fort.ghg, aes(fill = group08, x = long, y = lat, group = group) , size=0, alpha=0.9, color = rgb(0,0,0,0.3)) +
  theme_void() +
  scale_fill_manual(guide= "none", breaks = levels(eu_fort.ghg$group08), values = scalecolors, labels = c("< 0.5", "0.5-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "> 0.9"))+
  labs(
    title = "2008-2011",
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    
    plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -2, t = 0.4, l = 0, r = 0, unit = "cm")),
    plot.margin = unit(c(b = 0, t = 0, l = 0, r = -1), "cm"),
  ) +
  coord_map()

p4.g <- ggplot() +
  geom_polygon(data = eu_fort.ghg, aes(fill = group12, x = long, y = lat, group = group) , size=0, alpha=0.9, color = rgb(0,0,0,0.3)) +
  theme_void() +
  scale_fill_manual(guide= "none", breaks = levels(eu_fort.ghg$group12), values = scalecolors, labels = c("< 0.5", "0.5-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "> 0.9"))+
  labs(
    title = "2012-2015",
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    
    plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -2, t = 0.4, l = 0, r = 0, unit = "cm")),
    plot.margin = unit(c(b = 0, t = 0, l = 0, r = -1), "cm"),
  ) +
  coord_map()

p5.g <- ggplot() +
  geom_polygon(data = eu_fort.ghg, aes(fill = group16, x = long, y = lat, group = group) , size=0, alpha=0.9, color = rgb(0,0,0,0.3)) +
  theme_void() +
  scale_fill_manual(name = "TFCE", breaks = levels(eu_fort.ghg$group16), values = scalecolors, labels = c("< 0.5", "0.5-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "> 0.9"))+
  labs(
    title = "2016-2019",
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    
    plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -2, t = 0.4, l = 0, r = 0, unit = "cm")),
    plot.margin = unit(c(b = 0, t = 0, l = 0, r = -1), "cm"),
  ) +
  coord_map()

# p6.g <- ggplot() +
#   geom_polygon(data = eu_fort.ghg, aes(fill = val17, x = long, y = lat, group = group) , size=0, alpha=0.9, color = rgb(0,0,0,0.3)) +
#   theme_void() +
#   scale_fill_gradient2(guide = "none", low = "red", mid = "yellow", high = "green", midpoint = 0.75) +
#   labs(
#     title = "2017",
#   ) +
#   theme(
#     text = element_text(color = "#22211d"),
#     plot.background = element_rect(fill = "transparent", color = NA),
#     panel.background = element_rect(fill = "transparent", color = NA),
#     legend.background = element_rect(fill = "transparent", color = NA),
#     
#     plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -2, t = 0.4, l = 0, r = 0, unit = "cm")),
#     plot.margin = unit(c(b = 0, t = 0, l = 0, r = -1), "cm"),
#   ) +
#   coord_map()

p.map.ghg = ggarrange(p1.g, p2.g,p3.g,p4.g,p5.g, ncol = 3, nrow= 2)
p.map.ghg
#energy plots
p1.e <- ggplot() +
  geom_polygon(data = eu_fort.ener, aes(fill = group00, x = long, y = lat, group = group) , size=0, alpha=0.9, color = rgb(0,0,0,0.3)) +
  theme_void() +
  scale_fill_manual(guide = "none", breaks = levels(eu_fort.ener$group00), values = scalecolors, labels = c("< 0.5", "0.5-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "> 0.9"))+
  labs(
    title = "2000-2003",
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    
    plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -2, t = 0.4, l = 0, r = 0, unit = "cm")),
    plot.margin = unit(c(b = 0, t = 0, l = 0, r = -1), "cm"),
  ) +
  coord_map()

p2.e <- ggplot() +
  geom_polygon(data = eu_fort.ener, aes(fill = group04, x = long, y = lat, group = group) , size=0, alpha=0.9, color = rgb(0,0,0,0.3)) +
  theme_void() +
  scale_fill_manual(guide = "none", breaks = levels(eu_fort.ener$group04), values = scalecolors, labels = c("< 0.5", "0.5-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "> 0.9"))+
  labs(
    title = "2004-2007",
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    
    plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -2, t = 0.4, l = 0, r = 0, unit = "cm")),
    plot.margin = unit(c(b = 0, t = 0, l = 0, r = -1), "cm"),
  ) +
  coord_map()

p3.e <- ggplot() +
  geom_polygon(data = eu_fort.ener, aes(fill = group08, x = long, y = lat, group = group) , size=0, alpha=0.9, color = rgb(0,0,0,0.3)) +
  theme_void() +
  scale_fill_manual(guide = "none", breaks = levels(eu_fort.ener$group08), values = scalecolors, labels = c("< 0.5", "0.5-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "> 0.9"))+
  labs(
    title = "2008-2011",
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    
    plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -2, t = 0.4, l = 0, r = 0, unit = "cm")),
    plot.margin = unit(c(b = 0, t = 0, l = 0, r = -1), "cm"),
  ) +
  coord_map()

p4.e <- ggplot() +
  geom_polygon(data = eu_fort.ener, aes(fill = group12, x = long, y = lat, group = group) , size=0, alpha=0.9, color = rgb(0,0,0,0.3)) +
  theme_void() +
  scale_fill_manual(guide = "none", breaks = levels(eu_fort.ener$group12), values = scalecolors, labels = c("< 0.5", "0.5-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "> 0.9"))+
  labs(
    title = "2012-2015",
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    
    plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -2, t = 0.4, l = 0, r = 0, unit = "cm")),
    plot.margin = unit(c(b = 0, t = 0, l = 0, r = -1), "cm"),
  ) +
  coord_map()

p5.e <- ggplot() +
  geom_polygon(data = eu_fort.ener, aes(fill = group16, x = long, y = lat, group = group) , size=0, alpha=0.9, color = rgb(0,0,0,0.3)) +
  theme_void() +
  scale_fill_manual(name = "TFEE", breaks = levels(eu_fort.ener$group16), values = scalecolors, 
                    labels = c("< 0.5", "0.5-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "> 0.9"), drop = FALSE)+
  labs(
    title = "2016-2019",
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
  
    plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -2, t = 0.4, l = 0, r = 0, unit = "cm")),
    plot.margin = unit(c(b = 0, t = 0, l = 0, r = -1), "cm"),
  ) +
  coord_map()

# p6.e <- ggplot() +
#   geom_polygon(data = eu_fort.ener, aes(fill = val17, x = long, y = lat, group = group) , size=0, alpha=0.9, color = rgb(0,0,0,0.3)) +
#   theme_void() +
#   scale_fill_stepsn(name = "TFEE", breaks = c(0,0.3,0.5, 0.6, 0.7, 0.8, 0.9, 1) , colors = scalecolors)+
#   labs(
#     title = "2017",
#   ) +
#   theme(
#     text = element_text(color = "#22211d"),
#     plot.background = element_rect(fill = "transparent", color = NA),
#     panel.background = element_rect(fill = "transparent", color = NA),
#     legend.background = element_rect(fill = "transparent", color = NA),
#     
#     plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -2, t = 0.4, l = 0, r = 0, unit = "cm")),
#     plot.margin = unit(c(b = 0, t = 0, l = 0, r = -1), "cm"),
#   ) +
#   coord_map()


p.map.ener = ggarrange(p1.e, p2.e,p3.e,p4.e,p5.e, ncol = 3, nrow= 2)
p.map.ener

######Create map with both GHG and energy 

p.map.all = ggarrange(p1.e, p5.e, p1.g, p5.g, ncol = 2, nrow = 2)
p.map.all
#######Line plots 

countries = c("Belgium", "Luxembourg", "Denmark", "Estonia", "Germany", 
              "Iceland", "Lithuania", "Sweden")

minmax = c("Average", "Maximum", "Minimum")

subset1 = eff.sfa %>% 
  filter(country %in% countries)

subset2 = eff.sfa %>% 
  filter(country %in% minmax)

##energy lineplot  
p.ener.all = subset1 %>%
                ggplot( aes(x=year, y=enercobb, group=country, color=country)) +
  geom_line(size=1.5) +
  geom_point(size=3)+
  scale_color_discrete(name = "")+
  scale_x_discrete(breaks = c(2000, 2004, 2008, 2012, 2016, 2019))+
  theme_ipsum() +
  theme(axis.title.y = element_text(size=15),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom")+
  ylab("Efficiency score")+
  xlab("Year")    

p.ener.minmax = subset2 %>%
  ggplot( aes(x=year, y=enercobb, group=country, color=country)) +
  geom_line(size=1.5) +
  geom_point(size=3)+
  scale_color_discrete(name = "")+
  scale_x_discrete(breaks = c(2000, 2004, 2008, 2012, 2016, 2019))+
  theme_ipsum() +
  theme(axis.title.y = element_text(size=15),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.position = "bottom",
        legend.key.size = unit(1, "cm"))+
  ylab("Efficiency score")+
  xlab("Year")    


##ghg lineplots

p.ghg.all = subset1 %>%
  ggplot( aes(x=year, y=ghgcobb, group=country, color=country)) +
  geom_line(size=1.5) +
  geom_point(size=3)+
  scale_color_discrete(name = "")+
  scale_x_discrete(breaks = c(2000, 2004, 2008, 2012, 2016, 2019))+
  theme_ipsum() +
  theme(axis.title.y = element_text(size=15),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.position = "bottom",
        legend.key.size = unit(1, "cm"))+
  ylab("Efficiency score")+
  xlab("Year")    
       

p.ghg.minmax = subset2 %>%
  ggplot( aes(x=year, y=ghgcobb, group=country, color=country)) +
  geom_line(size=1.5) +
  geom_point(size=3)+
  scale_color_discrete(name = "")+
  scale_x_discrete(breaks = c(2000, 2004, 2008, 2012, 2016, 2019))+
  theme_ipsum() +
  theme(axis.title.y = element_text(size=15),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.position = "bottom",
        legend.key.size = unit(1, "cm"))+
  ylab("Efficiency score")+
  xlab("Year")             

ggarrange(p.ener.all, p.ener.minmax, legend = "bottom")
ggarrange(p.ghg.all, p.ghg.minmax, legend = "bottom")

#####Radar chart
#for presentation

countries = c("Belgium", "Denmark", "Estonia", "Germany", 
              "Iceland", "Lithuania", "Luxembourg", "Sweden", "Average")

#first for TFEE
radar.data1 = data.frame(matrix(ncol = 9, nrow = 2))

colnames(radar.data1) = countries

radar.data1[1,] = as.numeric(eff.ener.sfa$average[which(eff.ener.sfa$country %in% countries)])
radar.data1[2,] = as.numeric(eff.ener.dea$average[which(eff.ener.dea$country %in% countries)])

rownames(radar.data1) = c("SFA", "DEA")

radar.data1 = rbind(rep(1,9) , rep(0,9) , radar.data1)

#second for TFCE
radar.data2 = data.frame(matrix(ncol = 9, nrow = 2))

colnames(radar.data2) = countries

radar.data2[1,] = as.numeric(eff.ghg.sfa$average[which(eff.ghg.sfa$country %in% countries)])
radar.data2[2,] = as.numeric(eff.ghg.dea$average[which(eff.ghg.dea$country %in% countries)])

rownames(radar.data2) = c("SFA", "DEA")

radar.data2 = rbind(rep(1,9) , rep(0,9) , radar.data2)



#Color vector
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) )

# radar plot
radarchart(radar.data1  , axistype=1 , 
           #custom polygon
           pcol=colors_border , plwd=7 , plty=1,
           #custom the grid
           cglcol="dark grey", cglty=1, axislabcol="black", caxislabels = c(0, 0.25, 0.5, 0.75, 1), cglwd=1.5,calcex = 1.5,
           #custom labels
           vlcex=2
)

legend(x=-1.5, y=-0.5, legend = rownames(radar.data1[-c(1,2),]), bty = "n", pch=20 , col=colors_border , text.col = "black", cex=2, pt.cex=4)


radarchart(radar.data2  , axistype=1 , 
           #custom polygon
           pcol=colors_border , plwd=7 , plty=1,
           #custom the grid
           cglcol="dark grey", cglty=1, axislabcol="black", caxislabels = c(0, 0.25, 0.5, 0.75, 1), cglwd=1.5,calcex = 1.5,
           #custom labels
           vlcex=2
)

legend(x=-1.5, y=-0.5, legend = rownames(radar.data2[-c(1,2),]), bty = "n", pch=20 , col=colors_border , text.col = "black", cex=2, pt.cex=4)



#Plot total energy use and ghg emissions
#Create new frame with totals
totals = data.frame(matrix(ncol = 3, nrow = 20))
names(totals) = c("year", "ener", "ghg")
totals$year = as.numeric(seq(from=2000, to =2019))

for (i in 2000:2019) {
  
  sub = desc %>% select(year, eneruse, ghg) %>%
      filter(year == i) 
  
  totals$ener[which(totals$year == i)] = sum(sub$eneruse)
  totals$ghg[which(totals$year == i)] = sum(sub$ghg)
  
}


# Value used to transform the data
coeff <- 10

# A few constants
ghgColor <- "#69b3a2"
enerColor <- rgb(0.2, 0.6, 0.9, 1)

ggplot(totals, aes(x=year)) +
  
  geom_bar( aes(y=ghg), stat="identity", size=.1, fill=ghgColor, color="black", alpha=.4) + 
  geom_line( aes(y=ener), size=2, color=enerColor) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "GHG emissions in Mt CO2 eq.",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~., name="Energy consumption in Mt oil eq.")
  ) + 
  
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = ghgColor, size=12),
    axis.title.y.right = element_text(color = enerColor, size=12)
  ) +
  xlab("Years")             



####Scatterplot of efficiencies

avg.sfa = data.frame(country = eff.ener.sfa$country, ener = as.numeric(eff.ener.sfa$average), 
                     ghg = as.numeric(eff.ghg.sfa$average))

avg.sfa = avg.sfa[-31,]

avg.sfa$index = as.numeric((avg.sfa$ener+avg.sfa$ghg)/2)

avg.sfa$indic = cut(avg.sfa$index, breaks = c(0, 0.65, 0.75, 1), labels = c("< 0.65", "0.65 - 0.75", "> 0.75"))


countries = c("Iceland", "Ireland", "Czech Republic", "Sweden", "Latvia", "Romania", "Finland", "Belgium", "Slovakia",
              "Luxembourg", "Estonia", "Greece", "Denmark", "Norway","Slovenia", "France", "Netherlands", "Austria", "Croatia")


ggplot(avg.sfa, aes(x=ghg, y=ener, color = indic)) +
            geom_point(size = 6, alpha = 0.8) +
            #geom_abline(size = 0.8, color = "black", alpha = 0.8)+
            geom_smooth(method = "lm", color = "red", se = FALSE, size = 1.1, alpha = 0.8)+
            geom_text(    data=avg.sfa %>% filter(country %in% countries), # Filter data first
                         aes(label=country), nudge_x = 0, nudge_y = -0.025, 
                         check_overlap = F)+
            scale_color_manual(name = "Overall avg. efficiency", values = c("< 0.65" = "#ff3857", "0.65 - 0.75" = "#5157ff", "> 0.75" = "#00900a"))+
            scale_x_continuous(limits = c(0.45, 1.01), breaks = seq(from = 0, to = 1, by = 0.1))+
            scale_y_continuous(limits = c(0.2, 1.01), breaks = seq(from = 0, to = 1, by = 0.1))+
            theme_ipsum() +
            theme(axis.title.y = element_text(size=16),
                  axis.title.x = element_text(size = 16),
                  axis.text.x = element_text(size = 16),
                  axis.text.y = element_text(size = 16),
                  legend.text = element_text(size = 15),
                  legend.key.size = unit(1, "cm"),
                  legend.position = "bottom",
                  legend.title = element_text(size = 15))+
            ylab("Energy Efficiency")+
            xlab("Carbon Efficiency")   


###Output tables
ener.sfa.out = as.matrix(eff.ener.sfa)
ener.dea.out = as.matrix(eff.ener.dea)
ghg.sfa.out = as.matrix(eff.ghg.sfa)
ghg.dea.out = as.matrix(eff.ghg.dea)

ener.sfa.out[,-1] = as.numeric(round(as.numeric(ener.sfa.out[,-1]), digits = 3))
ener.dea.out[,-1] = round(as.numeric(ener.dea.out[,-1]), digits = 3)
ghg.sfa.out[,-1] = round(as.numeric(ghg.sfa.out[,-1]), digits = 3)
ghg.dea.out[,-1] = round(as.numeric(ghg.dea.out[,-1]), digits = 3)

##Complete  results

xtable(ener.sfa.out)
xtable(ener.dea.out)
xtable(ghg.sfa.out)
xtable(ghg.dea.out)


# 
# countries = c("Belgium", "Denmark", "Estonia", "Germany", 
#               "Iceland", "Lithuania", "Luxembourg", "Sweden", "Average")
# 
# radar.data = data.frame(matrix(ncol = 9, nrow = 4))
# 
# colnames(radar.data) = countries
# 
# radar.data[1,] = as.numeric(eff.ener.sfa$average[which(eff.ener.sfa$country %in% countries)])
# radar.data[2,] = as.numeric(eff.ghg.sfa$average[which(eff.ener.sfa$country %in% countries)])
# radar.data[3,] = as.numeric(eff.ener.dea$average[which(eff.ener.sfa$country %in% countries)])
# radar.data[4,] = as.numeric(eff.ghg.dea$average[which(eff.ener.sfa$country %in% countries)])
# 
# rownames(radar.data) = c("TFEE-SFA", "TFCE-SFA", "TFEE-DEA", "TFCE-DEA")
# 
# radar.data = rbind(rep(1,9) , rep(0,9) , radar.data)
# 
# 
# #Color vector
# colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0,0,0,1), rgb(0.7,0.5,0.1,0.9) )
# 
# # radar plot
# radarchart(radar.data  , axistype=1 ,
#                 #custom polygon
#                 pcol=colors_border , plwd=4 , plty=1,
#                 #custom the grid
#                 cglcol="dark grey", cglty=1, axislabcol="black", caxislabels = c(0, 0.25, 0.5, 0.75, 1), cglwd=0.8,
#                 #custom labels
#                 vlcex=1
#       )
# 
# legend(x=1, y=1.1, legend = rownames(radar.data[-c(1,2),]), bty = "n", pch=20 , col=colors_border , text.col = "black", cex=1, pt.cex=2)
# 

