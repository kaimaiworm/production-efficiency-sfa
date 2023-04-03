library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(stringi)
library(readxl)
library(tidyverse)
library(Benchmarking)
library(frontier)
library(plm)
library(modelr)
library(stargazer)
library(rstudioapi)

#set working directory
setwd(dirname(getActiveDocumentContext()$path)) 

input_path = paste0(getwd(), "/Input/")
output_path = paste0(getwd(), "/Output/")

#load data
load(paste0(input_path, "dataset_final.rda"))

dt = pdata.frame(dt, c("country", "year"))

#add log variables

dt$loggdp = log(dt$gdp)
dt$logcapital = log(dt$capital)
dt$loglabor = log(dt$labor)
dt$logco2 = log(dt$co2)
dt$logghg = log(dt$ghg)
dt$logeneruse = log(dt$eneruse)



##OLS regressions for fit
lm1 = lm(log(1/eneruse) ~ loggdp + logcapital + loglabor , data = dt)
lm2 = lm(log(1/ghg) ~ loggdp + logcapital + loglabor,data = dt)

lm3 =  lm(log(1/eneruse) ~ loggdp + logcapital + loglabor + I( 0.5 * loggdp^2 ) + I(0.5 * logcapital^2 )
                + I( 0.5 * loglabor^2 ) + I( logcapital * loggdp) + I(loglabor * loggdp) + I(logcapital * loglabor), data = dt)

lm4=  lm(log(1/ghg) ~ loggdp + logcapital + loglabor + I( 0.5 * loggdp^2 ) + I(0.5 * logcapital^2 )
                + I( 0.5 * loglabor^2 ) + I( logcapital * loggdp) + I(loglabor * loggdp) + I(logcapital * loglabor), data = dt)

#summaries

selfunc = function(x){
  
  R2 = summary(x)$adj.r.squared
  RMSE = rmse(x, data=dt)
  MAE = mae(x, data=dt)
  AIC = AIC(x)
  BIC = BIC(x)
  vec = c(R2, RMSE, MAE, AIC, BIC) 
  
  return(vec)
}

mdls = list(lm1, lm2, lm3, lm4)

modsel1 = data.frame(t(sapply(mdls, selfunc)))

colnames(modsel1) = c("R2", "RMSE", "MAE", "AIC", "BIC")


###sfa


#cobb douglas
 
enercobb = sfa(log(1/eneruse) ~ loggdp + logcapital + loglabor| urban + renew + indgdp, truncNorm = TRUE, data = dt)

ghgcobb = sfa(log(1/ghg) ~ loggdp + logcapital + loglabor| urban + renew + indgdp ,truncNorm = TRUE,  data = dt)

#translog

            
#enertrans = sfa(log(1/eneruse) ~ loggdp + logcapital + loglabor + I( 0.5 * loggdp^2 ) + I(0.5 * logcapital^2 )
#                + I( 0.5 * loglabor^2 ) + I( logcapital * loggdp) + I(loglabor * loggdp) + I(logcapital * loglabor)|urban + renew + indgdp,truncNorm = TRUE, data = dt)


#ghgtrans =  sfa(log(1/ghg) ~ loggdp + logcapital + loglabor + I( 0.5 * loggdp^2 ) + I(0.5 * logcapital^2 )
#                + I( 0.5 * loglabor^2 ) + I( logcapital * loggdp) + I(loglabor * loggdp) + I(logcapital * loglabor) | urban + renew + indgdp,truncNorm = TRUE, data = dt)



#log-likelihoods
loglik.ghgcobb = logLik(ghgcobb)
#loglik.ghgtrans = logLik(ghgtrans)
loglik.enercobb = logLik(enercobb)
#loglik.enertrans = logLik(enertrans)


#lrtests
lrtest(enercobb)
lrtest(ghgcobb)

#resettest
#resettestFrontier(enercobb)
#resettestFrontier(enertrans)
#resettestFrontier(ghgcobb)
#resettestFrontier(ghgtrans)

#efficiences
eff.enercobb = efficiencies(enercobb)
eff.ghgcobb = efficiencies(ghgcobb)
#eff.enertrans = efficiencies(enertrans)
#eff.ghgtrans = efficiencies(ghgtrans)

eff.sfa = data.frame("country" = dt$country, "year" = dt$year, enercobb = NA, ghgcobb = NA)
#eff.sfa$ghgtrans = eff.ghgtrans
#eff.sfa$ghgcobb = eff.ghgcobb
#eff.sfa$enertrans = eff.enertrans
#eff.sfa$enercobb = eff.enercobb

countries = as.character(unique(dt$country))

for (i in 1:30) {
  
  temp1 = eff.enercobb[i,]
  temp2 = eff.ghgcobb[i,]
  
  eff.sfa$enercobb[which(eff.sfa$country == countries[i])] = temp1
  eff.sfa$ghgcobb[which(eff.sfa$country == countries[i])] = temp2
  
  
}


#summary
#summary(enertrans)
#summary(ghgtrans)


#Add yearly averages, maximum and minimum 

avg = data.frame(matrix(ncol = 4, nrow = 20))
max = data.frame(matrix(ncol = 4, nrow = 20))
min = data.frame(matrix(ncol = 4, nrow = 20))

names(avg) = names(eff.sfa)
names(max) = names(eff.sfa)
names(min) = names(eff.sfa)

counter = 1

for (i in 2000:2019) {
  
  temp = eff.sfa %>% 
            filter(year == i)
  
  avg[counter,] = c("Average", i, as.numeric(colMeans(temp[,3:4])))
  max[counter,] = c("Maximum", i, as.numeric(apply(temp[,3:4], 2, max)))
  min[counter,] = c("Minimum", i, as.numeric(apply(temp[,3:4], 2, min)))
  
  counter = counter+1
}

eff.sfa = rbind(eff.sfa, avg, max, min)

#eff.sfa$enertrans = as.numeric(eff.sfa$enertrans)
eff.sfa$enercobb = as.numeric(eff.sfa$enercobb)
eff.sfa$ghgcobb = as.numeric(eff.sfa$ghgcobb)
#eff.sfa$ghgtrans = as.numeric(eff.sfa$ghgtrans)



#Comparison with DEA
load(paste0(input_path, "dataset_final.rda"))

#energy
eff.ener.dea = data.frame(matrix(ncol = 22, nrow = 30))
names(eff.ener.dea) = c("country", seq(from=2000, to=2019), "average")

eff.ener.dea$country = unique(dt$country) 

for (i in 2000:2019) {
  
  X = dt[which(dt$year == i), c("eneruse", "labor", "capital")]
  Y = dt[which(dt$year == i), c("gdp")]
  dea.temp = dea(X, Y, RTS = "vrs", ORIENTATION = "in")
  
  x1 = excess(dea.temp, X, Y) 
  x2 = slack(X, Y, dea.temp)
  
  #target = x1[1] #without slack
  
  target = x1[1] + x2$sx[,1] #with slack
  
  tfee = 1 - target/X[,1]
  
  eff.ener.dea[[as.character(i)]] = tfee
  
}

#emissions
eff.ghg.dea = data.frame(matrix(ncol = 22, nrow = 30))
names(eff.ghg.dea) = c("country", seq(from=2000, to=2019), "average")

eff.ghg.dea$country = unique(dt$country) 

for (i in 2000:2019) {
  
  X = dt[which(dt$year == i), c("ghg", "labor", "capital")]
  Y = dt[which(dt$year == i), c("gdp")]
  dea.temp = dea(X, Y, RTS = "vrs", ORIENTATION = "in")
  
  x1 = excess(dea.temp, X, Y) 
  x2 = slack(X, Y, dea.temp)
  
  #target = x1[1] #without slack
  
  target = x1[1] + x2$sx[,1] #with slack
  
  tfce = 1 - target/X[,1]
  
  eff.ghg.dea[[as.character(i)]] = tfce
  
}

#add row means
eff.ghg.dea$average = rowMeans(eff.ghg.dea[,2:21])
eff.ener.dea$average = rowMeans(eff.ener.dea[,2:21])

#add column means
eff.ghg.dea = rbind(eff.ghg.dea, c("Average", colMeans(eff.ghg.dea[,2:22])))
eff.ener.dea = rbind(eff.ener.dea, c("Average", colMeans(eff.ener.dea[,2:22])))


# ###Robustness 
# ##Half-normal distribution
# enertrans.half =  frontier::sfa(log(1/eneruse) ~ loggdp + logcapital + loglabor + I( 0.5 * loggdp^2 ) + I(0.5 * logcapital^2 )
#                  + I( 0.5 * loglabor^2 ) + I( logcapital * loggdp) + I(loglabor * loggdp) + I(logcapital * loglabor),truncNorm = FALSE, data = dt)
# 
# 
# ghgtrans.half =  frontier::sfa(log(1/ghg) ~ loggdp + logcapital + loglabor + I( 0.5 * loggdp^2 ) + I(0.5 * logcapital^2 )
#                 + I( 0.5 * loglabor^2 ) + I( logcapital * loggdp) + I(loglabor * loggdp) + I(logcapital * loglabor),truncNorm = FALSE, data = dt)
# 
# #efficiences
# eff.enertrans.half = efficiencies(enertrans.half)
# eff.ghgtrans.half = efficiencies(ghgtrans.half)
# 
# eff.sfa.half = data.frame("country" = dt$country, "year" = dt$year)
# eff.sfa.half$ghg = eff.ghgtrans.half
# eff.sfa.half$ener = eff.enertrans.half


###Descriptive analysis
#Simplify dataset
desc = dt %>% select(country, year, gdp, labor, capital, ghg, eneruse, pop, renew, urban, indgdp)
desc$eneruse = desc$eneruse
desc$ghg = desc$ghg/1000
desc$gdp = desc$gdp/1000
desc$labor = desc$labor/1000000
desc$capital = desc$capital/1000

desc$gdppc = desc$gdp/desc$pop
desc$laborpc = desc$labor/desc$pop
desc$capitalpc = desc$capital/desc$pop
desc$ghgpc = desc$ghg/desc$pop
desc$enerusepc = desc$eneruse/desc$pop

#Latex output

cols = c("eneruse", "ghg", "gdp", "labor", "capital", "renew", "urban", "indgdp")
stargazer(
  desc[, cols], type = "latex", 
  summary.stat = c("mean")
)



#save results

save(desc, file = paste0(output_path, "descriptive_data.rda"))
save(eff.sfa, file = paste0(output_path, "sfa_efficiencies.rda"))
#save(eff.sfa.half, file = paste0(output_path, "sfa_efficiencies_half.rda"))
save(eff.ghg.dea, file = paste0(output_path, "dea_efficiencies_ghg.rda"))
save(eff.ener.dea, file = paste0(output_path, "dea_efficiencies_ener.rda"))



# #efficiences
# eff.enercobb = efficiencies(enercobb)
# eff.ghgcobb = efficiencies(ghgcobb)
# eff.enertrans = efficiencies(enertrans)
# eff.ghgtrans = efficiencies(ghgtrans)
# 
# eff.compare1 = data.frame(eff.enercobb, eff.enertrans, eff.ghgcobb, eff.ghgtrans, row.names = paste0(dt$country, rep(seq(from=1998, to=2014, by=1), times = 30)))
# 
# 
# ##dt as data.frame with Z variables
# #cobb douglas
# 
# enercobb = sfa(log(1/eneruse) ~ loggdp + logcapital + loglabor | urban  + renew, data = dt)
# 
# ghgcobb = sfa(log(1/ghg) ~ loggdp + logcapital + loglabor | urban  + renew,  data = dt)
# 
# 
# 
# #translog
# enertrans =  sfa(log(1/eneruse) ~ loggdp + logcapital + loglabor + I( 0.5 * loggdp^2 ) + I(0.5 * logcapital^2 )
#                  + I( 0.5 * loglabor^2 ) + I( logcapital * loggdp) + I(loglabor * loggdp) + I(logcapital * loglabor)| urban + renew, data = dt)
# 
# 
# ghgtrans =  sfa(log(1/ghg) ~ loggdp + logcapital + loglabor + I( 0.5 * loggdp^2 ) + I(0.5 * logcapital^2 )
#                 + I( 0.5 * loglabor^2 ) + I( logcapital * loggdp) + I(loglabor * loggdp) + I(logcapital * loglabor)| urban +renew, data = dt)
# 
# 
# #efficiences
# eff.enercobb = efficiencies(enercobb)
# eff.ghgcobb = efficiencies(ghgcobb)
# eff.enertrans = efficiencies(enertrans)
# eff.ghgtrans = efficiencies(ghgtrans)
# 
# eff.compare2 = data.frame(eff.enercobb, eff.enertrans, eff.ghgcobb, eff.ghgtrans, row.names = paste0(dt$country, rep(seq(from=1998, to=2017, by=1), times = 30)))
# 
# #lr test
# ghgcobbnPZ = logLik(ghgcobb)
# ghgtransnPZ = logLik(ghgtrans)
# enercobbnPZ = logLik(enercobb)
# enertransnPZ = logLik(enertrans)
# 
# ghg.logLiks = cbind(ghg.logLiks, ghgcobbnPZ[1], ghgtransnPZ[1])
# ener.logLiks = cbind(ener.logLiks, enercobbnPZ[1], enertransnPZ[1])
# 
# ##dt as panel data.frame without Z variables
# dt = pdata.frame(dt, index = c("country", "year"))
# 
# #cobb douglas
# 
# enercobb = sfa(log(1/eneruse) ~ loggdp + logcapital + loglabor , data = dt)
# 
# ghgcobb = sfa(log(1/ghg) ~ loggdp + logcapital + loglabor,  data = dt)
# 
# #translog
# 
# 
# enertrans =  sfa(log(1/eneruse) ~ loggdp + logcapital + loglabor + I( 0.5 * loggdp^2 ) + I(0.5 * logcapital^2 )
#                  + I( 0.5 * loglabor^2 ) + I( logcapital * loggdp) + I(loglabor * loggdp) + I(logcapital * loglabor), data = dt)
# 
# 
# ghgtrans =  sfa(log(1/ghg) ~ loggdp + logcapital + loglabor + I( 0.5 * loggdp^2 ) + I(0.5 * logcapital^2 )
#                 + I( 0.5 * loglabor^2 ) + I( logcapital * loggdp) + I(loglabor * loggdp) + I(logcapital * loglabor), data = dt)
# 
# #efficiences
# eff.enercobb = efficiencies(enercobb)
# eff.ghgcobb = efficiencies(ghgcobb)
# eff.enertrans = efficiencies(enertrans)
# eff.ghgtrans = efficiencies(ghgtrans)
# 
# eff.compare3 = data.frame(eff.enercobb, eff.enertrans, eff.ghgcobb, eff.ghgtrans)
# 
# #lr test
# ghgcobbPnZ = logLik(ghgcobb)
# ghgtransPnZ = logLik(ghgtrans)
# enercobbPnZ = logLik(enercobb)
# enertransPnZ = logLik(enertrans)
# 
# ghg.logLiks = cbind(ghg.logLiks, ghgcobbPnZ[1], ghgtransPnZ[1])
# ener.logLiks = cbind(ener.logLiks, enercobbPnZ[1], enertransPnZ[1])
# 
# 
# ##dt as panel data.frame with Z variables
# #cobb douglas
# 
# enercobb = sfa(log(1/eneruse) ~ loggdp + logcapital + loglabor | urban + indgdp + renew, data = dt)
# 
# ghgcobb = sfa(log(1/ghg) ~ loggdp + logcapital + loglabor | urban + indgdp + renew,  data = dt)
# 
# #translog
# 
# 
# enertrans =  sfa(log(1/eneruse) ~ loggdp + logcapital + loglabor + I( 0.5 * loggdp^2 ) + I(0.5 * logcapital^2 )
#                  + I( 0.5 * loglabor^2 ) + I( logcapital * loggdp) + I(loglabor * loggdp) + I(logcapital * loglabor)| urban + indgdp + renew, data = dt)
# 
# 
# ghgtrans =  sfa(log(1/ghg) ~ loggdp + logcapital + loglabor + I( 0.5 * loggdp^2 ) + I(0.5 * logcapital^2 )
#                 + I( 0.5 * loglabor^2 ) + I( logcapital * loggdp) + I(loglabor * loggdp) + I(logcapital * loglabor)| urban + indgdp + renew, data = dt)
# 
# #efficiences
# eff.enercobb = efficiencies(enercobb)
# eff.ghgcobb = efficiencies(ghgcobb)
# eff.enertrans = efficiencies(enertrans)
# eff.ghgtrans = efficiencies(ghgtrans)
# 
# eff.compare4 = data.frame(eff.enercobb, eff.enertrans, eff.ghgcobb, eff.ghgtrans)



# #lr test
# ghgcobbPZ = logLik(ghgcobb)
# ghgtransPZ = logLik(ghgtrans)
# enercobbPZ = logLik(enercobb)
# enertransPZ = logLik(enertrans)
# 
# ghg.logLiks = cbind(ghg.logLiks, ghgcobbPZ[1], ghgtransPZ[1])
# ener.logLiks = cbind(ener.logLiks, enercobbPZ[1], enertransPZ[1])



