dev.set(dev.next())
rm(list=ls())
cat("\014")

# ---- Pareto Front ----

# Pareto front analysis based on priorities of energy use, CO2 exceedances, and number of sensors from bulk results

# Author(s): Brodie W. Hobson
# Last Modified: Tuesday 15 November 2022

#----Load packages----

# Load rstudioapi package
if ("rstudioapi" %in% rownames(installed.packages()) == FALSE) {install.packages("rstudioapi")}
require(rstudioapi)
# Load other required packages
if ("plotrix" %in% rownames(installed.packages()) == FALSE) {install.packages("plotrix")}
require(plotrix)
if ("BBmisc" %in% rownames(installed.packages()) == FALSE) {install.packages("BBmisc")}
require(BBmisc)
if ("rgl" %in% rownames(installed.packages()) == FALSE) {install.packages("rgl")}
require(rgl)
if ("DescTools" %in% rownames(installed.packages()) == FALSE) {install.packages("DescTools")}
require(DescTools)
if ("berryFunctions" %in% rownames(installed.packages()) == FALSE) {install.packages("berryFunctions")}
require(berryFunctions)
if ("pBrackets" %in% rownames(installed.packages()) == FALSE) {install.packages("pBrackets")}
require(pBrackets)


# Set working directory----
setwd(dirname(getActiveDocumentContext()$path))

fil_nam <- c("linear_2","linear_3","adjacent_2","adjacent_3")

baseline <- read.csv("./data/results_baseline.csv")
baseline_hvac <- rep(baseline$hc+baseline$cc+baseline$bb+baseline$fn,743)
baseline_co2 <- rep(rowSums(baseline[,grepl("co2",colnames(baseline))]),743)

for(nam in fil_nam){
  temp <- read.csv(paste0("./data/results_", nam, ".csv"))
  
  hvac <- temp$hc+temp$cc+temp$bb+temp$fn
  hvac <- 100-((1-hvac/baseline_hvac)*100)
  hvac1 <- normalize(hvac,method='range',range = c(0,1))
  hvac2 <- normalize(hvac,method='range',range = c(0,sqrt(2)))
  
  co2 <- rowSums(temp[,grepl("co2",colnames(temp))])
  co2 <- 100-((1-co2/baseline_co2)*100)
  co2[is.na(co2)] <- 0
  co21 <- normalize(co2,method='range',range = c(0,1))
  co22 <- normalize(co2,method='range',range = c(0,sqrt(2)))

  num_sens <- temp$num_zones
  num_sens1 <- normalize(num_sens,method='range',range = c(0,1))
  num_sens2 <- normalize(num_sens,method='range',range = c(0,sqrt(2)))
  
  euc_dist <- sqrt(co21^2+hvac1^2+num_sens1^2)
  euc_dist_hvac <- sqrt(co21^2+hvac2^2+num_sens1^2)
  euc_dist_co2 <- sqrt(co22^2+hvac1^2+num_sens1^2)
  euc_dist_sens <- sqrt(co21^2+hvac1^2+num_sens2^2)
  
  df <- cbind.data.frame(co2,hvac,num_sens,euc_dist,euc_dist_hvac,euc_dist_co2,euc_dist_sens)
  assign(nam,df)
}

df_all <- rbind.data.frame(linear_2_cleaned,linear_3_cleaned,adjacent_2_cleaned,adjacent_3_cleaned)

scenario <- rep(c(1:2),each=7430*2)

df_all <- cbind.data.frame(scenario,df_all)

mu <- vector()
mu_hvac <- vector()
mu_co2 <- vector()
mu_sens <- vector()
mu_ls <- list()
mu_hvac_ls <- list()
mu_co2_ls <- list()
mu_sens_ls <- list()

for(j in c(1:2)){
  mu <- vector()
  mu_hvac <- vector()
  mu_co2 <- vector()
  mu_sens <- vector()
  for(i in c(1:26)){
    ind <- which(temp$num_zones==i & df_all$scenario==j)
    mu[length(mu)+1] <- mean(df_all$euc_dist[ind])
    mu_hvac[length(mu_hvac)+1] <- mean(df_all$euc_dist_hvac[ind])
    mu_co2[length(mu_co2)+1] <- mean(df_all$euc_dist_co2[ind])
    mu_sens[length(mu_sens)+1] <- mean(df_all$euc_dist_sens[ind])
  }
  mu_ls[[length(mu_ls)+1]] <- mu
  mu_hvac_ls[[length(mu_hvac_ls)+1]] <- mu_hvac
  mu_co2_ls[[length(mu_co2_ls)+1]] <- mu_co2
  mu_sens_ls[[length(mu_sens_ls)+1]] <- mu_sens
}