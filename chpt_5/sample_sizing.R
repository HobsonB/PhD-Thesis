dev.set(dev.next())
rm(list=ls())
cat("\014")

# ---- Sample Size ----

# Determines the number of simulations required to get an adequate sampling of all possible sensor combinations based on preliminary sizing run of 100 EnergyPlus simulations

# Author(s): Brodie W. Hobson
# Last Modified: Tuesday 19 July 2022

# ----Load packages----

# Load rstudioapi package
if ("rstudioapi" %in% rownames(installed.packages()) == FALSE) {install.packages("rstudioapi")}
require(rstudioapi)
# Load other packages
if ("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")}
require(lubridate)
if ("matrixStats" %in% rownames(installed.packages()) == FALSE) {install.packages("matrixStats")}
require(matrixStats)

# ---- Set working directory ----
setwd(dirname(getActiveDocumentContext()$path))

# ---- Specify paths and static values ----

df <- read.csv("./data/results.csv")

eui <- df$hc+df$cc+df$bb+df$fn+df$lt

shapiro_ls <- list()
f_ls <- list()
t_ls <- list() 

for(j in c(1:20)){
  
  shapiro <- vector()
  f <- vector()
  t <- vector()
  
  for(i in c(3:100)){
    
    ind <- sample(c(1:100),i,replace=F)
  
    shapiro[length(shapiro)+1] <- shapiro.test(eui[ind])$p.value
    f[length(f)+1] <- var.test(eui,eui[ind])$p.value
    t[length(t)+1] <- t.test(eui, eui[ind], alternative = "two.sided", var.equal = T)$p.value
    
  }
  
  shapiro_ls[[length(shapiro_ls)+1]] <- shapiro
  f_ls[[length(f_ls)+1]] <- f
  t_ls[[length(t_ls)+1]] <- t

}

shapiro <- rowQuantiles((matrix(unlist(shapiro_ls), byrow=F, ncol = 20)), probs = 0.05)
f <- rowQuantiles((matrix(unlist(f_ls), byrow=F, ncol = 20)), probs = 0.05)
t <- rowQuantiles((matrix(unlist(t_ls), byrow=F, ncol = 20)), probs = 0.05)