###################
# CAUSAL DISCOVERY IN A COMPLEX INDUSTRIAL SYSTEM
# packages and functions
###################

library(corrplot)
library(data.table)
library(FIAR)
library(fpeek)
library(ggcorrplot)
library(ggplot2)
library(igraph)
library(lubridate)
library(R.utils)
library(reshape2)
library(sqldf)
library(stringr)
library(vars)
library(zoo)

##########
# get systemoverview


getSystemoverview <- function(){
 
   read.csv(unz("accp_dataset.zip", "accp_dataset/systemoverview.csv"))

}

##########
# create list of subsystems with number of PVs

getSubsyslist <- function(){
  
  sysover <- read.csv(unz("accp_dataset.zip", "accp_dataset/systemoverview.csv"))
  aa <- aggregate(sysover$Name, list(sysover$Subsystem), length)
  colnames(aa) <- c("Subsystem", "noPVs")
  aa
  
}

##########
# get list of PVs from subsystem s

PVsfromS <- function(s){
  
  sysover <- read.csv(unz("accp_dataset.zip", "accp_dataset/systemoverview.csv"))
  ss <- sysover[sysover$Subsystem == s, ]
  ss$Name
  
}

##########
# get file name for process variable pv in subsystem s in period p

ddfile <- function(s, pv, p){
  
    tmp <- paste("Period_", p, "/", sep = "")
    tmp <- paste(tmp, s, "/", sep = "")
    tmp <- paste(tmp, pv, ".csv", sep = "")
    tmp

}