### Process the data for WQ72
###


rm(list=ls())   # Clean: remove all variables in Enviornment

##load libraries

library(ggplot2)
library(plyr)
library(dplyr)

##Set working directories
wd <- "~/Documents/MyRWA/projects/WQ72/"
##scriptsLoc <- paste(wd,"scripts/analyze/", sep = "")
##dataLoc <- paste(wd,"data/", sep = "")
setwd(wd)

raw <- read.csv("WQ72.csv", header=FALSE)


f1 <- function(f1){
        title = 
}

