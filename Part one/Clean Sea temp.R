library(tidyverse)
Sea.temp <- read.csv("E:/SEA.TEMPERATURES.csv", header = TRUE)

unique(Air.temp[,"DURATION"])
unique(Air.temp[,"MIN"]) %>% print(n>30, na.print = "")
unique(Air.temp[,"TIME"])
unique(Air.temp[,"MAX"])





