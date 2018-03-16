library(tidyverse)
Air.temp <- read.csv("E:/AIR.TEMPERATURES.csv", header = TRUE)

unique(Air.temp[,"DURATION"])
unique(Air.temp[,"MIN"]) %>% print(n>30, na.print = "")
unique(Air.temp[,"TIME"])
unique(Air.temp[,"MAX"])





