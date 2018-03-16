library(tidyverse)
veg.1 <- read.csv("E:/veg1.csv", header = TRUE)

vegcnames.1 <- colnames(veg.1)

n_distinct(veg.1[,1])
n_distinct(veg.1[,2])
unique(veg.1[,2])
count<- apply(veg.1, 2, n_distinct)

count[count>1]


D <- names(count[count==1])

E <- names(count[count>1])


veg.2 <- select(veg.1, E)

cnames.2 <- colnames(veg.2)

apply(veg.2, 2, n_distinct)

veg.3 <- dplyr::rename(veg.2, 
                       Geo = `Geo.Level`, 
                       Commodity = `Commodity`,
                       Data = `Data.Item`,
                       Category = `Domain.Category`)

cnames.3 <- colnames(veg.3)
cnames.3

veg.3
unique(veg.3[,"Commodity"])
unique(veg.3[,"Data"]) %>% print(n=60, na.print = "")
unique(veg.3[,"Domain"])
unique(veg.3[,"Category"])
unique(veg.3[,"Value"])




