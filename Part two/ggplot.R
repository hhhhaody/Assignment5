veg.3 <- dplyr::rename(veg.2, 
                       Geo = `Geo.Level`, 
                       Commodity = `Commodity`,
                       Data = `Data.Item`,
                       Category = `Domain.Category`)
p<-ggplot(data = veg.3, mapping = aes(Data, Geo ), environment = parent.frame())
print(p)

plot(x = Domain()$Year, y = Domain()$close, type = "l",
     xlab = "Year", ylab = "DOMAIN", col = color, fg = color, col.lab = color, col.axis = color)