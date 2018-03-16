plot<-ggplot(data = Air.temp, mapping = aes(TIME, AVG ), environment = parent.frame())
plot + geom_boxplot(mapping = aes(TIME))


plot0<-ggplot(data = Sea.temp, mapping = aes(TIME, AVG ), environment = parent.frame())
plot0 + geom_boxplot(mapping = aes(TIME))
