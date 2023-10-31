
#--------------Q3.c
x = seq(0,1000,2) 
pois = dpois(x, lambda = 20 )
sum(pois)


#--------------Q8

foods = read.csv("D:/courses/statistics/tamrin/2/foods.csv")

#install.packages("ggplot2")
library("ggplot2")


#a.
ggplot(foods,aes(pricePerServing))+
  geom_histogram(aes(y = stat(density)),binwidth = 0.08) +
  scale_x_log10()+
  geom_density(col = "green")+
  labs(title="Histogram with density curve of pricePerServing")

#b.



ggplot(foods, aes(x = readyInMinutes, y=healthScore) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white") +
  labs(title="2D density plot of healthScore and readyInMinutes")


#c.
counted_each_group = table(foods$dishType)
sorted_table = data.frame(sort(counted_each_group))


ggplot(data=sorted_table, aes(x=Var1, y=Freq, fill=Var1)) +
  geom_bar(stat="identity") +
  theme(legend.position="none")+
  coord_flip()+
  labs(title="horizontal barplot to show dishType by their frequencies",x ='dishType',y = 'count')



#d.


ggplot(foods, aes(x=dishType, y=healthScore ,fill = dishType)) + 
  geom_boxplot()+
  theme(legend.position="none")+
  labs(title="the separate boxplots of the healthScore variable for each "dishType''.")




#e.
#install.packages("ggmosaic")
library('ggmosaic')


ggplot(foods)+
  geom_mosaic(aes(x=product(veryHealthy,dairyFree),fill=veryHealthy))+
  labs(title="the mosaic plot of veryHealthy and dairyFree")





















