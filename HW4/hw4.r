
#-----------------Q3

#install.packages("palmerpenguins")
library(palmerpenguins)
library("ggplot2")
penguins
#a.

df = penguins[, c('species', 'flipper_length_mm')]

#b.
ggplot(df, aes(species, flipper_length_mm)) +
  geom_jitter() +
  labs(title="Flipper_length of each species")
  


#c.

# Draw points on the qq-plot:
qqnorm(df$flipper_length_mm)
# Draw the reference line:
qqline(df$flipper_length_mm, col = "steelblue", lwd = 2) ## adds the line to the plot


#d.

ggplot(df, aes(species, flipper_length_mm))+ 
  geom_dotplot(binaxis='y', stackdir='center',
               stackratio=1.5, dotsize=0.3)+
  labs(title="Equality of variances")

#e.

aggregate(flipper_length_mm ~ species,
          data = df,
          function(x) round(c(mean = mean(x), sd = sd(x)), 2)
)



#by(df, df$species, summary ,stats ='Mean')



#descr(penguins,
#      headings = FALSE, # remove headings
#      stats = "common")

#f.
res_aov <- aov(flipper_length_mm ~ species,
               data = df
)

#install.packages("report")
library("report") 
report(res_aov)

#g.





#-------------------Q9

published_papers =c(25, 27, 35, 42, 28, 37, 40, 31, 29, 33, 30, 26, 31, 28, 30, 15)


#b.
#install.packages("boot")
library(boot)

mean_func <- function(data)
{
  
  c(mean(data))
}


myBootstrap <- boot(published_papers, mean_func(published_papers), R=1000)

#c.





#-------------------Q10


car = read.csv("D:/courses/statistics/tamrin/4/car_train.csv")
View(car)

price = na.omit(c(car$price))

#a.

#---------------i

n = 200
sample_of_price = sample(price , n ,replace = FALSE)
actual_mean =  mean(price)

mean_of_sample = mean(sample_of_price)
std_of_sample = sd(sample_of_price)

mu = 18000
z_score = (mean_of_sample - mu) / (std_of_sample / sqrt(n))

p_value = 2*pnorm(z_score ,lower.tail =FALSE)



#--------ii

confidence_interval=  c(mean_of_sample - qnorm(0.95+0.025)*std_of_sample/sqrt(n),
        mean_of_sample + qnorm(0.95+0.025)*std_of_sample/sqrt(n))

#-----------iii

z2  = (mean_of_sample - mu)/(std_of_sample/sqrt(n))


beta =  pnorm(z2 , lower.tail = TRUE)

#-------------iv

power =  1-beta


#b.







df_car = car[, c("price","odometer")]
selected = df_car[sample(nrow(df_car), 25), ]
price = selected[, c("price")]
df_car_2 = df_car[!row.names(df_car) %in% row.names(selected),]
selected_2 = df_car_2[sample(nrow(df_car_2), 25), ]
odometer = selected_2[, c("odometer")]
#H0: mean_price - mean_odometer = 0
#HA: mean_price - mean_odometer!= 0
mean_price = mean(price)
mean_odometer = mean(odometer)
sd_price = sd(price)
sd_odometer = sd(odometer)
SE =sqrt((sd_price^2/25 ) + (sd_odometer^2/25))
df = 24
T_df  = (mean_odometer-mean_price)/SE
p_value = 2* pt(T_df, df = df, lower.tail = FALSE)

  
#c.

Automatic_cars = car[car$transmission == "Automatic",]
Manual_cars = car[car$transmission == "Manual",]
#H0: mean_Automatic_cars - mean_Manual_cars = 0
#HA: mean_Automatic_cars - mean_Manual_cars > 0
a=Automatic_cars[, c("price")]
mean_Automatic_cars = mean(a)
mean_Manual_cars = mean(Manual_cars[, c("price")])
sd_Automatic_cars = sd(Automatic_cars[, c("price")])
sd_Manual_cars = sd(Manual_cars[, c("price")])
number_of_Automatic_cars = nrow(Automatic_cars)
number_of_Manual_cars = nrow(Manual_cars)
SE =sqrt((sd_Automatic_cars^2/number_of_Automatic_cars )+(sd_Manual_cars^2/number_of_Manual_cars))
df = min(number_of_Automatic_cars-1, number_of_Manual_cars-1)
T_df  = (mean_Automatic_cars-mean_Manual_cars)/SE
p_value = pt(T_df, df = df, lower.tail = FALSE)


p_value





