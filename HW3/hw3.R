



#----------Q2.c

z=qnorm(0.1)
# z = (38.2-37.1)/(0.5/sqrt(n))
n = (z*0.5/(38.2-37.1))^2

#-----------------Q9

metadata <- read.csv("D:/courses/statistics/tamrin/3/metadata.csv", header=TRUE)
View(metadata)




#----------------Q10
#---b

calculate_power = function(x){
  mu_alpha = x
  mu =11.5
  se = 0.14
  alpha = 0.05
  z_alpha = qnorm(alpha)
  
  x_bar = (z_alpha*se) + mu
  z =(x_bar - mu_alpha)/se
  powwer = 1 - pnorm(z)

}

means= c(9,10,11,12,13)

powers = lapply(means, calculate_power)

plot(means, powers,type = "o", main = "power for each population mean", xlab = 'population_mean', ylab = 'power')

#----c

x_1 <- seq(8, 14, length=1000)
y_1 <- dnorm(x, mean=11, sd=0.14)
plot(x_1, y_1, type="l", lwd=1, col="blue")

par(new=TRUE,)

x_2 <- seq(8, 14, length=1000)
y_2 <- dnorm(x, mean=11.5, sd=0.14)
plot(x_2, y_2, type="l", lwd=1, col="red")
legend(1,100,legend=c("y1","y2","y3"), col=c("blue","red","black"),
       pch=c("o","*","+"),lty=c(1,2,3), ncol=1)
legend(1, 95, legend=c("Line 1", "Line 2"),
       col=c("blue", "red"), lty=1:2, cex=0.8)

