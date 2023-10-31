
#------------------Q8
#a.

scores = c(57, 66, 72, 78, 79, 79, 81, 81, 82, 83, 84, 87, 88, 88, 89, 90, 91, 92, 94, 95)
#b.

median(scores)
mean(scores)
var(scores)
sd(scores)

#d.

boxplot(scores)

#e.

hist(scores)
plot(density(scores))


#-------------------------Q9
  
imdb <- read.csv("D:/courses/statistics/tamrin/1/imdb.csv")

#a.

sapply(imdb, typeof)

#b.

hist(imdb$year, main = 'the number of movies produced yearly',xlab = 'year',  ylab = 'number of movies' )

#c.

hist(imdb$USA_gross_income, main = 'USA_gross_income', xlab = 'gross_income')

#d.
-------------------------

imdb <- read.csv("D:/courses/statistics/tamrin/1/imdb.csv", stringsAsFactors=TRUE)


boxplot(imdb$tomatometer_status, imdb$duration, main = 'movie duration along with tomatometer_status', xlab = 'tomatometer_status',ylab = 'movie duration', las =1)


-------------------------------------
  
#e.
  
func = function(x){
  if (x > 200)
    return("very long")
  else if (x > 150)
    return("long")
  else if(x > 80)
    return("standard")
  else
    return("short")
}



imdb$DurationGroup = lapply(imdb$duration, func)



very_long = 100*nrow(imdb[imdb$DurationGroup =='very long',])/nrow(imdb)
long = 100*nrow(imdb[imdb$DurationGroup =='long',])/nrow(imdb)
standard = 100*nrow(imdb[imdb$DurationGroup =='standard',])/nrow(imdb)
short = 100*nrow(imdb[imdb$DurationGroup =='short',])/nrow(imdb)


percentage_of_class = c(very_long, long, standard, short)
labels_of_class = c("very_long","long","standard","short")



pie(percentage_of_class, labels = labels_of_class, main = "movies based on their durations pie chart", col = rainbow(length(percentage_of_class)))

legend("topright", labels_of_class, cex = 0.8,fill = rainbow(length(percentage_of_class)))

#f.

plot(imdb$USA_gross_income, imdb$worldwide_gross_income, main = "USA_gross_income VS worldwide_gross_income", xlab = 'USA_gross_income', ylab = 'worldwide_gross_income')







