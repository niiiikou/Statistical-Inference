#----------------------------Q6----------------------------

expected_rocognized = floor(320 * (1/26))
expected_not_rocognized = ceiling(320 * (25/26))
# declaring a data frame
DF <- data.frame(recognized = c(110, expected_rocognized) , 
                 not_recognized = c(210, expected_not_rocognized))
# changing row names of data frame
rownames(DF) <- c("Observed","Expected")
X_squared = (((DF$recognized[1]-DF$recognized[2])^2)/DF$recognized[2]) + (((DF$not_recognized[1]-DF$not_recognized[2])^2)/DF$not_recognized[2])
df = 1
p_value = pchisq(X_squared, df, lower.tail = FALSE)
DF
X_squared
df
p_value

#----------------------------Q7----------------------------

library(MASS)
chisq.test(caith)
chisq.test(caith)$expected

#----------------------------Q8----------------------------

coin = c(0, 1)
c = 0
n = 1000
for (i in 1:n){
  m = 1
  for (j in 1:20) {
    s = sample(coin, size = 1)
    m = m * s
  }
  if (m == 1) {
    c = c + 1
  }
}
p_value = c/n
p_value





