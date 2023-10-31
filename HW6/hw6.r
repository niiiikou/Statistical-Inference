


#--------------Q4------------------

#---------a---------------------

#install.packages("airports")
#install.packages("cherryblossom")
#install.packages("usdata")
#install.packages("openintro")
library(airports)
library(cherryblossom)
library(usdata)
library(openintro)



View(absenteeism )

#---------i---------------------
absenteeism$eth = 
  ifelse( absenteeism$eth == "A",0,1)
#---------ii---------------------
absenteeism$sex = 
  ifelse( absenteeism$sex == "F",0,1)

#---------iii---------------------
absenteeism$lrn = 
  ifelse( absenteeism$lrn == "AL",0,1)


#---------b---------------------

linearMod = lm(days ~ eth + sex + lrn, data=absenteeism) 
summary(linearMod)
#---------c---------------------




#---------d---------------------


#pairs(absenteeism )
summary(linearMod)$adj.r.squared

#---------e---------------------

#plot(fitted(linearMod), residuals(linearMod))

plot(x = linearMod, which = 1)



#--------------Q8------------------


#----------a---------

# load the data
data(state)
dat = as.data.frame(state.x77)
# correct column names
colnames(dat)[4] = "Life_Exp"  
colnames(dat)[6] = "HS_Grad"      



m_full = lm(Life_Exp ~ Population + Income + Illiteracy + Murder + HS_Grad + Frost + Area , data = dat)
summary(m_full)

m_step1 = lm(Life_Exp ~ Population + Illiteracy + Murder + HS_Grad + Frost + Area , data = dat)
summary(m_step1)

m_step2 = lm(Life_Exp ~ Population + Illiteracy + Murder + HS_Grad + Frost  , data = dat)
summary(m_step2)

m_step3 = lm(Life_Exp ~ Population + Murder + HS_Grad + Frost  , data = dat)
summary(m_step3)


m_step4 = lm(Life_Exp ~ Murder + HS_Grad + Frost  , data = dat)
summary(m_step4)


#----------b---------
modell = lm(Life_Exp ~  Murder, data = dat)
summary(modell)

#----------c---------
#load ggplot2
library(ggplot2)

#create histogram of residuals
ggplot(data = dat, aes(x = modell$residuals)) +
  geom_histogram(bins = 25, fill = 'steelblue', color = 'black') +
  labs(title = 'Histogram of Residuals', x = 'Residuals', y = 'Frequency')

summary(modell)


#----------d---------


plot( x = modell, which = 2 )









