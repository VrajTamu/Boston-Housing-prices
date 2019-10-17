# Problem 1
library(ISLR)
attach(Auto)

# Answer to part a
plot(Auto)
# Answer to part b
library(dplyr)
#mydata <- c(Auto)
newdata<-select(Auto,1:8)
newdata
cor(newdata)

# Answer to part c
Ans_1_c <- lm(mpg~cylinders+displacement+horsepower+weight+acceleration+year+origin, data=Auto)
Ans_1_c #produced the multiple regression model
summary(Ans_1_c)
par(mfrow=c(2,2))
plot(Ans_1_c)

# Answer to part e
round(cor(newdata),2)
library(tidyverse)
vif(Ans_1_c)
Ans_1_d <- lm(mpg~acceleration+year+origin, data=Auto)
plot(Ans_1_d)

cor(newdata)

# Answer to part f
abc <- lm(mpg ~ cylinders, data=Auto)
# Problem 1
library(ISLR)
attach(newdata)

# Answer to part a
plot(Auto)
# Answer to part b
library(dplyr)
#mydata <- c(Auto)
newdata<-select(Auto,1:8)
newdata
cor(newdata)

# Answer to part c
Ans_1_c <- lm(mpg~cylinders+displacement+horsepower+weight+acceleration+year+origin, data=Auto)
Ans_1_c #produced the multiple regression model
summary(Ans_1_c)
par(mfrow=c(2,2))
plot(Ans_1_c)

# Answer to part e
round(cor(newdata),2)
library(tidyverse)
vif(Ans_1_c)
Ans_1_d <- lm(mpg~acceleration+year+origin, data=Auto)
plot(Ans_1_d)

# Answer to part f
abc <- lm(mpg ~ cylinders, data=Auto)
summary(abc) #r2 value 60.47 in percentage
abc2 <- lm(mpg ~ cylinders*displacement, data=Auto)
summary(abc2) #r2 value is 67.69 hence will keep this inteaction term
abc3 <- lm(mpg ~ cylinders*displacement*horsepower, data=Auto)
summary(abc3) #r2 value is 75.24 hence will keeo this interaction term
abc4 <- lm(mpg ~ cylinders*displacement*horsepower*weight, data=Auto)
summary(abc4) #r2 there is 77.53 hence only lesser amount of increase in interaction term 
abc5 <- lm(mpg~ cylinders*displacement*horsepower*acceleration, data=Auto)
summary(abc5) #r2 value is 77.9 hence scanty increase in interaction term

#abc6 <- lm(mpg~ cylinders*displacement*horsepower*year, data=Auto)
#summary(abc6) #r2 value is 85.84 shows there is increase in value hence will keep intercation term

abc7 <- lm(mpg~ cylinders*displacement*horsepower*origin, data=Auto)
summary(abc7) # r2 calue increased to 87.44 hence very much increase due to interaction term

            # Problem 2

#Problem part (a) answer
library(ISLR)
H = Carseats
H
names(H)
abc8 <- lm(Sales ~ Price + Urban + US, data=Carseats)
abc8
summary(abc8)

          # Problem 2 part (b)



          # Problem 2 part (c)
# Sales = 13.043469 - 0.054459(Price) - 0.021916(UrbanYes) + 1.200573(USYes)
  
          # Problem part(d)
# Here for the variables such as Price, US we can reject the null hypothesis  
# of Bj = 0, as the p values are really small here.

          # Problem part(e)
a <- lm(Sales~., data=Carseats)
summary(a)
# From the table variables that are closely associated with Sales are   
# CompPrice, Income, Advertising, Price, ShelveLocGood, ShelveLocMedium and Age  
final_model = lm(Sales~ Price + US, data=Carseats)
summary(final_model)

          # Problem part(f)
  #r2 value for the model (e) is 87.2 while that of the model (a) is 23.93.
  # Hence, the model(e) fits better than model(a)

          #Problem part(g)
final_model <- lm(Sales~CompPrice + Income + Advertising + Price + ShelveLoc + Age, data=Carseats)
summary(final_model)

#require(car)
?vif
vif(final_model)

plot(final_model)

