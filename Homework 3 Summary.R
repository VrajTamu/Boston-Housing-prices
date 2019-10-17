library(MASS)
attach(Boston)
help(Boston)
fit1  <- lm(log(medv)~.,Boston)
fit1
summary(fit1)  
a
lm(log(medv)~age+rm+zn+rad,Boston,Boston) # Structural Properties
a
summary(a)
b <- lm(log(medv)~rad,Boston) # Accessibility (distance to major business location)
b
summary(b)

c <- lm(log(medv)~crim+ptratio+chas,Boston)
c
summary(c)
help("Boston")
a <- lm(medv~., Boston)
a
summary(a)

abcd <- lm(medv~age+ptratio+dis,Boston)
apply(Boston, 2, mean)
#### Prdictive Analysis
my.house <- predict(abcd,newdata=data.frame(age=45.02, ptratio=17.40,
                                           dis=2.1))
my.house
my.house + quantile(resid(abcd), prob=c(.025,.975)) 

help("order")
order_price <- Boston[order(-medv),]
order_price
costly_house <- order_price[1:25,]
costly_house

#top_features_rad <- lm(medv~rad,data=costly_house)
top_features <- lm(medv~.,data=costly_house)
summary(top_features)
#par(mfrow=c(6,6))
plot(medv~.,costly_house)



#Comprehensive model
d <- lm(log(medv)~age+rm+rad+crim+ptratio+chas,Boston)
d
summary(d)
e<- lm(log(rm)~crim+ptratio+chas+medv+lstat,Boston)
e
summary(e)
# bargaining analysis
my.bargain <- predict(e,newdata=data.frame(medv=22.67,crim=3.2, ptratio=40,
 chas=1,lstat=10)) # medv average is 22.67
my.bargain
#Average rooms per dwelling is 6.285 = 6
corrplot(cor(Boston))
plot(e)

summary(e)
cor(Boston)



















#summary(top_features_rad)
#plot(medv~rad,costly_house)
#plot(medv~zn,costly_house)

#apply(order_price, 2, mean)
#top_features_chas <- lm(medv~chas,costly_house)
#plot(medv~chas,costly_house)

#top_features_tax <- lm(medv~chas,costly_house)
#plot(medv~tax,costly_house)

#top_features_ptratio <- lm(medv~chas,costly_house)
#plot(medv~ptratio,costly_house)
#summary(Boston$ptratio)

