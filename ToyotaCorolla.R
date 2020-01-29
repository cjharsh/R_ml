# Toyota Corolla Dataset Analysis

data1 = read.csv("ToyotaCorolla.csv") # Reading the dataset

print(data1)

#PCA
str(data1)

data1$FuelType = factor(data1$FuelType, levels = c("CNG","Diesel","Petrol"), labels = c(0,1,2))

library(ggplot2)
ggplot(data1,aes(y=Price,x=Age))+geom_point()

ggplot(data1,aes(y=Price,x=KM))+geom_point()
ggplot(data1,aes(y=Price,x=FuelType))+geom_point()
ggplot(data1,aes(y=Price,x=HP))+geom_point()

qqnorm(data1$Price)
qqline(data1$Price)
qqplot(data1$Age,data1$Price)

library(caTools)

split=sample.split(data1$Price,SplitRatio = 0.7)

training_set=subset(data1,split==TRUE)
test_set = subset(data1,split==FALSE)

reg=lm(formula = Price ~ .,data = training_set)

y_pred=predict(reg,newdata = test_set) 
y_pred
summary(reg) 


cor.test(y_pred,test_set$Price)
