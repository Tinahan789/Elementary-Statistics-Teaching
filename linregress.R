# code for linear regression
# Remember to change all labels on plots
x= # independent data
y= # dependent data
cor(y,x) # gives correlation coefficient
# correlation is cov(x,y)/(sd(x)*sd(y))
plot(y~x, main="", xlab="", ylab = "") # scatter plot of data
model=lm(y~x) # creates linear regression model
summary(model) # gives coefficients/residuals/p-value
abline(model) # adds a line to the plot
plot(fitted(model)~y,main="", xlab="", ylab = "") # plots the predicted vs actual
res=resid(model) #saves the residuals from the model
hist(res,main="", xlab="", ylab = "") # plots a histogram of the residuals
plot(res~ fitted(model),main="", xlab="", ylab = "") # plots the residuals vs fitted values
###########################################################################################
# example with iris data set
# independent data
x=iris$Sepal.Length
# dependent data
y=iris$Petal.Width 
# gives correlation coefficient
cor(y,x) 
# correlation is cov(x,y)/(sd(x)*sd(y))
# scatter plot of data
plot(y~x, main="Iris: petal width ~ sepal length", xlab="sepal length", ylab = "petal width") 
# creates linear regression model
model=lm(y~x) 
# gives coefficients/residuals/p-value
summary(model)
# adds a line to the plot
abline(model, col="red") 
# plots the predicted vs actual
plot(fitted(model)~y,main="Iris model vs. actual values", 
     xlab="actual petal width", ylab = "predicted petal width") 
#saves the residuals from the model
res=resid(model) 
# plots a histogram of the residuals
hist(res,main="Histogram of Residuals from Iris Linear Model", 
     xlab="Residuals", ylab = "Frequency",col="skyblue") 
# plots the residuals vs fitted values
plot(res~ fitted(model),main="Residuals vs. Fitted", 
     xlab=" Fitted Values", ylab = "Residuals") 
