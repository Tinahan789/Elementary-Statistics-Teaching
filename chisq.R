# chi squared tests

# function that computes the chi-squared statistic parts
chi.s=function(o=observed,e=expected){
  cst=(o-e)^2/e
  return(cst)
}
#to find the chisquared statistic put all the pieces in a sum
c=sum(chi.s(o1,e1),chi.s(o2,e2),...,chi.s(ok,ek));c
#find the p-value with a chi-squared statistic
p=pchisq(c,2,lower.tail = FALSE);p


# chi-squared with p-value filled in and "rejection area" filled in
#these are the only two values you need to change:
d=3 # degrees of freedom
c=c # chi-squared statistic

# create density curve
curve(dchisq(x, df = d), from = 0, to = 30,
      main = 'Chi-Square Distribution (df = 3)', # remember to change the title
      ylab = 'Density',
      lwd = 2)

#find upper values for 95% of distribution: this is for alpha=0.05
upper95 <- qchisq(.95, d)

#create vector of x values: this is for alpha=0.05
x_upper95 <- seq(upper95, 30)

#create vector of chi-square density values: this is for alpha=0.05
p_upper95 <- dchisq(x_upper95, df = d)

#fill in portion of the density plot for upper 95% value to end of plot
#this shows the rejection area
polygon(c(x_upper95, rev(x_upper95)), c(p_upper95, rep(0, length(p_upper95))),
        col = adjustcolor('blue', alpha=0.3), border = NA)

# set Chi-squared statistic
chisq <- c 

#create vector of x values
x_chisq <- seq(chisq, 30)

#create vector of chi-square density values
p_chisq <- dchisq(x_chisq, df = d)

#fill in portion of the density plot for the p-value
polygon(c(x_chisq, rev(x_chisq)), c(p_chisq, rep(0, length(p_chisq))),
        col = adjustcolor('red', alpha=0.3), border = NA)
abline(v=c #c is the chisquared statistic
       ,lwd=2, col="red")
legend(10,.2,legend=c("Chi-Squared Statistic","p-value","Rejection Region"),
       col=c("red",adjustcolor('red', alpha=0.3),adjustcolor('blue', alpha=0.3)), 
       lty=1, lwd=10,cex=1.5)

#chi-squared with different degrees of freedom
curve(dchisq(x, df = 1), from = 0, to = 30,lwd=2,col="red",main = 'Chi-Square Distribution',ylab = 'Density')
curve(dchisq(x, df = 2),add=TRUE, from = 0, to = 30,lwd=2,col="orange")
curve(dchisq(x, df = 3),add=TRUE, from = 0, to = 30,lwd=2,col="yellow")
curve(dchisq(x, df = 4),add=TRUE, from = 0, to = 30,lwd=2,col="green")
curve(dchisq(x, df = 5),add=TRUE, from = 0, to = 30,lwd=2,col="blue")
curve(dchisq(x, df = 10),add=TRUE, from = 0, to = 30,lwd=2,col="purple")
legend(20,.5,legend=c("df=1", "df=2","df=3","df=4","df=5","df=10"),
       col=c("red", "orange","yellow", "green","blue", "purple"), lty=1, lwd=2,cex=1.5)