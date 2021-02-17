# z test
p.estimate=mean(climate$dx70_diff)
expected=0
s=sd(climate$dx70_diff)
n=197
SE=s/sqrt(n);SE
z=(p.estimate-expected)/SE;z
p.val=pnorm(-abs(z));p.val #one tailed test
p.val=2*pnorm(-abs(z));p.val #two tailed test
# Normal curve with p-value filled in and "rejection area" filled in
# Be aware of what you need for one and two tailed tests
s = 1
m = 0
a=.975
# create density curve
curve(dnorm(x, m,s), from = -4, to = 4,
      main = 'Normal Distribution', # remember to change the title
      ylab = 'Density',
      lwd = 2)
#upper tail rejection area
lower.x = qnorm(a,m,s)
upper.x = 100
step = (upper.x - lower.x) / 10000
bounds = c(m-3*s, m+3*s)
cord.x = c(lower.x,seq(lower.x,upper.x,step),upper.x)
cord.y = c(0,dnorm(seq(lower.x,upper.x,step),m,s),0)
polygon(cord.x,cord.y,col=adjustcolor('blue', alpha=0.3))
#lower tail rejection area
lower.x1 = -100
upper.x1 = qnorm(1-a,m,s)
step1 = (upper.x1 - lower.x1) / 10000
bounds = c(m-3*s, m+3*s)
cord.x1 = c(lower.x1,seq(lower.x1,upper.x1,step1),upper.x1)
cord.y1 = c(0,dnorm(seq(lower.x1,upper.x1,step1),m,s),0)
polygon(cord.x1,cord.y1,col=adjustcolor('blue', alpha=0.3))
# p-value on right side
lower.z = abs(z)
upper.z1 = 100
stepz = (upper.z1 - lower.z) / 10000
bounds = c(m-3*s, m+3*s)
cord.z1 = c(lower.z,seq(lower.z,upper.z1,stepz),upper.z1)
cord.z2 = c(0,dnorm(seq(lower.z,upper.z1,stepz),m,s),0)
polygon(cord.z1,cord.z2,col=adjustcolor('red', alpha=0.3))
#p-value on left side
lower.z1 = -100
upper.z = -abs(z)
step2 = (upper.z - lower.z1) / 10000
bounds = c(m-3*s, m+3*s)
cord.x2 = c(lower.z1,seq(lower.z1,upper.z,step2),upper.z)
cord.y2 = c(0,dnorm(seq(lower.z1,upper.z,step2),m,s),0)
polygon(cord.x2,cord.y2,col=adjustcolor('red', alpha=0.3))
#zscore
abline(v=z #z is the z-score
       ,lwd=2, col="red")
legend(-4,.4,legend=c("z score","p-value","Rejection Area"),
       col=c("red",adjustcolor('red', alpha=0.3),adjustcolor('blue', alpha=0.3)), 
       lty=1, lwd=10,cex=1)
#################################################################################
#t distribution
# Display the Student's t distributions with various
# degrees of freedom and compare to the normal distribution

x <- seq(-4, 4, length=100)
hx <- dnorm(x)

degf <- c(1, 3, 8, 30)
colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df=1", "df=3", "df=8", "df=30", "normal")

plot(x, hx, type="l", lty=2, xlab="x value",
     ylab="Density", main="Comparison of t Distributions")

for (i in 1:4){
  lines(x, dt(x,degf[i]), lwd=2, col=colors[i])
}

legend("topright", inset=.05, title="Distributions",
       labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)

# t test
p.estimate=53.3
expected=50
s=5.2
n=14
SE=s/sqrt(n);SE
t=(p.estimate-expected)/SE;t
df=n-1
p.val=pt(-abs(t), df);p.val #one tailed test
p.val=2*pt(-abs(t), df);p.val #two tailed test
# t dist with p-value filled in and "rejection area" filled in
# Be aware of what you need for one and two tailed tests
a=.975 #this sets the significance level
t=1.753247
# create density curve
curve(dt(x,df), from = -4, to = 4,
      main = 't Distribution', # remember to change the title
      ylab = 'Density',
      lwd = 2)
#upper tail rejection area
lower.x = qt(a,df)
upper.x = 100
step = (upper.x - lower.x) / 10000
bounds = c(-4, 4)
cord.x = c(lower.x,seq(lower.x,upper.x,step),upper.x)
cord.y = c(0,dt(seq(lower.x,upper.x,step), df),0)
polygon(cord.x,cord.y,col=adjustcolor('blue', alpha=0.3))
#lower tail rejection area
lower.x1 = -100
upper.x1 = qt(1-a,df)
step1 = (upper.x1 - lower.x1) / 10000
bounds = c(-4, 4)
cord.x1 = c(lower.x1,seq(lower.x1,upper.x1,step1),upper.x1)
cord.y1 = c(0,dt(seq(lower.x1,upper.x1,step1),df),0)
polygon(cord.x1,cord.y1,col=adjustcolor('blue', alpha=0.3))
# p-value on right side
lower.t = abs(t)
upper.t1 = 100
stept = (upper.t1 - lower.t) / 10000
bounds = c(-4,4)
cord.t1 = c(lower.t,seq(lower.t,upper.t1,stept),upper.t1)
cord.t2 = c(0,dt(seq(lower.t,upper.t1,stept),df),0)
polygon(cord.t1,cord.t2,col=adjustcolor('red', alpha=0.3))
#p-value on left side
lower.t1 = -100
upper.t = -abs(t)
step2 = (upper.t - lower.t1) / 10000
bounds = c(-4,4)
cord.x2 = c(lower.t1,seq(lower.t1,upper.t,step2),upper.t)
cord.y2 = c(0,dt(seq(lower.t1,upper.t,step2),df),0)
polygon(cord.x2,cord.y2,col=adjustcolor('red', alpha=0.3))
#tscore
abline(v=t #T score
       ,lwd=2, col="red")
legend(-4,.4,legend=c("T score","p-value","Rejection Area"),
       col=c("red",adjustcolor('red', alpha=0.3),adjustcolor('blue', alpha=0.3)), 
       lty=1, lwd=10,cex=1)
