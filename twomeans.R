# Paired Data Example L14.1
# Climate data from: https://www.openintro.org/data/index.php?data=climate70
climate=read.table("climate", header = TRUE)
climate$dx70_diff<-climate$dx70_2018-climate$dx70_1948 #adds a column of the differences for dx70 to data frame
climate$dx90_diff<-climate$dx90_2018-climate$dx90_1948 #adds a column of the differences for dx90 to data frame
str(climate)
head(climate)
tail(climate)
hist(climate$dx70_diff,col="skyblue")
mean(climate$dx70_diff)
sd(climate$dx70_diff)
# z test
p.estimate=mean(climate$dx70_diff)
expected=0
s=sd(climate$dx70_diff)
n=197
SE=s/sqrt(n);SE
z=(p.estimate-expected)/SE;z
p.val=2*pnorm(-abs(z));p.val #two tailed test
# t test
p.estimate=mean(climate$dx70_diff)
expected=0
s=sd(climate$dx70_diff)
n=197
SE=s/sqrt(n);SE
t=(p.estimate-expected)/SE;t
df=n-1
p.val=2*pt(-abs(t), df);p.val #two tailed test

#Plot for z
m=0;s=1;a=.975 # set parameters for standard normal
curve(dnorm(x, m,s), from = -4, to = 4,main = 'Normal Distribution', 
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
abline(v=z ,lwd=2, col="red")
legend(-4,.4,legend=c("z score","p-value","Rejection Area"),
                col=c("red",adjustcolor('red', alpha=0.3),adjustcolor('blue', alpha=0.3)), 
               lty=1, lwd=10,cex=1)
#plot for t
a=.975
#Draw density curve
curve(dt(x,df), from = -4, to = 4, main = 't Distribution', ylab = 'Density',lwd = 2)
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
#T score
abline(v=t ,lwd=2, col="red")
#legend
legend(-4,.4,legend=c("T score","p-value","Rejection Area"),
           col=c("red",adjustcolor('red', alpha=0.3),adjustcolor('blue', alpha=0.3)), 
             lty=1, lwd=10,cex=1)
########################################################################################
# function to find SE for two means
SE.x.y=function(s1=standard.deviation1,n1=sample.size1,s2=standard.deviation2,n2=sample.size2){
  SExy=sqrt((s1^2/n1)+(s2^2)/n2)
    return(SExy)
}
#function for t confidence interval
conf.int.t=function(p=point.estimate,c=confidence.level,s=standard.error, df=degrees.of.freedom)
{
  CI.l=p-qt(1-(1-c)/2,df)*s
  CI.u=p+qt(1-(1-c)/2, df)*s
  list(lower=CI.l,upper=CI.u)
}
#example L14.2
conf.int.t(3,.90,1.721918,9999)
