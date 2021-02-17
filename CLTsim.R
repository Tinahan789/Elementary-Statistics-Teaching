# CLT simulation
set.seed(100)
# set the number of independent observations
n=1000
# set the sample mean
m=0
# set the sample standard deviation
s=1
# set the bin width on the histogram - in R it's called "breaks"
b=100
# this generates n samples drawn according to a normal dist with mean m and sd s
z=rnorm(n,m,s)
# create a probability histogram that shows the distribution of the generated observations
# hist default is a frequency histogram
hist(z, prob=TRUE)
# draw the normal curve for comparison
curve(dnorm(x,mean=m,sd=s),add=TRUE,lwd=2,col="violet")