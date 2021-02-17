# This coin toss simulation illustrates the Law of Large Numbers
# create a function to simulate a n coin tosses (Heads=1, Tails=2) of a fair coin
# and compute the mean of the tosses
coin.toss = function(n) {
      pn=mean(rbinom(n,1,.5))
      return(pn)
}
# you can set a seed here
#set.seed(100)
# set the number of tosses
n=1000
# create a vector storing the means of coin tosses 1:n
p <- 0
for (i in 1:n) {
  p[i] <- coin.toss(i)
}
# plot the tosses 
plot(p, type = "l", xlab = "n=number of tosses", ylab="p_n")
# add a line showing the probability of tossing heads
abline(.5,0,col='red',lty=1, lwd=1)
