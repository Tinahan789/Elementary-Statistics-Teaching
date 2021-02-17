# Using the built in data set MT Cars, we will test wether or not number of cylinders has an effect on mpg
# So H0=mu1=mu2 (null hypothesis: cylinders has no impact on mpg)
# where mu1 is the mpg mean for 4 cyl,  mu2 is the mpg mean for 6 cyl (this dataset also includes 8 cyl cars, but we will only make the one comparison)
# attach the data set
attach(mtcars)

# change number of cylinders from numeric to factor
as.factor(cyl)
# create new data frame with the wanted values
new.mt=mtcars[mtcars$cyl!="8", 1:2]

# find the mean mpg for each of the cylinder types
# we use the assignment "x=" so that we can recall x by typing "x" rather then repeating a line of code
# using ";" allows us to run another command without taking up another line
# note that R (and most programming languages are case sensitive)
m1=mean(new.mt[new.mt$cyl=="4",1]);m1
m2=mean(new.mt[new.mt$cyl=="6",1]);m2

# find the point estimates for the differences in mpg means
point.estimate=m1-m2;point.estimate

#now we will run a permutation test
#set the seed so the results can be repeated
set.seed(100)

# randomly split the mpg values into two groups
perm = sample(1:2,size=nrow(new.mt),replace=TRUE,prob=c(1/2,1/2))

# simulation results (means of mpg) where any difference in mpg is due to chance
p1=mean(new.mt[perm==1,1])
p2=mean(new.mt[perm==2,1])

# create a data frame to compare mpg averages
m = data.frame("4cyl" = c(m1,p1), "6cyl" = c(m2,p2));m

# calculate the differences to compare with the point estimates
diff=p1-p2;diff
point.estimate

# we can repeat the permutation again to see what happens

# now we want to repeat this n times and store the differences
# Use the replicate function to create n permutations
n=100
x = replicate(n, {
  permute = sample(1:2,size=nrow(new.mt),replace=TRUE,prob=c(1/2,1/2))
})
#create a function to find the means at each index of permutation (i) for each category (m)
mpg.means = function(i,m) {
  mpgmean=mean(mtcars[x[,i]==m,1])
  return(mpgmean)
}
# store the mpg means
mpgmean1 <- 0
for (i in 1:n) {
  mpgmean1[i] <- mpg.means(i,1)
}
mpgmean2 <- 0
for (i in 1:n) {
  mpgmean2[i] <- mpg.means(i,2)
}

# compute the difference in mpg means
mpgdiff=mpgmean1-mpgmean2

# compute the mean of the differnce
permmean=mean(mpgdiff)

#look at a histogram of the differences in the sample vs the point estimate
hist(mpgdiff)
abline(v=point.estimate, lwd=2, col="purple")
# find the p-value
# find the number of times the absolute value of the permuted difference equaled or exceeded the point estimate
x=sum(ifelse(mpgdiff>=abs(point.estimate),1,0))
p.val=x/n;p.val