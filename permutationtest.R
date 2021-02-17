# Using the built in data set MT Cars, we will test wether or not number of cylinders has an effect on mpg
# So H0=mu1=mu2=mu3 (null hypothesis: cylinders has no impact on mpg)
# where mu1 is the mpg mean for 4 cyl,  mu2 is the mpg mean for 6 cyl, mu3 is the mpg mean for 8 cyl
# attach the data set
attach(mtcars)
# change number of cylinders from numeric to factor
as.factor(cyl)
# find the mean mpg for each of the cylinder types
# we use the assignment "x=" so that we can recall x by typing "x" rather then repeating a line of code
# using ";" allows us to run another command without taking up another line
# note that R (and most programming languages are case sensitive)
m1=mean(mtcars[mtcars$cyl=="4",1]);m1
m2=mean(mtcars[mtcars$cyl=="6",1]);m2
m3=mean(mtcars[mtcars$cyl=="8",1]);m3
# find the point estimates for the differences in mpg means
point.estimate1=m1-m2;point.estimate1
point.estimate2=m1-m3;point.estimate2
point.estimate3=m2-m3;point.estimate3
#now we will run a permutation test
#set the seed so the results can be repeated
set.seed(100)
# randomly split the mpg values into three groups
perm = sample(1:3,size=nrow(mtcars),replace=TRUE,prob=c(1/3,1/3,1/3))
# simulation results (means of mpg) where any difference in mpg is due to chance
p1=mean(mtcars[perm==1,1])
p2=mean(mtcars[perm==2,1])
p3=mean(mtcars[perm==3,1])
# create a data frame to compare mpg averages
m = data.frame("4cyl" = c(m1,p1), "6cyl" = c(m2,p2), "8cyl" = c(m3,p3));m
# calculate the differences to compare with the point estimates
diff1=p1-p2
diff2=p1-p3
diff3=p2-p3
# create a data frame to compare differences in mpg
d = data.frame("4-6diff" = c(point.estimate1,diff1), "4-8diff" = c(point.estimate2,diff2), "6-8diff" = c(point.estimate3,diff3));d
# we can repeat the permutation again to see what happens

# now we want to repeat this 100 times and store the differences
# Use the replicate function to create 100 permutations
n=100
x = replicate(n, {
 permute = sample(1:3,size=nrow(mtcars),replace=TRUE,prob=c(1/3,1/3,1/3))
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
mpgmean3 <- 0
for (i in 1:n) {
  mpgmean3[i] <- mpg.means(i,3)
}
# compute the differences in mpg means
mpgdiff1=mpgmean1-mpgmean2
mpgdiff2=mpgmean1-mpgmean3
mpgdiff3=mpgmean2-mpgmean3
# compute the means of the differnces
permmean1=mean(mpgdiff1)
permmean2=mean(mpgdiff2)
permmean3=mean(mpgdiff3)
# look at a data frame comparing the point estimate and the meaned permutation means
D = data.frame("4-6diff" = c(point.estimate1,permmean1), "4-8diff" = c(point.estimate2,permmean2), "6-8diff" = c(point.estimate3,permmean3));D
#look at a histogram of the permuted differences of 4cyl vs 6 cyl
hist(mpgdiff1)
abline(v=point.estimate1, lwd=2, col="purple")
# find the p-value
# find the number of times the absolute value of the permuted difference equaled or exceeded the point estimate
x1=sum(ifelse(mpgdiff1>=abs(point.estimate1),1,0))
p.val1=x1/n;p.val1
x2=sum(ifelse(mpgdiff2>=abs(point.estimate2),1,0))
p.val2=x2/n;p.val2
x3=sum(ifelse(mpgdiff3>=abs(point.estimate3),1,0))
p.val3=x3/n;p.val3