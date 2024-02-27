myci <- function(t) {
  n <- length(t) # n is the sample size
  se <- sd(t)/sqrt(n); # Find the standard error of the sample
  m <- mean(t); # Find the sample mean
  cv <- qt(0.975,df=n-1) # cv is a critical value for the t distribution. P( t > cv ) = 0.025 = P( t < -cv )
  c(m-cv*se,m+cv*se) # Return the 95% confidence interval
}
mycolor <- function(endpoints, mn) {
  if (mn < endpoints[1]) 
    "Red" # if the mean is below the left endpoint of the confidence interval
  else if (mn > endpoints[2]) 
    "Orange" # if the mean is above the right endpoint of the confidence interval
  else 
    "Black" # if the mean lies between the endpoints
}
#install plotrix package
require(plotrix)
n <- 25 # The Sample Size
num.reps <- 50 # The Number of Replicates, or Random Samples, of Size n
w <- replicate(num.reps,rnorm(n,mean=3,sd=.5)) # w is a matrix, where each column is one random sample
y <- apply(w,2,mean) # apply the mean function to each column of w
ci <- apply(w,2,myci) # apply the myci function to each column of w. Each column of ci has the endpoints of a conficence interval. The first row has the left end points, the second row has the right end points.
z <- apply(ci,2,mycolor,3) # apply the mycolor function to each column of ci. Note: 3 is the true mean.
plotCI(x=1:num.reps,y=y,li=ci[1,],ui=ci[2,],col=z,lwd=3,ylim=c(2,4)) # plot the confidence intervals. The first row of ci, namely ci[1,], has the left end points. The second row of ci, namely ci[2,], has the right end points.
abline(h=3,lwd=3) # draw a horizontal line at the value of the true mean

#example 1
curve(dexp(x,rate = 1),from = 0,to = 5)
a=qexp(.05,rate = 1)
a
b=qexp(.05+.9,rate = 1)
b
#example 2
x1=100.1682
sig=10
curve(dnorm(x),from = -3,to = 3)
a=qnorm(.025)
a
b=qnorm(.025+.95)
b
x1-sig*c(b,a)
#example 3
n=30
aver=sum(round(rexp(30,1/5),2))
#aver=101.26
curve(dchisq(x,df = 30),from = 0,to = 60)
a=qchisq(.95,df = 30)
a

2*aver/a

#example 4
y=function(y,n){n*(y)^(n-1)}
curve(y(x,12),from = 0,to = 1)

n=32
larg=max(round(runif(n,0,5),2))
#larg=4.87

c(larg/.975^(1/n),larg/.025^(1/n))
