#example 3
qbeta(.05,3,1);
t=runif(1,1,4);
rbeta(1,t,1);
t;

#example 4
qnorm(.95,10,1);
t=runif(1,10,20);
mean(rnorm(n = 16,mean = t,sd = 4));
t;

#example 5
n=10;
qbinom(.05,size =n,prob = .6 )
t=runif(1,0,1);
sum(rbinom(n = n,size=1,prob = t));
t;

#example 6
n=11;
b0=4;
qgamma(0.95,n,1/b0);
t=runif(1,2,7);
sum(rexp(n,1/t));
t;

#example 7
n=5;
c=function(a,n){(4^n*a)^(1/n)};
c(.05,n);
t=runif(1,2,5);
x=runif(n,0,t);
sort(x)[n];
t;

