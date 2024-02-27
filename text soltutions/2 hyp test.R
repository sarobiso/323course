#example 1
A=c(1.8,+1.4,-3.4,+4.8,+3.3,-0.1,+2.9,-1.1,+3.1,-1.4,+2.1,-0.7,+0.8,-2.7,+0.6,-0.6,-1.8,+1.2,-0.5,-1.1 )
B=c(-1.3,+10.0,-2.0,-7.3,+2.8,+6.8,-0.3,+1.5,-4.7,+22.6,+1.1,+4.3,+3.5,+6.7,+5.0,+11.2,-5.5,-2.3,-0.4,0.0)
alpha=.05
var.test(B,A,conf.level = 1-alpha)
sqrt(var.test(B,A,conf.level = 1-alpha)$conf.int)

var(A)
var(B)
var(A)/var(B)
pf(var(A)/var(B),19,19)

var.test(A,B,ratio = 1,alternative = "less")

#example 2
pf(.918^2/.919^2,49,47)
1-pf(.918^2/.919^2,49,47)

2*pf(.918^2/.919^2,49,47)

poolvar=(49*.918^2+47*.919^2)/(50+48-2)
TS=(4.12-3.917)/sqrt(poolvar*(1/50+1/48))
1-pt(TS,50+48-2)

########## If using unpooled? #TS changes ever so slightly and df changes slighty (more sign. differences if n is small)
df=(.918^2/50+.919^2/48)^2/((1/49)*(.918^2/50)^2+(1/47)*(.919^2/48)^2)
TS=(4.12-3.917)/sqrt((.918^2/50+.919^2/48))
1-pt(TS,df)

#example 3
A=c(71, -3, -1, -3, 286, 3, -8, -1, -6, 6, 40, 3, 72, 45, -6, -5, -5, -3, -3, -3, 121, 20, 76, -4, 0, 40, 52, 3, 4, -3, 47, -16, 0, 38, 40, 5, -5, 36, -6, -1, -4, -8, -6, 17, -3, 72, -2, 3, 5, -7)
B=c(31, 1, -2, -9, 0, 43, 21, 5, -6, -1, 1, -5, 3, -5, -7, -4, 44, -1, 42, -5, -7, 36, 17, 132, -3 , -4, -3, 7, -1 , 1, 0, 11, 5, 3, 16, 91, 12, 25, -2, -2, 6, 165, 1 , -2, 0, 36, 3, 4, -9, -4)
hist(A,freq = F)
curve(dnorm(x,mean(A),sd(A)),add=T,col="red",lwd=2)
hist(B,freq=F)
curve(dnorm(x,mean(B),sd(B)),add=T,col="red",lwd=2)

mean(A)
mean(B)
curve(dexp(x,mean(A)),col="red",lwd=2)
curve(dexp(x,mean(B)),col="blue",add=T,lwd=2)

#download and instal nortest package
ad.test(A)
ad.test(B)

var.test(A,B,ratio = 1)

#load install car package
data=data.frame(id=c(rep("A",length(A)),rep("B",length(B))),time=c(A,B))
data
leveneTest(data$time~data$id)

t.test(A,B,var.equal = T)
t.test(A,B,var.equal = F)

#Example 4
prop.test(x = c(64,68),n = c(374,378),alternative = "two.sided",correct = F)

(((64/374)-(68/378)))/sqrt(((64+68)/(374+378))*(1-(64+68)/(374+378))*((1/374)+(1/378)))
2*pnorm(-0.3161303)

#Example 5
(((224/840)-(113/648))-.1)/sqrt(((224/840)*(1-224/840)/(840))+((113/648)*(1-113/648)/(648)))
pnorm(-0.3617398)

#Example 6
prop.test(x=c(45,239),n = c(12853,79666),correct = F)
2*pnorm(sqrt(0.90813)) #not correct since the prob is over 1....
2*(1-pnorm(sqrt(0.90813)))
