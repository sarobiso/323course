#example 1 / example 2
a=qnorm(.025)*10/sqrt(100)+65
a
b=qnorm(.975)*10/sqrt(100)+65
b

teststat=(63.8-65)/(10/sqrt(100))
teststat
2*pnorm(teststat)

pnorm(b,66.5,10/sqrt(100))-pnorm(a,66.5,10/sqrt(100))

#load and install pwr package
install.packages("pwr")
library("pwr")
pwr.norm.test(d=(66.5-65)/10,n = 100,sig.level = 0.05,alternative = "two.sided")

truths=c(64.5,65,65.5,66)
1-(pnorm(b,truths,10/sqrt(100))-pnorm(a,truths,10/sqrt(100)))

d=(truths-65)/10
pwr.norm.test(d=d,n = 100,sig.level = 0.05,alternative = "two.sided")


z1=qnorm(.025)+sqrt(100)/10*(65-66.5)
z2=qnorm(.975)+sqrt(100)/10*(65-66.5)
pnorm(z2)-pnorm(z1)
1+pnorm(z1)-pnorm(z2)

truths<-seq(60,70,l=100)
d=(truths-65)/10

plot(d,pwr.norm.test(d=d,n=100,sig.level=0.05,alternative="two.sided")$power,
     type="l",ylim=c(0,1),ylab="power",xlab="(mu-muo)/sigma")

#example 3
qnorm(.95)*16/sqrt(16)+100
1-pnorm(qnorm(.95)*16/sqrt(16)+100,108,16/sqrt(16))

pwr.norm.test(d = (108-100)/16,n = 16,sig.level = 0.05,alternative = "greater")

truths<-seq(100,110,l=100)
d=(truths-100)/16

power=pwr.norm.test(d = d,n = 16,sig.level = 0.05,alternative = "greater")
power$power
plot(x = truths,y = power$power, type = "l")

(2*(qnorm(.95)-qnorm(.1)))^2

pwr.norm.test(d = (108-100)/16,power = .9,sig.level = 0.05,alternative = "greater")


pwr.norm.test(d = (101-100)/16,power = .9,sig.level = 0.05,alternative = "greater")
pwr.norm.test(d = (102-100)/16,power = .9,sig.level = 0.05,alternative = "greater")
pwr.norm.test(d = (103-100)/16,power = .9,sig.level = 0.05,alternative = "greater")
pwr.norm.test(d = (104-100)/16,power = .9,sig.level = 0.05,alternative = "greater")
pwr.norm.test(d = (105-100)/16,power = .9,sig.level = 0.05,alternative = "greater")
pwr.norm.test(d = (106-100)/16,power = .9,sig.level = 0.05,alternative = "greater")
pwr.norm.test(d = (107-100)/16,power = .9,sig.level = 0.05,alternative = "greater")
pwr.norm.test(d = (108-100)/16,power = .9,sig.level = 0.05,alternative = "greater")
pwr.norm.test(d = (109-100)/16,power = .9,sig.level = 0.05,alternative = "greater")

#example 5
qnorm(.01)
qnorm(.01,.2,sqrt(.2*.8/400))
qnorm(.01,.2,sqrt(.2*.8/400))*400

pnorm(72/400,.2,sqrt(.2*.8/400))

z=(qnorm(.01,.2,sqrt(.2*.8/400))-.17)/sqrt(.17*(1-.17)/400)
1-pnorm(z)        
pnorm(z)

((qnorm(.1)*sqrt(.2*.8)-qnorm(.95)*sqrt(.17*.83))/(.2-.17))^2
