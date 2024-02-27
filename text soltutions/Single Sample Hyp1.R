#Large sample Hyp

#example 1
qnorm(.95)
1-pnorm((10.1-8.8)*sqrt(250)/3.2)
#load and install BSDA
zsum.test(mean.x = 10.1,sigma.x = 3.2,n.x = 250,mu = 8.8,alt="greater")
zsum.test(mean.x = 9.767105,sigma.x = 3.2,n.x = 250,mu = 8.8,alt="greater")

#example 2
qt(0.05,df = 49)
pt((57.5-60)*sqrt(50)/10.2,df=49)
tsum.test(mean.x = 57.5,s.x = 10.2,n.x = 50,alternative = "less",mu = 60)

#example 3
tsum.test(mean.x = 12.9,s.x = 1.2,n.x = 30,alternative = "greater",mu = 12)
qt(.95,29)
qt(.95,29)*1.2/sqrt(30)+12


#download and install pwr package d=(truth-hyp)/sd
pwr.t.test(n = 30,d = (13-12)/1.2,sig.level = 0.05,type="one.sample",alternative = "greater")
#delta=truth-hyp
power.t.test(n = 30,delta = 13-12,sd = 1.2,sig.level = .05,alternative = "one.sided",type = "one.sample")
1-power.t.test(n = 30,delta = 13-12,sd = 1.2,sig.level = .05,alternative = "one.sided",type = "one.sample")$power

#example 4
prop.test(x = 181,n = 200,p = .92,alternative = "less",correct = F)
sqrt(prop.test(x = 181,n = 200,p = .92,alternative = "less",correct = F)$statistic)

#example 5
qchisq(.95,df = 9)
1-pchisq(9*.141^2/.1^2,9)

#example 6
prop.test(x = 274,n = 376,p = .55,alternative = "greater",correct = F)
