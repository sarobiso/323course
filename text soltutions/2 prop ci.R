#example 1
pointest=.4-.17
SE=sqrt(.4*.6/2100+.17*.83/1900)
ME=qnorm(1-.025)*SE
c(pointest-ME,pointest+ME)

prop.test(x=c(840,323),n=c(2100,1900),conf.level = 0.95,correct = F)

#example 2
pointest=61/680-138/692
SE=sqrt(61/680*(1-61/680)/680+138/692*(1-138/692)/692)
ME=qnorm(1-.025)*SE
c(pointest-ME,pointest+ME)

prop.test(x=c(61,138),n=c(680,692),conf.level = 0.95,correct = F)

#example 3
pointest=32/62-19/61
SE=sqrt(32/62*(1-32/62)/62+19/61*(1-19/61)/61)
ME=qnorm(1-.01/2)*SE
c(pointest-ME,pointest+ME)

prop.test(x=c(32,19),n=c(62,61),conf.level = 0.99,correct = F)
prop.test(x=c(32,19),n=c(62,61),conf.level = 0.95,correct = F)

#example 4
prop.test(x=c(76,98),n=c(154,164),conf.level = 0.99,correct = F)

#example 5
prop.test(x=c(17,8),n=c(47,54),conf.level = 0.95,correct = F)
