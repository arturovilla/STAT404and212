bbtable = read.csv("/Users/arturovillalobos/Desktop/STAT_212_rcode/HW5/baseball.csv",header = TRUE)

y = bbtable[,1]
x1 = bbtable[,2]
x2 = bbtable[,3]
x3 = bbtable[,4]
x4 = bbtable[,5]
x5 = bbtable[,6]
x6 = bbtable[,7]
x7 = bbtable[,8]
x8 = bbtable[,9]
x9 = bbtable[,10]
x10 = bbtable[,11]
x11 = bbtable[,12]
x12 = bbtable[,13]
x13 = bbtable[,14]
x14 = bbtable[,15]
x15 = bbtable[,16]
x16 = bbtable[,17]

# y = salary
# battingavg = x1
# onbasepercent = x2
# runs = x3
# hits = x4
# doubles = x5
# triples = x6
# homeruns = x7
# rbi = x8
# walks = x9
# strikeouts = x10
# stolenbases = x11
# errors = x12
# freeagenteli = x13
# freeagent = x14
# arbitrationeli = x15
# arbitration = x16

fit = lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16)

summary(fit)

#3
fithits = lm(y ~ x4)
plot(x4,y)


#testing weather or not we need certain variables.
fitreduced = lm(y~x3+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16)

ssefitfull = anova(fit)
ssefitreduced = anova(fitreduced)
summary(fitreduced)


155952067 - 154250172 / 5
1701895/5 = 340379


154250172/(337-16-1)
154250172/(320) = 482031.8
f = 0.7061339

#making a residuals plot of residuals and predicted values

plot(lm(fitreduced))

#kernal density 
resid = fitreduced$residuals
plot(density(resid))
abline(v = 0)

#normal probability plot of the standardized residuals
reducedstd = rstandard(fitreduced)
qqnorm(reducedstd, ylab="Standardized Residuals", xlab="Normal Scores", main="Normal probability plot of the standardized residuals") 
qqline(reducedstd)