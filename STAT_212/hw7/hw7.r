#problem number 3
rat = read.table("/Users/arturovillalobos/Desktop/STAT_212_rcode/hw7/SleepRem.txt",header=T)
ratdata = data.frame(rat)

fitrat = aov(values~as.factor(ind),data = ratdata)

anova(fitrat)

qf(.95,3,16)

anova(aov(resid(aov(ratdata$values~ratdata$ind))**2~ratdata$ind)) #tets assumptions of equal varience

shapiro.test(resid(aov(ratdata$values~ratdata$ind)))##tests normality assuptions

boxplot(resid(fitrat))#boxplot

plot(fitrat,which=2)

#problem number 6
pore = read.table("/Users/arturovillalobos/Desktop/STAT_212_rcode/hw7/PorousCarbon.txt",header=T)
poredata = data.frame(pore)

fitpore = aov(values~as.factor(temp),data = poredata)
anova(fitpore)

#analysis of varience
anova(aov(resid(aov(poredata$values~poredata$temp))**2~poredata$temp))
#normality test
shapiro.test(resid(aov(poredata$values~poredata$temp)))

boxplot(resid(fitpore))#boxplot

plot(fitpore,which=2)#qqplot

