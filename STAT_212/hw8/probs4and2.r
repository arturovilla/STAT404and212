pore = read.table("/Users/arturovillalobos/Desktop/STAT_212_rcode/hw8/PorousCarbon.txt",header=T)
poredata = data.frame(pore)

fitpore = aov(values~as.factor(temp),data = poredata)

#4a tukeys 95 % simukltanius CI 
Tukvals = TukeyHSD(fitpore)
plot(Tukvals)
##arrange the means in creasing order
mean(poredata$values[poredata$temp=="300"])
mean(poredata$values[poredata$temp=="400"])
mean(poredata$values[poredata$temp=="500"])
mean(poredata$values[poredata$temp=="600"])


#problem  2 
cell = read.table("/Users/arturovillalobos/Desktop/STAT_212_rcode/hw8/CellPhoneRL.txt",header = T)
celldata = data.frame(cell)
fitcell = aov(y~as.factor(S)*as.factor(C),data = celldata)
anova(fitcell)
#multiple comparisons
tukcell = TukeyHSD(fitcell)
plot(tukcell)

#interaction plot
interaction.plot(celldata$C,celldata$S,celldata$y)

#normality assumptions
plot(fitcell,which = 1)
plot(fitcell,which = 2)
