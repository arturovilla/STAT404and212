##Arturo Villalobos 827008236 3(a), 3(b), 3(d), 4 and 5, p. 382.

#3A 3B on pages

pilot = read.table("/Users/arturovillalobos/Desktop/STAT_212_rcode/hw9/PilotReacTimes.txt",header=T)
pilotdata = data.frame(pilot)

fitpilot = aov(times~design+pilot,data = pilotdata)
anova(fitpilot)

#4 
TukeyHSD(fitpilot,"design",conf.level=.99)
plot(TukeyHSD(fitpilot,"design",conf.level=.99))
#5done on poages and in notes

1-pf(19.547,3,12)
1-pf(32.014,4,12)
