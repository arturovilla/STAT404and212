# Bacteriatable = read.csv("BacteriaDeath.csv",header = FALSE)


# t = Bacteriatable[,1]
# y = Bacteriatable[,2]
# lgy = log(y)

# plot(t,y , title("Plot of T and Y"))

# plot(t,lgt , title("Plot of X and Log(Y)"))

# summary(lm(lgy~x))
# the coefficient  of t is -0.218425
# the intercept is  5.973160
WindSpeedtable = read.csv("WindSpeed.csv", header =FALSE)

x = WindSpeedtable[,2]
y = WindSpeedtable[,1]
oneoverx = 1/x

plot(x,y,title("Plot of Speed vs Output"))

plot(oneoverx, y, title("Plot of 1/Speed vs Output"))
#1/x plot suggest a more linear relationship
summary(lm(y~oneoverx))
# the intercept is 2.9789
# the coefficient is -6.9345


lines(c(0.40816327,0.09803922) , c(-6.9345*0.40816327+ 2.9789, -6.9345*0.09803922+ 2.9789))

-6.9345*(1/8) + 2.9789

outoutlm = lm(y~oneoverx)
outputes = resid(outoutlm)

plot(oneoverx, outputes, title("Plot of 1/Speed and the Residuals"))
abline(0,0)

#coefficient of determination
summary(lm(y~oneoverx))$r.squared

#confidense interval for slope 
fit = lm(y~oneoverx)
confint(fit , level = .99)

#confidence interval for average values at a particular x
predict(fit, interval = "confidence" , data.frame(oneoverx = .3125))
#confidense interval for predicted value at a x
predict(fit, interval = "predict" , data.frame(oneoverx = .1104))

