#extract data and fit ortho polynomial model 
Hardwoodtable = read.table("HardwoodTensileStr.txt",header = TRUE)

x = Hardwoodtable[,1]
y = Hardwoodtable[,2]

fit = lm(y~poly(x,3))

#get relevent coefficients from summary
summary(fit)

# scatter plot with regression line

plot(x,y , title("Plot of concentration vs strength with 3rd degree polynomial model"))
ploy3model <- predict(fit)
lines(x,ploy3model)

# plot of the residuals

hardwoodresiduals = resid(fit)
plot(x,hardwoodresiduals, title("plot of the residuals from the 3rd degree polynomial"))
abline(0,0)

# making a 5 order polynomial model

fit5 = lm(y~poly(x,5))
summary(fit5)
plot(x,y , title("Plot of concentration vs strength with 5th degree polynomial model"))
ploy5model <- predict(fit5)
lines(x,ploy5model)

#plot of the 5th order models residuals
hardwoodresiduals = resid(fit5)
plot(x,hardwoodresiduals, title("plot of the residuals from the 5th degree polynomial"))
abline(0,0)

# plotting the model with the biggest p value term ignored (x^4)
plot(x,y , title("Plot of concentration vs strength with 5th degree polynomial model with x^4 ommited"))
# 7.2479

summary(fit5)

lines(x, 34.1842 + 32.3021*x - 45.3963*(x^2) - 14.5740*(x^3) + 7.2479*(x^5))