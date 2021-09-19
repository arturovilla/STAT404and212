DIAandAGE = read.table("/Users/arturovillalobos/Desktop/STAT_212_rcode/hw4/TreeAgeDiamSugarMaple.txt",header = TRUE)
x = DIAandAGE[,1]
y = DIAandAGE[,2]
#create models
fit1 = lm(y~poly(x,1))
fit2 = lm(y~poly(x,2))
fit3 = lm(y~poly(x,3))
fit4 = lm(y~poly(x,4))
fit5 = lm(y~poly(x,5))
fit6 = lm(y~poly(x,6))
fit7 = lm(y~poly(x,7))
fit8 = lm(y~poly(x,8))

#get the AIC and BIC for each model
AIC(fit1)
AIC(fit1,k=log(length(y)))
AIC(fit2)
AIC(fit2,k=log(length(y)))
AIC(fit3)
AIC(fit3,k=log(length(y)))
AIC(fit4)
AIC(fit4,k=log(length(y)))
AIC(fit5)
AIC(fit5,k=log(length(y)))
AIC(fit6)
AIC(fit6,k=log(length(y)))
AIC(fit7)
AIC(fit7,k=log(length(y)))
AIC(fit8)
AIC(fit8,k=log(length(y)))


#making a residuals plot of residuals and predicted values

plot(lm(fit2))

# 95 percent CI for predicted age at a x value
predict(fit2, interval = "predict" , data.frame(x = 110))