##function from class leaps.AIC
leaps.AIC=function(X,y){
#
# X is an n x k matrix and y a vector of n values.
#
# This function fits linear models of the sort lm(y~X[,cols]), where
# cols is a subset of {1,...,k}.  It uses the package leaps to find
# the best model (according to R^2) among all those for which the
# number of elements of cols is j, and it does so for j=1,...,k.  It
# then determines AIC and BIC for each of the k models so determined.
#
# In case X and y aren't of the right mode, make them a matrix and
# a vector, respectively.
#
y=as.vector(y)
X=as.matrix(X)
num=ncol(X)
#
# Create the leaps output, returning only the best model for each
# number of variables (i.e., nbest=1). 
#
out=leaps(X,y,method='r2',nbest=1)
# 
# Initialize the aic and bic vectors.
#
aic=1:num
bic=1:num
#
# Compute all the AIC and BIC values.
#
for(j in 1:num){
#
# Determine the variables in the best model containing j independent
# variables. 
#
cols=(1:num)[out$which[j,]==TRUE]
fit=lm(y~X[,cols])
#
# Determine AIC and BIC for the best model with j variables.
#
aic[j]=AIC(fit)
bic[j]=AIC(fit,k=log(length(y)))
}
#
# Print out the output.
#
print("AIC values")
print(aic)
print("BIC values")
print(bic)
#
# Return the output.
#
list(aic,bic)
}


baseball = read.csv("/Users/arturovillalobos/Desktop/STAT_212_rcode/hw6/baseball.csv",header = TRUE)
baseballdata = data.frame(baseball)
X = as.matrix(baseballdata[,2:17])
y = as.vector(baseballdata[,1])
### install your packages command for the command line
install.packages("leaps")

## using the function 
outputAIC = leaps.AIC(X,y)
leaps(X,y,method = 'r2',nbest = 1)

#plots of AIC and BIC
plot(1:16, outputAIC[[1]],xlab = 'variables',ylab = 'AIC')
plot(1:16, outputAIC[[2]],xlab = 'variables',ylab = 'BIC')

##picking the model i chose 7 variables
fitreduced = lm(baseball$salary~.,baseball[,c(8:9,11:12,14:16)])
summary(fitreduced)
plot(lm(fitreduced))

##plot odf standardized residuals vs 1-337
reducedstd = rstandard(fitreduced)
plot(1:337,reducedstd)

#residuals vs predicted 
plot(lm(fitreduced))

#plot of normal probability plot of standardized residuals

reducedstd = rstandard(fitreduced)
qqnorm(reducedstd, ylab="Standardized Residuals", xlab="Normal Scores", main="Normal probability plot of the standardized residuals") 
qqline(reducedstd)

#cooks distance
cooksdist = cooks.distance(fitreduced)
plot(cooksdist,ylab = 'Cooks D')

