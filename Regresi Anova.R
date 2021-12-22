#Data
X1=seq(1.1,3,0.1)
X1
X2=4*X1^2+4*X1-1
X2
e=rnorm(20,mean=0,sd=2)
e
Y=10+(0.5*X1)+(0.1*X2)+e
Y

#Regresi
regresi=function(X,Y){
n   =length(Y)
X0  =rep (1,n)
X   =cbind(X0,X1,X2)
XtX =t(X)%*%X
XtY =t(X)%*%Y
YtY =t(Y)%*%Y
b   =solve(XtX)%*%XtY
p   =length(b)-1
Yhat=X%*%b
e   =Y-Yhat
SSE =t(e)%*%e
SST =YtY-(X0%*%Y)^2/n
SSR =SST-SSE
dfT =(n-1)
dfR =(p-1)
dfE =(n-p)
MSR =SSR/dfR
MSE =SSE/dfE
F   =MSR/MSE

pvalue = pf(F, dfR, dfE, lower.tail = FALSE)
rowreg = c(dfR, SSR, MSR, F, pvalue)
rowerr = c(dfE, SSE, MSE, "-",  "-")
rowtot = c(dfT, SST, "-", "-",  "-")
anovatable = data.frame (rbind(rowreg, rowerr, rowtot))
colnames (anovatable) = c ("df", "SS", "MS", "F", "p-value")
rownames (anovatable) = c ("regression", "Error", "Total")
cat("\n")
cat("regression coefficients:","\n")
print(b)
cat("\n")
cat ("ANOVA:","\n")
print(anovatable)
}

X =cbind (X1, X2)
regresi (X,Y)
t=t.test(X1,X2)
t