####read data 
Maine.unemp <- read.csv("Maine.dat", sep="", header = T) 
unemp <- Maine.unemp$unemp 
####simple plot 
ts.plot(unemp, ylab="(%)", main="Unemployment in Maine")
####another view 
plot(unemp, type="o", pch=20, ylab="(%)", main="Unemployment in Maine")
###Horizontal line plot
plot(unemp, type="h", ylab="(%)", main="Unemployment in Maine")

###GAM model 
library(mgcv)
tnum <- as.numeric(time(unemp))
mm <- rep(c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
mm <- factor(rep(mm,11),levels=mm)[1:128]
fit <- gam(log(unemp) ~ s(tnum) + mm)
###Plot of data and predictions
plot(log(unemp), ylab="(%)", main="Logged Unemployment in Maine")
lines(tnum, fitted(fit), col="red")
###decomposition for trend and seasonal effect
plot(fit, shade=TRUE, xlab="", ylab="Time", main="Trend")
seas.eff <- c(0,coef(fit)[2:12])-mean(c(0,coef(fit)[2:12]))
plot(1:12, seas.eff, xlab="Month", ylab="", type="h", main="Seasonal Effect")
points(1:12, seas.eff, pch=20)
abline(h=0, col="grey")
###Residual process 
plot(resid(fit), type="o", pch=20)
