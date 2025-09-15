####Read data and make a log-transformation
library(forecast)
library(fpp)

Maine.unemp <- read.csv("Maine.dat", sep="", header = T) 
tsd <- ts(Maine.unemp$unemploy, start = c(1996, 1), freq = 12)
fit <- stl(log(tsd), s.window="periodic")
plot(fit)

## Trend Forecast by Linear Extrapolation
length(log(tsd))
plot(fit$time.series[,2], xlim=c(1996, 2008+9/12))
rect(2004+8/12, 1 , 2006+7/12, 2, col="grey93", border=NA)
rect(2006+7/12, 1, 2008+6/12, 2, col="grey83", border=NA)
title("Trend Forecast by Linear Extrapolation")
xx <- time(fit$time.series[,2])[105:128]
yy <- fit$time.series[105:128,2]
fit.regr <- lm(yy~xx)
summary(fit.regr)
t.fore <- 1.494 + (0:23)/12 * coef(fit.regr)[2]
lines(xx, fitted(fit.regr), col="blue")
lines(xx[1]+(23:46)/12, t.fore, col="red")
lines(fit$time.series[,2])
box()

## Seasonal Forecast Using Last Values
season <- fit$time.series[,1]
l2y <- window(season,start=c(2004,9),end=c(2006,8))
s.fore <- ts(l2y, start=c(2006,9), end=c(2008,8), freq=12)

###Error  forecast 
rmndr <- fit$time.series[,3]
tsdisplay(rmndr)
fit.rmndr <- arima(rmndr, order=c(4,0,0), include.mean=F)
r.fore <- predict(fit.rmndr, n.ahead=24)$pred

## Adding the 3 Components
fore <- t.fore + s.fore + r.fore
## Displaying the Output
plot(log(tsd), xlim=c(1996, 2008.75), ylab="log(%)")
rect(2006+8/12, 0, 2008+9/12, 2, col="grey90", border=NA)
lines(fore, col="red")
box()

