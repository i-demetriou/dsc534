library(ggplot2)


####input data 
dat <- read.table("cbe.dat",sep="", header=T)
cbe <- ts(dat, start=1958, freq=12)
#####simple plot of all series
plot(cbe, main="Chocolate, Beer & Electricity")




#####nicer plots
cbedf <- data.frame(t=rep(as.numeric(time(cbe)), times=3),
                    values=c(cbe[,1], cbe[,2], cbe[,3]),
                    type=rep(c("choc", "beer", "elec"), each=nrow(cbe)))
ggplot(cbedf, aes(x=t, values, fill=type)) +
  geom_area(alpha=0.3) + geom_line() +
  facet_grid(type~., scales="free") +
  ggtitle("Production in Australia") +
  xlab("Year") + ylab("Production")


####First standardizing plot 

## Indexing the series by standardizing with the first observation
tsd <- cbe
tsd[,1] <- tsd[,1]/tsd[1,1]*100
tsd[,2] <- tsd[,2]/tsd[1,2]*100
tsd[,3] <- tsd[,3]/tsd[1,3]*100
## Plotting in one single frame
clr <- c("green3", "red3", "blue3")
plot(tsd, plot.type="single", ylab="Index", col=clr)
title("Indexed Chocolate, Beer & Electricity")
## Legend
ltxt <- names(dat)
legend("topleft", lty=1, col=clr, legend=ltxt)

## Indexing the series vs. the first period
tsd <- cbe
tsd[,1] <- tsd[,1]/mean(tsd[1:12,1])*100
tsd[,2] <- tsd[,2]/mean(tsd[1:12,2])*100
tsd[,3] <- tsd[,3]/mean(tsd[1:12,3])*100

## Plotting in one single frame
plot(tsd, plot.type="single", ylab="Index", col=clr)
title("Indexed Chocolate, Beer & Electricity")
## Legend
ltxt <- names(dat)
legend("topleft", lty=1, col=clr, legend=ltxt)
