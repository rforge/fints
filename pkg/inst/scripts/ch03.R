###
### 
### Ruey S. Tsay (2005)
### Analysis of Financial Time Series, 2nd ed.
### (Wiley)
###
### 

# p. 97
###
### ch. 3.  Conditional Heteroscedastic Models 
###
library(FinTS)

# p. 98 
##
## sec. 3.1.  Characteristics of volatility 
##

# p. 99
##
## sec. 3.2.  Structure of a model 
##

# Figure 3.1
data(m.intc7303)
ml.intc <- log(1+m.intc7303)

op <- par(mfcol=c(2,2))
acf(as.numeric(ml.intc), main="(a) Log returns", ylim=c(-.2, .4))
acf(as.numeric(ml.intc)^2, main="(b) Squared log returns", ylim=c(-.2, .4))
acf(abs(as.numeric(ml.intc)), main="(c) Absolute log returns", ylim=c(-.2, .4)) 
pacf(as.numeric(ml.intc)^2, main="(b) Squared log returns", ylim=c(-.2, .4))
par(op) 

# p. 101 
##
## sec. 3.3.  Model Building 
##

# sec. 3.3.1.  Testing for ARCH effect 

# p. 102
data(m.intc7303)
str(m.intc7303)

autocorTest(log(1+as.numeric(m.intc7303)), lag=12)

#archTest(log(1+as.numeric(m.intc7303)), lag=12)
# doesn't work.  


##??????????????
##
## Question sent to a package maintainer.  2007.12.23
##
##??????????????

##
## sec. 3.4.  The ARCH model
##

# p. 103 
# Figure 3.2
data(exch.perc)
str(exch.perc)

op <- par(mfrow=c(2,1))
plot(exch.perc, type="l", xlab="", ylab="fx",
     main="(a) Percentage change in exchange rate")
plot(exch.perc^2, type="l", xlab="", ylab="sq-fx",
     main="(b) Squared series")
par(op)

# p. 104 
# Figure 3.3

op <- par(mfrow=c(2,1))
acf(exch.perc, ylim=c(-.1, .1), main="(a) Sample ACF")

pacf(exch.perc^2, ylim=c(-.1, .1),
     main="(b) Partial ACF of the squared series")
par(op)

# sec. 3.4.1.  Properties of ARCH models   

# p. 106
# sec. 3.4.2.  Weaknesses of ARCH models

# sec. 3.4.3.  Building an ARCH model 

# p. 109
# sec. 3.4.4.  Some Examples

# Example 3.1
data(m.intc7303)
ml.intc <- log(1+m.intc7303)

# Possibiliities:
#garch {tseries} Fit GARCH Models to Time Series
#GarchFitting {fGarch} Univariate GARCH Time Series Fitting

library(tseries)
#arch3.fit <- garch(ml.intc, order=c(1, 3))
#Error in garch(ml.intc, order = c(1, 3)) : NAs in x

arch3.fit <- garch(as.numeric(ml.intc), order=c(1, 3))
summary(arch3.fit)
# very different from Tsay

##??????????????
##
## Question sent to a package maintainer.  20007.12.24
##
##??????????????

library(fGarch)
arch3.Fit <- garchFit(~garch(3, 0), data=ml.intc)

arch3.Fit <- garchFit(~garch(3, 0), data=as.numeric(ml.intc))

##??????????????
##
## Question sent to a package maintainer.  2007.12.24
##
##??????????????

# p. 112
# Figure 3.4
# plot of residuals from a fit that I don't know yet how to get


# p. 113 
##
## sec. 3.5.  The GARCH Model
##

# p. 116
#  sec. 3.5.1.  An Illustrative Example
data(sp500)
str(sp500)

# p. 117
# Figure 3.5
plot(sp500, xlab="year", ylab="rtn")
abline(h=0, lty="dashed")

# p. 117
# Figure 3.6 
op <- par(mfrow=c(2,1))
sp500acf <- acf(sp500, plot=FALSE, lag.max=30)
str(sp500acf)
plot(sp500acf, ylim=range(sp500acf$acf[-1]),
     xlab="lag", main="(a)", type="h")
plot(0:30, sp500acf$acf, ylim=range(sp500acf$acf[-1]),
     xlab="lag", main="(a)", type="h")
abline(h=0)
tst <- acf(sp500, ylim=range(sp500acf$acf[-1]),
     xlab="lag (years)", main="(a)", lag.max=30)
pacf(sp500^2, main="(b)")
par(op)

