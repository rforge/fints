library(FinTS)
###
### 
### Ruey S. Tsay (2005)
### Analysis of Financial Time Series, 2nd ed.
### (Wiley)
###
### 

###
### ch. 2.  Linear Time Series Analysis and Its Applications 
###
# p. 24

# p. 25 
##
## sec. 2.1.  Stationarity 
##

##
## sec. 2.2.  Correlation and Autocorrelation Function 
##

# p.  28  
# Figure 2.1.  Sample ACFs of monthly simple and log returns
#              of IBM stock, Jan. 1926 - Jan. 1997 
data(m.ibm2697)
str(m.ibm2697)
quantile(as.numeric(m.ibm2697))
op <- par(mfrow=c(2,1))
acf(m.ibm2697, ylim=c(-.2, .2), lag.max=100,
    main="(a) Simple returns")
acf(log(1+m.ibm2697), ylim=c(-.2, .2), lag.max=100,
    main="(b) Log returns")
par(op)

Box.test(m.ibm2697, 5, "Ljung-Box")
Box.test(m.ibm2697, 10, "Ljung-Box")

Box.test(log(1+m.ibm2697), 5, "Ljung-Box")
Box.test(log(1+m.ibm2697), 10, "Ljung-Box")

autocorTest(m.ibm2697, 5)
autocorTest(m.ibm2697, 10)

autocorTest(log(1+m.ibm2697), 5)
autocorTest(log(1+m.ibm2697), 10)

# p. 29  
# Figure 2.2.  Sample ACFs of monthly simple and log returns
#              of the value weighted index, Jan. 1926 - Jan. 1997 
data(m.vw2697)
op <- par(mfrow=c(2, 1))
acf(m.vw2697, ylim=c(-.2, .2), lag.max=100,
    main="(a) Simple returns")
acf(log(1+m.vw2697), ylim=c(-.2, .2), lag.max=100,
    main="(b) Log returns")
par(op)

# R function in stats package 
Box.test(m.vw2697, 5, "Ljung-Box")
Box.test(m.vw2697, 10, "Ljung-Box")

Box.test(log(1+m.vw2697), 5, "Ljung-Box")
Box.test(log(1+m.vw2697), 10, "Ljung-Box")

# FinTS function in Tsay, p. 30 
autocorTest(m.vw2697, 5)
autocorTest(m.vw2697, 10)

autocorTest(log(1+m.vw2697), 5)
autocorTest(log(1+m.vw2697), 10)

# p. 31
##
## sec. 2.3.  White Noise and Linear Time Series 
##

# p. 32  
##
## sec. 2.4.  Simple Autoregressive Models 
##

# p. 35
# Figure 2.3.  Autocorrelation function of AR(1) models 

op <- par(mfcol=c(1,2))
ph <- 0.8
plotArmaTrueacf(ph, lag.max=8)
mtext(paste("(a) AR(1) model for phi = ",
    substitute(p, list(p=ph))), cex=1.1, line=1)

ph <- (-0.8)
plotArmaTrueacf(ph, lag.max=8)
mtext(paste("(a) AR(1) model for phi = ",
    substitute(p, list(p=ph))), cex=1.1, line=1)

par(op)

# p. 37
# Figure 2.4.  Autocorrelation Function of AR(2) models

op <- par(mfcol=c(2,2))
ph <- c(1.2, -.35)
plotArmaTrueacf(ph)
mtext(paste("(a) AR(2) model for phi = (",
            paste(ph, collapse=", "), ")", sep=""), cex=1.1, line=1)

ph <- c(.6, -.4)
plotArmaTrueacf(ph)
mtext(paste("(b) AR(2) model for phi = (",
            paste(ph, collapse=", "), ")", sep=""), cex=1.1, line=1)

ph <- c(.2, .35)
plotArmaTrueacf(ph)
mtext(paste("(c) AR(2) model for phi = (",
            paste(ph, collapse=", "), ")", sep=""), cex=1.1, line=1)

ph <- c(-.2, .35)
plotArmaTrueacf(ph)
mtext(paste("(d) AR(2) model for phi = (",
            paste(ph, collapse=", "), ")", sep=""), cex=1.1, line=1)
par(op)

# p. 38
# Example 2.1.  Quarterly growth rate of real US GNP 
# Figure 2.5.  
data(q.gnp4791)
plot(q.gnp4791, type="b", xlab="year", ylab="growth")
abline(h=0)

(fit.ar3 <- ar(q.gnp4791, aic=FALSE, order=3))
# 0.3463   0.1770  -0.1421  sigma^2 estimated as  9.676e-05
class(fit.ar3) # ar 

(fit.ar3.burg <- ar.burg(q.gnp4791, aic=FALSE, order=3))
# 0.3474   0.1807  -0.1436  sigma^2 estimated as  9.427e-05
class(fit.ar3.burg) # ar 

(fit.ar3.yw <- ar.yw(q.gnp4791, aic=FALSE, order=3))
# 0.3480   0.1793  -0.1423  sigma^2 estimated as  9.427e-05
class(fit.ar3.yw) # ar 

(fit.ar3.mle <- ar.mle(q.gnp4791, aic=FALSE, order=3))
# 0.3480   0.1793  -0.1423  sigma^2 estimated as  9.427e-05
class(fit.ar3.mle) # ar 

(fit.ar3.ols <- ar.ols(q.gnp4791, aic=FALSE, order=3))
# 0.3509   0.1809  -0.1443  sigma^2 estimated as  9.563e-05 
class(fit.ar3.ols) # ar 

names(fit.ar3.mle)
str(fit.ar3.mle)

plotArmaTrueacf(fit.ar3.mle)

# period = 10.57 quarters
# vs. 10.83 in the book.  
# 

# p. 41-42
##
## Table 2.1.  Sample PACF and AIC for CRSP value-weighted Index
##

data(m.vw2697)
str(m.vw2697)
(pacf.vw <- pacf(m.vw2697, lag.max=10))
# "lag" in the function call is integer number of periods
# "lag" in the output is in fractions of years, so .083 = 1 month, etc.
2/sqrt(length(m.vw2697))

AIC5 <- array(NA, dim=c(11, 5),dimnames=list(0:10,
             c("yule-walker", "burg", "ols", "mle", "yw")))
for(i in 1:5)
  AIC5[, i] <- ar(m.vw2697, order.max=10,
      method=dimnames(AIC5)[[2]][i])$aic

all.equal(AIC5[, 1], AIC5[, 5])
# TRUE 

AIC.Tsay <- c(-5.807, -5.805, -5.817, -5.816, -5.819,
              -5.821, -5.819, -5.820, -5.821, -5.818)

AIC.Tsay-min(AIC.Tsay)
round(AIC5[,-1]/length(m.vw2697), 3)

# Tsay's definition of the AIC differs from the definition used in R
# by a factor of the number of observations.

# Apart from this difference, the numbers in Table 2.1
# seem to follow most closely method="burg" 
# showing 1 discrepancy out of 10 vs. 3 out of 10
# for the Yule-Walker and MLE alternatives.
# "ols" is very different from the others.

# pp. 41-42 
##
## Figure 2.6.  Sample PACF of US Quarterly real GNP growth 
##
data(q.gnp4791)
pacf(q.gnp4791, lag.max=12)

q.gnp4791.ar <- ar(q.gnp4791)
round(q.gnp4791.ar$aic, 3)
# NOTE:  The 'AICs' printed on p. 43 follow the definition
# of AIC used by R, NOT the definition on p. 42;
# Tsay's definition divides the R definition
# by the length of the series.

q.gnp4791.ar$order

data(m.vw2697) 
(ar.vw. <- ar(m.vw2697, order.max=3))
(ar.vw <- arima(m.vw2697, c(3,0,0)))

mean(m.vw2697)
# 0.009879116
sum(coef(ar.vw)[1:3])
# -0.02538
# phi0 = mu*(1-sum(phi))
mean(m.vw2697)*(1-sum(coef(ar.vw)[1:3]))
# 0.01013


names(ar.vw.)
ar.vw.$ar # match bottom of p. 43
ar.vw.$x.mean # vs. 0.0103 on p. 43
sqrt(ar.vw.$var.pred) # match bottom of p. 43

# p. 44
(fit.3 <- ARIMA(m.vw2697, order=c(3, 0, 0),
                Box.test.lag=12))
fit.3$Box.test
# X-squared = 16.7 vs. 16.9 in the book
# p-value = 0.053 vs. 0.050 in the book 

# drop the AR(2) parameter, estimate only the AR(1) and AR(3)
(fit.2 <- ARIMA(m.vw2697, order=c(3, 0, 0),
                fixed=c(NA, 0, NA, NA),
                Box.test.lag=12))
fit.2$Box.test
# X-squared = 17.0 vs. 17.2 in the book
# p-value = 0.075 vs. 0.070 in the  book 


# p. 49
# Table 2.2.  AR(3) Forecasts for Monthly Simple Returns of VW Index 
methods(class="Arima")
length(m.vw2697)
# 864
ar.vw858 <- arima(m.vw2697[1:858], order=c(3,0,0))

(pred.ar.vw <- predict(ar.vw858, 6))
# actual
m.vw2697[859:864]

# Figure 2.7.  AR(5) Forecasts for Monthly Log Returns of VW Index
(ar.vw858l <- arima(log(1+m.vw2697[1:858]), order=c(5,0,0)))
sqrt(ar.vw858l$sigma2)
# all the numbers match p. 50 except 'intercept'
str(ar.vw858l)
(1-sum(coef(ar.vw858l)[1:5]))*coef(ar.vw858l)["intercept"]

(pred.ar.vwl <- predict(ar.vw858l, 6))


ul <- c(log(1+m.vw2697[858]), with(pred.ar.vwl, pred+1.96*se))
ll <- c(log(1+m.vw2697[858]), with(pred.ar.vwl, pred-1.96*se))

plot(log(1+m.vw2697[851:864]),
     type="b", ylim=range(ul, ll))
lines(index(m.vw2697)[858:864], c(log(1+m.vw2697[858]), pred.ar.vwl$pred),
      type="b", col="red", lty="dashed")
lines(index(m.vw2697)[858:864], 
      ul, type="b", col="green", lty="dotted")
lines(index(m.vw2697)[858:864], 
      ll, type="b", col="green", lty="dotted")

plot(log(1+m.vw2697[851:864]), type="b", ylim=c(-.2, .2),
     xlab="year (and fraction)", ylab="vw")
lines(index(m.vw2697)[858:864], c(log(1+m.vw2697[858]), pred.ar.vwl$pred),
      type="b", col="red", lty="dashed")
lines(index(m.vw2697)[858:864], 
      ul, type="b", col="green", lty="dotted")
lines(index(m.vw2697)[858:864], 
      ll, type="b", col="green", lty="dotted")

##
## sec. 2.5.  Simple Moving Average Models
##

# p. 52-53
# Figure 2.8
# Monthly simple returns of CRSP equal-weighted index
# from January 1926 to December 2003
# ... used in chapter 1
data(m.ibmvwewsp2603)
str(m.ibmvwewsp2603)

op <- par(mfrow=c(2,1))
plot(m.ibmvwewsp2603[, "EW"], main="(a) Monthly simple returns",
     xlab="year", ylab="s-rtn")
acf(as.numeric(m.ibmvwewsp2603[, "EW"]), ylim=c(-.4, .4),
    main="(b) Sample ACF")
par(op)

# p. 54
(fit.ew <- ARIMA(as.numeric(m.ibmvwewsp2603[, "EW"]), order=c(0, 0, 9),
             fixed=c(NA, 0, NA, rep(0, 5), NA, NA),
             Box.test.lag=12))
fit.ew$Box.test

# pp. 55-56
##
## Table 2.3.  MA(9) Forecasts for EW
##
# data documented with ch01data, not ch02data 
data(m.ibmvwewsp2603)
str(m.ibmvwewsp2603)
plot(m.ibmvwewsp2603[, "EW"])
abline(v=index(m.ibmvwewsp2603)[926], col="green", lty="dashed")

(ma9.ew926 <- arima(as.numeric(m.ibmvwewsp2603[1:926, 'EW']),
                    order=c(0,0,9)))
# The numbers don't match perfectly, but they are moderately close.
# The difference may be due to difference in the accuracy of
# the algorithm in finding the actual maximum of the likelihood
# or to the likelihood surfact being fairly flat.

(pred.ma9.ew936 <- predict(ma9.ew926, 10))
# actual
m.ibmvwewsp2603[927:936, "EW"]

##
## sec. 2.6.  Simple ARMA Models
##

# pp. 59-60
data(m.3m4697)
str(m.3m4697)
quantile(m.3m4697)

op <- par(mfrow=c(2,1))
plot(log(1+m.3m4697), xlab="year", ylab="l-rtn")

acf(as.numeric(m.3m4697), main="")
par(op)

# What to do about EACF?
# Question sent to Prof. Tsay 2007.12.01




# p. 64 
##
## sec. 2.7.  Unit-Root Nonstationarity
##

# p. 66
data(m.3m4697)
(mean.log.3m <- mean(log(1+m.3m4697)))
sd(log(1+m.3m4697))

# Figure 2.10.  Log prices for 3M stock

log.3m <- cumsum(log(1+m.3m4697))
log.3m0 <- cumsum(log(1+m.3m4697)-mean.log.3m)

plot(log.3m, ylim=range(log.3m, log.3m0),
     xlab="year", ylab="log-price")
lines(log.3m0, lty="dotted", col="red")
lines(t4697, mean.log.3m*(1:length(m.3m4697)))

# p. 69
# Example 2.2.  US quarterly GDP
data(q.gdp4703)

# p. 70
# Figure 2.11.  
op <- par(mfcol=c(2,2))
plot(log(q.gdp4703), xlab="year", ylab="ln(GDP)",
     main="(a)")

acf(log(q.gdp4703), lag.max=16, main="(b)",
    xlab="lag (years)")

#plot(diff(q.gdp4703), xlab="year", ylab="diff(GDP)", main="(c)")
# This is clearly NOT Figure 2.20.  
plot(diff(log(q.gdp4703)), xlab="year", ylab="diff(ln(GDP))",
     main="(c)")
# Looks like Figure 2.10(c)

pacf(diff(log(q.gdp4703)), lag.max=16, main="(d)",
     xlab="lag (years)")
par(op)

qqnorm(diff(log(q.gdp4703)), datax=TRUE)
abline(h=c(-2, 2))
abline(h=-1.9)
pnorm(-1.9)
# 0.029 
# Slightly nonnormal, with just over 5%
# of observations = mild outliers.  
# Therefore, I don't theink we need to worry about
# nonnormality.

library(tseries)
adf.test(log(as.numeric(q.gdp4703)), a="e", k=9)
#Dickey-Fuller = -0.9741, Lag order = 10, p-value = 0.05918

library(urca)
(fadf.c.l.gdp <- ur.df(log(q.gdp4703), type="drift", lags=9))
summary(fadf.c.l.gdp)
#Value of test-statistic is: -1.1306 7.0185 
#Critical values for test statistics: 
#      1pct  5pct 10pct
#tau2 -3.46 -2.88 -2.57
#phi1  6.52  4.63  3.81

library(fUnitRoots)
(fadf.c.l.gdp <- adfTest(log(q.gdp4703), type="c", lags=9))
# Dickey-Fuller: -1.1306;  P VALUE: 0.6358 
str(fadf.c.l.gdp)
 
library(uroot)
gdp.uroot <- ts(log(q.gdp4703), frequency=4, start=c(1947,1))

ADF.d.l.gdp <- ADF.test(gdp.uroot, c(1,0,0),
                        selectlags=list(mode=as.numeric(1:9)))
#Warning message:
#In interpolpval(code = code, stat = adfreg[, 3], N = N) :
#  p-value is greater than printed p-value
summary(ADF.d.l.gdp)
summary(ADF.d.l.gdp@lmadf)
# This call to ADF.test produces essentially the same answer
# as S-Plus unitroot 

# p. 71
# Figure 2.12.  log(S&P 500), 1990.01.02 - 2003.12.31

data(d.sp9003lev)
plot(d.sp9003lev, xlab="year", ylab="S&P 500")
plot(log(d.sp9003lev), xlab="year", ylab="ln(index)")

fit.d.sp <- ar(as.numeric(diff(log(d.sp9003lev))))
#Call: ar(x = as.numeric(diff(log(d.sp9003lev))))
#Coefficients:
#      1        2        3        4        5        6        7        8  
#-0.0035  -0.0246  -0.0349   0.0046  -0.0369  -0.0191  -0.0450   0.0010  
#      9       10       11       12       13  
# 0.0094   0.0145  -0.0143   0.0512   0.0316  
#Order selected 13  sigma^2 estimated as  0.0001101 

str(fit.d.sp)
aic.d.sp <- fit.d.sp$aic
plot(names(aic.d.sp), aic.d.sp, type="b")

# Order 13 = min AIC

library(tseries)
adf.test(as.numeric(log(d.sp9003lev)), k=14)
# Dickey-Fuller = -0.9566, Lag order = 14, p-value = 0.9458
# book:  -0.99658, p-value = 0.9469

library(urca)
(urca.c.l.sp <- ur.df(log(d.sp9003lev), type="trend", lags=14))
summary(urca.c.l.sp)
#Value of test-statistic is: -2.2389 3.7382 2.5972 
#Critical values for test statistics: 
#      1pct  5pct 10pct
#tau3 -4.04 -3.45 -3.15
#phi2  6.50  4.88  4.16
#phi3  8.73  6.49  5.47

library(fUnitRoots)
(fadf.c.l.gdp <- adfTest(log(d.sp9003lev), lags=14, type="ct"))
# Dickey-Fuller: -0.9566, P VALUE: 0.9458 
# matches adf.test NOT ur.df
# For q.gdp4703, adfTest matched uf.df NOT adf.test

library(uroot)

ADF.test(log(d.sp9003lev), c(1,1,0),
         selectlags=list(mode="signf", Pmax=14))
#Error in if (t0[2] != 1) { : missing value where TRUE/FALSE needed
# ADF.test requires "a univariate time series object"
# Here t0 = start(log(d.sp9003lev)),
# which is a 'Date' object, NOT a vector of length 2.  
str(d.sp9003lev)
index(d.sp9003lev)[1:11]
table(diff(index(d.sp9003lev)))
#   1    2    3    4    7 
#2766   35  648   81    1
# Conclusion:
# I'm not eager to try harder to use ADF.test

# p. 72
##
## sec. 2.8.  Seasonal Models
##
data(q.jnj)

op <- par(mfcol=c(2,1))
plot(q.jnj, xlab="year", ylab="earnings",
     main="(a) Earnings per share", type="o") 
plot(log(q.jnj), xlab="year", ylab="ln-earnings",
     main="(b) Log earnings per share", type="o")
par(op)

# p. 74
# Figure 2.14.  
op <- par(mfcol=c(2,2))
acf(log(q.jnj))
acf(diff(log(q.jnj)))
acf(diff(log(q.jnj), 4))

dxds <- diff(diff(log(q.jnj)), 4)
dsdx <- diff(diff(log(q.jnj), 4))
all.equal(dxds, dsdx)
# TRUE 
acf(dxds)
par(op)

# p. 76
# Example 2.3

(fit.jnj <- arima(log(q.jnj), order=c(0, 1, 1),
                 seasonal=list(order=c(0, 1, 1))))
str(fit.jnj)
sqrt(fit.jnj$sigma2)

# Figure 2.15
# p. 77
fit.jnj76 <- arima(log(q.jnj[1:76]), order=c(0, 1, 1),
                 seasonal=list(order=c(0, 1, 1)))
(pred.jnj76 <- predict(fit.jnj76, 8))

str(q.jnj)
plot(q.jnj[71:84], xlab="year", ylim=c(0, 30),
     ylab="earning")
points(exp(pred.jnj76$pred))
ul <- with(pred.jnj76, pred+1.96*se)
ll <- with(pred.jnj76, pred-1.96*se)

lines(exp(ul), col="red", lty="dashed")
lines(exp(ll), col="red", lty="dashed")
