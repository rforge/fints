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

##
## Figure 2.6.  Sample PACF of US Quarterly real GNP growth 
##
data(q.gnp4791)
pacf(q.gnp4791, lag.max=12)

q.gnp4791.ar <- ar(q.gnp4791)
q.gnp4791.ar$aic

ar.vw. <- ar(m.vw2697, order.max=3)
names(ar.vw.)
ar.vw.$ar # match bottom of p. 43
ar.vw.$x.mean # vs. 0.0103 on p. 43
sqrt(ar.vw.$var.pred) # match bottom of p. 43 

(Lj.vw3 <- Box.test(ar.vw.$resid, 12, "Ljung-Box"))
# Right statistic, wrong degrees of freedom
names(Lj.vw3)

pchisq(Lj.vw3$statistic, 9, lower.tail=FALSE) 

names(ar.vw.)
sqrt(diag(ar.vw.$asy.var.coef))

# p. 44
# drop the AR(2) parameter, estimate only the AR(1) and AR(3)
(fit.2 <- arima(m.vw2697, order=c(3, 0, 0),
                fixed=c(NA, 0, NA, NA)))
(fit.2Lj <- Box.test(fit.2$resid, 12, "Ljung-Box"))
pchisq(fit.2Lj$statistic, 10, lower.tail=FALSE)

# p. 46 
(ar.vw <- arima(m.vw2697, order=c(3,0,0)))

# p. 49
# Table 2.2
methods(class="Arima")
length(m.vw2697)
# 864
ar.vw858 <- arima(m.vw2697[1:858], order=c(3,0,0))

(pred.ar.vw <- predict(ar.vw858, 6))
# actual
m.vw2697[859:864]

# Figure 2.7
ul <- c(m.vw2697[858], pred.ar.vw$pred+1.96*pred.ar.vw$se)
ll <- c(m.vw2697[858], pred.ar.vw$pred-1.96*pred.ar.vw$se)

plot(m.vw2697[851:864], type="b", ylim=range(ul, ll))
lines(c(m.vw2697[858], pred.ar.vw$pred), type="b", col="red", lty="dashed")
lines(ul, type="b", col="green", lty="dotted")
lines(ll, type="b", col="green", lty="dotted")

plot(m.vw2697[851:864], type="b", ylim=c(-.2, .2))
lines(c(m.vw2697[858], pred.ar.vw$pred), type="b", col="red", lty="dashed")
lines(ul, type="b", col="green", lty="dotted")
lines(ll, type="b", col="green", lty="dotted")

# using an AR(5) per p. 50
(ar5.vw858 <- arima(m.vw2697[1:858], order=c(5,0,0)))
sqrt(ar5.vw858$sigma2)

# Numbers are slightly diffrent from those on p. 50
# Different estimation method?  

(pred.ar5.vw <- predict(ar5.vw858, 6))

# Figure 2.7
ul5 <- c(m.vw2697[858], pred.ar5.vw$pred+1.96*pred.ar5.vw$se)
ll5 <- c(m.vw2697[858], pred.ar5.vw$pred-1.96*pred.ar5.vw$se)

plot(m.vw2697[851:864], type="b", ylim=c(-.2, .2))
lines(c(m.vw2697[858], pred.ar5.vw$pred), type="b", col="red", lty="dashed")
lines(ul5, type="b", col="green", lty="dotted")
lines(ll5, type="b", col="green", lty="dotted")

range(ul, ll)
range(ul5, ll5)
# The AR(3) and AR(5) forecasts are pretty similar.

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
(fit.ew <- arima(as.numeric(m.ibmvwewsp2603[, "EW"]), order=c(0, 0, 9),
             fixed=c(NA, 0, NA, rep(0, 5), NA, NA)))
(Lj.ew <- Box.test(fit.ew$resid, 12, "Ljung-Box"))
# Right statistic, wrong degrees of freedom
pchisq(Lj.ew$statistic, 9, lower.tail=FALSE) 



(fit0 <- arima0(as.numeric(m.ibmvwewsp2603[, "EW"]), order=c(0, 0, 9),
             fixed=c(NA, 0, NA, rep(0, 5), NA, NA)))



# p. 21
# Figure 1.6.  Time plot of daily exchange rate
#              between US dollara and Japanese Yen 

op <- par(mfrow=c(2,1))
plot(d.fxjp00, xlab="year", ylab="Yens", type="l")
plot(diff(d.fxjp00), xlab="year", ylab="Change", type="l")
par(op)

# p. 22
# Table 1.3.  Descriptive Statistics of Selected US Financial Time Series

(m.bondRtns <- rbind(
  "1-12 months"=FinTS.stats(100*m.fama.bond5203[, "m1.12"]), 
  "24-36 months"=FinTS.stats(100*m.fama.bond5203[, "m24.36"]), 
  "48-60 months"=FinTS.stats(100*m.fama.bond5203[, "m48.60"]), 
  "61-120 months"=FinTS.stats(100*m.fama.bond5203[, "m61.120"]) ))

(m.treasuryRtns <- rbind(
  "1 year"=FinTS.stats(m.gs1),
  "3 years"=FinTS.stats(m.gs3),
  "5 years"=FinTS.stats(m.gs5),
  "10 years"=FinTS.stats(m.gs10) ))

(w.treasuryRtns <- rbind(
  "3 months"=FinTS.stats(w.tb3ms),
  "6 months"=FinTS.stats(w.tb6ms) ) )

#write.table(rbind(m.bondRtns, m.treasuryRtns, w.treasuryRtns),
#            "Table1-3.csv", sep=",")
