library(FinTS)
###
### 
### Ruey S. Tsay (2005)
### Analysis of Financial Time Series, 2nd ed.
### (Wiley)
###
### 

###
### ch. 1.  Financial Time Series and Their Characteristics
###

##
## sec. 1.1.  Assett Returns
##

# p. 4
##
## Table 1.1.  Illustration of the effects of compounding
##
frequencies <- c(1, 2, 4, 12, 52, 365, Inf)
(Table1.1 <- data.frame(Type=c(
    "Annual", "Semiannual", "Quarterly",
    "Monthly", "Weekly", "Daily", "Continuously"),
  Number.of.payments=frequencies, 
  Interest.rate.per.period=0.1/frequencies,
  Net.Value=compoundInterest(0.1,
    frequency=frequencies) ) )

# p.  6  
##
## Example 1.1.
##
logRtn <- .0446
100*expm1(logRtn)

mo.logRtns <- c(.0446, -.0734, .1077)
(q.logRtns <- sum(mo.logRtns))

##
## sec. 1.2.  Distributional Properties of Returns
##
# p. 7
# sec. 1.2.1.  Review of Statistical Distributions and their Moments

# p. 10
##
## Example 1.2.
## 
data(d.ibmvwewsp6203)
s.ibm <- sd(d.ibmvwewsp6203[, "IBM"])
(skew.ibm <- skewness(d.ibmvwewsp6203[, "IBM"]))
(n.ibm <- dim(d.ibmvwewsp6203)[1])
(skew.sd <- sqrt(6/n.ibm))
(t.sk <- (skew.ibm/skew.sd))
pnorm(-t.sk)

# A slightly more accurate version of this is
# agostino.test 
library(moments)
skew.test <- agostino.test(as.numeric(d.ibmvwewsp6203[, "IBM"]))

# p. 11
##
## Table 1.2.  Descriptive Statistics for Daily and Monthly
##             Simple and Log Returns for Selected Indexes and Stocks
##
data(d.ibmvwewsp6203)
data(d.intc7303)
data(d.3m6203)
data(d.msft8603)
data(d.c8603)
(Daily.Simple.Returns.pct <- rbind(
    FinTS.stats(100*d.ibmvwewsp6203[, "SP"]),     
    FinTS.stats(100*d.ibmvwewsp6203[, "VW"]),     
    FinTS.stats(100*d.ibmvwewsp6203[, "EW"]), 
    FinTS.stats(100*d.ibmvwewsp6203[, "IBM"]),     
    FinTS.stats(100*d.intc7303[,"Intel"]),     
    FinTS.stats(100*d.3m6203[, "MMM"]), 
    FinTS.stats(100*d.msft8603[, 'MSFT']), 
    FinTS.stats(100*d.c8603[, "C"]) 
) )

(Daily.log.Returns.pct <- rbind(
    FinTS.stats(100*log(1+d.ibmvwewsp6203[, "SP"])), 
    FinTS.stats(100*log(1+d.ibmvwewsp6203[, "VW"])),     
    FinTS.stats(100*log(1+d.ibmvwewsp6203[, "EW"])), 
    FinTS.stats(100*log(1+d.ibmvwewsp6203[, "IBM"])),     
    FinTS.stats(100*log(1+d.intc7303[,"Intel"])),     
    FinTS.stats(100*log(1+d.3m6203[, "MMM"])), 
    FinTS.stats(100*log(1+d.msft8603[, 'MSFT'])), 
    FinTS.stats(100*log(1+d.c8603[, "C"])) 
) )

data(m.ibmvwewsp2603)
data(m.intc7303)
data(m.3m4603)
data(m.msft8603)
data(m.c8603)
(Monthly.Simple.Returns.pct <- rbind(
    FinTS.stats(100*m.ibmvwewsp2603[, "SP"]),     
    FinTS.stats(100*m.ibmvwewsp2603[, "VW"]),     
    FinTS.stats(100*m.ibmvwewsp2603[, "EW"]), 
    FinTS.stats(100*m.ibmvwewsp2603[, "IBM"]),     
    FinTS.stats(100*m.intc7303[,"Intel"]),     
    FinTS.stats(100*m.3m4603[, "MMM"]), 
    FinTS.stats(100*m.msft8603[, 'MSFT']), 
    FinTS.stats(100*m.c8603[, "C"]) 
) )

(Monthly.log.Returns.pct <- rbind(
    FinTS.stats(100*log(1+m.ibmvwewsp2603[, "SP"])), 
    FinTS.stats(100*log(1+m.ibmvwewsp2603[, "VW"])),     
    FinTS.stats(100*log(1+m.ibmvwewsp2603[, "EW"])), 
    FinTS.stats(100*log(1+m.ibmvwewsp2603[, "IBM"])),     
    FinTS.stats(100*log(1+m.intc7303[,"Intel"])),     
    FinTS.stats(100*log(1+m.3m4603[, "MMM"])), 
    FinTS.stats(100*log(1+m.msft8603[, 'MSFT'])), 
    FinTS.stats(100*log(1+m.c8603[, "C"])) 
) )

dimnames(Daily.Simple.Returns.pct)[[1]] <-
  c("SP", "VW", "EW", "IBM", "Intel", "3M", "MSFT", "Citi")
dimnames(Daily.log.Returns.pct)[[1]] <-
  c("SP", "VW", "EW", "IBM", "Intel", "3M", "MSFT", "Citi")
dimnames(Monthly.Simple.Returns.pct)[[1]] <-
  c("SP", "VW", "EW", "IBM", "Intel", "3M", "MSFT", "Citi")
dimnames(Monthly.log.Returns.pct)[[1]] <-
  c("SP", "VW", "EW", "IBM", "Intel", "3M", "MSFT", "Citi")

write.table(rbind(Daily.Simple.Returns.pct, Daily.log.Returns.pct,
                  Monthly.Simple.Returns.pct, Monthly.log.Returns.pct),
            "Table1-1.csv", sep=",")

# p. 12

# Comparable to S-Plus Demonstration
# module(finmetrics)
# x = matrix(scan(file = 'd-ibmvwewsp6203.txt'), 5) % load the data
# ibm = x[2,]*100 % compute percentage returns

data(d.ibmvwewsp6203)
quantile(100*as.numeric(d.ibmvwewsp6203[, "IBM"]))
FinTS.stats(100*d.ibmvwewsp6203[, "IBM"])

# p. 15
# Sec. 1.2.2.  Distributions of Returns
# p. 16
# Figure 1.1.  Comparisons of Finite Mixture, Stable
#              and Standard Normal Distributions
library(distrEx)

N01 <- Norm()
N04 <- Norm(0, 4)

contamNorm <- ConvexContamination(N01, N04, size = 0.05)

plot(dnorm, xlim=c(-4, 4))
text(.75, .39, "Normal") 
x <- seq(-4, 4, len=201)
lines(x, d(contamNorm)(x), lty="dotted", col="red", lwd=2)
text(0, 0.351, "Mixture", col="red")
lines(x, dcauchy(x), lty="dashed", col="green", lwd=2)
text(0, 0.25, "Cauchy", col="green")

# p. 17
# Sec. 1.2.5.  Empirical properties of returns

# p. 18 
# Figure 1.2.  Time plots of monthly returns of IBM stock

op <- par(mfrow=c(2,1))
plot(m.ibmvwewsp2603[, "IBM"], xlab="year", ylab="s-rtn")
plot(log(1+m.ibmvwewsp2603[, "IBM"]), xlab="year", ylab="log-rtn")
par(op)


# Figure 1.3.  Time plots of monthly returens of the value-weighted index

op <- par(mfrow=c(2,1))
plot(m.ibmvwewsp2603[, "VW"], xlab="year", ylab="s-rtn")
plot(log(1+m.ibmvwewsp2603[, "VW"]), xlab="year", ylab="log-rtn")
par(op)

# p. 19
# Figure 1.4.  Comparison of empirical and normal densities
# for the monthly simple and log returns of IBM stock

library(logspline)

m.ibm <- m.ibmvwewsp2603[, "IBM"]
dens.ibm.rtns <- logspline(100*m.ibm)
m.log.ibm <- 100*log(1+m.ibm)
dens.ibm.logrtns <- logspline(m.log.ibm)

op <- par(mfrow=c(1,2))
plot(dens.ibm.rtns, xlim=c(-40, 40), xlab="simple returns", ylab="density")
x.ibm <- seq(-40, 40, len=201)
lines(x.ibm, dnorm(x.ibm, mean=100*mean(m.ibm), s=100*sd(m.ibm)),
      lty="dotted", col="red", lwd=2)

plot(dens.ibm.logrtns, xlim=c(-40, 40), xlab="log returns", ylab="density")
lines(x.ibm, dnorm(x.ibm, mean=mean(m.log.ibm), s=sd(m.log.ibm)),
      lty="dotted", col="red", lwd=2)
par(op)

qqnorm(100*m.ibm, datax=TRUE)
qqnorm(m.log.ibm, datax=TRUE)

# normal plots may provide a more sensitive evaluation
# of skewness and kurtosis than density plots

# The kurtosis here does not look extreme.
# Similar plots of the daily returns show
# much more extreme kurtosis.

# p. 20
##
## 1.3.  Processes Considered
##

