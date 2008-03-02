## 
## 9.2.1 A Single-Factor Model
##

# Note: S-Plus code in book works on R except that
# input is from .rda files in package FinTS and these 
# changes:
#
# S-Plus: solve(x, y) # non-square x
# R: solve.qr(x, y)
#
# S-Plus: var(x, SumSquares=T)
# R:  (nrow(x) - 1) * var(x)
library(FinTS)

# input returns file
#load("m.fac9003.rda")
data(m.fac9003)
x <- m.fac9003

# Table 9.1 showing means and standard deviations
# of stocks
cbind(mean = colMeans(x[,-14]), sd = sd(x[,-14]))

# Code at bottom of page 408 producing
# table middle of p 409 showing beta.hat, sigma
# and R squared.
xmtx <- cbind(1, as.numeric(x[, 14]))
rtn <- x[, -14]
xit.hat <- qr.solve(xmtx, rtn)
beta.hat <- t(xit.hat[2,])
E.hat <- rtn - xmtx %*% xit.hat
D.hat <- diag(crossprod(E.hat)/(168-2))
r.square <- 1 - (168 - 2) * D.hat / diag((168 -1 ) * var(rtn))
tab <- cbind(beta.hat = c(beta.hat), `sigma(i)` = sqrt(D.hat), r.square)
round(tab, 3)

# Same as last snippet but making use of lm() and summary()
f <- function(y) {
    L <- lm(y ~ x[,14])
    S <- summary(L)
    c(beta.hat = coef(L)[[2]], sigma = S$sigma, r.squared = S$r.squared)
}
tab <- t(apply(x[,-14], 2, f))
round(tab, 3)

# Figure 9.1 Bar chart of columns 1 and 3 of tab
opar <- par(mfrow = c(2, 1))
barplot(tab[, 1], main = quote(hat(beta)))
barplot(tab[, 3], main = quote(R^2))
par(opar)

# Code on bottom of page 409 and on page 410
cov.r <- var(x[, 14]) * (t(beta.hat) %*% beta.hat) + diag(D.hat)
sd.r <- sqrt(diag(cov.r))
corr.r <- cov.r / outer(sd.r, sd.r)
print(corr.r, digits = 1, width = 2)
print(cor(rtn), digits = 1, width = 2)

# Code on middle of page 411
w.gmin.model <- solve(cov.r) %*% rep(1, nrow(cov.r))
w.gmin.model <- w.gmin.model / sum(w.gmin.model)
round(t(w.gmin.model), 4)

w.gmin.data <- solve(var(rtn)) %*% rep(1, nrow(cov.r))
w.gmin.data <- w.gmin.data / sum(w.gmin.data)
round(t(w.gmin.data), 4)

# Code at bottom of page 411 and top of page 412
resi.cov <- t(E.hat) %*% E.hat/(168 - 2)
resi.sd <- sqrt(diag(resi.cov))
resi.cor <- resi.cov / outer(resi.sd, resi.sd)
print(resi.cor, digits = 1, width = 2)

## 
## 9.2.2 Multifactor Models
##

#load("m.cpice16.dp7503.rda")
data(m.cpice16.dp7503)
y1 <- m.cpice16.dp7503
library(vars)
# the VARselect() value of BIC and 
# S-Plus VAR() value of BIC are 
# translated and scaled versions of each other
vs <- VARselect(y1, lag.max = 13)
var3.fit <- VAR(y1, 3)
res <- resid(var3.fit)[166:333,]
#load("m.fac9003.rda")
data(m.fac9003)
da <- m.fac9003
xmtx <- cbind(1, res)
rtn <- da[, -14]
xit.hat <- qr.solve(xmtx, rtn)
beta.hat <- t(xit.hat[2:3,])

E.hat <- rtn - xmtx %*% xit.hat
D.hat <- diag(crossprod(E.hat)/(168-3))
r.square <- 1 - (168-3) * D.hat / diag((nrow(rtn) - 1) * var(rtn))


