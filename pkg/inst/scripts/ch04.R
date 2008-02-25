###
### 
### Ruey S. Tsay (2005)
### Analysis of Financial Time Series, 2nd ed.
### (Wiley)
###
### 

###
### ch. 4.  Nonlinear models and their applications 
###
library(FinTS)
# p. 154

# p. 156 
##
## sec. 4.1.  Nonlinear models  
##

# sec. 4.1.1.  Bilinear model 

# p. 167
# Example 4.1.
data(m.ibmvwewsp2603)
str(m.ibmvwewsp2603)

ew2697 <- window(m.ibmvwewsp2603[, "VW"], end=yearmon(1997+11/12))
str(ew2697)
# correct number of observations.  

#??? Need code for bilinear estimation.

# sec. 4.1.2.  Threshold autoregression (TAR)

library(tsDyn)

