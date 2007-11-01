####
####
#### Create the data objects used in chapter 1 
####
####

##
## 0.  TsayFiles directory 
##

# Adjust the following to
# setwd to 'TsayFiles' 
getwd()
#setwd("..")
#setwd("FinTS")
#setwd("pkg")
#setwd("inst")
#setwd("scripts")
#setwd("TsayFiles")

TsayDir <- "../FinTS/pkg/inst/scripts/TsayFiles/"

# 0.1.  Tsayfiles$ch01$text 

library(FinTS)

data(TsayFiles)  

TsayFiles$ch01$text[1:2,]
ch01 <- with(TsayFiles$ch01, text[text[, 4]=="TRUE", ])
sort(table(ch01[, "data"]))
# Confirm:  all unique
# Exercises or text in other chapters may use
# some of these same data;  we need to check then.  

as.vector(ch01[, "data"])

# [1] "d-ibmvwewsp6203" "d-intc7303"      "d-3m6203"        "d-msft8603"     
# [5] "d-c8603"         "m-ibmvwewsp2603" "m-intc7303"      "m-3m4603"       
# [9] "m-msft8603"      "m-c8603"         "m-gs10"          "m-gs1"          
#[13] "d-fxjp00"        "m-fama-bond5203" "m-gs3"           "m-gs5"          
#[17] "w-tb3ms"         "w-tb6ms"

##
## 1.  d-ibmvwewsp6203
##     Daily simple returns of IBM, VW, EW, SP (7/3/62-12/31/03):
##      (Format: date, IBM, VW, EW & SP)
##
(d.i.dat <- paste(TsayDir, ch01[1, "file"], sep="") )

readLines(d.i.dat, 3)
d.ibmvwewsp6203 <- read.table(d.i.dat)
d.ibmvwewsp6203[1:2,]

##
## 2.  d-intc7303
##     Daily simple returns of Intel stock (12/15/72-12/31/03) 
##

##
## 3.  d-3m6203
##     Daily simple returns of 3M stock
##

##
## 4.  d-msft8603
##     Daily simple returns of Microsoft stock
##

##
## 5.  d-c8603
##     Daily simple returns of Citi-group stock
##

##
## 6.  m-ibmvwewsp2603
##     Monthly simple returns of IBM, VW, EW, SP (1/26-12/03)
##     (Format: date, IBM, VW, EW, & SP) 

##
## 7.  m-intc7303
##     Monthly simple returns of Intel stock
##

##
## 8.  m-3m4603
##     Monthly simple returns of 3M stock
##

##
## 9.  m-msft8603
##     Monthly simple returns of Microsoft stock
##

##
## 10.  m-c8603
##      Monthly simple returns of Citi-group stock
##

##
## 11.  m-gs10 & m-gs1
##      Monthly 10-yr  and 1-yr Treasury constant maturity rates
##      (4/53-3/04):  (Format: year, month, date, rate): 
##
ch01[11:12,]

##
## 12.  d-fxjp00
##      Daily exchange rate between U.S. dollar and Japanese yen
##      (Format: ddmmyy, fx)
##
ch01[13,]

##
## 13.  m-fama-bond5203
##      Monthly bond returns (1-12m, 24-36m, 48-60m, 61-120m)
##      (Format: date, bond returns) 
##
ch01[14, ]

##
## 14.  m-gs3 & m-gs5 
##      Monthly 3-yr and 5-yr Treasury constant maturity rates
##
ch01[15:16,]

##
## 15.  w-tb3ms & w-tb6ms 
##      Weekly Treasury Bill rates - 3 & 6 months
##
        
