library(RCurl)

# Read CSV
data <- read.csv(text = getURL("https://raw.githubusercontent.com/joshkyh/amss/master/ObservedData.csv"))



library(MASS)

data(UScrime)
UScrime[,-2] = log(UScrime[,-2])
library(BAS)
crime.ZS = bas.lm(y ~ .
                  , data = UScrime
                  , prior = "ZS-null"
                  , modelprior = uniform()
                  , method = "MCMC")

diagnostics(crime.ZS)

plot(crime.ZS, which =1, add.smooth = F)
plot(crime.ZS, which = 2, add.smooth = F)
abline(h=1)

plot(crime.ZS, which=3)
plot(crime.ZS, which=4)

image(crime.ZS, rotate =F)

coef.ZS = coef(crime.ZS)
plot(coef.ZS, subset = 5:6)

