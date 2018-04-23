library(RCurl)

# Read CSV
data <- read.csv(text = getURL("https://raw.githubusercontent.com/joshkyh/amss/master/ObservedData.csv"))

data <- data[, c('revenue', 'tv.spend', 'search.spend')]

# Simplify column names
names(data) <- c('revenue', 'tv', 'search')



# Created lagged variables
library(Hmisc)
data$revenue.lag1y <- Lag(data$revenue, 52)
data$revenue.lag3m <- Lag(data$revenue, 13)
data$revenue.lag1w <- Lag(data$revenue, 1)


# Create moving average
library(zoo)

data$tv_lag1w <- Lag(data$tv, 1)
data$search_lag1w <- Lag(data$search, 1)

data$tv_p3m <- rollmean(data$tv, k=13, fill=NA, align = "right")
data$search_p3m <- rollmean(data$search, k=13, fill= NA, align = "right")

data$tv_p1y <- rollmean(data$tv, k=52, fill=NA, align = "right")
data$search_p1y <- rollmean(data$search, k=52, fill= NA, align = "right")


# Drop first year data
data <- data[53:nrow(data),]

# Fit the Bayesian Adaptive Sampling Model

library(BAS)
bas.fit = bas.lm(revenue ~ .
                  , data = data
                  , prior = "ZS-null"
                  , modelprior = uniform()
                  , method = "deterministic")

summary(bas.fit)

plot(bas.fit, which =1, add.smooth = F)
plot(bas.fit, which = 2, add.smooth = F)
abline(h=1)

plot(bas.fit, which=3)
plot(bas.fit, which=4)

image(bas.fit, rotate =F)

coef.ZS = coef(bas.fit)
plot(coef.ZS)

