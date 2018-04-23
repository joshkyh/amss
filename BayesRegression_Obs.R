library(RCurl)

# Read CSV
data <- read.csv(text = getURL("https://raw.githubusercontent.com/joshkyh/amss/master/ObservedData.csv"))

data <- data[, c('revenue', 'tv.spend', 'search.spend')]

data$TVxSearch <- data$tv.spend * data$search.spend

library(BAS)
bas.fit = bas.lm(revenue ~ .
                  , data = data
                  , prior = "ZS-null"
                  , modelprior = uniform()
                  , method = "deterministic")

diagnostics(bas.fit)

plot(bas.fit, which =1, add.smooth = F)
plot(bas.fit, which = 2, add.smooth = F)
abline(h=1)

plot(bas.fit, which=3)
plot(bas.fit, which=4)

image(bas.fit, rotate =F)

coef.ZS = coef(bas.fit)
plot(bas.fit)


