
########################################################################
###### Data Pre-processing
########################################################################

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


########################################################################
###### Check for collinearity and multicollinearity
########################################################################

library(corrplot)
corrplot(cor(data), method = "number") # Display the correlation coefficient

# From correlation matrix analysis, we remove revenue.lag1w
data <- data[ , !names(data) %in% c("revenue.lag1w")]

library(car)
lm.fit <- lm(revenue ~ ., data = data)

vif(lm.fit)
# From vif analysis, we remove search_p3m
data <- data[ , !names(data) %in% c("search_p3m")]

########################################################################
###### Cross Validation
########################################################################

library(BAS)
library(Metrics)
library(glmnet)

# The following df is to contain the RMSE for each row in the leave one out cross validation
rmse_df <- data.frame(bma = rep(0, NROW(data))
                      , hpm = rep(0, NROW(data))
                      , mpm = rep(0, NROW(data))
                      , lasso = rep(0, NROW(data)))

# The following is to prepare the regularization terms for lasso, and to prepare
# the input matrix for lasso
grid = 10^seq(10,-2, length=100)
x <- model.matrix(revenue ~ ., data)[,-1]
y <- data$revenue

# The for loop iterates through each row of the input data frame.
# Each row would take turn to become a one-row test set
# The rest of the rows would be used to fit the Bayesian and Lasso models
# The RMSE of the test set is recorded in rmse_df

for(i in c(1:NROW(data))) {
print(i)
train <- data[-i, ]
test <- data[i,]

bas.fit = bas.lm(revenue ~ .
                  , data = train
                  , prior = "ZS-null"
                  , modelprior = uniform()
                  , method = "deterministic")

rmse_df$bma[i] <- rmse(predict(bas.fit, newdata = test, estimator = 'BMA')$fit, test$revenue)
rmse_df$hpm[i] <- rmse(predict(bas.fit, newdata = test, estimator = 'HPM')$fit, test$revenue)
rmse_df$mpm[i] <- rmse(predict(bas.fit, newdata = test, estimator = 'MPM')$fit, test$revenue)


cv.out = cv.glmnet(x[-i,], y[-i], alpha = 1, lambda = grid)
bestlam = cv.out$lambda.min
lasso.mod = glmnet(x[-i,], y[-i], alpha = 1, lambda = grid)

lasso.pred = predict(lasso.mod, newx = matrix((x[i,]), ncol=9), s=bestlam, type = "response")
rmse_df$lasso[i]  <- rmse(lasso.pred, y[i])
}



# T-test in the difference in performance
# Create bar charts with confidence intervals

mean(rmse_df$bma)
mean(rmse_df$hpm)
mean(rmse_df$mpm)
mean(rmse_df$lasso)
mean(data$revenue)
library(reshape2)

rmse_melt <-melt (rmse_df, measure = 1:4)
rmse_melt$value = rmse_melt$value / 10^6

tgc <- summarySE(rmse_melt, measurevar="value", groupvars=c("variable"))
tgc2 <- tgc
tgc2$variable <- factor(tgc2$variable)

ggplot(tgc2, aes(x=variable, y=value)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Method") +
  ylab("RMSE (millions)") +
  ggtitle("Standard Error of RMSE") +
  theme_bw()

t.test(x = rmse_df$mpm, y = rmse_df$lasso)




bas.fit = bas.lm(revenue ~ .
                 , data = data
                 , prior = "ZS-null"
                 , modelprior = uniform()
                 , method = "deterministic")

View(cbind.data.frame(
  variables = c("intercept", names(data)[2:10])
, BMA = coef(bas.fit, estimator = "BMA")$postmean
, HPM = coef(bas.fit, estimator = "HPM")$postmean
, MPM = coef(bas.fit, estimator = "MPM")$postmean
))

cv.out = cv.glmnet(x, y, alpha = 1, lambda = grid)
bestlam = cv.out$lambda.min
lasso.mod = glmnet(x, y, alpha = 1, lambda = bestlam)
predict(lasso.mod, lambda = bestlam, type="coefficients")

summary(bas.fit)
plot(bas.fit, which =1, add.smooth = F)
plot(bas.fit, which = 2, add.smooth = F)
abline(h=1)

plot(bas.fit, which=3)
plot(bas.fit, which=4)

image(bas.fit, rotate =F)

coef.ZS = coef(bas.fit)
plot(coef.ZS)





## SummarySE from http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#Helper%20functions
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}
