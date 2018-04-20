# getBicop is a snippet from https://stats.stackexchange.com/questions/15011/generate-a-random-variable-with-a-defined-correlation-to-an-existing-variables/15035#15035
# returns a data frame of two variables which correlate with a population correlation of rho
# If desired, one of both variables can be fixed to an existing variable by specifying x
getBiCop <- function(n, rho, mar.fun=rnorm, x = NULL, ...) {
  if (!is.null(x)) {X1 <- x} else {X1 <- mar.fun(n, ...)}
  if (!is.null(x) & length(x) != n) warning("Variable x does not have the same length as n!")
  
  C <- matrix(rho, nrow = 2, ncol = 2)
  diag(C) <- 1
  
  C <- chol(C)
  
  X2 <- mar.fun(n)
  X <- cbind(X1,X2)
  
  # induce correlation (does not change X1)
  df <- X %*% C
  
  ## if desired: check results
  #all.equal(X1,X[,1])
  #cor(X)
  
  return(df)
}

set.seed(3)

data = data.frame(revenue = runif(min = 1000, max = 2000, n=52*3))
data$search.spend = getBiCop(n=52*3, rho = 0.0052, x = data$revenue)[,2]*10

cor(data)

data$log_revenue = log(data$revenue+1)
data$log_search.spend = log(data$search.spend+1)
data$search.spend_sq = data$search.spend^2

linear.fit <- lm(revenue ~ search.spend, data = data)
log.fit <- lm(log_revenue ~ search.spend, data = data)
quad.fit <- lm(revenue ~ search.spend + search.spend_sq, data = data)

x_max = 600
y_max  = 4000

search.spend.grid = data.frame(search.spend =seq(from = 0, to= x_max, by=x_max/100))
search.spend.grid$search.spend_sq = search.spend.grid$search.spend ^2

plotdf = data.frame(search.spend.grid)
plotdf$pred_linear <-  predict(linear.fit, search.spend.grid)
plotdf$pred_log <-  exp(predict(log.fit, search.spend.grid))
plotdf$pred_quad <- predict(quad.fit, search.spend.grid)


plot(data$search.spend, data$revenue
     , xlim = c(0,x_max), ylim = c(0,y_max)
     , xlab="Search Spend"
     , ylab="Revenue")
lines (plotdf$search.spend, plotdf$pred_linear, type = 'l',col='red', lwd =2)
lines (plotdf$search.spend, plotdf$pred_log, type = 'l',col='green', lwd =2)
lines (plotdf$search.spend, plotdf$pred_quad, type = 'l',col='purple', lwd =2)

legend("topright"
       , legend=c(paste("log_revenue model, r_sq =", round(summary(log.fit)$r.squared,2))
                  , paste("linear model, r_sq =", round(summary(linear.fit)$r.squared,2))
                  , paste("quadratic model, r_sq =", round(summary(quad.fit)$r.squared,2)))
       ,col=c("red", "green", "purple")
       , lwd=2
       , lty=1
       )

