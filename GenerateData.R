


install.packages("devtools")

library(devtools)

install_github("google/amss")



library(amss)
n.years <- 4
time.n <- n.years * 52


activity.transition <- matrix(
  c(0.60, 0.30, 0.10,  # migration originating from inactive state
    0.60, 0.30, 0.10,  # exploratory state
    0.60, 0.30, 0.10),  # purchase state
  nrow = length(kActivityStates), byrow = TRUE)
favorability.transition <- matrix(
  c(0.03, 0.07, 0.65, 0.20, 0.05,  # migration from the unaware state
    0.03, 0.07, 0.65, 0.20, 0.05,  # negative state
    0.03, 0.07, 0.65, 0.20, 0.05,  # neutral state
    0.03, 0.07, 0.65, 0.20, 0.05,  # somewhat favorable state
    0.03, 0.07, 0.65, 0.20, 0.05),  # favorable state
  nrow = length(kFavorabilityStates), byrow = TRUE)


# a sinusoidal pattern
market.rate.nonoise <-
  SimulateSinusoidal(n.years * 52, 52,
                     vert.trans = 0.6, amplitude = 0.25)
# with some added noise
market.rate.seas <- pmax(
  0, pmin(1,
          market.rate.nonoise *
            SimulateAR1(length(market.rate.nonoise), 1, 0.1, 0.3)))

nat.mig.params <- list(
  population = 2.4e8,
  market.rate.trend = 0.68,
  market.rate.seas = market.rate.seas,
  # activity states for newly responsive (in-market & un-satiated)
  prop.activity = c(0.375, 0.425, 0.2),
  # brand favorability, initial proportions.
  prop.favorability = c(0.03, 0.07, 0.65, 0.20, 0.05),
  # everyone is a switcher
  prop.loyalty = c(1, 0, 0),
  transition.matrices = list(
    activity = activity.transition,
    favorability = favorability.transition))


budget.index <- rep(1:n.years, each = 52)

tv.flighting <-
  pmax(0,
       market.rate.seas +
         SimulateAR1(length(market.rate.seas), -0.7, 0.7, -0.7))
tv.flighting <- tv.flighting[c(6:length(tv.flighting), 1:5)]


tv.activity.trans.mat <- matrix(
  c(1.00, 0.00, 0.00,  # migration originating from the inactive state
    0.00, 1.00, 0.00,  # exploratory state
    0.00, 0.00, 1.00),  # purchase state
  nrow = length(kActivityStates), byrow = TRUE)
tv.favorability.trans.mat <- matrix(
  c(0.4,  0.0,  0.4, 0.2, 0.0,  # migration from the unaware state
    0.0,  0.9,  0.1, 0.0, 0.0,  # negative state
    0.0,  0.0,  0.6, 0.4, 0.0,  # neutral state
    0.0,  0.0,  0.0, 0.8, 0.2,  # somewhat favorable state
    0.0,  0.0,  0.0, 0.0, 1.0),  # favorable state
  nrow = length(kFavorabilityStates), byrow = TRUE)


params.tv <- list(
  audience.membership = list(activity = rep(0.4, 3)),
  budget = rep(c(545e5, 475e5, 420e5, 455e5), length = n.years),
  budget.index = budget.index,
  flighting = tv.flighting,
  unit.cost = 0.005,
  hill.ec = 1.56,
  hill.slope = 1,
  transition.matrices = list(
    activity = tv.activity.trans.mat,
    favorability = tv.favorability.trans.mat))


cpc.min <- 0.8
cpc.max <- 1.1


# uncapped spend, shut off the first 2 of every 13 weeks
spend.cap.fn <- function(time.index, budget, budget.index) {
  if ((time.index %% 13) > 1) {
    return(Inf)
  } else {
    return(0)
  }
}


bid.fn <- function(time.index, per.capita.budget, budget.index) {
  return(1.1)
}

kwl.fn <- function(time.index, per.capita.budget, budget.index) {
  return(4.5 * per.capita.budget)
}


search.activity.trans.mat <- matrix(
  c(0.05, 0.95, 0.00,  # starting state: inactive
    0.00, 0.85, 0.15,  # starting state: exploratory
    0.00, 0.00, 1.00),  # starting: purchase
  nrow = length(kActivityStates), byrow = TRUE)
search.favorability.trans.mat <- matrix(
  c(1.0, 0.0, 0.0, 0.0, 0.0,  # unaware
    0.0, 1.0, 0.0, 0.0, 0.0,  # negative
    0.0, 0.0, 1.0, 0.0, 0.0,  # neutral
    0.0, 0.0, 0.0, 1.0, 0.0,  # favorable
    0.0, 0.0, 0.0, 0.0, 1.0),  # loyal
  nrow = length(kFavorabilityStates), byrow = TRUE)


params.search <- list(
  audience.membership = list(activity = c(0.01, 0.3, 0.4)),
  budget = (2.4e7 / n.years) * (1:n.years),
  budget.index = budget.index,
  spend.cap.fn = spend.cap.fn,
  bid.fn = bid.fn,
  kwl.fn = kwl.fn,
  query.rate = 1,
  cpc.min = cpc.min,
  cpc.max = cpc.max,
  ctr = list(activity = c(0.005, 0.08, 0.10)),
  relative.effectiveness = c(0, 0.1, 1),
  transition.matrices = list(
    activity = search.activity.trans.mat,
    favorability = search.favorability.trans.mat))


sales.params <- list(
  competitor.demand.max = list(loyalty = c(0.8, 0, 0.8)),
  advertiser.demand.slope = list(favorability = rep(0, 5)),
  advertiser.demand.intercept = list(
    favorability = c(0.014, 0, 0.2, 0.3, 0.9)),
  price = 80)


sim.data <- SimulateAMSS(
  time.n = time.n,
  nat.mig.params = nat.mig.params,
  media.names = c("tv", "search"),
  media.modules = c(
    `DefaultTraditionalMediaModule`,
    `DefaultSearchMediaModule`),
  media.params = list(params.tv, params.search),
  sales.params = sales.params)

burn.in.length <- 52
final.year.end <- n.years * 52
final.year.start <- final.year.end - 51
observed.data <- sim.data$data[(burn.in.length + 1):final.year.end, ]

observed.data[,
              market.rate :=
                market.rate.seas[(burn.in.length + 1):final.year.end]]

dirname(rstudioapi::getSourceEditorContext()$path)
fName = paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/ObservedData.csv')


write.csv(observed.data, file = fName)












