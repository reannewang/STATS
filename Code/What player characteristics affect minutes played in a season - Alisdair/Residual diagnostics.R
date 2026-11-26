#Residuals vs fitted
plot(FinalModel,which = 1)

#Q-Q plot
plot(FinalModel,which = 2)

#Scale location plot
plot(FinalModel,which = 3)

#Residuals vs leverage
plot(FinalModel,which = 5)

players$residuals <- resid(FinalModel)
# Calculate residual variance for each position and training load combination
players$variance <- ave(
  players$residuals,
  players$position,
  players$training_load,
  FUN = var
)

# Calculate weights
players$weights <- 1 / players$variance

# Fit weighted least squares model
WLSModel <- lm(
  minutes_played ~ position + training_load,
  data = players,
  weights = weights
)

# View results
summary(WLSModel)

