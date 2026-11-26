#-----------------Setup-----------------
setwd("C:/Users/alisd/OneDrive/Documents/R/Stat modelling")

players <- read.csv("league_players.csv")
matches <- read.csv("ashford_matches.csv")

#Declare these are factors not values
players$position <- as.factor(players$position)
players$injured <- as.factor(players$injured)
players$training_load <- as.factor(players$training_load)


FullModel <- lm(minutes_played ~ age + position + bmi + training_load + prev_injuries + injured, data = players)
summary(FullModel)

SelectInteractionModel <- lm(minutes_played ~ age * position + bmi * position + training_load + prev_injuries * injured + age * injured + bmi * training_load, data = players)
summary(SelectInteractionModel)

anova(FullModel,SelectInteractionModel)
#Interactions not significant enough to keep

#Refining non-interaction model

No_age <- update(FullModel, .~. - age)
No_position <- update(FullModel, . ~ . - position)
No_bmi <- update(FullModel, .~. - bmi)
No_training_load <- update(FullModel, . ~ . - training_load)
No_previous_injuries <- update(FullModel, .~. - prev_injuries)
No_injured <- update(FullModel, .~. - injured)

anova(No_age, FullModel)
anova(No_position, FullModel)
anova(No_bmi, FullModel)
anova(No_training_load, FullModel)
anova(No_previous_injuries, FullModel)
anova(No_injured, FullModel)

#Position and training load are the only significant parameters
#So will now test reduced model with only these against full model

ReducedModel <- lm(minutes_played ~ position + training_load, data=players)
summary(ReducedModel)
anova(ReducedModel,FullModel)

#Reduced model is better

FinalModel<-ReducedModel
coef(FinalModel)
