#-----------------Setup-----------------
setwd("C:/Users/alisd/OneDrive/Documents/R/Stat modelling")

players <- read.csv("league_players.csv")
matches <- read.csv("ashford_matches.csv")

#Declare these are factors not values
players$position <- as.factor(players$position)
players$injured <- as.factor(players$injured)
players$training_load <- as.factor(players$training_load)


#-----------------EDA-----------------
hist(players$minutes_played,
     main = "Histogram of minutes played.",
     xlab = "Minutes played."
     )

qqnorm(players$minutes_played,
       main = "Normal Q-Q Plot of Minutes Played")

qqline(players$minutes_played, col = "red")

boxplot(players$minutes_played,
        main = "Boxplot of minutes played.",
        xlab = "Minutes played."
        )

boxplot(minutes_played ~ position,
        data = players,
        main = "Minutes played by position.",
        xlab = "Position.",
        ylab = "Minutes played."
)

boxplot(minutes_played ~ injured,
        data = players,
        main = "Playing Time depending on injury.",
        xlab = "Injured.",
        ylab = "Minutes Played."
        )

plot(players$age, players$minutes_played,
     main = "Playing Time vs Age.",
     xlab = "Age.",
     ylab = "Minutes Played."
     )

plot(players$bmi, players$minutes_played,
     main = "Playing Time vs BMI.",
     xlab = "BMI.",
     ylab = "Minutes Played."
     )


boxplot(minutes_played ~ prev_injuries,
        data = players,
        main = "Playing Time by previous injuries.",
        xlab = "Previous injuries.",
        ylab = "Minutes Played."
        )

boxplot(minutes_played ~ training_load,
        data = players,
        main = "Playing Time by Training Load.",
        xlab = "Training Load.",
        ylab = "Minutes Played."
        )

