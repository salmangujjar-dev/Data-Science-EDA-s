library(dplyr)
library(ggplot2)

data = read.csv("C:/Users/Salman/Desktop/25-assments-code-in-R-main/IPL 2022/IPL 2022.csv")

head(data)

ggplot(data, aes(x = match_winner)) + geom_histogram(stat = "count", fill="sky blue") +
  ggtitle("Number of Matches Won in IPL 2022") + theme(plot.title = element_text(hjust = 0.5))



won_by = table(data$won_by)
won_by = as.data.frame(won_by)

won_by$Var1 = as.character(won_by$Var1)

won_by$Var1 <- replace(won_by$Var1, won_by$Var1 == "Wickets", "Chasing")
won_by$Var1 <- replace(won_by$Var1, won_by$Var1 == "Runs", "Defending")


ggplot(won_by, aes(x="", y=Freq, fill=Var1)) +
  geom_col() +
  coord_polar(theta = "y") +
  geom_label(aes(label = Freq),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  theme_void() +
  ggtitle("Number of Matches Won By Defending Or Chasing") + theme(plot.title = element_text(hjust = 0.5))

toss = table(data$toss_decision)
toss = as.data.frame(toss)


ggplot(toss, aes(x="", y=Freq, fill=Var1)) +
  geom_col() +
  coord_polar(theta = "y") +
  geom_label(aes(label = Freq),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  theme_void() +
  ggtitle("Toss Decision") + theme(plot.title = element_text(hjust = 0.5))

ggplot(data, aes(x = top_scorer)) + geom_histogram(stat = "count", fill="sky blue") +
  ggtitle("Top Scorers in IPL 2022") + theme(plot.title = element_text(hjust = 0.5))

ggplot(data, aes(x = top_scorer, y = highscore, fill=highscore)) + geom_bar( stat="identity") +
  ggtitle("Top Scorers in IPL 2022") + theme(plot.title = element_text(hjust = 0.5))

ggplot(data, aes(x = player_of_the_match)) + geom_histogram(stat = "count", fill="sky blue") +
  ggtitle("Most Player of the Match Awards") + theme(plot.title = element_text(hjust = 0.5))

ggplot(data, aes(x = best_bowling)) + geom_histogram(stat = "count", fill="sky blue") +
  ggtitle("Best Bowlers in IPL 2022") + theme(plot.title = element_text(hjust = 0.5))



dfm <- pivot_longer(data, c('first_ings_wkts', 'second_ings_wkts'), names_to="variable", values_to="value")

ggplot(dfm, aes(x = `venue`, y = value)) + geom_bar(aes(fill = variable), stat="identity", position = "dodge") +
  ggtitle("Countries with Highest Deaths") + theme(plot.title = element_text(hjust = 0.5))


