library(dplyr)
library(ggplot2)
library(scales)

data = read.csv("C:/Users/Salman/Desktop/25-assments-code-in-R-main/Financial Budget/India_budget_2021.csv")

colnames(data) <- c("Department", "Funds_allocated")

data <- drop_na(data)

head(data)


data <- data[c(1,9,12,15,19,24,42,43,44),]
data[nrow(data)+1, ] <- c("Others", 592971.08)

data

ggplot(data, aes(x = Department, y = Funds_allocated)) + geom_histogram(stat="identity", fill="sky blue") +
  ggtitle("Number of Matches Won in IPL 2022") + theme(plot.title = element_text(hjust = 0.5),
                                                       axis.text.x=element_text(size=8, angle=90))

values = data$Funds_allocated
labels = data$Department

df <- as.data.frame(cbind(labels, values))
df$values <- as.numeric(df$values)

sum_of_obsrv <- sum(df$values)


ggplot(df, aes(x="", y=values, fill=labels)) +
  geom_col() +
  geom_label(aes(label = percent(values/sum_of_obsrv)),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  coord_polar(theta = "y") + 
  theme_void() + 
  ggtitle("Percentage of Total Cases and Deaths") + theme(plot.title = element_text(hjust = 0.5))



