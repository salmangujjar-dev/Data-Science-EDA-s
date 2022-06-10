library(ggplot2)
library(tidyr)

data = read.csv("C:/Users/Salman/Desktop/25-assments-code-in-R-main/Netflix Data Analysis/netflix_titles.csv",header = T ,encoding = "UTF-8")

dim(data)

colnames(data)

head(data)

df = table(data$rating)
df = as.data.frame(df)
df <- df[-1,]

df$Freq <- as.numeric(df$Freq)

sum_of_obsrv <- sum(df$Freq)

ggplot(df, aes(x="", y=Freq, fill=Var1)) +
  geom_col() +
  geom_label(aes(label = percent(Freq/sum_of_obsrv)),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  coord_polar(theta = "y") + 
  theme_void() + 
  ggtitle("Distribution of Content Ratings on Netflix") + theme(plot.title = element_text(hjust = 0.5))

content = table(data$director)
content <- as.data.frame(content)
content <- content[order(content$Freq, decreasing = T),]
content <- head(content, 6)
content <- content[-1,]


ggplot(content, aes(x = Freq , y = Var1)) + geom_histogram(stat = "identity", fill="sky blue") +
  labs(y="Director", x="Total Content")+
  ggtitle("Top 5 Directors on Netflix") + theme(plot.title = element_text(hjust = 0.5))

actors = table(data$cast)
actors <- as.data.frame(actors)
actors <- actors[order(actors$Freq, decreasing = T),]
actors <- head(actors, 6)
actors <- actors[-1,]


ggplot(actors, aes(x = Freq , y = Var1)) + geom_histogram(stat = "identity", fill="sky blue") +
  labs(y="Actor", x="Total Content")+
  ggtitle("Top 5 Actors on Netflix") + theme(plot.title = element_text(hjust = 0.5))

dfm = data[data$release_year>=2010, ]

ggplot(dfm, aes(x=release_year)) + geom_line(aes(color = type), stat="count") +
  labs(x="Year", y="Total Content") +
  ggtitle("Trend of content produced over the years on Netflix") + theme(plot.title = element_text(hjust = 0.5))




