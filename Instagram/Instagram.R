library(tidyverse)
library(scales)


data = read.csv("C:/Users/Salman/Desktop/25-assments-code-in-R-main/Instagram/Instagram.csv")
head(data)

colSums(is.na(data))

data <- drop_na(data)

summary(data)

ggplot(data, aes(x=`From.Home`, color = "density")) + geom_histogram(aes(y = ..density..), bins=9, fill = '#67B7D1', alpha = 0.5) +
  geom_density(aes(color=`From.Home`)) +  
  geom_density(color = 'blue') + 
  scale_color_manual(values = c('density' = '#67B7D1')) +
  ggtitle("Distribution of Impressions From Home") + theme(plot.title = element_text(hjust = 0.5))

ggplot(data, aes(x=`From.Hashtags`, color = "density")) + geom_histogram(aes(y = ..density..), bins=25, fill = '#67B7D1', alpha = 0.5) +
  geom_density(aes(color=`From.Hashtags`)) +  
  geom_density(color = 'blue') + 
  scale_color_manual(values = c('density' = '#67B7D1')) +
  ggtitle("Distribution of Impressions From Home") + theme(plot.title = element_text(hjust = 0.5))

ggplot(data, aes(x=`From.Explore`, color = "density")) + geom_histogram(aes(y = ..density..), bins=25, fill = '#67B7D1', alpha = 0.5) +
  geom_density(aes(color=`From.Explore`)) +  
  geom_density(color = 'blue') + 
  scale_color_manual(values = c('density' = '#67B7D1')) +
  ggtitle("Distribution of Impressions From Home") + theme(plot.title = element_text(hjust = 0.5))

home = sum(data$From.Home)
hashtags = sum(data$From.Hashtags)
explore = sum(data$From.Explore)
other = sum(data$From.Other)

labels = c('From Home','From Hashtags','From Explore','Other')
values = c(home, hashtags, explore, other)

dfm = as.data.frame(cbind(labels, values))

dfm$values <- as.numeric(dfm$values)
sum_of_obsrv <- sum(dfm$values)

ggplot(dfm, aes(x="", y=values, fill=labels)) +
  geom_col() +
  geom_label(aes(label = percent(values/sum_of_obsrv)),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  coord_polar(theta = "y") + 
  theme_void() + 
  ggtitle("Impressions on Instagram Posts From Various Sources") + theme(plot.title = element_text(hjust = 0.5))


ggplot(data, aes(x=Impressions, y=Likes)) + geom_point(color="blue", size=3) +
  geom_smooth(method = "lm", color ='blue',se = FALSE) +
ggtitle("Relationship Between Likes and Impressions") + theme(plot.title = element_text(hjust = 0.5))


ggplot(data, aes(x=Impressions, y=Comments)) + geom_point(color="blue", size=3) +
  geom_smooth(method = "lm", color ='blue',se = FALSE) +
  ggtitle("Relationship Between Comments and Impressions") + theme(plot.title = element_text(hjust = 0.5))

ggplot(data, aes(x=Impressions, y=Shares)) + geom_point(color="blue", size=3) +
  geom_smooth(method = "lm", color ='blue',se = FALSE) +
  ggtitle("Relationship Between Shares and Impressions") + theme(plot.title = element_text(hjust = 0.5))

ggplot(data, aes(x=Impressions, y=Saves)) + geom_point(color="blue", size=3) +
  geom_smooth(method = "lm", color ='blue',se = FALSE) +
  ggtitle("Relationship Between Saves and Impressions") + theme(plot.title = element_text(hjust = 0.5))

my_cor_data <- data[, sapply(data, is.numeric)]

cor(my_cor_data)

conversion_rate = (sum(data$Follows) / sum(data$Profile.Visits)) * 100
print(conversion_rate)

ggplot(data, aes(x=Profile.Visits, y=Follows)) + geom_point(color="blue", size=3) +
  geom_smooth(method = "lm", color ='blue',se = FALSE) +
  ggtitle("Relationship Between Profile Visits and Followers Gained") + theme(plot.title = element_text(hjust = 0.5))

