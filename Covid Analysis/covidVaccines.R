library(ggplot2)
library(dplyr)

covid=read.csv('C:/Users/Salman/Desktop/25-assments-code-in-R-main/Covid Analysis/country_vaccinations.csv')

head(covid)

summary(covid)

table(covid$country)
cases = which(covid$country!='England' & covid$country!='Scotland' & covid$country!='Wales' & covid$country!='Northern Ireland')
covid = covid[cases,]
table(covid$country)

table(covid$vaccines)

df = covid %>% select(vaccines, country)
head(df)

unique(df$vaccines)

dict =list()

for (i in 1:nrow(df)){
  boolean = df$country[i] %in% dict[[df$vaccines[i]]]
  if (boolean == FALSE){
    dict[[df$vaccines[i]]] = append(dict[[df$vaccines[i]]], df$country[i])
  }
}
head(dict)

dfm=head(covid,2000)

ggplot(dfm, aes(x=country)) + geom_bar(stat="count", fill="blue")