library(tidyverse)
tvshows=read.csv('C:/Users/Salman/Desktop/25-assments-code-in-R-main/Tv Shows/tv_shows.csv')


dup=tvshows[!duplicated(tvshows$Title),]
dup[dup$Rotten.Tomatoes=='']
dup$Rotten.Tomatoes=gsub('','0',dup$Rotten.Tomatoes)
dup$IMDb=gsub('','0',dup$IMDb)
dup$IMDb <- ifelse(nchar(dup$IMDb)=="", "0", dup$IMDb)



dup$Rotten.Tomatoes=as.numeric(dup$Rotten.Tomatoes)

typeof(bar)
no_na=dup[which(dup$IMDb!=""),]
no_na=no_na[which(no_na$Rotten.Tomatoes!=""),]
class(dup$IMDb)

bar=no_na[7:11]
bar
bar=data.frame(sum(bar$Netflix),sum(bar$Hulu),sum(bar$Prime.Video),sum(bar$Disney.))

bar=as.numeric(unlist(bar))

barplot(bar , border=F , names.arg=c('netflix','hulu','prime video','disney') , 
                  las=2 , 
                  col=c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6) , rgb(0.3,0.9,0.4,0.6) ,  rgb(0.3,0.9,0.4,0.6)) , 
                  ylim=c(0,2000) , 
                  main="" )

