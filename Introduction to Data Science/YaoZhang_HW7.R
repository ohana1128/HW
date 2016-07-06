library(dplyr)
library(ggplot2)
gdp <- read.csv("gdpDataFiveYear.csv")

#1. Produce a scatter plot of life expectancy "lifeExp" vs GDP "gdpPercap"where each point also displays two other variables the year and the continent. Try at least two different ways to produce such plot. Discuss which one you like better and why.
ggplot(gdp,aes(x=gdpPercap,y=lifeExp,group=year,color=continent))+geom_point()
ggplot(gdp,aes(x=gdpPercap,y=lifeExp,color=continent))+geom_point()+facet_grid(.~year)
#The second one I like most, since it clearly shows the difference among continents and you could also see the trends by years.

#2.Produce a scatter plot of life expectancy "lifeExp" vs GDP "gdpPercap" where each point is colored according to the continent. Add alpha blending. Do you think you need it?
ggplot(gdp,aes(x=gdpPercap,y=lifeExp,color=continent))+geom_point()
ggplot(gdp,aes(x=gdpPercap,y=lifeExp,color=continent,alpha=factor(continent)))+geom_point()
#I don't think I need it, since it didn't improve the way of showing information.

#3.Produce a scatter plot of life expectancy "lifeExp" vs GDP "gdpPercap" where each point has a different mark according to the continent. Add some jitter. Do you think you need it?
ggplot(gdp,aes(x=gdpPercap,y=lifeExp,shape=continent))+geom_point()
ggplot(gdp,aes(x=gdpPercap,y=lifeExp))+geom_jitter(aes(shape=continent))
#I don't think I need it, since it didn't change too much after adding the jitter.

#4.Produce a scatter plot of life expectancy "lifeExp" vs GDP "gdpPercap" where each point has a different mark according to the continent. Add the plot title and Y-axes and X-axes labels.
ggplot(gdp,aes(x=gdpPercap,y=lifeExp,shape=continent))+geom_point()+labs(x="GDP",y="life expectancy")+ggtitle("LifeExap vs. GDP")

#5. Produce a scatter plot of life expectancy "lifeExp" vs GDP "gdpPercap" on different panels which each panel corresponds to a continent.
ggplot(gdp,aes(x=gdpPercap,y=lifeExp))+geom_point()+facet_grid(.~continent)

#6. Produce a scatter plot of life expectancy "lifeExp" vs GDP "gdpPercap" where each point is colored according to the continent. Add simple regression lines by year.Is there a pattern you can comment on?
ggplot(gdp,aes(x=gdpPercap,y=lifeExp,color=continent,group=1))+geom_point()+facet_grid(.~year)+geom_smooth(method="lm",se=FALSE)
#In the year of 1952, the life expectancy has positive correlation with GDP; however,between the year of 1957 and 2002, the life expectancy has negative correlation with GDP increase.

#7. Pick only countries in Africa, Produce a scatter plot of life expectancy "lifeExp" vs GDP "gdpPercap" in 2007 where each point is labeled according to the country.
Africa <- gdp[ which(gdp$continent=="Africa" & gdp$year=="2007"),]
ggplot(Africa,aes(x=gdpPercap,y=lifeExp,label=country))+geom_point()+geom_text()

#8. Produce bar graphs to display the total population by continent, by year. Try two different ways. Discuss the pattern.
ggplot(gdp,aes(x=year,y=pop))+geom_bar(stat="identity")+facet_wrap(~continent)
ggplot(gdp,aes(x=year,y=pop,fill=continent)) + geom_bar(stat="identity",position="dodge")
#Asia has the most population and kept increasing with the fastes speed. Americas and Africa's pupolation also increases steadily. However, the European and Oceania population hardly changed.

#9. Produce piechart(s) to display the total population by continent, by year. Try two different ways. Discuss the pattern.
ggplot(gdp,aes(x=year,y=pop,fill=continent)) + geom_bar(stat="identity",position="dodge")+coord_polar(theta="y")
ggplot(gdp,aes(x=year,y=pop))+geom_bar(stat="identity")+coord_polar(theta="y")+facet_wrap(~continent)
#Asia has the most population

#10. Produce Boxplots of life expectancy in 2007 by continent. Comment
Year2007 <- gdp[ which(gdp$year=="2007"),]
ggplot(Year2007,aes(x=continent,y=lifeExp))+geom_boxplot()
#Each continent has various mean, Oceania has the highest average life expectancy and Africa has the lowest life expectancy. Additionally, Africa and Asia's life expentancy has greater variances then the other three continents.

#11. Produce Density plots of life expectancy in 2007 by continent. Comment
ggplot(Year2007,aes(x=lifeExp))+geom_density(aes(color=continent))
#Afria's life expectancy mainly distributes around 40 to 60; other four continents' life expectancy mainly distribute among 60 to 80. Especially the Europe, mostly around 70-80.

#12. Consider only European countries. Produce a plot to compare life expectancy between countries in year 2007.
Europe <- gdp[ which(gdp$continent=="Europe" & gdp$year=="2007"),]
ggplot(Europe,aes(x=country,y=lifeExp)) + geom_bar(stat="identity")

#13. Get the maximum and minimum of GDP per capita ("gdpPercap" ) for all continents in 2007. Display the spread of GDP per capita within the continents, your choice of graphics.
ggplot(Year2007,aes(x=country,y=gdpPercap,label=country)) + geom_point()+geom_text()

#14. Compute a mean of life expectancy for different years. How is life expectancy changing over time on different continents?
LIFE <- group_by(gdp,continent, year)
LifeExp <- as.data.frame(summarise(LIFE, mean=mean(lifeExp)))
ggplot(LifeExp, aes(x = year, y = mean, group = continent, color=continent))+geom_line()
#Overall,all continents' life expectancy increased. Africa's life expectancy has been static between 1990 and 2000, while other four continents kept increasing all the time.

#15. Find countries with interesting stories. Open-ended, I know, just try your best.
LIFEtry <- group_by(gdp,country,continent, year)
LifeExpTry <- as.data.frame(summarise(LIFEtry, mean=mean(lifeExp)))
ggplot(LifeExpTry, aes(x = year, y = mean, group = country, color=continent))+geom_line()
#one African country looks interesting, so I'll take more look at the African countries.
Africa1 <- gdp[ which(gdp$continent=="Africa"),]
LIFEtryAf <- group_by(Africa1,country,year)
LifeExpTryAf <- as.data.frame(summarise(LIFEtryAf, mean=mean(lifeExp)))
ggplot(LifeExpTryAf, aes(x = year, y = mean, color=country))+geom_line()
#Rwanda's population dropped significantly around 1992
AfricaR <- as.data.frame(gdp[ which(gdp$country=="Rwanda"),])
ggplot(AfricaR, aes(x=year, y=lifeExp))+geom_line()+ggtitle("Rwanda's Population")
#After conducting a brief research on Rwanda's history, I found the history event "Rwanda Genocide" on Wikipedia:The Rwandan Genocide was a genocidal mass slaughter of Tutsi and moderate Hutu in Rwanda by members of the Hutu majority. During the approximate 100-day period from April 7 to mid-July 1994, an estimated 500,000–1,000,000 Rwandans were killed.This explains perfectly they sudden drop of pupolation.
