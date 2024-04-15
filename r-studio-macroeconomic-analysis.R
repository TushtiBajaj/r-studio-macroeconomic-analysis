#Assignment 1
#Dataset link: https://vincentarelbundock.github.io/Rdatasets/csv/AER/GrowthDJ.csv
mydata <- read.csv('GrowthDJ.csv')
colnames(mydata)
rownames(mydata)
mydata <- mydata[,2:11]
library(dplyr)
mydata %>% attach()
typeof(oil)
typeof(inter)
typeof(oecd)
mydata <- mydata %>% rename(internet = inter)
mydata <- mydata %>% mutate(oil = recode_factor(oil, 'yes' = '1', 'no' = '0' ), 
                            internet = recode_factor(internet, 'yes' = '1', 'no' = '0'),
                            oecd = recode_factor(oecd, 'yes' = '1', 'no' = '0'))
levels(mydata$internet)
library(ggplot2)
qplot(x = internet, y = gdpgrowth,geom = "boxplot", data = mydata, xlab = 'Quality of data', 
      ylab = 'Avergae Gdp growth from 1960 to 1985', fill = I("pink"))
mydata <- mydata %>% na.omit()
summary <- mydata %>% group_by(internet) %>% summarise(num.obvs = n(), mean.gdp = round(mean(gdpgrowth), 0),
                                                       sd.gdp = round(sd(gdpgrowth), 0), se.gdp = round(sd(gdpgrowth)/sqrt(num.obvs),0),
                                                       min.gdp = min(gdpgrowth), max.gdp = max(gdpgrowth))
summary
gdp.t.test <- t.test(gdpgrowth ~ internet, data = mydata)
gdp.t.test
gdp.t.test$estimate
gdp.t.test$conf.int
gdp.t.test$statistic
gdp.t.test$p.value
estimate.diff.gdp <- round(gdp.t.test$estimate[1] - gdp.t.test$estimate[2], 1)
estimate.diff.gdp
conf.level <- attr(gdp.t.test$conf.int, "conf.level") * 100
conf.level

#OUTPUT: The estimated average rate of GDP growth is 1.5 percentile points higher in countries with
#quality internet data compared to countries that don't with 95% confidence interval.

#Assignment 2
#Dataset : https://vincentarelbundock.github.io/Rdatasets/csv/AER/TradeCredit.csv
library(tidyverse)
data <- read.csv('TradeCredit.csv')
colnames(data)
nrow(my.data)
data <- data[, 2:8]
my.data <- data
#Ques 1
fit <- lm(gnp ~ trade, data = my.data)
summary(fit)
predicted.values <- predict(fit)
predicted.values
#Ques 2
plot(my.data$gnp,my.data$trade)
#Ques 3
residual.values <- residuals(fit)
residual.values
coefficient.values <- coefficients(fit)
coefficient.values
#Ques 4
plot(fit)
plot(residual.values)
my.data$gnp <- log(my.data$gnp)
my.data$trade <- log(my.data$trade)
fit.2 <- lm(gnp ~ trade, data = my.data)
residual.values.2 <- residuals(fit.2)
plot(residual.values.2)
#Ques 5
fit.3 <- lm(gnp ~ trade + I(trade/price), data = my.data)
plot(fit.3)
