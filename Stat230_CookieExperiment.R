#reading the cookie data
cookie <- read.csv("C:\\Users\\alyss\\Downloads\\CookieData - Sheet1.csv", header = T, sep = ",")
cookie

#first 6 rows of the data
head(cookie)

#make into factors
cookie$temp <- as.factor(cookie$temp)
cookie$ingredient <- as.factor(cookie$ingredient)

#means and sd of ingredient and temp
tapply(cookie$size, cookie$temp, mean)
tapply(cookie$size, cookie$temp, sd)
tapply(cookie$size, cookie$ingredient, mean)
tapply(cookie$size, cookie$ingredient, sd)

#anova with interaction
cookie.lm <- lm(size ~ ingredient * temp, data = cookie)
anova(cookie.lm)

#boxplot of data
library(ggplot2)
ggplot(cookie, aes(x = size, y = ingredient))+
  geom_boxplot()
hist(cookie$size, main = "Cookie Size", xlab = "Diameter (cm)", col = c("darkseagreen2", "lightpink2", "skyblue2", "plum3"))

#within variance calculation
fit <- aov(size ~ temp + ingredient, data = cookie)
within.var <- deviance(fit) / df.residual(fit)
within.var

#interaction plot between temp and ingredient
interaction.plot(x.factor = cookie$ingredient, trace.factor = cookie$temp, response = cookie$size, fun = mean, xlab = "Ingredient", ylab = "Size", trace.label = "Temperature")
interaction.plot(x.factor = cookie$temp, trace.factor = cookie$ingredient, response = cookie$size, fun = mean, xlab = "Temperature", ylab = "Size", trace.label = "Ingredient", col = c("black", "red", "blue"))

#Mean comparisons for sample size 17

ttemp <- (5.910784 - 5.623529) / sqrt(((1/17)+ (1/17)) * 0.069361)
ttemp
1 - pt(ttemp, df = 5, lower.tail = T)

tmixsoda <- (5.919118 - 6.038235) / sqrt(((1/17)+ (1/17)) * 0.069361)
tmixsoda
pt(tmixsoda, df = 5, lower.tail = T)

tsodapowder <- (6.038235 - 5.344118) / sqrt(((1/17)+ (1/17)) * 0.069361)
tsodapowder
1 - pt(tsodapowder, df = 5, lower.tail = T)

tmixpowder <- (5.919118 - 5.344118) / sqrt(((1/17)+ (1/17)) * 0.069361)
tmixpowder
1 - pt(tmixpowder, df = 5, lower.tail = T)

#CORRECT ANALYSIS WITH SAMPLE SIZE 6

library(tidyverse)

cookie <- read_csv("C:\\Users\\alyss\\Downloads\\CookieData - Sheet1.csv")

#Average of each factor to create sample size 6
cookie %>%
  group_by(temp, ingredient) %>%
  summarise(mean = mean(size)) %>%
  rename(size = mean) %>%
  select(size, everything()) -> cookie

#anova for sample size 6
cookie_lm <- lm(size ~ temp + ingredient, data = cookie)
anova(cookie_lm)

#Effect sizes
aggregate(size ~ temp, data = cookie, FUN = mean)
5.910784 - mean(cookie$size)
5.623529- mean(cookie$size)

aggregate(size ~ ingredient, data = cookie, FUN = mean)
5.919118 - mean(cookie$size)
5.344118 - mean(cookie$size)
6.038235 - mean(cookie$size)

#mean comparisons
ttemp <- (5.910784 - 5.623529) / sqrt(2 * 0.069361)
ttemp
1 - pt(ttemp, df = 5, lower.tail = T)


tmixsoda <- (5.919118 - 6.038235) / sqrt(2 * 0.069361)
tmixsoda
pt(tmixsoda, df = 5, lower.tail = T)

tsodapowder <- (6.038235 - 5.344118) / sqrt(2 * 0.069361)
tsodapowder
1 - pt(tsodapowder, df = 5, lower.tail = T)

tmixpowder <- (5.919118 - 5.344118) / sqrt(2 * 0.069361)
tmixpowder
1 - pt(tmixpowder, df = 5, lower.tail = T)
