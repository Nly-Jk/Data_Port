library(psych)
library(careless)
library(carData)
library(Hmisc)
library(tidyverse)
library(careless)
library(car)
library(fastDummies)

#Factoring categorical data
clean.data$Sex <- as.factor(clean.data$Sex)
clean.data$Sex <- factor(clean.data$Sex, levels = c("male","female","lgbtq","none"))

clean.data$Year <- as.factor(clean.data$Year)
#clean.data$SE <- clean.data$SE/16

#Visualization
ggplot(clean.data, aes(x = SDS, y = SE)) +
  geom_point() + # scatter dots
  geom_smooth(method = lm, se = TRUE) + #add a linear fit line to the previous plot
  theme_classic()

#Hierarchical regression
levels(clean.data$Sex)
media <- dummy_cols(clean.data, select_columns = "Sex", remove_first_dummy = TRUE)
media.lm <- lm(SE ~ Age, media)
media2.lm <- lm(SE ~ Age + Sex_female+ Sex_lgbtq, media)
media3.lm <- lm(SE ~ Age +Sex_female+ Sex_lgbtq+ SDS, media)
summary(media.lm)
summary(media2.lm)
summary(media3.lm)

#Testing ANOVA from 3 models
anova(media.lm, media2.lm, media3.lm)

citation("psych")

#correlation matrix with average score
cordata <- clean.data[,c(2,3,41,42)]
cormat <- clean.data[,c(3,41,42)]
describe(cordata)
sd(cordata$Sex)
sd(cordata$Age)
sd(cordata$SDS)
sd(cordata$SE)

rcorr(as.matrix(cormat))