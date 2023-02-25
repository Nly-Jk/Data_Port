library(emmeans)
library(psych)
library(carData)

lab13.lm <- lm(stuen ~ learnenga + skilldev + learnenga:skilldev, data = lab13.data) 
summary(lab13.lm)

skilldev_minus1 <- mean(lab13.data$skilldev) - sd(lab13.data$skilldev)
skilldev_mean <- mean(lab13.data$skilldev)
skilldev_plus1 <- mean(lab13.data$skilldev) + sd(lab13.data$skilldev)
locations <- list(skilldev = c(skilldev_minus1, skilldev_mean, skilldev_plus1))
locations

emtrends(lab13.lm, ~ skilldev , var = "learnenga", at = locations)

xy_value <- list(learnenga = c(seq(min(lab13.data$learnenga), max(lab13.data$learnenga),
                                   by = 0.5)),
                 skilldev = c(skilldev_minus1, skilldev_mean, skilldev_plus1))

emmip(lab13.lm, skilldev ~ learnenga, at = xy_value, CIs = TRUE)

source("process.R")
process(data = lab13.data, y = "stuen", x = "learnenga", w = "skilldev", model = 1)

skilldev1 <- 3.3750
skilldev2 <- 4.0000
skilldev3 <- 4.4100
locations2 <- list(skilldev = c(skilldev1, skilldev2, skilldev3))
locations2
emtrends(ส.lm, ~ skilldev , var = "learnenga", at = locations2)

confint(lab13.lm)


