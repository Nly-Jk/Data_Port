library(psych)
library(car)
library(carData)



psych::scoreItems(SDA.Lab.W9.Data)

lab9.list <- list(stu_en = c("Emo_1","Emo_2","Emo_3","Emo_4","Phy_1","Phy_2","Phy_3","Phy_4","CogIn_1","CogIn_2","CogIn_3","CogIn_4","CogOut_1","CogOut_2","CogOut_3","CogOut_4"),
                  learn_Sup = c("LearnSup_1","LearnSup_2","LearnSup_3","LearnSup_4","LearnSup_5", "LearnSup_6", "LearnSup_7", "LearnSup_8", "LearnSup_9", "LearnSup_10"),
                  teach_Qual = c("TeachQual_1", "TeachQual_2", "TeachQual_3", "TeachQual_4", "TeachQual_5", "TeachQual_6", "TeachQual_7", "TeachQual_8", "TeachQual_9", "TeachQual_10"),
                  learn_En = c("LearnEnga_1", "LearnEnga_2", "LearnEnga_3", "LearnEnga_4", "LearnEnga_5", "LearnEnga_6"),
                  skill_Dev = c("SkillDev_1", "SkillDev_2", "SkillDev_3", "SkillDev_4", "SkillDev_5", "SkillDev_6", "SkillDev_7", "SkillDev_8"),
                  learning_Res = c("LearnRes_1", "LearnRes_2", "LearnRes_3", "LearnRes_4", "LearnRes_5", "LearnRes_6", "LearnRes_7"))
lab9_scores <- scoreItems(key = lab9.list, items = SDA.Lab.W9.Data, min = 1, max = 6, missing = TRUE, impute = "mean")
head(lab9_scores$scores)

# combine data.frame
lab9 <- cbind(SDA.Lab.W9.Data, lab9_scores$scores) 
head(lab9)

#Regression Model
stu_en.lm <- lm(stu_en ~ learn_Sup + teach_Qual + learn_En + skill_Dev + learning_Res , data = lab9_calculated)
summary(stu_en.lm)

#leverage
predictors <- lab9[c("learn_Sup", "teach_Qual", "learn_En", "skill_Dev", "learning_Res")]
lab9$maha_dis <- psych::outlier(predictors)

lab9$maha_p <- pchisq(lab9$maha_dis, df = 4, lower.tail = FALSE)
lab9[lab9$maha_p < .001, ] 

#leverage_hat
lab9$hat <- hatvalues(stu_en.lm)
head(lab9)
hat_cutoff <- 2*6/nrow(lab9)
high_leverage_cases <- subset(lab9, subset = hat > hat_cutoff)
high_leverage_cases

#Distance
outlierTest(stu_en.lm)

#Influence
lab9$cook <- cooks.distance(stu_en.lm)
subset(lab9, subset = cook > .5)

#Elimination
re_lab9 <- lab9[-c(18,68,102), ]
re.lm <- lm(stu_en ~ learn_Sup + teach_Qual + learn_En + skill_Dev + learning_Res, re_lab9)
summary(re.lm)

#Normality
car::qqPlot(stu_en.lm)

#linearity
crPlots(stu_en.lm)
ncvTest(stu_en.lm)

#Heteroscedasticity
spreadLevelPlot(stu_en.lm)

#Multi-collinearity
vif(stu_en.lm)

#95Ci
confint(stu_en.lm)