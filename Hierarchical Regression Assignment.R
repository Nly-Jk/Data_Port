library(psych)
library(car)
library(carData)
library(fastDummies)

SDA.Lab.W10.Data$Gender <- factor(SDA.Lab.W10.Data$Gender, levels = c("male","female","lgbtq"), labels = c("male","female","lgbtq"))
str(SDA.Lab.W10.Data$Gender)

psych::scoreItems(SDA.Lab.W10.Data)

lab10.list <- list(stu_en = c("Emo_1","Emo_2","Emo_3","Emo_4","Phy_1","Phy_2","Phy_3","Phy_4","CogIn_1","CogIn_2","CogIn_3","CogIn_4","CogOut_1","CogOut_2","CogOut_3","CogOut_4"),
                  learn_Sup = c("LearnSup_1","LearnSup_2","LearnSup_3","LearnSup_4","LearnSup_5", "LearnSup_6", "LearnSup_7", "LearnSup_8", "LearnSup_9", "LearnSup_10"),
                  teach_Qual = c("TeachQual_1", "TeachQual_2", "TeachQual_3", "TeachQual_4", "TeachQual_5", "TeachQual_6", "TeachQual_7", "TeachQual_8", "TeachQual_9", "TeachQual_10"),
                  learn_En = c("LearnEnga_1", "LearnEnga_2", "LearnEnga_3", "LearnEnga_4", "LearnEnga_5", "LearnEnga_6"),
                  skill_Dev = c("SkillDev_1", "SkillDev_2", "SkillDev_3", "SkillDev_4", "SkillDev_5", "SkillDev_6", "SkillDev_7", "SkillDev_8"),
                  learning_Res = c("LearnRes_1", "LearnRes_2", "LearnRes_3", "LearnRes_4", "LearnRes_5", "LearnRes_6", "LearnRes_7"))
lab10_scores <- scoreItems(key = lab10.list, items = SDA.Lab.W10.Data, min = 1, max = 6, missing = TRUE, impute = "mean")
head(lab10_scores$scores)

# combine data.frame
lab10 <- cbind(SDA.Lab.W10.Data, lab10_scores$scores) 
head(lab10)

#Dummy Coding
lab10$genderDum <- ifelse(lab10$gender == "female", 1, 0)
lab10$genderDum2 <- ifelse(lab10$gender == "lgbtq", 1, 0)

lab10 <- dummy_cols(lab10, select_columns = "Gender", remove_first_dummy = TRUE)

#lab10.lm <- lm(stu_en ~ Year + Age + Gender_female + Gender_lgbtq, lab10)
lab10.lm <- lm(stu_en ~ Year + Age + Gender, lab10, contrasts = list(Gender = "contr.treatment"))
summary(lab10.lm)

lab10_2.lm <- lm(stu_en ~ Year + Age + learn_Sup + teach_Qual + learn_En + skill_Dev + learning_Res + Gender, lab10, contrasts = list(Gender = "contr.treatment"))
summary(lab10_2.lm)

lab10R.lm <- lm(stu_en ~  learn_Sup + teach_Qual + learn_En + skill_Dev + learning_Res, lab10)
summary(lab10R.lm)
anova(lab10.lm,lab10_2.lm)

influenceIndexPlot(lab10.lm)
influenceIndexPlot(lab10_2.lm)

vif(lab10_2.lm)

anova(lab10.lm, lab10_2.lm)