library(psych)
library(careless)
library(carData)
library(Hmisc)
library(haven)
library(lattice)
library(survival)
library(Formula)
library(ggplot2)
library(tibble)

student_spss <- read_sav("x3800219_Student_Engagement_Data.sav")
student_spss

SE_data <- X3800219_Student_Engagement_Data

view(SE_data)

se_complete <- subset(SE_data, subset = SE_data$Finished == 1)
nrow(se_complete)

se_complete_ans <- subset(se_complete, subset = se_complete$Status == 0) 
nrow(se_complete_ans)

se_consent <- subset(se_complete_ans, subset = se_complete_ans$Consent == 1) 
nrow(se_consent)

boxplot(se_consent$Duration__in_seconds_)

outlier_3sd <- function(var){
  m <- mean(var)
  s <- sd(var)
  loc <- which(var < m - (3*s) | var > m + (3*s))
  value <- var[loc]
  output <- cbind(loc, value)
  colnames(output) <- c("Location", "Outlier_Values")
  return(output)
}
outlier <- outlier_3sd(se_consent$Duration__in_seconds_)
se_outlier <- se_consent[-c(67,121),]

describe(se_outlier)
se_new <- se_outlier[, c(19:78)]
se_new <- add_column(se_new, ID = 1:160, .before = "Year")
describe(se_new)

se_new$Age <- as.numeric(se_new$Age)
se_new$Gender <- factor(se_new$Gender, labels = c("Male", "Female", "LGBT" ) )

perNAage <- rowMeans(is.na(subset(se_new, select = Age)))
se_new[perNAage > .5, ]

perNAgender <- rowMeans(is.na(subset(se_new, select = Gender))) 
se_new[perNAgender > .5, ]

perNAyear <- rowMeans(is.na(subset(se_new, select = Year))) 
se_new[perNAyear > .5, ]

perNAemo <- rowMeans(is.na(subset(se_new, select = 5:8)))
se_new[perNAemo > .5, ]

perNAphy <- rowMeans(is.na(subset(se_new, select = 9:12)))
se_new[perNAphy > .5, ]

perNAcogin <- rowMeans(is.na(subset(se_new, select = 13:16)))
se_new[perNAcogin > .5, ]

perNAcogout <- rowMeans(is.na(subset(se_new, select = 17:20)))
se_new[perNAcogout > .5, ]

perNALS <- rowMeans(is.na(subset(se_new, select = 21:30)))
se_new[perNALS > .5, ]

perNATQ <- rowMeans(is.na(subset(se_new, select = 31:40)))
se_new[perNATQ > .5, ]

perNALE <- rowMeans(is.na(subset(se_new, select = 41:46)))
se_new[perNALE > .5, ]

perNASK <- rowMeans(is.na(subset(se_new, select = 47:54)))
se_new[perNASK > .5, ]

perNALR <- rowMeans(is.na(subset(se_new, select = 55:61)))
se_new[perNALR > .5, ]

tobedeleted <-  perNALR > .5 | perNAgender > .5 | perNAage > .5 |perNAyear > .5|perNAemo > .5|perNAphy > .5|perNAcogin > .5|
                perNAcogout > .5|perNALS > .5|perNATQ > .5|perNALE > .5|perNASK > .5

se_new[tobedeleted, ]
se_tobedelete <- se_new[!tobedeleted, ]

long <- longstring(se_tobedelete)
se_tobedelete[long > 15, ]

se_clean <- se_tobedelete[!long > 15,]

describe(se_clean)

se_impute <- se_clean
begin <- 2
end <- 61

for (i in begin:end) {
  se_impute[,i] <- impute(se_clean[,i], fun = mean)
}
all(complete.cases(se_impute[, begin:end]))

describe(se_impute)

psych::scoreItems(se_impute)

keys.list <- list(Emotional_Engagement = c("Emo_1","Emo_2","Emo_3","Emo_4"),
                  Physical_Engagement =c("Phy_1","Phy_2","Phy_3","Phy_4"),
                  Cognitive_Engagement_in_Class =c("CogIn_1","CogIn_2","CogIn_3","CogIn_4"),
                  Cognitive_Engagement_Out_of_Class =c("CogOut_1","CogOut_2","CogOut_3","CogOut_4"), 
                  Learner_Support= c("LearnSup_1","LearnSup_2","LearnSup_3","LearnSup_4","LearnSup_5", "LearnSup_6", "LearnSup_7", "LearnSup_8", "LearnSup_9", "LearnSup_10"),
                  Teaching_Quality = c("TeachQual_1", "TeachQual_2", "TeachQual_3", "TeachQual_4", "TeachQual_5", "TeachQual_6", "TeachQual_7", "TeachQual_8", "TeachQual_9", "TeachQual_10"),
                  Learner_Engagement = c("LearnEnga_1", "LearnEnga_2", "LearnEnga_3", "LearnEnga_4", "LearnEnga_5", "LearnEnga_6"),
                  Skill_Development = c("SkillDev_1", "SkillDev_2", "SkillDev_3", "SkillDev_4", "SkillDev_5", "SkillDev_6", "SkillDev_7", "SkillDev_8"),
                  Learning_Resource = c("LearnRes_1", "LearnRes_2", "LearnRes_3", "LearnRes_4", "LearnRes_5", "LearnRes_6", "LearnRes_7"))
se_scores <- scoreItems(key = keys.list, items = se_impute, min = 1, max = 6, missing = TRUE, impute = "mean")
head(se_scores$scores)

se_calculated <- cbind(se_impute, se_scores$scores)
head(se_calculated)

describe(se_calculated)

outlier_3sd(se_calculated$Emotional_Engagement) 
outlier_3sd(se_calculated$Physical_Engagement) 
outlier_3sd(se_calculated$Cognitive_Engagement_in_Class) 
outlier_3sd(se_calculated$Cognitive_Engagement_Out_of_Class) 
outlier_3sd(se_calculated$Learner_Support) 
outlier_3sd(se_calculated$Teaching_Quality) 
outlier_3sd(se_calculated$Learner_Engagement) 
outlier_3sd(se_calculated$Skill_Development) 
outlier_3sd(se_calculated$Learning_Resource) 

se_calculated_outlier <- se_calculated[-c(37,71,79,110,117,127),]

describe(se_calculated)
describe(se_calculated_outlier)

hist(se_calculated_outlier[, 62:70])