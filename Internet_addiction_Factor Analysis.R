library(psychTools)
library(Hmisc)
library(psych)
library(QuantPsyc)
library(coefficientalpha)
library(qgraph)
library(dplyr)

View(iat_data)
iat_data <- psy_IAT
describe(iat_data)
iat_data$ave <- rowMeans(iat_data[3:37])
describe(iat_data$ave)

summary(iat_data$sex)

sd(iat_data$ave)
iat_ans <- iat_data[3:37]

#Check outlier
outlier_3sd <- function(var){
  m <- mean(var)
  s <- sd(var)
  loc <- which(var < m - (3*s) | var > m + (3*s))
  value <- var[loc]
  output <- cbind(loc, value)
  colnames(output) <- c("Location", "Outlier_Values")
  return(output)
}

for (i in 1:35) {
  outlier_3sd(iat_ans[,i])
}

fa.parallel(iat_ans, fm = "pa", fa = "fa")
---------------------------------------------------------------------------------
iat3 <- fa(iat_ans, fm = "pa", nfactors = 3, rotate = "Promax")
print(iat3,cut = .10, sort = TRUE)

#case 3 fac
#fac1_3 <- c("X8","X31","X21","X24","X23","X7","X34","X30","X1","X22","X13")
#fac2_3 <- c("X12","X25","X15","X35","X16")
#fac3_3 <- c("X19","X17","X26","X3","X4","11","6")
#cross = 29,9,18,14,33
#Load ไม่ถึง .35 = 10
#load ผิด Fac = 5
#iat_cut3 <- iat_ans[,-c(29,9,18,14,33,10,5,2,32,27,28,20)]

iat_cut3 <- iat_ans[,-c(29,9,18,14,33,10,32,27,28,2,5,20)]
iat_cut3_fa <- fa(iat_cut3, fm = "pa", nfactors = 3, rotate = "Promax")
print(iat_cut3_fa, cut = .10, sort = TRUE)

iat_cut3_v2 <- iat_ans[,-c(29,9,18,14,33,10,32,27,28,2,5,20,11,13,30)]
iat_cut3_v3 <- iat_ans[,-c(29,9,18,14,33,10,32,27,28,2,5,20,11,13,30,1,22)]

iat_cut3_fa <- fa(iat_cut3, fm = "pa", nfactors = 3, rotate = "Promax")
print(iat_cut3_v2_fa, cut = .10, sort = TRUE)

iat_cut3_v2_fa <- fa(iat_cut3_v2, fm = "pa", nfactors = 3, rotate = "Promax")
print(iat_cut3_v2_fa, cut = .10, sort = TRUE)

iat_cut3_v3_fa <- fa(iat_cut3_v3, fm = "pa", nfactors = 3, rotate = "Promax")
print(iat_cut3_v3_fa, cut = .10, sort = TRUE)
---------------------------------------------------------------------------------
fac1_1 <- c("X8","X31","X21","X24","X23","X7","X34","X30","X1","X22","X13")
fac2_1 <- c("X12","X25","X15","X35","X16")
fac3_1 <- c("X19","X17","X26","X3","X4","11","6")

fac1_2 <- c("X8","X31","X21","X24","X23","X7","X34","X13","X30")
fac2_2 <- c("X12","X25","X15","X35","X16")
fac3_2 <- c("X19","X17","X26","X3","X4","X6","11")
  
fac1_3 <- c("X8","X31","X21","X24","X23","X7","X34")
fac2_3 <- c("X12","X25","X15","X35","X16")
fac3_3 <- c("X19","X17","X26","X3","X4","X6")

iat_data$F1 <- rowMeans(iat_ans[fac1_3])
iat_data$F2 <- rowMeans(iat_ans[fac2_3])
iat_data$F3 <- rowMeans(iat_ans[,fac3_3])
iat_data$v3_ave <- rowMeans((iat_ans[,c(fac1_3,fac2_3,fac3_3)]))

cor.test(iat_data$F1,iat_data$v3_ave)
cor.test(iat_data$F2,iat_data$v3_ave)
cor.test(iat_data$F3,iat_data$v3_ave)

alpha.bef = psych::alpha(iat_ans) 
alpha.fac1_1 = psych::alpha(iat_ans[fac1_1]) 
alpha.fac3_1 = psych::alpha(iat_ans[fac3_1])
alpha.fac1_2 = psych::alpha(iat_ans[fac1_2]) 
alpha.fac3_2 = psych::alpha(iat_ans[fac3_2])
alpha.fac1_3 = psych::alpha(iat_ans[fac1_3]) 
alpha.fac2_3 = psych::alpha(iat_ans[fac2_3]) 
alpha.fac3_3 = psych::alpha(iat_ans[fac3_3])
alpha_cut_3 = psych::alpha(iat_cut3) 
alpha_cut_3_v2 = psych::alpha(iat_cut3_v2) 
alpha_cut_3_v3 = psych::alpha(iat_cut3_v3) 
print(alpha.fac1_1,digits=3)
print(alpha.fac1_2,digits=3)
print(alpha.fac3_1,digits=3)
print(alpha.fac1_3,digits=3)
print(alpha.fac2_3,digits=3)
print(alpha.fac3_3,digits=3)
print(alpha.bef)
print(alpha_cut_3)
print(alpha_cut_3_v2)
print(alpha_cut_3_v3)

omega.iat_ans <- psych::omega(iat_ans,3)
omega.iat_cut3 <- psych::omega(iat_cut3,3)
omega.iat_cut3_v2 <- psych::omega(iat_cut3_v2,3)
omega.iat_cut3_v3 <- psych::omega(iat_cut3_v3,3)

summary(omega.iat_ans, prob = .95)
summary(omega.iat_cut3, prob = .95)
summary(omega.iat_cut3_v2, prob = .95)
summary(omega.iat_cut3_v3, prob = .95)

new_iat$F1 <- iat_cut3_v3 

#Network analysis
groups <- list(fac1_3 <- c("X8","X31","X21","X24","X23","X7","X34","X30","X1","X22","X13"),
               fac2_3 <- c("X12","X25","X15","X35","X16"),
               fac3_3 <- c("X19","X17","X26","X3","X4","X11","X6"))

cor <- cor(iat_cut3)
q.iat_cut3 <- qgraph(cor, layout = "spring",
                      labels = colnames(iat_cut3), groups = groups)

cor.test(iat_ans[fac1_3],iat_ans[fac2_3], method=c("pearson", "kendall", "spearman"))

beck <- c(23,16,16,14,15,15,21,12,9,12,2,24,23)
ave  <- c(3.06,3.72,3.39,3.11,2.72,3.17,3.61,2.50,2.44,4.39,2.17,4.00,2.28)

test_group <- data.frame(beck,ave)
describe(test_group$beck)
beck_use <- Beck[2:16,]
colnames(beck_use) <- c("beck","ave","af1","af2","af3")

cor.test(beck_use$beck,beck_use$af1)
cor.test(beck_use$beck,beck_use$af2)
cor.test(beck_use$beck,beck_use$af3)
cor.test(beck_use$beck,beck_use$ave)
beck_use$beck <- as.numeric(beck_use$beck)
beck_use$ave <- as.numeric(beck_use$ave)
beck_use$af1 <- as.numeric(beck_use$af1)
beck_use$af2 <- as.numeric(beck_use$af2)
beck_use$af3 <- as.numeric(beck_use$af3)

typeof(beck_use$beck)
