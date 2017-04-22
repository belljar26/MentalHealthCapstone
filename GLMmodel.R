# GLM model-- using Gender, Family HX, Obs.Consequence, Age, Region
survey_clean5$Gender <- as.factor(survey_clean5$Gender)
survey_clean5$family_history <- as.factor(survey_clean5$family_history)
survey_clean5$mental_health_consequence <- as.factor(survey_clean5$mental_health_consequence)
survey_clean5$region <- as.factor(survey_clean5$region)
survey_clean5$care_options <-as.factor(survey_clean5$care_options)
survey_clean5$treatment <- as.factor(survey_clean5$treatment)
survey_clean5$work_interfere <-as.factor(survey_clean5$work_interfere)
survey_clean5$coworkers <- as.factor(survey_clean5$coworkers)
#split data into training & testing sets
library(caTools)
set.seed(50)
split <- sample.split(survey_clean5$treatment, SplitRatio = .65)
train <- subset(survey_clean5, split == TRUE)
test <- subset(survey_clean5, split == FALSE)
MHmod1 <- glm(treatment ~ Gender + Age + care_options + work_interfere + region + mental_health_consequence +
                family_history, data= train, family="binomial")
summary(MHmod1)
# significant variables are Gender, Age, Knowledge of Care Options, Family HX, Work Interference
# run model on unseen data
predict.test <- predict(MHmod1, type="response", newdata= test)
table(test$treatment, predict.test > .5)

(165 + 199)/ (165 + 53 + 24 + 199)
# accuracy .8253 on test (unseen) data
#sensitivity  or true positive rate = TP/TP + FN
199/(199 + 24)
# sensitivity = .89
# baseline 50% (637 cases or 50% seek help)
library(ROCR)
ROCRpred <- prediction(predict.test, test$treatment)
as.numeric(performance(ROCRpred, "auc") @y.values)
#AUC= 88.70
MHmod2 <- glm(treatment ~ Gender + Age + care_options + work_interfere +
                 family_history, data= train, family= "binomial")
summary(MHmod2)
MH2.test <- predict(MHmod2, type= "response", newdata = test)
table(test$treatment, MH2.test > .5)

# MHmod 2confusion matrix accuracy decreases slightly
(164 + 198)/ (164 + 54 + 25 + 198)
# .820
#Sensivity .887
198/(198 + 25)
# AIC mod 2 = 650.3 AIC mod 1 = 645-- AIC mod 2 is slightly better
ROCRpredMH2 <- prediction(MH2.test, test$treatment)
as.numeric(performance(ROCRpredMH2, "auc") @y.values)
# AUC 88.9
write_csv(survey_clean5, "survey_clean5.csv")
