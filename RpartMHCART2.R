# Rpart on Mental Health Data set
# subset data set
MHTree <- survey_clean5[, c(5:6, 9:30)]
View(MHTree)
set.seed(88)
splrpart <-sample.split(MHTree$treatment, SplitRatio = .70)
TrainMHTree <- subset(MHTree, splrpart== TRUE)
TestMHTree <-subset(MHTree, splrpart == FALSE)
library(rpart)
library(rpart.plot)
MHTreeMod <- rpart(treatment ~ ., data=TrainMHTree, 
                   method= "class")
prp(MHTreeMod)
predictMHTree <-predict(MHTreeMod, newdata= TestMHTree, type= "class")
table(TestMHTree$treatment, predictMHTree)
(146 + 140)/ (146 +41 + 51 + 140)
#.756 accuracy
library(ROCR)
PredROCRMH <- predict(MHTreeMod, newdata = TestMHTree)
predMH1 <- prediction(PredROCRMH[,2], TestMHTree$treatment)
perfMH <- performance(predMH1, "tpr","fpr")
plot(perfMH)
write_csv(MHTree, "MHTree.csv")
