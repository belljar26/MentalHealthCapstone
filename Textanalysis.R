TextMH <- read.csv("survey_clean5.csv", stringsAsFactors = FALSE)
View(TextMH)
TextMH <- TextMH[, c(12,31)]
View(TextMH)
# remove NO comment entries
TextMH1 <- subset(TextMH, !comments == "NONE")
View(TextMH1)
str(TextMH1)
# double check to ensure we have 163 comment entries-yes

library(SnowballC)
library(tm)
# Creat corpus
corpusmh <-Corpus(VectorSource(TextMH1$comments))
corpusmh <- tm_map(corpusmh,tolower)
corpusmh <- tm_map(corpusmh, PlainTextDocument)
corpusmh <- tm_map(corpusmh, removePunctuation)
corpusmh <- tm_map(corpusmh, removeWords, stopwords("english"))
corpusmh <- tm_map(corpusmh, stemDocument)
# re run corpus vector source command above before calling MHdtm
MHdtm<- DocumentTermMatrix(corpusmh)
MHdtm
inspect(MHdtm[100:105,100:120])
findFreqTerms(MHdtm, lowfreq = 10)
sparseMH <- removeSparseTerms(MHdtm, .98)
sparseMH
MHsparseDF <- as.data.frame(as.matrix(sparseMH))
View(MHsparseDF)
MHsparseDF$treatment <-TextMH1$treatment
MHsparseDF[, "that"]= NULL
MHsparseDF[, "and"]= NULL
MHsparseDF[,"the"]= NULL
MHsparseDF[, "been"]=NULL
MHsparseDF[, "its"]=NULL
MHsparseDF[,"hence"]= NULL
MHsparseDF[,"are"]=NULL
MHsparseDF[,"this"]= NULL
MHsparseDF[, "was"]=NULL
MHsparseDF[,"with"]=NULL
MHsparseDF[,"were"]=NULL
MHsparseDF[, "did"]=NULL
MHsparseDF[, "have"]= NULL
MHsparseDF[, "our"]= NULL
MHsparseDF[, "get"]=NULL
MHsparseDF[, "is"]=NULL
MHsparseDF[, "which"]=NULL
MHsparseDF[, "for"]=NULL
MHsparseDF[, "how"]=NULL
MHsparseDF[, "but"]=NULL
MHsparseDF[, "also"]=NULL
MHsparseDF[, "their"]=NULL
MHsparseDF[, "who"]=NULL
MHsparseDF[, "has"]=NULL
MHsparseDF[, "just"]=NULL
MHsparseDF[, "lot"]=NULL
MHsparseDF[, "does"]=NULL
MHsparseDF[, "than"]=NULL
MHsparseDF[, "gets"]=NULL
MHsparseDF[, "still"]=NULL
MHsparseDF[, "could"]=NULL
MHsparseDF[, "not"]=NULL
MHsparseDF[, "ever"]=NULL
MHsparseDF[, "he"]=NULL
MHsparseDF[, "she"]=NULL
MHsparseDF[,"want"]=NULL
MHsparseDF[, "doesn"]=NULL

str(MHsparseDF)
head(MHsparseDF)
# build model
library(caTools)
set.seed(32)
splText <-sample.split(MHsparseDF$treatment, SplitRatio = .60)
trainText <- subset(MHsparseDF, splText == TRUE)
testText <-subset(MHsparseDF, splText == FALSE)
library(rpart)
library(rpart.plot)
CARTtext <- rpart(treatment ~ ., data= trainText, method= "class")
prp(CARTtext)
# test model
PredText <- predict(CARTtext, newdata = testText)
PredText[1:10,]
PredTextProb <-PredText[,2]
table(testText$treatment, PredTextProb >.5)
(6+35)/(6 + 17 + 7 + 35)
# 63 percent accuracy on test set
# baseline
table(testText$treatment)
# 23 No, 42 Yes for Treatment
42/(23+42)
#64.6  accuracy
library(ROCR)
predROCRtext <- prediction(PredTextProb, testText$treatment)
perTextROCR <- performance(predROCRtext, "tpr", "fpr")
plot(perTextROCR, colorize= TRUE)
# AUC value
performance(predROCRtext, "auc")@y.values
# AUC 54%
