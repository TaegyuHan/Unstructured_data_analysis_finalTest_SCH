
# --------------------------------------------------------------------------- #
# 데이터 확인
nrow(statisticsPreProcessTrainData)
nrow(statisticsPreProcessTestData)

nrow(Peakdataframe)
nrow(PeakTestdataframe)

# > nrow(statisticsPreProcessTrainData)
# [1] 5264
# > nrow(statisticsPreProcessTestData)
# [1] 1331
# > 
#   > nrow(Peakdataframe)
# [1] 5264
# > nrow(PeakTestdataframe)
# [1] 1331
# --------------------------------------------------------------------------- #


# --------------------------------------------------------------------------- #
# 
nameslist <- c()

for (i in 85:75){
  nameslist <- c(nameslist, paste0("nrow",i))
}

PeakstatisticTrainData <- cbind(statisticsPreProcessTrainData[c("mean", "min")],
                                Peakdataframe[nameslist],
                                event = Peakdataframe$event)

PeakstatisticTestData <- cbind(statisticsPreProcessTestData[c("mean", "min")],
                               PeakTestdataframe[nameslist],
                               event = PeakTestdataframe$event)

# save(PeakstatisticTrainData, file = "PeakstatisticTrainData.RData")
# save(PeakstatisticTestData, file = "PeakstatisticTestData.RData")

# --------------------------------------------------------------------------- #


# --------------------------------------------------------------------------- #
# RF 모델

# 모델 호출
RF <- RWeka::make_Weka_classifier("weka/classifiers/trees/RandomForest")

RFModelPKST <- RF(as.factor(event)~., data=PeakstatisticTrainData)

summary(RFModelPKST)

Folds10 <- evaluate_Weka_classifier(RFModelPKST, 
                                    numFolds = 10, complexity = TRUE, class = TRUE)

# 모델 저장
# setwd(MODEL_PATH)
# .jcache(RFModelPKST$classifier)
# save(RFModelPKST, file="RFModelPKST.rda")


# 예측
predPKST <- predict(RFModelPKST, newdata = PeakstatisticTestData[1:13])


showRFPKST <- predShowConfusionMatrix(PeakstatisticTestData$event, predPKST)

# 이미지 저장
saveggplot( plot = AllDataPKST, fileName = "showRFPKST", width = 600, height = 500)


# --------------------------------------------------------------------------- #


# --------------------------------------------------------------------------- #
# NB 모델

# 모델 호출

NBModelPKST <- naiveBayes(as.factor(event)~., data=PeakstatisticTrainData)

summary(NBModelPKST)

# 모델 저장
# setwd(MODEL_PATH)
# save(NBModelPKST, file="NBModelPKST.rda")

# 예측
predNBPKST <- predict(NBModelPKST, newdata = PeakstatisticTestData[1:13])


showNBPKST <- predShowConfusionMatrix(PeakstatisticTestData$event, predNBPKST)

# 이미지 저장
saveggplot( plot = showNBPKST, fileName = "showNBPKST", width = 600, height = 500)

# --------------------------------------------------------------------------- #








